
(define (path-components path)
  (collect (split (first (split path "?")) "/") (fn (x) (> (string-length x) 0))))

(define (file-name path)
  (first (reverse (path-components path))))

(define (file->object file)
  (object ()
          'type 'file
          'path file
          'name (file-name file)
          'info (whatis file 2)
          'contents (try (do 
                             (open-file file 'for-reading 'binary)
                             (finally (close (the open-file)))
                           (read-bytes (the open-file))))))

(define (folder->object folder)
  (object ()
          'type 'folder
          'path folder
          'name (file-name folder)
          'info (whatis folder 2)
          'contents (join (map (fn (f) (file->object (format folder "/" f)))
                               (list-files (format folder "/*")))
                          (map (fn (f) (folder->object (format folder "/" f)))
                               (list-folders (format folder "/*"))))))


(define (locate-component component tree)
  (if (cons? tree)
      (first (collect tree (fn (f) (= component f.name))))
      (if (= component tree.name)
          tree
          ())))

(define (locate components tree)
  (case components
    ((name) (locate-component name tree))
    ((root . others) 
     (if (cons? tree)
         (locate others (locate-component root tree))
         (if (and (= tree.type 'folder) (= root tree.name))
             (locate others tree.contents)
             ())))))

(define (serve-file file port)
  (http-respond port 200 (list (cons 'Content-Type (name file.info.mime-type))
                               (cons 'Content-Length (bytes-size file.contents))
                               (cons 'Connection "close"))
                T)
  (write-bytes port file.contents))

(define REPO (box))

(define (serve path port)
  (case (locate (path-components path) (REPO))
    (() (http-respond port 404 (list (cons 'Connection "close")) T))
    (thing (case thing.type
             ('file (serve-file thing port))
             (_ (http-respond port 404 (list (cons 'Connection "close")) T))))))


(define (start)
  (with-incoming-connections-to-port 31415
                                     (fn (port client-info)
                                         (let ((r (http-parse port)))
                                           (write r)
                                           (case (first r)
                                             (('GET url _) (serve url port))))
                                         (close port)
                                         T)
                                     (fn ()
                                         (launch "http://127.0.0.1:31415/index.html"))))


(define (main . folder)
  (case folder
                                        ; Call with two arguments - the repo folder and the name of the output exe.
                                        ; The repo will be packaged up into the exe and made ready to be served to
                                        ; a browser.
    ((repo exe)
     (write (open-file "__temp.scm" 'for-writing 'binary)
            (list 'REPO (cons 'list (get (folder->object repo) 'contents))))
     (close (the open-file))
     (system *program* "--exec" exe "__temp.scm")
     (system "del" "__temp.scm"))

                                        ; If passed only the repo folder, serve the repo directly
    ((repo)
     (REPO (get (folder->object repo) 'contents))
     (start))

                                        ; Call with no arguments to start serving whats in the repo.
    (() (start))))

