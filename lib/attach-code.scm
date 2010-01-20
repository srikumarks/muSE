; Use (attach-code output-file binary-file scmfile1 scmfile2 ...)
; to attach the given muSE code to the given binary file and
; save the result at the given output-file path.
;
; You can load this pseudo-module using
;    (require attach-code)
(module attach-code ()
	(define (file f flag)
	  (open-file f flag 'binary)
	  (finally (close (the open-file)))
	  (the open-file))

	(define (input-file f) (file f 'for-reading))
	(define (output-file f) (file f 'for-writing))

	(define (file->bytes f)
	  (try (read-bytes (input-file f))))

	(define (append-file-data outport size list-of-files)
	  (case list-of-files
		(() 
		 (write-bytes outport (string->bytes (format ";" (string size "%010lld") " muSEexec")))
		 T)
		((f . fs) 
		 (let ((b (file->bytes f))
			   (prefix (string->bytes (format "(load #" (string (bytes-size b) "%010lld") "[")))
			   (suffix (string->bytes "])")))
		   (write-bytes outport prefix)
		   (write-bytes outport b)
		   (write-bytes outport suffix)
		   (append-file-data outport (+ size (bytes-size prefix) (bytes-size b) (bytes-size suffix)) fs)))))


	(define (main outfile binfile . scmfiles)
		(try (do
			   (write-bytes (output-file outfile) (file->bytes binfile))
			   (append-file-data (the output-file) 0 scmfiles)))))

