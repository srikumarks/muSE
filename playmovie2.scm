; This file does exactly the same thing as playmovie.scm,
; but does it using muSE's object system. To do something
; different, it plays two movies.

(frame-rate 60.0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A movie class. Defines a movie that will 
; zoom in and start rotating.
(class Movie ()
  (init-with-url: (fn (self url xyz)
                    (let ((m (movie url)))
                      (-> self 'offset xyz)
                      (-> self 'url url)
                      (-> self 'movie m)
                      (-> self 'pos (dup (0.0 0.0 -4.0))) ; We need to duplicate the data because each 
                                                          ; instance needs its own copy for animation.
                      (-> self 'angles (dup (0.0 0.0 0.0)))
                      (-> self 'scale (dup 1.0))
                      
                      ; Introduce the video once it has been loaded.
                      (watch m (fn (stat) 
                                 ;(message "loaded") 
                                 (<- self :start) (unwatch m))))
                    self))
  
  (:start (fn (self)
            (put .movie 'rate 1.0)
            (let (((x y z) .pos)
                  ((ax ay az) .angles))
              (anim z (+ z 2.0) 5)
              (anim ax (+ ax 360) 5))
            (schedule (fn () (<- self :rotate-y-forever)) 5)))
  
  (:render (fn (self)
             (projection (perspective 90.0 1.0 0.1 10.0)
                         (model (apply translate .offset)
                                (apply translate .pos)
                                (let (((ax ay az) .angles)
                                      (s .scale)
                                      (m .movie))
                                  (rotate ax 1 0 0)
                                  (rotate ay 0 1 0)
                                  (rotate az 0 0 1)
                                  (model (scale s s 1)
                                         (m 0)))))))
  
  (zoom-by: (fn (self dz)
              (let (((x y z) .pos))
                (anim z (+ z dz) 0.5))))
  
  (seek-by: (fn (self dt)
              (let ((m .movie))
                (put m 'time (+ dt (get m 'time))))))
  
  (:rotate-y-forever (fn (self)
                       (let (((ax ay az) .angles))
                         
                         ; Increase the value of roty by 36 degrees over the next 2 seconds.
                         (anim ay (+ ay 36) 2)
                         
                         ; After 2 seconds, continue to rotate a bit more.
                         ; Doing it this way gives us the chance to animate roty based
                         ; on the mouse position as well as in an automatic loop.
                         (schedule (fn () (<- self :rotate-y-forever)) 2))))
  
  (:mouse-drag (fn (self)
                 (let (((ax ay az) .angles))
                   (anim ay (* 360 (- (/ MouseX Width) 1/2)) 0.1))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Load a movie. Pick one and comment out the others.

(define urls '(((-0.5 -0.5 -0.5) "http://movies.apple.com/movies/independent/the_ten_commandments/the_ten_commandments-h.ref.mov")
               ((0.5 0.5 0.5) "http://movies.apple.com/movies/us/apple/getamac/apple_getamac_holiday_480x272.mov")))

(define movies (map (fn ((xyz url)) (<- (new Movie) init-with-url: url xyz)) urls))

; Propagates a method invocation to a list of movies
(define (all-movies . msg+args) (for-each {fn (m) (apply <- (cons m msg+args))} movies))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The render function.

(define (render) (all-movies :render))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Respond to some keyboard input.

(define (key-down k)
  (case k
    ; Press '=' key to zoom in.
    ("=" (all-movies zoom-by: 0.5))
    
    ; Press '-' key to zoom out
    ("-" (all-movies zoom-by: -0.5))
    
    ; Press right arrow key to step forward by 6 seconds.
    ('right (all-movies seek-by: 6.0))
    
    ; Press left arrow key to step backward by 6 seconds.
    ('left (all-movies seek-by: -6.0))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Rotate the video when the user drags the mouse.

(define (mouse-drag) (all-movies :mouse-drag))

