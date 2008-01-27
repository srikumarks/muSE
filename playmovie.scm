(frame-rate 60.0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Load a movie. Pick one and comment out the others.

;(define m (movie "http://movies.apple.com/movies/independent/the_ten_commandments/the_ten_commandments-h.ref.mov"))
;(define m (movie "http://movies.apple.com/movies/sony/the_bands_visit/the_bands_visit_720p.mov"))
(define m (movie "http://movies.apple.com/movies/us/apple/getamac/apple_getamac_holiday_480x272.mov"))

; Define initial values of movie's display properties.
(define s 1.0)
(define rotx 0.0)
(define roty 0.0)
(define rotz 0.0)
(define x 0.0)
(define y 0.0)
(define z -4.0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The render function.

(define (render)
  (projection (perspective 90.0 1.0 0.1 10.0)
              (model
               (translate x y z)
               (rotate rotx 1 0 0)
               (rotate roty 0 1 0)
               (rotate rotz 0 0 1)
               (model
                (scale s s 1)
                (m 0) ; Displays the movie as a quad
                ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Respond to some keyboard input.

(define (seek m dt) (put m 'time (+ dt (get m 'time))))

(define (key-down k)
  (case k
    ; Press '=' key to zoom in.
    ("=" (anim z (+ z 0.5) 0.5))
    
    ; Press '-' key to zoom out
    ("-" (anim z (- z 0.5) 0.5))

    ; Press right arrow key to step forward by 6 seconds.
    ('right (seek m 6.0))
    
    ; Press left arrow key to step backward by 6 seconds.
    ('left (seek m -6.0)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Rotate the video when the user drags the mouse.

(define (mouse-drag)
  (anim roty (* 360 (- (/ MouseX Width) 1/2)) 0.1))


(define (rotate-y-forever) ; Function to rotate the 'roty' value for ever.
  
  ; Increase the value of roty by 36 degrees over the next 2 seconds.
  (anim roty (+ roty 36) 2)		

  ; After 2 seconds, continue to rotate a bit more.
  ; Doing it this way gives us the chance to animate roty based
  ; on the mouse position as well as in an automatic loop.
  (schedule rotate-y-forever 2)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Introduce the video once it has been loaded.

(watch m (fn (stat)	
           (put m 'rate 1.0)
           (anim z (+ z 2.0) 5)
           (anim rotx (+ rotx 360) 5)
           (schedule rotate-y-forever 5)
           (unwatch m)
           ))