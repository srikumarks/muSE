(frame-rate 60.0)

; Load an image
(define hwimage 
  (image "http://homepage.mac.com/srikumarks/.Pictures/Photo%20Album%20Pictures/2002%2D07%2D11%2004.22.21%20%2D0700/photos0029.jpg"))

; Let s be the scale factor for the image.
; Initially the scale is 0.0
(define s 0.0)

; When the image load is complete, animate
; the scale from 0.0 to 1.0 over 1 second
; and stop monitoring the image state.
(watch hwimage (fn (state) 
                 (anim s 1.0 1.0)
                 (unwatch hwimage)))

; Show the image according to the scale value.
(define (render) 
  (model (scale s s s)
         (hwimage 0)))
