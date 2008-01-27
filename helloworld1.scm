(frame-rate 60.0)

; Load an image
(define hwimage 
  (image "http://homepage.mac.com/srikumarks/.Pictures/Photo%20Album%20Pictures/2002%2D07%2D11%2004.22.21%20%2D0700/photos0029.jpg"))

; Display it in every frame
(define (render) (hwimage 0))
