(frame-rate 60.0)

(define images
  ;      x     y     url
  (list (-0.5  -0.5 "http://homepage.mac.com/srikumarks/.Pictures/Photo%20Album%20Pictures/2002%2D07%2D11%2004.22.21%20%2D0700/photos0029.jpg")
        (-0.5 0.0 "http://homepage.mac.com/srikumarks/.Pictures/Photo%20Album%20Pictures/2002%2D07%2D11%2004.22.21%20%2D0700/photos0028.jpg")
        (-0.5 0.5 "http://homepage.mac.com/srikumarks/.Pictures/Photo%20Album%20Pictures/2002%2D07%2D11%2004.22.21%20%2D0700/photos0016.jpg")
        (0.0 0.0  "http://homepage.mac.com/srikumarks/.Pictures/Photo%20Album%20Pictures/2002%2D07%2D11%2004.22.21%20%2D0700/Imagephotos0011.jpg")
        (0.5 -0.5  "http://homepage.mac.com/srikumarks/.Pictures/Photo%20Album%20Pictures/2002%2D07%2D11%2004.22.21%20%2D0700/photos0019.jpg")
        (0.5 0.0  "http://homepage.mac.com/srikumarks/.Pictures/Photo%20Album%20Pictures/2002%2D07%2D11%2004.22.21%20%2D0700/photos0010.jpg")
        (0.5 0.5  "http://homepage.mac.com/srikumarks/.Pictures/Photo%20Album%20Pictures/2002%2D07%2D11%2004.22.21%20%2D0700/photos0020.jpg")))

(define (load-image (x y url))
  (let ((im (image url))
        (imx (dup x))
        (imy (dup y))
        (ims (dup 0.0)))
    (watch im (fn (stat)
                (anim ims 0.25 0.5)
                (unwatch im)))
    (list im imx imy ims)))

(define image-states (map load-image images))

(define (render)
  (for-each (fn ((im x y s))
              (model (translate (* 1.5 x) y 0)
                     (scale s s 1.0)
                     (im 0)))
            image-states))


