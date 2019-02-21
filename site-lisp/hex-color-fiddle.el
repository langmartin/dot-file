;;; failed experiment, playing around with desaturating colors

(defun c-component (color index)
  (string-to-number
   (substring color index (+ 2 index))))

(defun c-desaturate (color multiplier)
  (let* ((r (c-component color 1))
         (g (c-component color 3))
         (b (c-component color 5))
         (L (+ (* 0.3 r)
               (* 0.6 g)
               (* 0.1 b))))
    (concat
     "#"
     (mapconcat '(lambda (c)
                   (format
                    "%02X"
                    (+ c (* multiplier (- L c)))))
                (list r g b)
                ""))))

(c-desaturate "#415575" 2.1)

(c-component "#415575" 2)

(string-to-number (substring "#415575" 5 7) 16)
