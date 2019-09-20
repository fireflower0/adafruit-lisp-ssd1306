(defpackage #:adafruit-lisp-ssd1306/shape
  (:use #:cl
        #:adafruit-lisp-ssd1306/8x8-font
        #:adafruit-lisp-ssd1306/wrapper/wiringpi)
  (:export #:ssd1306-draw-line
           #:ssd1306-draw-rect
           #:ssd1306-draw-fill-rect
           #:ssd1306-draw-circle
           #:ssd1306-draw-fill-circle
           #:ssd1306-draw-triangle
           #:ssd1306-draw-fill-triangle
           #:ssd1306-draw-char
           #:ssd1306-draw-string
           #:ssd1306-draw-bmp
           #:ssd1306-invert-display))
(in-package #:adafruit-lisp-ssd1306/shape)

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defun bresenham-line-algorithm (sx sy dx dy &key (color +white+))
  (let* ((x sx) (y sy)
         (wx (abs (- dx sx)))
         (wy (abs (- dy sy)))
         (diff 0))
    (while (or (/= x dx) (/= y dy))
      (ssd1306-draw-pixel x y :color color)
      (if (>= wx wy)
          (progn
            (if (< sx dx)
                (unless (>= x 127)
                  (incf x))
                (unless (<= x 0)
                  (decf x)))
            (incf diff (ash (- dy sy) 1))
            (if (> diff wx)
                (unless (>= y 63)
                  (incf y)
                  (decf diff (ash wx 1)))
                (unless (<= y 0)
                  (decf y)
                  (incf diff (ash wx 1)))))
          (progn
            (if (< sy dy)
                (unless (>= y 63)
                  (incf y))
                (unless (<= y 0)
                  (decf y)))
            (incf diff (ash (- dx sx) 1))
            (if (> diff wy)
                (unless (>= x 127)
                  (incf x)
                  (decf diff (ash wy 1)))
                (unless (<= x 0)
                  (decf x)
                  (incf diff (ash wy 1)))))))))

(defun ssd1306-draw-line (sx sy dx dy &key (color +white+))
  (when (> sx 127) (setf sx 127))
  (when (> sy 63) (setf sy 63))
  (when (> dy 63) (setf dy 63))
  (unless (and (= sx dx) (= sy dy))
    (let* ((x0 sx) (x1 dx) (y0 sy) (y1 dy) (x 0) (y 0)
           (length-x (abs (- x1 x0)))
           (length-y (abs (- y1 y0))))
      (if (> length-x length-y)
          (let ((deg-y (/ length-y length-x)))
            (if (< x0 x1)
                (do ((i x0 (1+ i))) ((>= i (+ x1 1)))
                  (setf y (abs (+ y0 (round (* (abs (- i x0)) deg-y)))))
                  (ssd1306-draw-pixel i y :color color))
                (do ((i x0 (1- i))) ((< i x1))
                  (setf y (abs (+ y0 (round (* (abs (- i x0)) deg-y)))))
                  (ssd1306-draw-pixel i y :color color))))
          (let ((deg-x (/ length-x length-y)))
            (if (< y0 y1)
                (do ((i y0 (1+ i))) ((>= i (+ y1 1)))
                  (setf x (abs (+ x0 (round (* (abs (- i y0)) deg-x)))))
                  (ssd1306-draw-pixel x i :color color))
                (do ((i y0 (1- i))) ((< i y1))
                  (setf x (abs (+ x0 (round (* (abs (- i y0)) deg-x)))))
                  (ssd1306-draw-pixel x i :color color))))))))

(defun ssd1306-draw-rect (x y w h &key (color +white+))
  (ssd1306-draw-line x y (+ x w) y :color color)
  (ssd1306-draw-line (+ x w) y (+ x w) (+ y h) :color color)
  (ssd1306-draw-line (+ x w) (+ y h) x (+ y h) :color color)
  (ssd1306-draw-line x (+ y h) x y) :color color)

(defun ssd1306-draw-fill-rect (x y w h &key (color +white+))
  (dotimes (n h)
    (ssd1306-draw-line x (+ y n) (+ x w) (+ y n) :color color)))

(defun ssd1306-draw-circle (cx cy r &key (color +white+))
  (let ((x r) (y 0)
        (f (+ (* -2 r) 3)))
    (while (>= x y)
      (ssd1306-draw-pixel (+ cx x) (+ cy y) :color color)
      (ssd1306-draw-pixel (- cx x) (+ cy y) :color color)
      (ssd1306-draw-pixel (+ cx x) (- cy y) :color color)
      (ssd1306-draw-pixel (- cx x) (- cy y) :color color)
      (ssd1306-draw-pixel (+ cx y) (+ cy x) :color color)
      (ssd1306-draw-pixel (- cx y) (+ cy x) :color color)
      (ssd1306-draw-pixel (+ cx y) (- cy x) :color color)
      (ssd1306-draw-pixel (- cx y) (- cy x) :color color)
      (when (>= f 0)
        (decf x)
        (decf f (* 4 x)))
      (incf y)
      (incf f (+ (* 4 y) 2)))))

(defun ssd1306-draw-fill-circle (cx cy r &key (color +white+))
  (dotimes (n r)
    (ssd1306-draw-circle cx cy n :color color)))

(defun ssd1306-draw-triangle (x0 y0 x1 y1 x2 y2 &key (color +white+))
  (ssd1306-draw-line x0 y0 x1 y1 :color color)
  (ssd1306-draw-line x1 y1 x2 y2 :color color)
  (ssd1306-draw-line x2 y2 x0 y0 :color color))

(defun ssd1306-draw-fill-triangle (x0 y0 x1 y1 x2 y2 &key (color +white+))
  (dotimes (n (/ (- x1 x2) 2))
    (ssd1306-draw-line x0 (+ y0 n) (- x1 n) (- y1 n) :color color)
    (ssd1306-draw-line (- x1 n) (- y1 n) (+ x2 n) (- y2 n) :color color)
    (ssd1306-draw-line x0 (+ y0 n) (+ x2 n) (- y2 n) :color color)))

(defun ssd1306-invert-display (flg)
  (command (if flg +ssd1306-invert-display+ +ssd1306-normal-display+)))

(defun ssd1306-draw-char (x y char &key (fill nil))
  (let ((index (- (char-code char) #X20))
        (color (if fill
                   `(,+white+ ,+black+)
                   `(,+black+ ,+white+))))
    (dotimes (count1 8)
      (do ((count2 7 (1- count2))
           (count3 0 (1+ count3)))
          ((< count2 0))
        (if (zerop (ldb (byte 1 count3) (aref font-8x8 index count1)))
            (ssd1306-draw-pixel (+ count2 x) (+ count1 y) :color (first color))
            (ssd1306-draw-pixel (+ count2 x) (+ count1 y) :color (second color)))))))

(defun ssd1306-draw-string (x y str &key (fill nil))
  (let ((len (length str)))
    (loop for char across str
       for i from 0 to len
       do (ssd1306-draw-char (+ (* i 8) x) y char :fill fill))))

(defun open-bmp-file (file-path)
  (with-open-file (s file-path :direction :input :element-type '(unsigned-byte 8))
    (let ((buf (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence buf s)
      buf)))

(defun get-bmp-file-info (bmp-file pos size)
  (let ((bmp-info (reverse (subseq bmp-file pos (+ pos size)))))
    bmp-info))

(defun get-bmp-info-value (bmp-file offset bytes)
  (let ((buf-arr (get-bmp-file-info bmp-file offset bytes))
        base-str
        bmp-info-value)
    (dotimes (count bytes)
      (setf base-str (concatenate 'string base-str
                                  (format nil "~2,'0x" (aref buf-arr count)))))
    (setf bmp-info-value (parse-integer base-str :radix 16))))

(defun create-color (bmp-file base)
  (let* ((red      (ash (aref bmp-file base) -3))
         (green    (ash (aref bmp-file (+ base 1)) -2))
         (blue     (ash (aref bmp-file (+ base 2)) -3))
         (rgb-bit1 (logior (ash red 3) (ash green -3)))
         (rgb-bit2 (logior (ash green -5) blue)))
    (list rgb-bit1 rgb-bit2)))

(defun ssd1306-draw-bmp (file-path)
  (let* ((bmp-file (open-bmp-file file-path))
         (size     (get-bmp-info-value bmp-file 2  4))
         (base     (get-bmp-info-value bmp-file 10 4))
         (width    (get-bmp-info-value bmp-file 18 4))
         (height   (get-bmp-info-value bmp-file 22 4))
         (image    (get-bmp-file-info bmp-file base (- size base)))
         (x (- width 1)) (y 0) (count 0))
    (dotimes (n (* width height))
      (if (equal (create-color image count) '(#XFF #X1F))
          (ssd1306-draw-pixel x y :color +white+)
          (ssd1306-draw-pixel x y :color +black+))
      (if (> x 0)
          (decf x)
          (progn
            (setf x (1- width))
            (incf y)))
      (incf count 3))))
