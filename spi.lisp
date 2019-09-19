(defpackage #:adafruit-lisp-ssd1306/spi
  (:use #:cl
        #:adafruit-lisp-ssd1306/constants
        #:adafruit-lisp-ssd1306/8x8-font
        #:adafruit-lisp-ssd1306/wrapper/wiringpi)
  (:export #:ssd1306-init))
(in-package #:adafruit-lisp-ssd1306/spi)

;; Parameters
(defparameter *width*  128)
(defparameter *height* 64)
(defparameter *pages*  (/ *height* 8))
(defparameter *buffer* (make-array `(,(* *width* *pages*))
                                   :initial-element 0))
;; SPI
(defparameter *rst* 23)
(defparameter *dc* 4)
(defparameter *spi-cs* 1)
(defparameter *spi-speed* 8000000)

(defun spi-data-rw (channel data &optional (len (length data)))
  (let ((mp (cffi:foreign-alloc :unsigned-char
                                :count len
                                :initial-contents data)))
    (wiringpi-spi-data-rw channel mp len)
    (let ((rval (loop for i from 0 below len
                   collect (cffi:mem-aref mp :unsigned-char i))))
      (cffi:foreign-free mp)
      rval)))

(defun command (data)
  (digital-write *dc* +low+)
  (spi-data-rw *spi-cs* (list data)))

(defun data (data)
  (digital-write *dc* +high+)
  (spi-data-rw *spi-cs* (list data)))

(defun write-list (func data)
  (dotimes (n (length data))
    (funcall func (aref data n))))

(defun ssd1306-init (&optional (vcc-state +ssd1306-switch-cap-vcc+))
  (wiringpi-setup-gpio)
  (pin-mode *dc* +output+)
  (pin-mode *rst* +output+)
  (wiringpi-spi-setup *spi-cs* *spi-speed*)

  (digital-write *rst* +low+)
  (delay 50)
  (digital-write *rst* +high+)

  (command +ssd1306-display-off+)
  (command +ssd1306-set-display-clock-div+)
  (command #X80) ; the suggested ratio
  (command +ssd1306-set-multi-plex+)
  (command #X3F)
  (command +ssd1306-set-display-offset+)
  (command #X00) ; no offset
  (command (logior +ssd1306-set-start-line+ #X00))
  (command +ssd1306-charge-pump+)
  (if (= vcc-state +ssd1306-external-vcc+)
      (command #X10)
      (command #X14))
  (command +ssd1306-memory-mode+)
  (command #X00)
  (command (logior +ssd1306-seg-remap+ #X01))
  (command +ssd1306-com-scan-dec+)
  (command +ssd1306-set-com-pins+)
  (command #X12)
  (command +ssd1306-set-contrast+)
  (if (= vcc-state +ssd1306-external-vcc+)
      (command #X9F)
      (command #XCF))
  (command +ssd1306-set-pre-charge+)
  (if (= vcc-state +ssd1306-external-vcc+)
      (command #X22)
      (command #XF1))
  (command +ssd1306-set-vcom-detect+)
  (command #X40)
  (command +ssd1306-display-all-on-resume+)
  (command +ssd1306-normal-display+)
  (command +ssd1306-display-on+))

(defun ssd1306-clear ()
  (fill *buffer* 0))

(defun ssd1306-set-contrast (contrast)
  (when (or (< contrast 0) (> contrast 255))
    (error "Contrast must be a value from 0 to 255"))
  (command +ssd1306-set-contrast+)
  (command contrast))

(defun ssd1306-display ()
  (command +ssd1306-column-addr+)
  (command 0)             ; Column start address (0 = reset)
  (command (- *width* 1)) ; Column end address
  (command +ssd1306-page-addr+)
  (command 0)             ; Page start address (0 = reset)
  (command (- *pages* 1)) ; Page end address
  (do ((i 0 (+ i 16)))
      ((= i (length *buffer*)))
    (write-list #'data (subseq *buffer* i (+ i 16)))))

(defun ssd1306-clear-display ()
  (ssd1306-clear)
  (ssd1306-display))

(defun ssd1306-draw-pixel (x y &key (color +white+))
  (when (or (< x 0) (> x 127))
    (error "x must be a value from 0 to 127"))
  (when (or (< y 0) (> y 64))
    (error "y must be a value from 0 to 63"))
  (let ((index (+ x (* (floor y *pages*) *width*)))
        (value (ash 1 (rem y 8))))
    (setf (aref *buffer* index)
          (cond ((= color +white+)
                 (logior (aref *buffer* index) value))
                ((= color +black+)
                 (logand (aref *buffer* index) (lognot value)))
                ((= color +inverse+)
                 (logxor (aref *buffer* index) value))))))
