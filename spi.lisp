(defpackage #:adafruit-lisp-ssd1306/spi
  (:use #:cl
        #:adafruit-lisp-ssd1306/constants
        #:adafruit-lisp-ssd1306/wrapper/wiringpi)
  (:export #:ssd1306-init))
(in-package #:adafruit-lisp-ssd1306/spi)

;; Parameters
(defparameter *width*  128)
(defparameter *height* 64)
(defparameter *pages*  (/ *height* 8))
(defparameter *buffer* (make-list (* *width* *pages*)
                                  :initial-element 0))
(defparameter *dc* 0) ; Data(high:1)/Command(low:0)
(defparameter *cs* 0) ; Chip Select (0 or 1)

(defun spi-data-rw (channel values &optional (len (length values)))
  (let ((mp (cffi:foreign-alloc :unsigned-char
                                :count len
                                :initial-contents values)))
    (wiringpi-spi-data-rw channel mp len)
    (let ((rval (loop for i from 0 below len
                   collect (cffi:mem-aref mp :unsigned-char i))))
      (cffi:foreign-free mp)
      rval)))

(defun command (values)
  (digital-write *dc* +low+)
  (spi-data-rw *cs* values))

(defun data (values)
  (digital-write *dc* +high+)
  (spi-data-rw *cs* values))

;; 主要パーツすぐに試せるボードのSSD1306の場合
;; cs      : 1
;; speed   : 8000000
;; dc      : 4
;; rst     : 23
;; Example : (ssd1306-init :chip-select 1 :spi-speed 8000000 :data-command 4 :reset 23)
(defun ssd1306-init (&key
                       chip-select
                       spi-speed
                       data-command
                       reset
                       (vcc-state +ssd1306-switch-cap-vcc+))
  ;; Set a Chip Select Number and a Data/Command Pin
  (setf *cs* chip-select
        *dc* data-command)

  ;; Setup GPIO
  (wiringpi-setup-gpio)
  (pin-mode *dc* +output+)
  (pin-mode rst +output+)

  ;; Setup SPI
  (wiringpi-spi-setup *cs* spi-speed)

  ;; Reset SSD1306
  (digital-write reset +low+)
  (delay 50)
  (digital-write reset +high+)

  ;; Initialize SSD1306
  (command `(,+ssd1306-display-off+
             ,+ssd1306-set-display-clock-div+
             #XF0 ; the suggested ratio
             ,+ssd1306-set-multi-plex+
             #X3F
             ,+ssd1306-set-display-offset+
             #X00 ; no offset
             ,(logior +ssd1306-set-start-line+ #X00)
             ,+ssd1306-charge-pump+
             ,(if (= vcc-state +ssd1306-external-vcc+) #X10 #X14)
             ,+ssd1306-memory-mode+
             #X00
             ,(logior +ssd1306-seg-remap+ #X01)
             ,+ssd1306-com-scan-dec+
             ,+ssd1306-set-com-pins+
             #X12
             ,+ssd1306-set-contrast+
             ,(if (= vcc-state +ssd1306-external-vcc+) #X9F #XCF)
             ,+ssd1306-set-pre-charge+
             ,(if (= vcc-state +ssd1306-external-vcc+) #X22 #XF1)
             ,+ssd1306-set-vcom-detect+
             #X40
             ,+ssd1306-display-all-on-resume+
             ,+ssd1306-normal-display+
             ,+ssd1306-display-on+)))

;; Setup contrast
(defun ssd1306-set-contrast (contrast)
  (when (or (< contrast 0) (> contrast 255))
    (error "Contrast must be a value from 0 to 255"))
  (command `(,+ssd1306-set-contrast+ contrast)))

(defun ssd1306-display ()
  (command `(,+ssd1306-set-low-column+
             ,+ssd1306-set-high-column+
             ,+ssd1306-set-start-line+))
  (data *buffer*))

(defun ssd1306-clear ()
  (fill *buffer* 0))

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
    (setf (nth index *buffer*)
          (cond ((= color +white+)
                 (logior (nth index *buffer*) value))
                ((= color +black+)
                 (logand (nth index *buffer*) (lognot value)))
                ((= color +inverse+)
                 (logxor (nth index *buffer*) value))))))
