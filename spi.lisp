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
(defparameter *buffer* (make-list `(,(* *width* *pages*))
                                  :initial-element 0))
(defparameter *dc* 0) ; Data(high:1)/Command(low:0)
(defparameter *cs* 0) ; Chip Select (0 or 1)

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
  (spi-data-rw *cs* data))

(defun data (data)
  (digital-write *dc* +high+)
  (spi-data-rw *cs* data))

(defun ssd1306-init (cs speed dc rst
                     &optional (vcc-state +ssd1306-switch-cap-vcc+))
  ;; Set a Chip Select Number and a Data/Command Pin
  (setf *cs* cs *dc* dc)

  ;; Setup GPIO
  (wiringpi-setup-gpio)
  (pin-mode *dc* +output+)
  (pin-mode rst +output+)

  ;; Setup SPI
  (wiringpi-spi-setup *cs* speed)

  ;; Reset SSD1306
  (digital-write rst +low+)
  (delay 50)
  (digital-write rst +high+)

  ;; Initialize SSD1306
  (command `(,+ssd1306-display-off+
             ,+ssd1306-set-display-clock-div+
             #XF0 ; the suggested ratio
             ,+ssd1306-set-multi-plex+
             #X3F
             ,+ssd1306-set-display-offset+
             #X00 ; no offset
             ,(logior ,+ssd1306-set-start-line+ #X00)
             ,+ssd1306-charge-pump+
             #X14
             ,+ssd1306-memory-mode+
             #X00
             ,(logior ,+ssd1306-seg-remap+ #X01)
             ,+ssd1306-com-scan-dec+
             ,+ssd1306-set-com-pins+
             #X12
             ,+ssd1306-set-contrast+
             #XCF
             ,+ssd1306-set-pre-charge+
             #XF1
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
  (command `(+ssd1306-set-low-column+
             +ssd1306-set-high-column+
             +ssd1306-set-start-line+))
  (data *buffer*))

(defun ssd1306-clear ()
  (fill *buffer* 0))

(defun ssd1306-clear-display ()
  (ssd1306-clear)
  (ssd1306-display))
