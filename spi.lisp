(defpackage #:adafruit-lisp-ssd1306/spi
  (:use #:cl
        #:adafruit-lisp-ssd1306/8x8-font
        #:adafruit-lisp-ssd1306/wrapper/wiringpi)
  (:export #:ssd1306-init))
(in-package #:adafruit-lisp-ssd1306/spi)

;; Constants
(defconstant +ssd1306-i2c-address+           #X3C)
(defconstant +ssd1306-set-contrast+          #X81)
(defconstant +ssd1306-display-all-on-resume+ #XA4)
(defconstant +ssd1306-display-all-on+        #XA5)
(defconstant +ssd1306-normal-display+        #XA6)
(defconstant +ssd1306-invert-display+        #XA7)
(defconstant +ssd1306-display-off+           #XAE)
(defconstant +ssd1306-display-on+            #XAF)
(defconstant +ssd1306-set-display-offset+    #XD3)
(defconstant +ssd1306-set-com-pins+          #XDA)
(defconstant +ssd1306-set-vcom-detect+       #XDB)
(defconstant +ssd1306-set-display-clock-div+ #XD5)
(defconstant +ssd1306-set-pre-charge+        #XD9)
(defconstant +ssd1306-set-multi-plex+        #XA8)
(defconstant +ssd1306-set-low-column+        #X00)
(defconstant +ssd1306-set-high-column+       #X10)
(defconstant +ssd1306-set-start-line+        #X40)
(defconstant +ssd1306-memory-mode+           #X20)
(defconstant +ssd1306-column-addr+           #X21)
(defconstant +ssd1306-page-addr+             #X22)
(defconstant +ssd1306-com-scan-inc+          #XC0)
(defconstant +ssd1306-com-scan-dec+          #XC8)
(defconstant +ssd1306-seg-remap+             #XA0)
(defconstant +ssd1306-charge-pump+           #X8D)
(defconstant +ssd1306-external-vcc+          #X01)
(defconstant +ssd1306-switch-cap-vcc+        #X02)

;; Scrolling constants
(defconstant +ssd1306-activate-scroll+                      #X2F)
(defconstant +ssd1306-deactivate-scroll+                    #X2E)
(defconstant +ssd1306-set-vertical-scroll-area+             #XA3)
(defconstant +ssd1306-right-horizontal-scroll+              #X26)
(defconstant +ssd1306-left-horizontal-scroll+               #X27)
(defconstant +ssd1306-vertical-and-right-horizontal-scroll+ #X29)
(defconstant +ssd1306-vertical-and-left-horizontal-scroll+  #X2A)

;; Color
(defconstant +black+   0)
(defconstant +white+   1)
(defconstant +inverse+ 2)

;; Parameters
(defparameter *width*  128)
(defparameter *height* 64)
(defparameter *pages*  (/ *height* 8))
(defparameter *buffer* (make-array `(,(* *width* *pages*))
                                   :initial-element 0))
(defparameter *vcc-state* 0)

;; SPI
(defparameter *rst* 23)
(defparameter *dc* 4)
(defparameter *spi-ch* 0)
(defparameter *spi-cs* 1)
(defparameter *spi-speed* 8000000)

(defconstant +ctrl-reg+ #X20)
(defconstant +read+     #X80)
(defconstant +write+    #X3F)

(defconstant +high+ 1)
(defconstant +low   0)

(defun spi-data-rw (channel data &optional (len (length data)))
  (let ((mp (cffi:foreign-alloc :unsigned-char
                                :count len
                                :initial-contents data)))
    (wiringpi-spi-data-rw channel mp len)
    (let ((rval (loop for i from 0 below len
                   collect (cffi:mem-aref mp :unsigned-char i))))
      (cffi:foreign-free mp)
      rval)))

(defun spi-write (value)
  (spi-data-rw +spi-cs+ (list (logand +ctrl-reg+ +write+) value)))

(defun command (value)
  (pin-mode *dc* +low+)
  (spi-write value))

(defun data (value)
  (pin-mode *dc* +high+)
  (spi-write value))

(defun ssd1306-init (&optional (vcc-state +ssd1306-switch-cap-vcc+))
  (setf *vcc-state* vcc-state)
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
