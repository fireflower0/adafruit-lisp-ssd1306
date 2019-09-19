(defpackage #:adafruit-lisp-ssd1306/constants
  (:use #:cl)
  (:export #:+ssd1306-i2c-address+
           #:+ssd1306-set-contrast+
           #:+ssd1306-display-all-on-resume+
           #:+ssd1306-display-all-on+
           #:+ssd1306-normal-display+
           #:+ssd1306-invert-display+
           #:+ssd1306-display-off+
           #:+ssd1306-display-on+
           #:+ssd1306-set-display-offset+
           #:+ssd1306-set-com-pins+
           #:+ssd1306-set-vcom-detect+
           #:+ssd1306-set-display-clock-div+
           #:+ssd1306-set-pre-charge+
           #:+ssd1306-set-multi-plex+
           #:+ssd1306-set-low-column+
           #:+ssd1306-set-high-column+
           #:+ssd1306-set-start-line+
           #:+ssd1306-memory-mode+
           #:+ssd1306-column-addr+
           #:+ssd1306-page-addr+
           #:+ssd1306-com-scan-inc+
           #:+ssd1306-com-scan-dec+
           #:+ssd1306-seg-remap+
           #:+ssd1306-charge-pump+
           #:+ssd1306-external-vcc+
           #:+ssd1306-switch-cap-vcc+
           #:+ssd1306-activate-scroll+
           #:+ssd1306-deactivate-scroll+
           #:+ssd1306-set-vertical-scroll-area+
           #:+ssd1306-right-horizontal-scroll+
           #:+ssd1306-left-horizontal-scroll+
           #:+ssd1306-vertical-and-right-horizontal-scroll+
           #:+ssd1306-vertical-and-left-horizontal-scroll+
           #:+black+
           #:+white+
           #:+inverse+
           #:+high+
           #:+low+))
(in-package #:adafruit-lisp-ssd1306/constants)

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

;; Digital write
(defconstant +high+ 1)
(defconstant +low+  0)
