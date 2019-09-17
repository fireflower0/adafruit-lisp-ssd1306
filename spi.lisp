(defpackage #:adafruit-lisp-ssd1306/spi
  (:use #:cl
        #:adafruit-lisp-ssd1306/8x8-font
        #:adafruit-lisp-ssd1306/wrapper/wiringpi))
(in-package #:adafruit-lisp-ssd1306/spi)

(defparameter *rst* 23)
(defparameter *dc* 4)
(defparameter *spi-ch* 0)
(defparameter *spi-cs* 1)
