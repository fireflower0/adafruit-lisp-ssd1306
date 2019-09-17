(uiop:define-package #:ssd1306-i2c
  (:use #:cl
        #:ssd1306-i2c/app)
  (:export #:start))
(in-package #:ssd1306-i2c)

(defun start (&rest args)
  (main args))
