(uiop:define-package #:ssd1306-spi
  (:use #:cl
        #:ssd1306-spi/app)
  (:export #:start))
(in-package #:ssd1306-spi)

(defun start (&rest args)
  (main args))
