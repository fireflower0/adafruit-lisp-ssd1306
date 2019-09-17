(defsystem "ssd1306-i2c"
  :class :package-inferred-system
  :version "0.1.0"
  :author ""
  :license ""
  :description ""
  :depends-on ("cffi"
               "ssd1306-i2c/boot"
               "cl-syntax-annot"
               "adafruit-lisp-ssd1306"))

(register-system-packages "ssd1306-i2c/boot" '(#:ssd1306-i2c))
