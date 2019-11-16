(defsystem "ssd1306-i2c"
  :class :package-inferred-system
  :version "0.1.0"
  :author ""
  :license ""
  :description ""
  :depends-on ("cffi"
               "clispi"
               "ssd1306-i2c/boot"
               "cl-syntax-annot"))

(register-system-packages "ssd1306-i2c/boot" '(#:ssd1306-i2c))
