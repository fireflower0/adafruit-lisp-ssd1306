(defsystem "ssd1306-spi"
  :class :package-inferred-system
  :version "0.1.0"
  :author ""
  :license ""
  :description ""
  :depends-on ("cffi"
               "ssd1306-spi/boot"
               "cl-syntax-annot"))

(register-system-packages "ssd1306-spi/boot" '(#:ssd1306-spi))
