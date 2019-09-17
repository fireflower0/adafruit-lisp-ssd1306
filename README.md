# adafruit-lisp-ssd1306

128x64 ssd1306 OLED library for Raspberry Pi

## Description

128x64 ssd1306 OLED library for Common Lisp. (Raspberry Pi only)
You can purchase from the following.

[0.96インチ 128x64ドット有機ELディスプレイ](http://akizukidenshi.com/catalog/g/gP-12031/)

## Usage

Add "adafruit-lisp-ssd1306" to dependencies-on of asd file.
The following is an example.

```
(defsystem <your project name>
  :class :package-inferred-system
  :version "0.1.0"
  :author ""
  :license ""
  :description ""
  :depends-on ("cffi"
               "ssd1306-i2c/boot"
               "cl-syntax-annot"
               "adafruit-lisp-ssd1306"))
```

Import and use "adafruit-lisp-ssd1306/i2c".
The following is an example.

```
(uiop:define-package #:<your project name>/app
  (:use #:cl
        #:ssd1306-i2c/wrapper/wiringpi
        #:adafruit-lisp-ssd1306/i2c)
  (:export #:main))
(in-package #:<your project name>/app)
```

## Available APIs

```
ssd1306-init
ssd1306-clear
ssd1306-display
ssd1306-clear-display
ssd1306-draw-pixel
ssd1306-draw-line
ssd1306-draw-rect
ssd1306-draw-fill-rect
ssd1306-draw-circle
ssd1306-draw-fill-circle
ssd1306-draw-triangle
ssd1306-draw-fill-triangle
ssd1306-draw-char
ssd1306-draw-string
ssd1306-draw-bmp
ssd1306-invert-display
```

## Author

fireflower0

## License

MIT
