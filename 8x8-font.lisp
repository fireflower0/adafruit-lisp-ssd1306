(defpackage #:adafruit-lisp-ssd1306/8x8-font
  (:use #:cl)
  (:export #:font-8x8))
(in-package #:adafruit-lisp-ssd1306/8x8-font)

(defparameter font-8x8
  (make-array
   '(95 8)
   :initial-contents
   '((#X00 #X00 #X00 #X00 #X00 #X00 #X00 #X00)    ; space #X20
     (#X30 #X78 #X78 #X30 #X30 #X00 #X30 #X00)    ; !
     (#X6C #X6C #X6C #X00 #X00 #X00 #X00 #X00)    ; "
     (#X6C #X6C #XFE #X6C #XFE #X6C #X6C #X00)    ; #
     (#X18 #X3E #X60 #X3C #X06 #X7C #X18 #X00)    ; $
     (#X00 #X63 #X66 #X0C #X18 #X33 #X63 #X00)    ; %
     (#X1C #X36 #X1C #X3B #X6E #X66 #X3B #X00)    ; &
     (#X30 #X30 #X60 #X00 #X00 #X00 #X00 #X00)    ; '
     (#X0C #X18 #X30 #X30 #X30 #X18 #X0C #X00)    ; (
     (#X30 #X18 #X0C #X0C #X0C #X18 #X30 #X00)    ; )
     (#X00 #X66 #X3C #XFF #X3C #X66 #X00 #X00)    ; *
     (#X00 #X30 #X30 #XFC #X30 #X30 #X00 #X00)    ; +
     (#X00 #X00 #X00 #X00 #X00 #X18 #X18 #X30)    ; ,
     (#X00 #X00 #X00 #X7E #X00 #X00 #X00 #X00)    ; -
     (#X00 #X00 #X00 #X00 #X00 #X18 #X18 #X00)    ; .
     (#X03 #X06 #X0C #X18 #X30 #X60 #X40 #X00)    ; / (forward slash)
     (#X3E #X63 #X63 #X6B #X63 #X63 #X3E #X00)    ; 0 #X30
     (#X18 #X38 #X58 #X18 #X18 #X18 #X7E #X00)    ; 1
     (#X3C #X66 #X06 #X1C #X30 #X66 #X7E #X00)    ; 2
     (#X3C #X66 #X06 #X1C #X06 #X66 #X3C #X00)    ; 3
     (#X0E #X1E #X36 #X66 #X7F #X06 #X0F #X00)    ; 4
     (#X7E #X60 #X7C #X06 #X06 #X66 #X3C #X00)    ; 5
     (#X1C #X30 #X60 #X7C #X66 #X66 #X3C #X00)    ; 6
     (#X7E #X66 #X06 #X0C #X18 #X18 #X18 #X00)    ; 7
     (#X3C #X66 #X66 #X3C #X66 #X66 #X3C #X00)    ; 8
     (#X3C #X66 #X66 #X3E #X06 #X0C #X38 #X00)    ; 9
     (#X00 #X18 #X18 #X00 #X00 #X18 #X18 #X00)    ; :
     (#X00 #X18 #X18 #X00 #X00 #X18 #X18 #X30)    ; ;
     (#X0C #X18 #X30 #X60 #X30 #X18 #X0C #X00)    ; <
     (#X00 #X00 #X7E #X00 #X00 #X7E #X00 #X00)    ; =
     (#X30 #X18 #X0C #X06 #X0C #X18 #X30 #X00)    ; >
     (#X3C #X66 #X06 #X0C #X18 #X00 #X18 #X00)    ; ?
     (#X3E #X63 #X6F #X69 #X6F #X60 #X3E #X00)    ; @ #X40
     (#X18 #X3C #X66 #X66 #X7E #X66 #X66 #X00)    ; A
     (#X7E #X33 #X33 #X3E #X33 #X33 #X7E #X00)    ; B
     (#X1E #X33 #X60 #X60 #X60 #X33 #X1E #X00)    ; C
     (#X7C #X36 #X33 #X33 #X33 #X36 #X7C #X00)    ; D
     (#X7F #X31 #X34 #X3C #X34 #X31 #X7F #X00)    ; E
     (#X7F #X31 #X34 #X3C #X34 #X30 #X78 #X00)    ; F
     (#X1E #X33 #X60 #X60 #X67 #X33 #X1F #X00)    ; G
     (#X66 #X66 #X66 #X7E #X66 #X66 #X66 #X00)    ; H
     (#X3C #X18 #X18 #X18 #X18 #X18 #X3C #X00)    ; I
     (#X0F #X06 #X06 #X06 #X66 #X66 #X3C #X00)    ; J
     (#X73 #X33 #X36 #X3C #X36 #X33 #X73 #X00)    ; K
     (#X78 #X30 #X30 #X30 #X31 #X33 #X7F #X00)    ; L
     (#X63 #X77 #X7F #X7F #X6B #X63 #X63 #X00)    ; M
     (#X63 #X73 #X7B #X6F #X67 #X63 #X63 #X00)    ; N
     (#X3E #X63 #X63 #X63 #X63 #X63 #X3E #X00)    ; O
     (#X7E #X33 #X33 #X3E #X30 #X30 #X78 #X00)    ; P #X50
     (#X3C #X66 #X66 #X66 #X6E #X3C #X0E #X00)    ; Q
     (#X7E #X33 #X33 #X3E #X36 #X33 #X73 #X00)    ; R
     (#X3C #X66 #X30 #X18 #X0C #X66 #X3C #X00)    ; S
     (#X7E #X5A #X18 #X18 #X18 #X18 #X3C #X00)    ; T
     (#X66 #X66 #X66 #X66 #X66 #X66 #X7E #X00)    ; U
     (#X66 #X66 #X66 #X66 #X66 #X3C #X18 #X00)    ; V
     (#X63 #X63 #X63 #X6B #X7F #X77 #X63 #X00)    ; W
     (#X63 #X63 #X36 #X1C #X1C #X36 #X63 #X00)    ; X
     (#X66 #X66 #X66 #X3C #X18 #X18 #X3C #X00)    ; Y
     (#X7F #X63 #X46 #X0C #X19 #X33 #X7F #X00)    ; Z
     (#X3C #X30 #X30 #X30 #X30 #X30 #X3C #X00)    ; [
     (#X60 #X30 #X18 #X0C #X06 #X03 #X01 #X00)    ; \ (back slash)
     (#X3C #X0C #X0C #X0C #X0C #X0C #X3C #X00)    ; ]
     (#X08 #X1C #X36 #X63 #X00 #X00 #X00 #X00)    ; ^
     (#X00 #X00 #X00 #X00 #X00 #X00 #X00 #XFF)    ; _
     (#X18 #X18 #X0C #X00 #X00 #X00 #X00 #X00)    ; ` #X60
     (#X00 #X00 #X3C #X06 #X3E #X66 #X3B #X00)    ; a
     (#X70 #X30 #X3E #X33 #X33 #X33 #X6E #X00)    ; b
     (#X00 #X00 #X3C #X66 #X60 #X66 #X3C #X00)    ; c
     (#X0E #X06 #X3E #X66 #X66 #X66 #X3B #X00)    ; d
     (#X00 #X00 #X3C #X66 #X7E #X60 #X3C #X00)    ; e
     (#X1C #X36 #X30 #X78 #X30 #X30 #X78 #X00)    ; f
     (#X00 #X00 #X3B #X66 #X66 #X3E #X06 #X7C)    ; g
     (#X70 #X30 #X36 #X3B #X33 #X33 #X73 #X00)    ; h
     (#X18 #X00 #X38 #X18 #X18 #X18 #X3C #X00)    ; i
     (#X06 #X00 #X06 #X06 #X06 #X66 #X66 #X3C)    ; j
     (#X70 #X30 #X33 #X36 #X3C #X36 #X73 #X00)    ; k
     (#X38 #X18 #X18 #X18 #X18 #X18 #X3C #X00)    ; l
     (#X00 #X00 #X66 #X7F #X7F #X6B #X63 #X00)    ; m
     (#X00 #X00 #X7C #X66 #X66 #X66 #X66 #X00)    ; n
     (#X00 #X00 #X3C #X66 #X66 #X66 #X3C #X00)    ; o
     (#X00 #X00 #X6E #X33 #X33 #X3E #X30 #X78)    ; p #X70
     (#X00 #X00 #X3B #X66 #X66 #X3E #X06 #X0F)    ; q
     (#X00 #X00 #X6E #X3B #X33 #X30 #X78 #X00)    ; r
     (#X00 #X00 #X3E #X60 #X3C #X06 #X7C #X00)    ; s
     (#X08 #X18 #X3E #X18 #X18 #X1A #X0C #X00)    ; t
     (#X00 #X00 #X66 #X66 #X66 #X66 #X3B #X00)    ; u
     (#X00 #X00 #X66 #X66 #X66 #X3C #X18 #X00)    ; v
     (#X00 #X00 #X63 #X6B #X7F #X7F #X36 #X00)    ; w
     (#X00 #X00 #X63 #X36 #X1C #X36 #X63 #X00)    ; x
     (#X00 #X00 #X66 #X66 #X66 #X3E #X06 #X7C)    ; y
     (#X00 #X00 #X7E #X4C #X18 #X32 #X7E #X00)    ; z
     (#X0E #X18 #X18 #X70 #X18 #X18 #X0E #X00)    ; {
     (#X0C #X0C #X0C #X00 #X0C #X0C #X0C #X00)    ; |
     (#X70 #X18 #X18 #X0E #X18 #X18 #X70 #X00)    ; }
     (#X3B #X6E #X00 #X00 #X00 #X00 #X00 #X00)))) ; ~ #X7E
