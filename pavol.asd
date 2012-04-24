;;;; pavol.asd

(asdf:defsystem #:pavol
  :serial t
  :depends-on (#:stumpwm
               #:cl-ppcre)
  :components ((:file "package")
               (:file "pavol")))

