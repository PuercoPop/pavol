;;;; pavol.asd

(asdf:defsystem #:pavol
  :serial t
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "pavol")))

