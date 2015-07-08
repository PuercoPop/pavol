(in-package :asdf-user)

(defsystem "pavol"
  :depends-on ("stumpwm" "cl-ppcre")
  :components ((:file "pavol")))
