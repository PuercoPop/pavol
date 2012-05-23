;;;; pavol.asd

(asdf:defsystem #:pavol
  :description "A simple volume control for StumpWM and PulseAudio"
  :version "1.0"
  :author "Diogo F. S. Ramos <diogofsr@gmail.com>"
  :license "GPL3"
  :serial t
  :depends-on (#:stumpwm
               #:cl-ppcre)
  :components ((:file "pavol")))

