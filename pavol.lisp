;;;; pavol.lisp

(defpackage #:pavol
  (:use #:cl)
  (:export :vol+
           :vol-
           :toggle-mute
           :show-volume-bar
           :*pavol-keymap*
           :set-interactive
           :unset-interactive))

(in-package #:pavol)

;;; "pavol" goes here. Hacks and glory await!

(defparameter *pavol-max* 65536)

(defparameter *pavol-keymap*
  (let ((m (stumpwm:make-sparse-keymap)))
    (labels ((dk (k c)
               (stumpwm:define-key m k c)))
      (dk (stumpwm:kbd "j") "pavol-vol-")
      (dk (stumpwm:kbd "k") "pavol-vol+")
      (dk (stumpwm:kbd "m") "pavol-toggle-mute")
      (dk (stumpwm:kbd "RET") "pavol-exit-interactive")
      (dk (stumpwm:kbd "C-g") "pavol-exit-interactive")
      (dk (stumpwm:kbd "ESC") "pavol-exit-interactive")
      m)))

(defun volume ()
  (let ((str-sinks
         (stumpwm:run-shell-command "pacmd list-sinks" t)))
    (multiple-value-bind (start end start-reg end-reg)
        (cl-ppcre:scan "volume: 0:(.*?)%" str-sinks)
      (declare (ignore start end))
      (parse-integer (subseq str-sinks (elt start-reg 0) (elt end-reg 0))))))

(defun mutep ()
  (let ((str-sinks
         (stumpwm:run-shell-command "pacmd list-sinks" t)))
    (multiple-value-bind (start end start-reg end-reg)
        (cl-ppcre:scan "muted: (.*)" str-sinks)
      (declare (ignore start end))
      (string= "yes" (subseq str-sinks (elt start-reg 0) (elt end-reg 0))))))

(defun percentage->integer (percentage)
  (truncate (* *pavol-max* percentage) 100))

(defun set-volume (percentage)
  (stumpwm:run-shell-command
   (format nil "pacmd set-sink-volume 0 ~a"
           (percentage->integer percentage))))

(defun mute (state)
  (stumpwm:run-shell-command
   (format nil "pacmd set-sink-mute 0 ~:[0~;1~]" state)))

(defun set-interactive ()
  (stumpwm::push-top-map pavol:*pavol-keymap*))

(defun unset-interactive ()
  (stumpwm::pop-top-map))

(defun interactivep ()
  (equal stumpwm:*top-map* pavol:*pavol-keymap*))

(defun show-volume-bar ()
  (let ((percent (volume)))
    (funcall (if (interactivep)
                 #'stumpwm::message-no-timeout
                 #'stumpwm:message)
             (format nil "~:[OPEN~;MUTED~]~%^B~3d%^b [^[^7*~a^]]"
                     (mutep) percent (stumpwm::bar percent 50 #\# #\:)))))

(defun volume-up (percentage)
  (set-volume (min (+ (volume) percentage) 100)))

(defun volume-down (percentage)
  (set-volume (max (- (volume) percentage) 0)))

(defun vol+ (percentage)
  (volume-up percentage)
  (show-volume-bar))

(defun vol- (percentage)
  (volume-down percentage)
  (show-volume-bar))

(defun toggle-mute ()
  (if (mutep)
      (mute nil)
      (mute t))
  (show-volume-bar))

(in-package #:stumpwm)

(defcommand pavol-vol+ () ()
  "Increase the volume by 5 points"
  (pavol:vol+ 5))

(defcommand pavol-vol- () ()
  "Decrease the volume by 5 points"
  (pavol:vol- 5))

(defcommand pavol-toggle-mute () ()
  "Toggle mute"
  (pavol:toggle-mute))

(defcommand pavol-exit-interactive () ()
  "Exit the interactive mode for changing the volume"
  (pavol:unset-interactive))

(defcommand pavol-interactive () ()
  "Change the volume interactively using `j', `k' and `m' keys"
  (pavol:set-interactive)
  (pavol:show-volume-bar))
