;;;; pavol.lisp

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

(defun show-volume-bar ()
  (let ((percent (volume)))
    (stumpwm:message
     (concatenate 'string
      (format nil "~:[OPEN~;MUTED~]" (mutep))
      (format nil "~C^B~A%" #\Newline percent) "^b [^[^7*"
      (stumpwm::bar percent 50 #\# #\:) "^]]"))))

(defun volume-up (percentage)
  (let ((new-percentage
         (let ((cur (volume)))
           (if (> (+ cur percentage) 100)
               100
               (+ cur percentage)))))
    (set-volume new-percentage)))

(defun volume-down (percentage)
  (let ((new-percentage
         (let ((cur (volume)))
           (if (< (- cur percentage) 0)
               0
               (- cur percentage)))))
    (set-volume new-percentage)))

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
  (pop-top-map)
  (message "Done changing volume"))

(defcommand pavol-interactive () ()
  "Change the volume interactively using `j', `k' and `m' keys"
  (message "Changing volume interactively")
  (push-top-map pavol:*pavol-keymap*))




