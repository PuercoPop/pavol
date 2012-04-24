;;;; pavol.lisp

(in-package #:pavol)

;;; "pavol" goes here. Hacks and glory await!

(defparameter *pavol-max* 65536)

(defun pavol-volume ()
  (let ((str-sinks
         (stumpwm:run-shell-command "pacmd list-sinks" t)))
    (multiple-value-bind (start end start-reg end-reg)
        (cl-ppcre:scan "volume: 0:(.*?)%" str-sinks)
      (declare (ignore start end))
      (parse-integer (subseq str-sinks (elt start-reg 0) (elt end-reg 0))))))

(defun pavol-mute-p ()
  (let ((str-sinks
         (stumpwm:run-shell-command "pacmd list-sinks" t)))
    (multiple-value-bind (start end start-reg end-reg)
        (cl-ppcre:scan "muted: (.*)" str-sinks)
      (declare (ignore start end))
      (string= "yes" (subseq str-sinks (elt start-reg 0) (elt end-reg 0))))))

(defun pavol-percentage->integer (percentage)
  (let ((r (truncate (* *pavol-max* percentage) 100)))
    r))

(defun pavol-set-volume (percentage)
  (stumpwm:run-shell-command
   (format nil "pacmd set-sink-volume 0 ~a"
           (pavol-percentage->integer percentage))))

(defun pavol-mute (state)
  (stumpwm:run-shell-command
   (format nil "pacmd set-sink-mute 0 ~a" (if state 1 0))))

(defun pavol-show-volume-bar ()
  (let ((percent (pavol-volume)))
    (stumpwm:message
     (concatenate 'string
      (format nil "~:[OPEN~;MUTED~]" (pavol-mute-p))
      (format nil "~C^B~A%" #\Newline percent) "^b [^[^7*"
      (stumpwm::bar percent 50 #\# #\:) "^]]"))))

(defun pavol-volume-up (percentage)
  (let ((new-percentage
         (let ((cur (pavol-volume)))
           (if (> (+ cur percentage) 100)
               100
               (+ cur percentage)))))
    (pavol-set-volume new-percentage)))

(defun pavol-volume-down (percentage)
  (let ((new-percentage
         (let ((cur (pavol-volume)))
           (if (< (- cur percentage) 0)
               0
               (- cur percentage)))))
    (pavol-set-volume new-percentage)))

(defun vol+ (percentage)
  (pavol-volume-up percentage)
  (pavol-show-volume-bar))

(defun vol- (percentage)
  (pavol-volume-down percentage)
  (pavol-show-volume-bar))

(defun toggle-mute ()
  (if (pavol-mute-p)
      (pavol-mute nil)
      (pavol-mute t))
  (pavol-show-volume-bar))

(in-package #:stumpwm)

(stumpwm:defcommand pavol-vol+ () ()
  "Increase the volume by 5 points"
  (pavol:vol+ 5))

(stumpwm:defcommand pavol-vol- () ()
  "Decrease the volume by 5 points"
  (pavol:vol- 5))

(stumpwm:defcommand pavol-toggle-mute () ()
  "Toggle mute"
  (pavol:toggle-mute))
