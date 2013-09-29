;;;; pavol.lisp

(defpackage #:pavol
  (:use #:cl))

(in-package #:pavol)

;;; "pavol" goes here. Hacks and glory await!

(defparameter *pavol-max* 65536)

(defvar *pavol-index* 0 "The index used when changing the volume or
muting a sink")

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

(defstruct sink-input "A sink input"
  (mute-p nil)
  name
  index
  volume)

(defun copy-sparse-keymap (sparse-keymap)
  (stumpwm::copy-kmap sparse-keymap))

(defparameter *pavol-application-list-keymap*
  (let ((m (copy-sparse-keymap stumpwm::*menu-map*)))
    (labels ((dk (k c)
               (stumpwm:define-key m k c)))
      (dk (stumpwm:kbd "j") 'stumpwm::menu-down)
      (dk (stumpwm:kbd "k") 'stumpwm::menu-up)
      m)))

(defun sink-input-index-mute-p (sink-input-index)
  "Is the sink input mute?"
  (ppcre:register-groups-bind (state)
      ("\\ *muted:\\ *(\\w*)" sink-input-index)
    (not (string= state "no"))))

(defun sink-input-index-name (sink-input-index)
  "The application name of the sinput index."
  (ppcre:register-groups-bind (name)
      ("\\ *application.name = (.*)" sink-input-index)
    (subseq name 1 (1- (length name)))))

(defun sink-input-index-index (sink-input-index)
  "The index os a sink input."
  (cl-ppcre:register-groups-bind (index)
      ("\\ *index: (\\d*)" sink-input-index)
    (when index (parse-integer index))))

(defun sink-input-index-volume (sink-input-index)
  "The volume of a sink input."
  (cl-ppcre:register-groups-bind (volume)
      ("\\ *volume:\\ *0:\\ *(\\d*)\\ *%" sink-input-index)
    (when volume (parse-integer volume))))

(defun process-sink-input-index (sink-input-index)
  "Return a sink input structure from a index returned by pacmd."
  (make-sink-input
   :mute-p (sink-input-index-mute-p sink-input-index)
   :name (sink-input-index-name sink-input-index)
   :index (sink-input-index-index sink-input-index)
   :volume (sink-input-index-volume sink-input-index)))

(defun list-sink-inputs ()
  "A list of the active sink inputs."
  (let ((sink-inputs nil))
    (cl-ppcre:do-matches-as-strings 
        (match "(?s:index:.+?(?=index:|>>>))"
          (stumpwm:run-shell-command "pacmd list-sink-inputs" t))
      (push (process-sink-input-index match) sink-inputs))
    sink-inputs))

(defun sink-input->alist (sink-input)
  (cons (concatenate 'string
                     (sink-input-name sink-input) " "
                     (make-volume-bar (sink-input-volume sink-input)))
        sink-input))

(defun sink-inputs-selection ()
  (mapcar #'sink-input->alist (list-sink-inputs)))

(defun volume ()
  (if (zerop *pavol-index*)
      (cl-ppcre:register-groups-bind (volume)
          ("volume: 0:(.*?)%" (stumpwm:run-shell-command "pacmd list-sinks"
                                                         t))
        (when volume
          (parse-integer volume)))
      (sink-input-volume (find *pavol-index*
                               (list-sink-inputs)
                               :key #'sink-input-index))))

(defun mutep ()
    (if (zerop *pavol-index*)
        (cl-ppcre:register-groups-bind (mutep)
            ("muted: (.*)" (stumpwm:run-shell-command "pacmd list-sinks"
                                                      t))
          (when mutep
            (string= "yes" mutep)))
        (sink-input-mute-p (find *pavol-index*
                                 (list-sink-inputs)
                                 :key #'sink-input-index))))

(defun percentage->integer (percentage)
  (truncate (* *pavol-max* percentage) 100))

(defun set-volume (percentage)
  (if (zerop *pavol-index*)
      (stumpwm:run-shell-command 
       (format nil "pacmd set-sink-volume 0 ~a"
               (percentage->integer percentage)))
      (stumpwm:run-shell-command
       (format nil "pacmd set-sink-input-volume ~a ~a"
               *pavol-index*
               (percentage->integer percentage)))))

(defun mute (state)
  (if (zerop *pavol-index*)
      (stumpwm:run-shell-command
       (format nil "pacmd set-sink-mute 0 ~:[0~;1~]" state))
      (stumpwm:run-shell-command
       (format nil
               "pacmd set-sink-input-mute ~a ~:[0~;1~]"
               *pavol-index*
               state))))

(defun set-interactive (&optional (index 0))
  (setf *pavol-index* index)
  (stumpwm::push-top-map *pavol-keymap*))

(defun unset-interactive ()
  (setf *pavol-index* 0)
  (stumpwm::pop-top-map))

(defun interactivep ()
  (equal stumpwm:*top-map* *pavol-keymap*))

(defun make-volume-bar (percent)
  "Return a string that represents a volume bar"
  (format nil "^B~3d%^b [^[^7*~a^]]"
          percent (stumpwm::bar percent 50 #\# #\:)))

(defun show-volume-bar ()
  (let ((percent (volume)))
    (funcall (if (interactivep)
                 #'stumpwm::message-no-timeout
                 #'stumpwm:message)
             (format nil "~:[OPEN~;MUTED~]~%~a"
                     (mutep) (make-volume-bar percent)))))

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

(stumpwm:defcommand pavol-vol+ () ()
  "Increase the volume by 5 points"
  (vol+ 5))

(stumpwm:defcommand pavol-vol- () ()
  "Decrease the volume by 5 points"
  (vol- 5))

(stumpwm:defcommand pavol-toggle-mute () ()
  "Toggle mute"
  (toggle-mute))

(stumpwm:defcommand pavol-exit-interactive () ()
  "Exit the interactive mode for changing the volume"
  (unset-interactive))

(stumpwm:defcommand pavol-interactive () ()
  "Change the volume interactively using `j', `k' and `m' keys"
  (set-interactive)
  (show-volume-bar))

(stumpwm:defcommand pavol-application-list () ()
  "Give the ability to control independent applications.

They are actually input sinks in pulseaudio's terminology."
  (let ((sinks (sink-inputs-selection)))
    (if (null sinks)
        (stumpwm:message "No application is running")
        (let* ((stumpwm::*menu-map* *pavol-application-list-keymap*)
               (sink (stumpwm::select-from-menu (stumpwm:current-screen)
                                                sinks)))
          (when sink
            (set-interactive (sink-input-index (cdr sink)))
            (show-volume-bar))))))
