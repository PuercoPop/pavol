;;; pavol.lisp --- simple StumpWM module to interact with pulseaudio

;;; Copyright (C) 2012-2014 Diogo F. S. Ramos

;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;;; Commentary:

;;; This module doesn't intent to substitute complex programs like
;;; pavucontrol but allow a very primitive control over one sink and
;;; applications.

;;; Usage
;;; -----

;;; The following commands are defined:

;;; + pavol-vol+
;;; + pavol-vol-
;;; + pavol-toggle-mute
;;; + pavol-interactive
;;; + pavol-exit-interactive
;;; + pavol-application-list

;;; The first three commands are supposed to be used using multimedia
;;; keys.  For this, you have to assign the appropriate keys in your
;;; `.stumpwmrc'. For example:

;;;   (define-key *top-map*
;;;               (kbd "XF86AudioRaiseVolume")
;;;               "pavol-vol+")

;;; `pavol-interactive' can be used if you don't have or don't want to
;;; use the media keys.  Once called, you can use `j', `k' and `m'
;;; keys to control the volume and exit the interactive mode using
;;; `ESC', `RET' or `C-g'.

;;; With `pavol-application-list' you can list the running
;;; applications that can have their volume controlled by pulseaudio.
;;; Navigate the menu using `j' and `k' keys and select the desired
;;; application using `RET'.

;;; Dependencies
;;; ------------

;;; Pavol is known to work with:
;;;
;;; * SBCL 1.1.18
;;; * PulseAudio 5.0

;;; Installation
;;; ------------

;;; Pavol is an ASDF system, so you should put it where ASDF can find
;;; and load it with `load-module' or `asdf:load-system'.
;;;
;;; You can also load it directly, using (load "/path/to/pavol.lisp").

;;; Limitations
;;; -----------

;;; As stated before, pavol is a *very* simple module to control a
;;; single sink.  So, if you have more than one sink, it'll probably
;;; fail to control your volume, without any warning.

;;; PulseAudio is a complex system, but pavol will only let you do
;;; three things:

;;; + Increase volume
;;; + Decrease volume
;;; + Mute

;;;; Code:

(defpackage #:pavol
  (:use #:cl))

(in-package #:pavol)

;;; "pavol" goes here. Hacks and glory await!

(defparameter *pavol-max* 65536)


;;;; Keymaps
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

(defparameter *pavol-application-list-keymap*
  (let ((m (stumpwm::copy-kmap stumpwm::*menu-map*)))
    (labels ((dk (k c)
               (stumpwm:define-key m k c)))
      (dk (stumpwm:kbd "j") 'stumpwm::menu-down)
      (dk (stumpwm:kbd "k") 'stumpwm::menu-up)
      m)))


;;;; External command
(defun pacmd (control-string &rest args)
  (stumpwm:run-shell-command
   (apply #'format nil
          (concatenate 'string "pacmd " control-string) args)
   t))


;;;; Sink

;;; A "sink" is normally the computer speakers
;;;
;;; There can be more than one Sink

(defclass sink ()
  ((index :initarg :index :reader sink-index)))

(defun number-of-sinks (raw) (read-from-string raw))

(defun list-sinks ()
  (let* ((r (pacmd "list-sinks"))
         (n (number-of-sinks r))
         ss)
    (ppcre:do-register-groups (i) ("\\s+index: (\\d+)" r)
      (push (make-instance 'sink-input :index i) ss))
    (assert (= (length ss) n))
    ss))

(defun default-sink ()
  (ppcre:register-groups-bind (i) ("(?m:^\\s+[*]\\s+index: (\\d+))"
                                   (pacmd "list-sinks"))
    (make-instance 'sink :index i)))

(defun raw-default-sink ()
  (let ((sinks (pacmd "list-sinks")))
    (multiple-value-bind (s e) (ppcre:scan "(?m:^\\s+[*]\\s+index:)" sinks)
      (when s
        (subseq sinks
                s
                (ppcre:scan "(?m:^\\s+index: +\\d+)" sinks :start e))))))

(defun raw-sink-from-index (index)
  (let ((sinks (pacmd "list-sinks")))
    (multiple-value-bind (s e)
        (ppcre:scan (format nil "(?m:^\\s+[*]?\\s+index: +~a\\s+)" index)
                    sinks)
      (when s
        (subseq sinks
                s
                (ppcre:scan "(?m:^\\s+[*]?\\s+index: +\\d+\\s+)"
                            sinks :start e))))))

(defun sink->raw (sink)
  (or (raw-sink-from-index (sink-index sink))
      (error "invalid sink")))

(defun sink-volume (sink)
  (ppcre:register-groups-bind (volume)
      ("(?m:^\\s+volume: +front-left: +\\d+ +/ +(\\d+)%)" (sink->raw sink))
    (if (null volume)
        (error "sink malformed")
        (values (parse-integer volume)))))

(defun (setf sink-volume) (percentage sink)
  (assert (<= 0 percentage 100))
  (pacmd "set-sink-volume ~a ~a"
         (sink-index sink)
         (percentage->integer percentage))
  percentage)

(defun sink-mute-p (sink)
  (ppcre:register-groups-bind (state)
      ("(?m:^\\s+muted: +(\\w+))"
       (sink->raw sink))
    (not (string= state "no"))))

(defun (setf sink-mute-p) (state sink)
  (pacmd "set-sink-mute ~a ~:[0~;1~]" (sink-index sink) state)
  state)


;;;; Sink Input

;;; A sink input is normally a program which is playing some sound.

;;; There can be more than one sink input.

(defclass sink-input ()
  ((index :initarg :index :reader sink-input-index)))

(defun raw-sink-input-from-index (index)
  (let ((sink-inputs (pacmd "list-sink-inputs")))
    (multiple-value-bind (s e)
        (ppcre:scan (format nil "(?m:^\\s+index: +~a\\s+)" index) sink-inputs)
      (when s
        (subseq sink-inputs
                s
                (ppcre:scan "(?m:^\\s+index: +\\d+)" sink-inputs :start e))))))

(defun sink-input->raw (sink-input)
  (or (raw-sink-input-from-index (sink-input-index sink-input))
      (error "invalid sink")))

(defun sink-input-volume (sink-input)
  (ppcre:register-groups-bind (volume)
      ("(?m:^\\s+volume: +front-left: +\\d+ +/ +(\\d+)%)"
       (sink-input->raw sink-input))
    (if (null volume)
        (error "sink malformed")
        (values (parse-integer volume)))))

(defun (setf sink-input-volume) (percentage sink-input)
  (assert (<= 0 percentage 100))
  (pacmd "set-sink-input-volume ~a ~a"
         (sink-input-index sink-input)
         (percentage->integer percentage))
  percentage)

(defun sink-input-mute-p (sink-input)
  (ppcre:register-groups-bind (state)
      ("(?m:^\\s+muted: +(\\w+))"
       (sink-input->raw sink-input))
    (not (string= state "no"))))

(defun (setf sink-input-mute-p) (state sink-input)
  (pacmd "set-sink-input-mute ~a ~:[0~;1~]"
         (sink-input-index sink-input)
         state)
  state)

(defun sink-input-name (sink-input)
  "The application name of the sink input."
  (ppcre:register-groups-bind (name)
      ("(?m:^\\s+application.name += +(\".*))"
       (sink-input->raw sink-input))
    (when name (read-from-string name))))

(defun number-of-sink-inputs (raw) (read-from-string raw))

(defun list-sink-inputs ()
  "A list of the sink inputs available."
  (let* ((r (pacmd "list-sink-inputs"))
         (n (number-of-sink-inputs r))
         ss)
    (ppcre:do-register-groups (i) ("\\s+index: (\\d+)" r)
      (push (make-instance 'sink-input :index i) ss))
    (assert (= (length ss) n))
    ss))

(defun sink-input->alist (sink-input)
  (cons (concatenate 'string
                     (sink-input-name sink-input) " "
                     (make-volume-bar (sink-input-volume sink-input)))
        sink-input))

(defun sink-inputs-selection ()
  (mapcar #'sink-input->alist (list-sink-inputs)))


;;;; Default Sink

;;; Functions that deal with the default sink.  This is what the user
;;; normally wants.
(defun volume ()
  (sink-volume (default-sink)))

(defun (setf volume) (percentage)
  (setf (sink-volume (default-sink)) percentage))

(defun mutep ()
  (sink-mute-p (default-sink)))

(defun (setf mutep) (state)
  (setf (sink-mute-p (default-sink)) state))

(defun volume-up (percentage)
 (setf (volume) (min (+ (volume) percentage) 100)))

(defun volume-down (percentage)
  (setf (volume) (max (- (volume) percentage) 0)))

(defun show-volume-bar ()
  (let ((percent (volume)))
    (funcall (if (interactivep)
                 #'stumpwm::message-no-timeout
                 #'stumpwm:message)
             (format nil "~:[OPEN~;MUTED~]~%~a"
                     (mutep) (make-volume-bar percent)))))

(defun vol+ (percentage)
 (volume-up percentage)
 (show-volume-bar))

(defun vol- (percentage)
  (volume-down percentage)
  (show-volume-bar))

(defun toggle-mute ()
  (setf (mutep) (not (mutep)))
  (show-volume-bar))


;;;; Utility functions
(defun percentage->integer (percentage)
  (truncate (* *pavol-max* percentage) 100))

(defun make-volume-bar (percent)
  "Return a string that represents a volume bar"
  (format nil "^B~3d%^b [^[^7*~a^]]"
          percent (stumpwm::bar percent 50 #\# #\:)))

(defun set-interactive ()
  (stumpwm::push-top-map *pavol-keymap*))

(defun unset-interactive ()
  (stumpwm::pop-top-map))

(defun interactivep ()
  (equal stumpwm:*top-map* *pavol-keymap*))


;;;; Commands
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
