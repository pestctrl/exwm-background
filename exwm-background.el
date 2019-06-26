;;; exwm-background.el --- Allow sending X windows to the background in EXWM -*- lexical-binding: t; -*-

;; Copyright 2019 Benson Chu <bensonchu457@gmail.com>

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Version: 0.1
;; Package-Requires: ((exwm "22.1"))
;; URL: https://github.com/pestctrl/exwm-background
;; Keywords: tools

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; I saw an internet post online about someone wanting to watch youtube
;; videos or read articles behind EXWM via transparency, and I wanted
;; that as well. Thus, I wrote this package.

;; I do this by advising certain functions in exwm in order to let EXWM
;; essentially "forget" about these windows, in that EXWM will not
;; attempt to hide these windows or bring them to the foreground.


;;; Code:

(require 'xelb)
(require 'exwm)

;; transparency toggle
(defvar exwm-background/current-transparency 85)
(defvar exwm-background/unfocused-opacity 50)
(setq window-system-default-frame-alist `((x . ((alpha . (,exwm-background/current-transparency . ,exwm-background/unfocused-opacity)) ))))

(defun exwm-background/increase-transparency ()
  (interactive)
  (if (>= exwm-background/current-transparency 100)
      (message "Max opacity")
    (setf exwm-background/current-transparency
          (+ exwm-background/current-transparency 5))
    (set-frame-parameter (selected-frame) 'alpha `(,exwm-background/current-transparency . 50))
    (message (format "%d" exwm-background/current-transparency))))

(defun exwm-background/decrease-transparency ()
  (interactive)
  (if (<= exwm-background/current-transparency 0)
      (message "Minimum opacity")
    (setf exwm-background/current-transparency
          (- exwm-background/current-transparency 5))
    (set-frame-parameter (selected-frame) 'alpha `(,exwm-background/current-transparency . 50))
    (message (format "%d" exwm-background/current-transparency))))


(defvar exwm-background/viewing-background nil)

(defun my/toggle-viewing-background ()
  "Allow for easy switching between 0 transparency and 
     current transparency, in order to take peeks at the 
     X window in the background."
  (interactive)
  (cond (exwm-background/viewing-background
         (if (consp exwm-background/viewing-background)
             (my/set-window-transparency )
           (set-frame-parameter (selected-frame) 'alpha `(,my/current-transparency . 50))))
        (exwm--id
         )
        (t
         (setq my/viewing-background (not exwm-background/viewing-background))
         (set-frame-parameter (selected-frame) 'alpha `(,(if exwm-background/viewing-background 0 exwm-background/current-transparency) . 50)))))


;; Send a window to the back, for viewing while transparent
;; Reference material: 
;; - exwm--id-buffer-alist
;; - xcb:ConfigureWindow
;; - exwm-floating--set-floating
;; - https://www.x.org/releases/X11R7.7/doc/libxcb/tutorial/index.html#winmap

(defvar exwm-background/exwm-background-window -1)

(defun exwm-background/exwm-hide-advice (fun id)
  (unless (= exwm-background/exwm-background-window id)
    (funcall fun id)))

(advice-add #'exwm-layout--hide :around #'exwm-background/exwm-hide-advice)

(defun exwm-background/xelb-foreground-window (id)
  (let ((req (make-instance 'xcb:ConfigureWindow
                            :value-mask xcb:ConfigWindow:StackMode
                            :stack-mode xcb:StackMode:Above
                            :window id)))
    (xcb:+request exwm--connection req)
    (xcb:flush exwm--connection)))

(defun exwm-background/exwm-show-advice (id &optional window)
  (when (= exwm-background/exwm-background-window id)
    (setq exwm-background/exwm-background-window -1)
    (exwm-background/xelb-foreground-window id)))

(advice-add #'exwm-layout--show :before #'exwm-background/exwm-show-advice)

(setq exwm-background/exwm-background-window -1)

(defun exwm-background/xelb-background-window (id)
  (let ((req (make-instance 'xcb:ConfigureWindow
                            :value-mask xcb:ConfigWindow:StackMode
                            :stack-mode xcb:StackMode:Below
                            :window id)))
    (xcb:+request exwm--connection req)
    (xcb:flush exwm--connection)))

(defun exwm-background/exwm-background-window ()
  (interactive)
  (when (not (= exwm-background/exwm-background-window -1))
    (let ((id exwm-background/exwm-background-window))
      (exwm-background/xelb-foreground-window id)
      (exwm-layout--hide id)))
  (when exwm--id
    (setq exwm-background/exwm-background-window exwm--id)
    (exwm-background/xelb-background-window exwm--id))
  (ibuffer))

;; Weird behavior when un-fullscreening a
;; youtube window that has been backgrounded
(defun exwm-background/exwm-unset-fullscreen (&optional id)
  ;; I don't forsee the error happening
  ;; in a situation where an id is not passed
  (when (and id
             (= id exwm-background/exwm-background-window))
    (let ((buffer (exwm--id->buffer id)))
      ;; Hmm, there seems to be a bug in exwm, where this doesn't respond properly.
      ;; Even without my code
      ;; Fullscreened window, in another workspace, browse-url, can't reset screen for some resaon 
      ;; (switch-to-buffer buffer)
      ;; For now, we shall just unbreak it by running the advice
      (exwm-background/exwm-show-advice exwm-background/exwm-background-window))))
;; (exwm-background/exwm-show-advice exwm-background/exwm-background-window)

;; Further investigation required to fix this bug.
;; Relevant functions:
;; exwm--on-ClientMessage - This function is called during the browse-url. It will attempt to un-fullscreen the firefox window
;; exwm-layout-unset-fullscreen - This is the function that is dispatched to by the previous function

(advice-add #'exwm-layout-unset-fullscreen
            :before
            #'exwm-background/exwm-unset-fullscreen)

;; Send emacs frame to the background
(defvar exwm-background/background-workspace nil)

(defun exwm-background/exwm-workspace--set-active (fun frame active)
  (if (eq frame exwm-background/background-workspace)
      (if (not active)
          (set-frame-parameter frame 'alpha `(100 . 100))
        (set-frame-parameter frame 'alpha `(,exwm-background/current-transparency . 50))
        (funcall fun frame active))
    (funcall fun frame active)))

(advice-add #'exwm-workspace--set-active :around #'exwm-background/exwm-workspace--set-active)

(defun exwm-background/exwm-workspace-background ()
  "Sets the current  EXWM workspace to be persistent. 
When you switch away from the workspace, the workspace 
will become opaque in order to maximize visibility behind
the other workspace. "
  (interactive)
  (setq exwm-background/background-workspace
        (cond ((not (= -1 exwm-background/exwm-background-window))
               (message "Can't set workspace as background when X window is background")
               nil)
              ((null exwm-background/background-workspace)
               (message "Frame is now the background frame!")
               (selected-frame))
              ((not (eq exwm-background/background-workspace (selected-frame)))
               (set-frame-parameter exwm-background/background-workspace 'alpha `(,exwm-background/current-transparency . 50))
               (message "Frame is now the background frame!")
               (selected-frame))
              (t (message "No longer background frame!")
                 nil))))


(advice-add #'exwm-workspace--set-active :around #'exwm-background/exwm-workspace--set-active)
;; send keyevent to specific window
(defun exwm-background/exwm-input--fake-key-to-window (id event)
  "Fake a key event equivalent to Emacs event EVENT."
  (let* ((keysym (xcb:keysyms:event->keysym exwm--connection event))
         keycode)
    (when (= 0 (car keysym))
      (user-error "[EXWM] Invalid key: %s" (single-key-description event)))
    (setq keycode (xcb:keysyms:keysym->keycode exwm--connection
                                               (car keysym)))
    (when (/= 0 keycode)
      (exwm--log "id=#x%x event=%s keycode" id event keycode)
      (dolist (class '(xcb:KeyPress xcb:KeyRelease))
        (xcb:+request exwm--connection
            (make-instance 'xcb:SendEvent
                           :propagate 0 :destination id
                           :event-mask xcb:EventMask:NoEvent
                           :event (xcb:marshal
                                   (make-instance class
                                                  :detail keycode
                                                  :time xcb:Time:CurrentTime
                                                  :root exwm--root :event id
                                                  :child 0
                                                  :root-x 0 :root-y 0
                                                  :event-x 0 :event-y 0
                                                  :state (cdr keysym)
                                                  :same-screen 1)
                                   exwm--connection)))))
    (xcb:flush exwm--connection)))

(defun exwm-background/exwm-send-key-to-background ()
  "If an X window is in the background, send a key input to that window. 
Not entirely bug-free, due to my lack of understanding of reading and 
translating key sequences. Some keys like 'escape' don't always work. 
Needs to be fixed"
  (interactive)
  (if (= exwm-background/exwm-background-window -1)
      (message "There is no background window...")
    (let ((event (read-event "Sequence? ")))
      (exwm-background/exwm-input--fake-key-to-window exwm-background/exwm-background-window event))))

(defun exwm-background/exwm-send-key-to-background-loop ()
  "Suffers from same bugs as the other one, but loops until 'q' is pressed."
  (interactive)
  (if (= exwm-background/exwm-background-window -1)
      (message "There is no background window...")
    (let (event)
      (while (progn
               (setq event (read-key "Sequence? "))
               (or (not (eq (type-of event) 'integer))
                   (not (= 113 event))))
        (exwm-background/exwm-input--fake-key-to-window exwm-background/exwm-background-window event)))))


(provide 'exwm-background)
