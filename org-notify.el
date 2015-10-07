;;; org-notify.el --- Notify org deadlines via notify-send

;; Copyright (C) 2015 Stephen Pegoraro

;; Author: Stephen Pegoraro <spegoraro@tutive.com>
;; Version: 0.1.0
;; Package-Requires: ((s "1.10.0") (dash "2.12.0") (alert "1.2))
;; Keywords: org, org-mode, notify, notifications
;; URL: https://github.com/groksteve/org-notify

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides functions to display system notifications for
;; any org-mode deadlines that are due in your agenda. To perform a
;; one-shot check call (org-notify-deadlines). To enable repeated
;; checking call (org-notify-enable) and to disable call
;; (org-notify-disable). You can set the checking interval by changing
;; the org-notify-interval variable to the number of seconds you'd
;; like.


;;; Code:

(require 's)
(require 'dash)
(require 'alert)

(defvar org-notify-interval 300
  "Interval in seconds to recheck and display deadlines.")

(defvar org-notify-todo-states '("TODO" "STARTED")
  "List of strings containing possible todo states for your org files.")

(defvar org-notify-done-state "DONE"
  "String containing the default state for done items.")

(defvar org-notify-notification-title "*org*"
  "Title to be sent with notify-send.")

(defun org-notify--get-deadlines ()
  "Return the current org agenda as text only."
  (org-agenda-list 1)
  (let ((agenda-content (buffer-substring-no-properties (point-min) (point-max))))
    (org-agenda-quit)
    (-map (lambda (dl) (s-chop-prefix "Deadline:   " dl))
	  (-flatten (s-match-strings-all "Deadline:.+" agenda-content)))))

(defun org-notify--filter-active (deadlines)
  "Remove any completed headings from the provided DEADLINES."
  (-remove (lambda (dl)
	     (s-starts-with? org-notify-done-state dl)) deadlines))

(defun org-notify--strip-states (deadlines)
  "Remove the todo states from DEADLINES."
  (-map (lambda (dl)
	  (s-trim (s-chop-prefixes org-notify-todo-states dl))) deadlines))

(defun org-notify-check ()
  "Check for active, due deadlines and initiate notifications."
  (interactive)
  (let ((active-deadlines (org-notify--filter-active (org-notify--get-deadlines))))
    (dolist (dl (org-notify--strip-states active-deadlines))
      (alert dl :title org-notify-notification-title))))

(defun org-notify-enable ()
  "Enable the notification timer.  Cancels existing timer if running."
  (interactive)
  (org-notify-disable)
  (run-at-time 0 org-notify-interval 'org-notify-check))

(defun org-notify-disable ()
  "Cancel the running notification timer."
  (interactive)
  (dolist (timer timer-list)
    (if (eq (elt timer 5) 'org-notify-check)
	(cancel-timer timer))))

(provide 'org-notify)
;;; org-notify.el ends here
