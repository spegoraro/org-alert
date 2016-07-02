;;; org-alert.el --- Notify org deadlines via notify-send

;; Copyright (C) 2015 Stephen Pegoraro

;; Author: Stephen Pegoraro <spegoraro@tutive.com>
;; Version: 0.1.0
;; Package-Requires: ((s "1.10.0") (dash "2.12.0") (alert "1.2"))
;; Keywords: org, org-mode, notify, notifications
;; URL: https://github.com/groksteve/org-alert

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
;; one-shot check call (org-alert-deadlines). To enable repeated
;; checking call (org-alert-enable) and to disable call
;; (org-alert-disable). You can set the checking interval by changing
;; the org-alert-interval variable to the number of seconds you'd
;; like.


;;; Code:

(require 's)
(require 'dash)
(require 'alert)
(require 'org-agenda)


(defvar org-alert-interval 300
  "Interval in seconds to recheck and display deadlines.")


(defvar org-alert-notification-title "*org*"
  "Title to be sent with notify-send.")


(defun org-alert--preserve-agenda-buffer ()
  "Rename any existing agenda buffer to avoid clobbering."
  (let ((agenda-buffer (get-buffer org-agenda-buffer-name)))
    (if agenda-buffer
	(with-current-buffer agenda-buffer
	  (rename-buffer (concat "~" org-agenda-buffer-name))))))


(defun org-alert--restore-agenda-buffer ()
  "Restore the renamed agenda buffer if it exists."
  (let ((agenda-buffer (get-buffer (concat "~" org-agenda-buffer-name))))
    (if agenda-buffer
	(with-current-buffer agenda-buffer
	  (rename-buffer org-agenda-buffer-name)))))


(defun org-alert--strip-prefix (headline)
  "Remove the scheduled/deadline prefix from HEADLINE."
  (replace-regexp-in-string ".+:\s+" "" headline))


(defun org-alert--unique-headlines (regexp agenda)
  "Return unique headlines from the results of REGEXP in AGENDA."
  (let ((matches (-distinct (-flatten (s-match-strings-all regexp agenda)))))
    (--map (org-alert--strip-prefix it) matches)))


(defun org-alert--get-headlines ()
  "Return the current org agenda as text only."
  (let ((agenda-setup org-agenda-window-setup))
    (setq org-agenda-window-setup 'current-window)
    (org-agenda-list 1)
    (setq org-agenda-window-setup agenda-setup)
    (let ((agenda (buffer-substring-no-properties (point-min) (point-max))))
      (kill-buffer)
      (org-alert--unique-headlines "\\(Sched.+:.+\\|Deadline:.+\\)" agenda))))


(defun org-alert--headline-complete? (headline)
  "Return whether HEADLINE has been completed."
  (--any? (s-starts-with? it headline) org-done-keywords-for-agenda))


(defun org-alert--filter-active (deadlines)
  "Remove any completed headings from the provided DEADLINES."
  (-remove 'org-alert--headline-complete? deadlines))


(defun org-alert--strip-states (deadlines)
  "Remove the todo states from DEADLINES."
  (--map (s-trim (s-chop-prefixes org-todo-keywords-for-agenda it)) deadlines))


(defun org-alert-check ()
  "Check for active, due deadlines and initiate notifications."
  (interactive)
  (org-alert--preserve-agenda-buffer)
  (let ((active (org-alert--filter-active (org-alert--get-headlines))))
    (dolist (dl (org-alert--strip-states active))
      (alert dl :title org-alert-notification-title)))
  (org-alert--restore-agenda-buffer))


(defun org-alert-enable ()
  "Enable the notification timer.  Cancels existing timer if running."
  (interactive)
  (org-alert-disable)
  (run-at-time 0 org-alert-interval 'org-alert-check))


(defun org-alert-disable ()
  "Cancel the running notification timer."
  (interactive)
  (dolist (timer timer-list)
    (if (eq (elt timer 5) 'org-alert-check)
	(cancel-timer timer))))



(provide 'org-alert)
;;; org-alert.el ends here
