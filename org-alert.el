;;; org-alert.el --- Notify org deadlines via notify-send

;; Copyright (C) 2015 Stephen Pegoraro

;; Author: Stephen Pegoraro <spegoraro@tutive.com>
;; Version: 0.1.0
;; Package-Requires: ((s "1.10.0") (dash "2.11.0") (alert "1.2"))
;; Keywords: org, org-mode, notify, notifications, calendar
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

(defvar org-alert-headline-regexp "\\(Sched.+:.+\\|Deadline:.+\\)"
  "Regexp for headlines to search in agenda buffer.")

(defvar org-alert-alert-before nil
  "Alert only before the set time(min) of the task time, nil means alert all day.")

(defun org-alert--strip-prefix (headline)
  "Remove the scheduled/deadline prefix from HEADLINE."
  (replace-regexp-in-string ".*:\s+" "" headline))


(defun org-alert--unique-headlines (regexp agenda)
  "Return unique headlines from the results of REGEXP in AGENDA."
  (let ((matches (-distinct (-flatten (s-match-strings-all regexp agenda)))))
    (--map (org-alert--strip-prefix it) matches)))

(defun org-alert--filter-not-due-todos (agenda)
  "Return only near due todos"
  (string-join
   (remove-if
    #'(lambda (line)
        (when (string-match "\\([0-9]*[0-9]:[0-9][0-9]\\)." line)
          (let* ((match (match-string 0 line))
                 (diff (-
                        (float-time
                         (date-to-time
                          (format-time-string (concat "%Y-%m-%d " match))))
                        (float-time
                         (date-to-time
                          (current-time-string))))))
            (if org-alert-alert-before
                (> diff (* org-alert-alert-before 60))
              nil)
            )))
    (split-string agenda "\n"))
   "\n")
  )

(defun org-alert--get-headlines ()
  "Return the current org agenda as text only."
  (with-temp-buffer
    (let ((org-agenda-sticky nil)
	  (org-agenda-buffer-tmp-name (buffer-name)))
      (ignore-errors (org-agenda-list 1))
      (org-alert--unique-headlines org-alert-headline-regexp
        (org-alert--filter-not-due-todos
          (buffer-substring-no-properties (point-min) (point-max)))))))


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
  ;; avoid interrupting current command.
  (unless (minibufferp)
    (save-window-excursion
      (save-excursion
        (save-restriction
          (let ((active (org-alert--filter-active (org-alert--get-headlines))))
            (dolist (dl (org-alert--strip-states active))
              (alert dl :title org-alert-notification-title))))))
    (when (get-buffer org-agenda-buffer-name)
      (ignore-errors
    	(with-current-buffer org-agenda-buffer-name
    	  (org-agenda-redo t))))))


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
