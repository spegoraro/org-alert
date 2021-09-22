;;; org-alert.el --- Notify org deadlines via notify-send

;; Copyright (C) 2015 Stephen Pegoraro

;; Author: Stephen Pegoraro <spegoraro@tutive.com>
;; Version: 0.2.0
;; Package-Requires: ((org "9.0") (alert "1.2"))
;; Keywords: org, org-mode, notify, notifications, calendar
;; URL: https://github.com/spegoraro/org-alert

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

(require 'cl-lib)
(require 'alert)
(require 'org-agenda)


(defvar org-alert-interval 300
  "Interval in seconds to recheck and display deadlines.")

;; TODO look for a property of the agenda entry as suggested in
;; https://github.com/spegoraro/org-alert/issues/20
(defvar org-alert-notify-cutoff 10
  "Time in minutes before a deadline a notification should be sent.")

(defvar org-alert-notification-title "*org*"
  "Title to be sent with notify-send.")

(defvar org-alert-match-string
  "SCHEDULED>=\"<today>\"+SCHEDULED<\"<tomorrow>\"|DEADLINE>=\"<today>\"+DEADLINE<\"<tomorrow>\""
  "property/todo/tags match string to be passed to `org-map-entries'.")

(defvar org-alert-time-match-string
  "\\(?:SCHEDULED\\|DEADLINE\\):.*<.*\\([0-9]\\{2\\}:[0-9]\\{2\\}\\).*>"
  "regex to find times in an org subtree. The first capture group
is used to extract the time")

(defun org-alert--read-subtree ()
  "Return the current subtree as a string. Adapted from
`org-copy-subtree` from org-mode."
  (org-preserve-local-variables
   (let (beg end folded (beg0 (point)))
     (org-back-to-heading t)
     (setq beg (point))
     (skip-chars-forward " \t\r\n")
     (save-match-data
       (save-excursion (outline-end-of-heading)
		       (setq folded (org-invisible-p)))
       (ignore-errors (org-forward-heading-same-level (1- n) t))
       (org-end-of-subtree t t))
     ;; Include the end of an inlinetask
     (when (and (featurep 'org-inlinetask)
		(looking-at-p (concat (org-inlinetask-outline-regexp)
				      "END[ \t]*$")))
       (end-of-line))
     (setq end (point))
     (goto-char beg0)
     (when (> end beg)
       (setq org-subtree-clip-folded folded)
       (buffer-substring-no-properties beg end)))))

;; I think this is unnecessary now that we're using read-subtree
;; instead of copy-subtree
(defun org-alert--strip-text-properties (text)
  "Strip all of the text properties from a copy of TEXT and
return the stripped copy"
  (let ((text (substring text)))
    (set-text-properties 0 (length text) nil text)
    text))

(defun org-alert--grab-subtree ()
  "Return the current org subtree as a string with the
text-properties stripped"
  (let* ((subtree (org-alert--read-subtree))
	 (text (org-alert--strip-text-properties subtree)))
    (apply #'concat
	   (cl-remove-if #'(lambda (s) (string= s ""))
			 (cdr (split-string text "\n"))))))

(defun org-alert--to-minute (hour minute)
  "Convert HOUR and MINUTE to minutes"
  (+ (* 60 hour) minute))

(defun org-alert--check-time (time &optional now)
  "Check that TIME is less than current time"
  (let* ((time (mapcar #'string-to-number (split-string time ":")))
	 (now (or now (decode-time (current-time))))
	 (now (org-alert--to-minute (decoded-time-hour now) (decoded-time-minute now)))
	 (then (org-alert--to-minute (car time) (cadr time))))
    (<= (- then now) org-alert-notify-cutoff)))

(defun org-alert--parse-entry ()
  "Parse an entry from the org agenda and return a list of the
heading and the scheduled/deadline time"
  (let ((head (org-alert--strip-text-properties (org-get-heading t t t t)))
	(body (org-alert--grab-subtree)))
    (string-match org-alert-time-match-string body)
    (list head (match-string 1 body))))

(defun org-alert--dispatch ()
  (let* ((entry (org-alert--parse-entry))
	 (head (car entry))
	 (time (cadr entry)))
    (if time
	(when (org-alert--check-time time)
	  (alert (concat time ": " head) :title org-alert-notification-title))
      (alert head :title org-alert-notification-title))))

(defun org-alert-check ()
  "Check for active, due deadlines and initiate notifications."
  (interactive)
  (org-map-entries 'org-alert--dispatch org-alert-match-string 'agenda
		   '(org-agenda-skip-entry-if 'todo
					      org-done-keywords-for-agenda))
  t)

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
