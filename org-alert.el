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

(require 'alert)
(require 'org-agenda)


(defvar org-alert-interval 300
  "Interval in seconds to recheck and display deadlines.")


(defvar org-alert-notification-title "*org*"
  "Title to be sent with notify-send.")

(defvar org-alert-match-string
  "SCHEDULED>=\"<today>\"+SCHEDULED<\"<tomorrow>\"|DEADLINE>=\"<today>\"+DEADLINE<\"<tomorrow>\""
  "property/todo/tags match string to be passed to `org-map-entries'.")

;; for use in place of (org-get-heading) in dispatch. trying to grab
;; scheduled date from the subtree of the heading instead of just the
;; heading
(defun org-alert--parse-entry ()
  (org-get-heading t t t t)
  (org-copy-subtree)
  (let* ((text (current-kill 0))
	 (beg 0)
	 (end (length text)))
    (set-text-properties beg end nil text)
    text))

;; somewhere have (and cutoff (< cutoff ...)) to check if notif should
;; be sent, and let cutoff default to nil to preserve the old behavior
(defun org-alert--dispatch ()
  (alert (org-get-heading t t t t) :title org-alert-notification-title))

(defun org-alert-check ()
  "Check for active, due deadlines and initiate notifications."
  (interactive)
  (org-map-entries 'org-alert--dispatch org-alert-match-string 'agenda
                   '(org-agenda-skip-entry-if 'todo
                                              org-done-keywords-for-agenda)))


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
