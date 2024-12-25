;; -*- lexical-binding: t; -*-

(defvar test-alert-notifications nil
  "The notifications received so far")

(defun test-alert-notify (info)
  (push info test-alert-notifications))

(defun test-alert-reset ()
  (setq test-alert-notifications nil))

(alert-define-style 'test-alert :notifier #'test-alert-notify)

(cl-defmacro with-test-org (&rest body)
  `(let ((org-directory ".")
		 (org-agenda-files (list "test.org"))
		 (alert-default-style 'test-alert)
		 ;; TODO the fact that I have to include this from my own config is a
		 ;; really bad sign for the default value in the package
		 (org-alert-match-string
		  "SCHEDULED<\"<yesterday>\"+SCHEDULED<\"<tomorrow>\""))
	 ,@body
	 (test-alert-reset)))

(ert-deftest org-alert-custom-cutoff ()
  "checks that we can extract the correct cutoff from the
 PROPERTIES of a subtree"
  (with-test-org
    (should (equal
			 '(("remindern test" "09:55" 15))
			 (org-map-entries
			  'org-alert--parse-entry org-alert-match-string 'agenda
			  '(org-agenda-skip-entry-if 'todo
										 org-done-keywords-for-agenda))))))

(ert-deftest check-alert-default ()
  "Check that `org-alert-check` sends an alert from `test.org`.

This works because the default `org-alert-notify-after-event-cutoff` is
nil, so any time in the past will be alerted."
  (with-test-org
   (org-alert-check)
   (should (= (length test-alert-notifications) 1))))

(ert-deftest check-alert-none ()
  "Check that `org-alert-check` does not send an alert from `test.org` with
a post-event cutoff set."
  (with-test-org
   (let ((org-alert-notify-after-event-cutoff 60))
   (org-alert-check)
   (should (= (length test-alert-notifications) 0)))))

;; TODO idea here is generate an org file with a timestamp in the near future to
;; check if the notification actually works

;; (let ((org-directory ".")
;;       (org-agenda-files (list "test.org")))
;;   (org-alert-check))

;; (org-time-stamp '(16) nil)
;; <2023-05-20 Sat 10:22>

