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

(defmacro with-current-time (time &rest body)
  "Override `current-time` to return `Sat May 20 09:40:01 2023`, 15 minutes
before the scheduled event in `test.org`."
  ;; (current-time-string '(25704 52657 0 0))
  (declare (indent defun))
  (let ((time (or time '(25704 52657 0 0))))
	`(cl-letf (((symbol-function 'current-time)
				(lambda () ',time)))
	   ,@body)))

(ert-deftest org-alert-custom-cutoff ()
  "checks that we can extract the correct cutoff from the
 PROPERTIES of a subtree"
  (with-test-org
    (should (equal
			 '(("remindern test" "09:55" 15))
			 (org-alert--map-entries 'org-alert--parse-entry)))))

(ert-deftest check-alert-default ()
  "Check that `org-alert-check` sends an alert from `test.org`.

This works because the default `org-alert-notify-after-event-cutoff` is
nil, so any time in the past will be alerted."
  (with-test-org
   (org-alert-check)
   (should (= (length test-alert-notifications) 1))))

(ert-deftest check-alert-none-cutoff ()
  "Check that `org-alert-check` does not send an alert from `test.org` with
a post-event cutoff set."
  (with-test-org
   (let ((org-alert-notify-after-event-cutoff 60))
   (org-alert-check)
   (should (= (length test-alert-notifications) 0)))))

(ert-deftest check-alert-some-remindern ()
  "Check that `org-alert-check` sends an alert from `test.org` with
a post-event cutoff set but the current time set appropriately."
  (with-test-org
   (with-current-time nil
	 (let ((org-alert-notify-after-event-cutoff 60))
	   (org-alert-check)
	   (should (= (length test-alert-notifications) 1))))))
