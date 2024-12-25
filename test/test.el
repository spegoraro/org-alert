;; -*- lexical-binding: t; -*-

(defvar test-alert-notifications nil
  "The notifications received so far")

(defun test-alert-notify (info)
  (push info test-alert-notifications))

(defun test-alert-reset ()
  (setq test-alert-notifications nil))

(alert-define-style 'test-alert :notifier #'test-alert-notify)

(cl-defmacro with-test-org (agenda-file &rest body)
  (declare (indent defun))
  (let ((agenda-file (or agenda-file "test.org")))
	`(let ((org-directory ".")
		   (org-agenda-files (list ,agenda-file))
		   (alert-default-style 'test-alert)
		   ;; TODO the fact that I have to include this from my own config is a
		   ;; really bad sign for the default value in the package
		   (org-alert-match-string
			"SCHEDULED<\"<yesterday>\"+SCHEDULED<\"<tomorrow>\""))
	   (with-environment-variables (("TZ" "UTC4"))
		 (unwind-protect
			 (progn ,@body)
		   (test-alert-reset))))))

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
  (with-test-org nil
    (should (equal
			 '(("remindern test" "09:55" 15))
			 (org-alert--map-entries 'org-alert--parse-entry)))))

(ert-deftest check-alert-default ()
  "Check that `org-alert-check` sends an alert from `test.org`.

This works because the default `org-alert-notify-after-event-cutoff` is
nil, so any time in the past will be alerted."
  (with-test-org nil
	(org-alert-check)
	(should (= (length test-alert-notifications) 1))))

(ert-deftest check-alert-none-cutoff ()
  "Check that `org-alert-check` does not send an alert from `test.org` with
a post-event cutoff set."
  (with-test-org nil
	(let ((org-alert-notify-after-event-cutoff 60))
	  (org-alert-check)
	  (should (= (length test-alert-notifications) 0)))))

(ert-deftest check-alert-some-remindern ()
  "Check that `org-alert-check` sends an alert from `test.org` with
a post-event cutoff set but the current time set appropriately."
  (with-test-org nil
	(with-current-time (25704 52667 0 0) ; 9:40:11
	  (let ((org-alert-notify-after-event-cutoff 60))
		(should (= (length test-alert-notifications) 0))
		(org-alert-check)
		(should (= (length test-alert-notifications) 1))))))

(ert-deftest check-alert-none-remindern ()
  "Check that `org-alert-check` sends an alert from `test.org` with
a post-event cutoff set but the current time set appropriately."
  (with-test-org nil
	;; (current-time-string '(25704 52655 0 0)) => "Sat May 20 09:39:59 2023" or
	;; just before the notification should trigger
	(with-current-time (25704 52655 0 0)
	  (let ((org-alert-notify-after-event-cutoff 60))
		(org-alert-check)
		(should (= (length test-alert-notifications) 0))))))

(ert-deftest check-alert-some ()
  (with-test-org "plain.org"
	(with-current-time (25704 52957 0 0) ; 9:45:01
	  (org-alert-check)
	  (should (= (length test-alert-notifications) 1)))))

(ert-deftest check-alert-none ()
  (with-test-org "plain.org"
	(with-current-time (25704 52945 0 0) ; 9:44:49
	  (should (= (length test-alert-notifications) 0))
	  (org-alert-check)
	  (should (= (length test-alert-notifications) 0)))))
