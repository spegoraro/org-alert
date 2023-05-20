;; -*- lexical-binding: t; -*-

(ert-deftest org-alert-custom-cutoff ()
  "checks that we can extract the correct cutoff from the
 PROPERTIES of a subtree"
  (let ((org-directory ".")
	(org-agenda-files (list "test.org")))
    (should (equal
	     '(("remindern test" "09:55" 15))
	     (org-map-entries 'org-alert--parse-entry org-alert-match-string 'agenda
			      '(org-agenda-skip-entry-if 'todo
							 org-done-keywords-for-agenda))))))

;; TODO idea here is generate an org file with a timestamp in the near future to
;; check if the notification actually works

;; (let ((org-directory ".")
;;       (org-agenda-files (list "test.org")))
;;   (org-alert-check))

;; (org-time-stamp '(16) nil)
;; <2023-05-20 Sat 10:22>

