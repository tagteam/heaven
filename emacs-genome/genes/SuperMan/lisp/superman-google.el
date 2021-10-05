;;; superman-google.el --- superman has google

;; Copyright (C) 2013-2020  Thomas Alexander Gerds

;; Author: Thomas Alexander Gerds <tag@biostat.ku.dk>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
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
;; 
;; Export meetings to google calendar via gcalcli.
;; If you capture a meeting in this form
;;
;; ,----
;; |  *** iconic Batgirl
;; |  :PROPERTIES:
;; |  :Date:   <1973-01-18 Thu 11:00>
;; |  :Participants: Robin, WonderWoman
;; |  :CaptureDate: <2013-01-16 Wed>
;; |  :GoogleCalendar: Bat
;; |  :END:
;; `----
;;
;; then finalizing the capture \C-c \C-c
;; will prompt you for exporting the appointment
;; to your calendar "Bat"
;;
;; If property GoogleCalendar is not set,
;; the prompt uses the value of 
;; superman-google-default-calendar
;;
;;; Code:

(defvar superman-google-cmd
  "gcalcli" ;; "google"
  "Full path to google command line utility")

(defvar superman-google-calendars
  nil
  "List of google calendar names. E.g. '(\"Work\" \"Family\").")

(defvar superman-google-default-calendar
  nil
  "Name of your default google calendar.")

(defvar superman-google-date-entry
  "MeetingDate"
  "String matching the org-entry which contains the date and time of the appointment.")

(defvar superman-google-default-duration
   "60"
   "Default duration for appointment export to google calendar")

(defun superman-google-export-appointment ()
  (interactive)
   (save-window-excursion
    (org-back-to-heading t)
    (org-narrow-to-subtree)
    (let* ((g-cal (org-entry-get nil "GoogleCalendar"))
	   (g-remind (org-entry-get nil "GoogleReminderMinutes"))
	   (g-string (progn
		       ;; should be g-title but g-string is sjover
		       (looking-at org-complex-heading-regexp)
		       (match-string-no-properties 4)))
	   (g-entry (org-entry-get nil superman-google-date-entry))
	   (g-place (or (org-entry-get nil "Location") ""))
	   (g-date
	    ;; Strip this type of string "<2015-03-24 Tue 13:00--14:20>"
	    (if (string-match "\\( +\\)\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)\\(-+\\)\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)" g-entry)
		(let* ((a (match-string-no-properties 2 g-entry))
		       (sep (match-string-no-properties 3 g-entry))
		       (b (match-string-no-properties 4 g-entry))
		       (d (concat (replace-in-string g-entry (concat sep b) ""))))
		  (string-match org-ts-regexp d)
		  (match-string-no-properties 1 d))
	      (string-match org-ts-regexp g-entry)
	      (match-string-no-properties 1 g-entry)))
	   (g-duration
	    (or (superman-google-calculate-duration g-entry)
		superman-google-default-duration)))
      (when (or
	     ;; use entry calendar
	     g-cal
	     ;; interactively choose google calendar
	     (and 
	      g-entry
	      (progn
		(setq g-cal
		      (completing-read
		       (concat "Choose google calendar (default: " superman-google-default-calendar "): ")
		       (mapcar #'(lambda (entry) (cons entry nil)) superman-google-calendars)
		       nil t nil nil
		       superman-google-default-calendar))
		(org-set-property "GoogleCalendar" g-cal)
		g-cal))))
      (let* ((pre-command
	      (concat superman-google-cmd
		      " --calendar " "'" g-cal "' add"
		      " --title '" g-string "'"
		      " --where '" g-place "'"
		      " --when '" g-date "'"
		      " --duration '" g-duration "'"
		      " --description ''"
		      " --remind '" (or g-remind "0") "'"))
	     (g-command
	      (read-string "Google calendar entry: " pre-command)))
	(when (> (length g-command) 0)
	  (superman-run-cmd g-command
			    "*Superman-google-calendar*"
			    (concat "Running\n" g-command " returned:\n\n"))))))
   (widen))


;; gnus articles med outlook invitation
(defun superman-google-export-outlook ()
  (interactive)
  (save-window-excursion
    (goto-char (point-min))
    (when (re-search-forward "[ Accept ]" nil t)
      (let* ((g-cal superman-google-default-calendar)
	     (g-remind "0")
	     (g-string (save-excursion
			 ;; should be g-title but g-string is sjover
			 (re-search-forward "^Summary:[ \t]+\\(.*\\)$" nil t)
			 (match-string-no-properties 1)))
	     (g-entry (save-excursion
			(re-search-forward "^Time:[ \t]+\\(.*\\)$" nil t)
			(match-string-no-properties 1)))
	     (g-place  (save-excursion
			 (re-search-forward "^Location:[ \t]+\\(.*\\)$" nil t)
			 (match-string-no-properties 1)))
	     (g-date
	      ;; Strip this type of string "<2015-03-24 Tue 13:00--14:20>"
	      (if (string-match "\\( +\\)\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)\\(-+\\)\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)" g-entry)
		  (let* ((a (match-string-no-properties 2 g-entry))
			 (sep (match-string-no-properties 3 g-entry))
			 (b (match-string-no-properties 4 g-entry))
			 (d (concat (replace-in-string g-entry (concat sep b) ""))))
		    (string-match org-ts-regexp d)
		    (match-string-no-properties 1 d))
		(string-match org-ts-regexp g-entry)
		(match-string-no-properties 1 g-entry)))
	     (g-duration
	      (or (superman-google-calculate-duration g-entry)
		  superman-google-default-duration)))
	(when (or
	       ;; use entry calendar
	       g-cal
	       ;; interactively choose google calendar
	       (and 
		g-entry
		(progn
		  (setq g-cal
			(completing-read
			 (concat "Choose google calendar (default: " superman-google-default-calendar "): ")
			 (mapcar #'(lambda (entry) (cons entry nil)) superman-google-calendars)
			 nil t nil nil
			 superman-google-default-calendar))
		  (org-set-property "GoogleCalendar" g-cal)
		  g-cal))))
	(let* ((pre-command
		(concat superman-google-cmd
			" --calendar " "'" g-cal "' add"
			" --title '" g-string "'"
			" --where '" g-place "'"
			" --when '" g-date "'"
			" --duration '" g-duration "'"
			" --description ''"
			" --remind '" (or g-remind "0") "'"))
	       (g-command
		(read-string "Google calendar entry: " pre-command)))
	  (when (> (length g-command) 0)
	    (superman-run-cmd g-command
			      "*Superman-google-calendar*"
			      (concat "Running\n" g-command " returned:\n\n"))))))
    (widen)))

(defun superman-google-calculate-duration (string)
  "Calculate duration of meeting in minutes.

STRING is an org-date-range such as  
'<2015-01-19 Mon 13:00>--<2015-01-19 Mon 13:09>'
"
  (interactive)
  (when (string-match "\\( +\\)\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)\\(-+\\)\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)" string)
    (let* ((a (match-string-no-properties 2 string))
	   (sep (match-string-no-properties 3 string))
	   (b (match-string-no-properties 4 string))
	   (range-string
	    (concat (replace-in-string string (concat sep b) "") sep (replace-in-string string (concat a sep) ""))))
      (setq string range-string)))
  (cond
   ((string-match org-tr-regexp string)
    (let* ((ts1 (match-string-no-properties 1 string))
	   (ts2 (match-string-no-properties 2 string))
	   (havetime (or (> (length ts1) 15) (> (length ts2) 15)))
	   (time1 (org-time-string-to-time ts1))
	   (time2 (org-time-string-to-time ts2))
	   (t1 (org-float-time time1))
	   (t2 (org-float-time time2))
	   (diff (abs (- t2 t1)))
	   (negative (< (- t2 t1) 0))
	   m)
      (if havetime
	  (if negative
	      (error "Negative time range")
	    (number-to-string (floor (/ diff 60)))))))
   ((string-match org-ts-regexp string) "60")))

  ;; (progn (string-match "\\( +\\)\\(\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)\\(-+\\)\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)\\)" "<2015-03-24 Tue 13:00--14:20>")
	 ;; (list (match-string-no-properties 1 "<2015-03-24 Tue 13:00--14:20>")
	       ;; (match-string-no-properties 2 "<2015-03-24 Tue 13:00--14:20>")
	       ;; (match-string-no-properties 3 "<2015-03-24 Tue 13:00--14:20>")
	       ;; (match-string-no-properties 4 "<2015-03-24 Tue 13:00--14:20>")
	       ;; (match-string-no-properties 5 "<2015-03-24 Tue 13:00--14:20>")
	       ;; (match-string-no-properties 6 "<2015-03-24 Tue 13:00--14:20>")
	       ;; (match-string-no-properties 7 "<2015-03-24 Tue 13:00--14:20>")
	       ;; (match-string-no-properties 8 "<2015-03-24 Tue 13:00--14:20>")
	       ;; (match-string-no-properties 9 "<2015-03-24 Tue 13:00--14:20>")
	       ;; )
  ;; (progn (string-match org-tsr-regexp-both "<2015-01-19 Mon 13:00>--<2015-01-20 Tue 13:00>") (match-string-no-properties 1 "<2015-01-19 Mon 13:00>--<2015-01-20 Tue 13:00>"))
  ;; (progn (string-match org-ts-regexp2 "<2015-03-24 Tue 13:00--14:20>")
  ;; (progn (string-match  "\\(\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\( +[^]+0-9>\r\n -]+\\)?\\( +\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\)?\\(-+\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\)?\\)" "<2015-03-24 Tue 13:00--14:20>")
  ;; (list (match-string-no-properties 1 "<2015-03-24 Tue 13:00--14:20>")
  ;; (match-string-no-properties 2 "<2015-03-24 Tue 13:00--14:20>")
  ;; (match-string-no-properties 3 "<2015-03-24 Tue 13:00--14:20>")
  ;; (match-string-no-properties 4 "<2015-03-24 Tue 13:00--14:20>")
  ;; (match-string-no-properties 5 "<2015-03-24 Tue 13:00--14:20>")
  ;; (match-string-no-properties 6 "<2015-03-24 Tue 13:00--14:20>")
  ;; (match-string-no-properties 7 "<2015-03-24 Tue 13:00--14:20>")
  ;; (match-string-no-properties 8 "<2015-03-24 Tue 13:00--14:20>")
  ;; (match-string-no-properties 9 "<2015-03-24 Tue 13:00--14:20>")
  ;; ))
  ;; org-tsr-regexp
  ;; (superman-google-calculate-duration
  ;; "<2015-01-19 Mon 13:00>--<2015-01-20 Tue 13:00>")
 ;; (superman-google-calculate-duration "<2015-03-24 Tue 13:00--14:20>")

(provide 'superman-google)
;;; superman-google.el ends here


