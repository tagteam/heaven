;;; superman.el --- org project manager

;; Copyright (C) 2013-2016 Thomas Alexander Gerds

;; Authors:
;; Thomas Alexander Gerds <tag@biostat.ku.dk>
;; Klaus Kaehler Holst <kkho@biostat.ku.dk>
;;
;; Keywords: tools

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

;; SuperMan is a project which manages all your other projects
;; Q: Does the super project contain itself?
;; A: Nice question. Some answers are in here: www.logicomix.com

;;; Code:

;;{{{ superman 

(defvar superman-greetings 
  "The project manager helps you to switch between your projects."
  "Welcome text for `superman'. Set to nil or \"\" to suppress the message.")

(defalias 'S 'superman)
(defun superman ()
  "Function to control the list of active projects (`superman-project-alist'). It displays 
the contents of the file `superman-profile'."
  (interactive)
  (let* ((cats-buffer "*S*")
	 (cats (reverse superman-project-categories))
	 (cat-alist (mapcar (lambda (x) (list x)) cats))
	 (howmany-cats (length cats))
	 (cat-number-one (car cats))
	 (projects superman-project-alist))
    (switch-to-buffer cats-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (org-mode)
    (font-lock-mode -1)
    (superman-make-header)
    (goto-char (point-max))
    ;; parse projects by category using superman-balls
    (while projects
      (let* ((pro (car projects))
	     (cat (or (cdr (assoc "category" (cadr pro))) "Krypton"))
	     (m (- howmany-cats (length (member cat cats))))
	     (tail (cdr (nth m cat-alist))))
	(if tail
	    (setcdr (nth m cat-alist) (append tail (list pro)))
	  (when (nth m cat-alist)
	    (setcdr (nth m cat-alist) (list pro)))))
      (setq projects (cdr projects)))
    ;; (insert "\n")
    ;; project directory
    ;; (insert (superman-make-button
    ;; "Set-up:" '(:fun superman-show-setup
    ;; :face superman-header-button-face :help "Edit superman(ager) set-up"))
    ;; " "
    ;; (superman-make-button
    ;; "Edit" '(:fun superman-edit-setup
    ;; :face superman-capture-button-face :help "Change superman(ager) set-up"))
    ;; " "
    ;; (superman-make-button
    ;; "Diagnose" '(:fun superman-diagnose-setup
    ;; :face superman-capture-button-face :help "Diagnose superman set-up.")))
    ;; welcome text
    (when (and superman-greetings (stringp superman-greetings))
      (insert "\n" superman-greetings "\n"))
    ;; action buttons
    (insert "\n" (superman-make-button
		  "New project" '(:fun superman-capture-project
				       :face superman-capture-button-face
				       :help "Start new project or register existing project" :width 15)))
    (insert " " (superman-make-button
		 "Choose project" '(:fun superman-switch-to-project
					 :face superman-capture-button-face
					 :help "Select a project and turn it on" :width 15)))
    ;; (superman-view-insert-action-buttons
    ;; '((" New project " superman-capture-project)))
    ;; ("Meeting" superman-capture-meeting)
    ;; ("Task" superman-capture-task)))
    (insert "\n")
    ;; loop over categories
    (while cat-alist
      (let* ((cat (car cat-alist))
	     (cat-name (car cat))
	     (cat-fun 'superman-tab)
	     ;; `(lambda () (interactive)
	     ;; (superman-capture-project nil ,cat-name)))
	     (tail (cdr cat)))
	;; (insert "\n** " cat-name)
	(insert "\n** "
		(superman-make-button
		 cat-name
		 `(:fun ,cat-fun
			:face superman-capture-button-face
			:help ,(concat "Add project in category " cat-name))))
	(put-text-property (point-at-bol) (point-at-eol) 'face 'org-level-2)
	(put-text-property (point-at-bol) (point-at-eol) 'cat 'cat-name)
	(put-text-property (point-at-bol) (point-at-eol) 'balls superman-balls)
	(put-text-property (point-at-bol) (point-at-eol) 'display (concat "★ " cat-name))
	(insert " [" (number-to-string (length tail)) "]")
	;; loop over projects (tail) in category
	(insert "\n")
	(superman-format-loop tail superman-balls)
	(put-text-property (- (point-at-eol) 1) (point-at-eol) 'tail cat-name)
	;; column names
	(org-back-to-heading)
	(end-of-line)
	(let ((first-item (next-single-property-change (point-at-eol) 'superman-item-marker)))
	  (when first-item
	    (goto-char first-item)
	    (forward-line -1)
	    (end-of-line)
	    (insert "\n")
	    (insert (superman-column-names superman-balls))
	    ;; sorting
	    ;; (goto-char (next-single-property-change (point-at-bol) 'superman-item-marker))
	    (when (next-single-property-change (point-at-bol) 'sort-key)
	      (goto-char (+ 2 (next-single-property-change (point-at-bol) 'sort-key)))
	      (superman-sort-section))))
	(goto-char (point-max))
	(setq cat-alist (cdr cat-alist)))))
  (goto-char (point-min))
  (superman-on)
  (setq buffer-read-only t))

(defun superman-show-setup ()
  (interactive)
  (superman-edit-setup 'edit))

;; FIXME
(defun superman-edit-setup (&optional read-only)
  "Edit heading Set-up in `superman-profile'"
  (interactive)
  (let* ((scene (current-window-configuration))
	 marker
	 current-set-up
	 (superman-setup-scene-hook
	  (if read-only
	      (append '(superman-view-item-mode)
		      'superman-setup-scene-hook)
	    superman-setup-scene-hook))
	 (clean-scene-hook #'(lambda ()
			       (goto-char (point-min))
			       ;; (outline-next-heading)
			       (when (re-search-forward ":PROPERTIES:" nil t)
				 (superman-parse-setup (point) (superman-defaults))))))
    ;; find current set-up
    (find-file superman-profile)
    (widen)
    (show-all)
    (goto-char (point-min))
    (if (re-search-forward "SupermanSetup" nil t)
	(superman-parse-setup (point) (superman-defaults))
      (while (looking-at "\\#\\+")
	(forward-line 1))
      (insert "\n")
      (insert (superman-set-up-defaults)))
    (org-back-to-heading)
    (setq marker (point-marker)
	  current-set-up
	  (buffer-substring
	   (point)
	   (progn
	     (org-end-of-subtree t t)
	     (if (and (org-at-heading-p)
		      (not (eobp))) (backward-char 1))
	     (point))))
    (superman-capture-whatever
     marker
     "Superman setup"
     0 ;; level 0 because we are pasting a heading in
     current-set-up
     nil (not read-only) scene nil 
     read-only nil clean-scene-hook nil)))

(defun superman-parse-setup (pom list &optional fill)
  "Parse properties at point or marker POM and loop over 
list. List contains (variable value) pairs. If a property matches the
symbol-name of a variable, then the value of the variable is set to the value of the property.

If FILL is non-nil fill property list with those elements of list that do not match
the existing properties."
  (let ((setup list))
    (while setup
      (let* ((el (car setup))
	     (var (car el))
	     (varname (symbol-name var))
	     (value (superman-get-property pom varname nil)))
	(if value
	    (set var value)
	  (when fill 
	    (org-entry-put pom varname (cadr el)))))
      (setq setup (cdr setup)))))

(defun superman-set-up-defaults ()
  (let ((props (apply #'concat
		      (mapcar #'(lambda (d)
				  (let ((thing (cadr d))
					value)
				    (concat ":" (symbol-name (car d)) ": "
					    (cond
					     ((ignore-errors (setq value (symbol-value (intern thing))))
					      (cond ((symbolp value) (symbol-name value))
						    ((integerp value) (number-to-string value))
						    ((stringp value) value)))
					     ((symbolp thing) (symbol-name thing))
					     ((integerp thing) (number-to-string thing))
					     ((stringp thing) thing))
					    "\n")))
			      (superman-defaults)))))
    (concat
     "*** SupermanSetup\n:PROPERTIES:\n"
     props
     ":END:\n")))

(defun superman-defaults ()
  "Create an alist of superman options."
  (require 'superman-manager)
  `((superman-default-directory ,superman-default-directory)
    (superman-default-category ,(or superman-default-directory "Krypton"))
    (superman-org-location ,superman-org-location)
    ;; (superman-project-kal-el ,superman-project-kal-el)
    (superman-cmd-git ,superman-cmd-git)
    ;; (superman-switch-to-project [f2])
    (superman-save-buffers ,superman-save-buffers)
    (superman-select-project-completion-format
     ,superman-select-project-completion-format)))

     
(defun superman-make-header ()
  "Insert header into superman project view buffer"
  (goto-char (point-min))
  (insert  (superman-make-button
	    " SuperMan "
	    '(:fun superman-redo :face superman-face)))
  ;; (insert "SuperMan(ager)")
  (put-text-property (point-at-bol) (point-at-eol) 'redo-cmd '(superman))
  ;; (put-text-property (point-at-bol) (point-at-eol) 'face 'org-level-1)
  (put-text-property (point-at-bol) (point-at-eol) 'index superman-profile)
  (put-text-property (point-at-bol) (point-at-eol) 'nickname "Kal-El")
  (when (fboundp 'eg)
    (insert " "
	    (superman-make-button "Home"
				  '(:fun eg
					 :face superman-next-project-button-face
					 :help "Show EmacsGenome"))))
  (insert

   "  "
   (superman-make-button "Agenda"
			 '(:fun superman-agenda
			 :face superman-next-project-button-face
			 :help "Agenda across all projects"))
   "  "
   (superman-make-button "Calendar"
			 '(:fun superman-calendar
			 :face superman-next-project-button-face
			 :help "Project-wide calendar"))
   "  "
   (superman-make-button "TodoList"
			 '(:fun superman-todo
			 :face superman-next-project-button-face
			 :help "TodoList across all projects"))
   "\n"))

;;}}}
;;{{{ balls and format

(defvar superman-balls
  '((todo ("width" 9) ("face" superman-get-todo-face))
    (priority ("width" 8) ("face" superman-get-priority-face))
    (hdr ("width" 27) ("face" font-lock-function-name-face)
	 ("name" "Description")
	 ("fun" superman-trim-project-nickname))
    ("marker" ("width" 33)
     ("name" "Project")
     ("face" superman-next-project-button-face)
     ("fun" superman-make-project-button))
    ("lastvisit" ("fun" superman-trim-date)
     ("width" 17)
     ("face" font-lock-type-face)
     ("sort-key" t))
    ("others" ("width" 66) ("face" font-lock-keyword-face))
    ("InitialVisit" ("fun" superman-trim-date)
     ("width" 17)
     ("face" font-lock-type-face)
     ("sort-key" nil)))
  "Definition of columns to be shown in overview buffer of superman projects.")


(defun superman-format-loop (list balls)
  "Loop over list and insert all items formatted according to balls."
  (while list
    (let* ((item (superman-format-thing (car list) balls)))
      (insert item)
      (end-of-line)
      (insert "\n")
      (setq list (cdr list)))))

(defun superman-get-priority-face (kwd)
  "Get face for priority symbols"
  (if (< (length kwd) 3)
     'org-priority
    (or (org-face-from-face-or-color
	 'priority 'org-priority
	 (cdr (assoc (string-to-char
		      (substring kwd 2 3))
		     org-priority-faces)))
	'org-priority)))

(defun superman-get-todo-face (kwd)
  "A slight modification of `org-get-tag-face'"
  (or (org-face-from-face-or-color
       'todo 'org-todo (cdr (assoc kwd org-todo-keyword-faces)))
      (and (member kwd org-done-keywords) 'org-done)
      'org-todo))

;;}}}
;;{{{ parse project index files
(defvar superman-agenda-file (concat superman-default-directory "SuperAgenda.org"))

;;org-diary
;;org-agenda
;;org-agenda-list
;;org-timeline
;;org-agenda-get-day-entries
(defun superman-parse-index-files ()
  (interactive)
  (let ((org-agenda-buffer-name (concat "*S-TODO*"))
	(org-agenda-sticky nil)
	(org-agenda-custom-commands
	 `(("P" "Projects-parser"
	    ((,(intern (if superman-todo-tags "tags-todo" "alltodo"))
	      ,(if superman-todo-tags superman-todo-tags "")
	      ((org-agenda-files
		(reverse (superman-index-list
			  nil nil nil nil nil
			  superman-exclude-from-todo-regexp))))))
	    ((org-agenda-window-setup 'current-window)
	     (org-agenda-finalize-hook
	      'superman-parser-copy-items))))))
    (push ?P unread-command-events)
    (call-interactively 'org-agenda)))

(defun superman-parser-copy-items ()
  (interactive)
  (let* ((org-agenda-files `(,superman-agenda-file))
	 (org-refile-targets '((org-agenda-files :maxlevel . 2))))
    (while (ignore-errors
	     (goto-char (next-single-property-change (point) 'org-hd-marker)))
      (org-with-point-at (get-text-property (point-at-bol) 'org-hd-marker) (org-copy))
      (goto-char (point-at-eol)))))
	

;;}}}
;;{{{ black board
(defvar superman-black-board "~/.superman-black-board.org" "File in which to save the black board")
(defun superman-black-board () (interactive) (find-file superman-black-board))
;;}}}
;;{{{ Agenda
(defun superman-make-agenda-title (string face)
  (put-text-property 0 (length string) 'face face string)
  string)
(defalias 'S-agenda 'superman-agenda)
(defun superman-agenda (&optional project)
  "Similar to `superman-calendar' but does not show
all dates."
  (interactive)
  (let ((org-agenda-buffer-name "*SuperMan-Agenda*")
	(org-agenda-sticky nil)
	(org-agenda-custom-commands nil))
    (add-to-list 'org-agenda-custom-commands
		 '("A" "Superman agenda"
		   ((agenda "" ((org-agenda-files (superman-index-list)))))
		   ((org-agenda-compact-blocks nil)
		    (org-agenda-show-all-dates nil)
		    (org-agenda-buffer-name "*SuperMan-Agenda*")
		    (org-agenda-this-buffer-name "*SuperMan-Agenda*")
		    (org-agenda-window-setup 'current-window)
		    (org-agenda-overriding-header
		     (concat (superman-make-agenda-title "SupermanAgenda" 'org-level-2)
			     "  "
			     (superman-make-button "TodoList"
						   '(:fun superman-todo
							  :face superman-next-project-button-face
							  :help "TodoList across all projects"))
			     "  "
			     (superman-make-button "Calendar"
						   '(:fun superman-calendar
							  :face superman-next-project-button-face
							  :help "Project-wide calendar"))
			     "  "
			     (superman-make-button "Projects"
						   '(:fun superman
							  :face superman-next-project-button-face
							  :help "List of projects"))
			     "\n"
			     (superman-make-button "\nAdd a Meeting\n"
						   '(:fun (lambda ()
							    (interactive)
							    (superman-capture-meeting
							     nil nil "Capture meeting for project: "))
							  :face superman-capture-button-face
							  :help "Add a meeting to project"))
			     "\n")))))
    (push ?A unread-command-events)
    (call-interactively 'org-agenda)))

;;}}}
;;{{{ Todo lists
(defvar superman-todo-tags nil "Regexp to match todo-tags that should popup in the global todo list")
(defvar superman-exclude-from-todo-regexp nil "Regexp to match index-files that should not contribute todo lists")


(defalias 'S-todo 'superman-todo)
(defun superman-todo (&optional project read-index-files)
  "Show todo list for PROJECT."
  (interactive)
  (let* ((org-agenda-buffer-name (concat "*S-TODO*"))
	 (org-agenda-sticky nil)
	 (org-agenda-custom-commands
	  `(("P" "Projects-TODO"
	     ((,(intern (if superman-todo-tags "tags-todo" "alltodo"))
	       ,(if superman-todo-tags superman-todo-tags "")
	       ((org-agenda-files
		 (superman-index-list
		  nil nil nil nil nil
		  superman-exclude-from-todo-regexp)))))
	     ((org-agenda-window-setup 'current-window)
	      (pretty superman-pretty-agenda)
	      (org-agenda-finalize-hook 'superman-pretty-todolist))))))
    (push ?P unread-command-events)
    (call-interactively 'org-agenda)))

(defun superman-pretty-todolist ()
  (superman-format-agenda
   superman-todolist-balls
   '(superman-todo)
   (superman-make-button 
    "Todo-list (all projects)"
    '(:fun superman-redo
	   :face superman-face
	   :help (concat "Refresh agenda. Last update: " 
			 (format-time-string "%r"))))
   (concat "  "
	   (superman-make-button "Agenda"
				 '(:fun superman-agenda
					:face superman-next-project-button-face
					:help "Agenda across all projects"))
	   "  "
	   (superman-make-button "Calendar"
				 '(:fun superman-calendar
					:face superman-next-project-button-face
					:help "Project-wide calendar"))
	   "  "
	   (superman-make-button "Projects"
				 '(:fun superman
					:face superman-next-project-button-face
					:help "List of projects"))
	   "\n\n"
	   (superman-make-button "New task"
				 '(:fun (lambda ()
					  (interactive)
					  (superman-capture-task nil nil "Capture task for project: "))
					:face superman-capture-button-face
					:help "Add a task to one of the projects")))
   nil 'pretty))

;;}}}
;;{{{ Calendar

(defun superman-calendar (&optional project)
  "Show events from all projects in a calendar view."
  (interactive)
  (let ((org-agenda-buffer-name "*SuperMan-Calendar*")
	(org-agenda-sticky nil)
	(org-agenda-custom-commands nil))
    (add-to-list 'org-agenda-custom-commands
		 '("C" "Superman calendar"
		   ((agenda "" ((org-agenda-files (superman-index-list)))))
		   ((org-agenda-compact-blocks nil)
		    (org-agenda-show-all-dates t)
		    (org-agenda-span 7)
		    (org-agenda-buffer-name "*SuperMan-Calendar*")
		    (org-agenda-this-buffer-name "*SuperMan-Calendar*")
		    (org-agenda-window-setup 'current-window)
		    ;; (org-agenda-finalize-hook 'superman-add-appointments)
		    (org-agenda-overriding-header
		     (concat (superman-make-agenda-title "Superman calendar" 'org-level-2)
			     "  "
			     (superman-make-button "TodoList"
						   '(:fun superman-todo
							  :face superman-next-project-button-face
							  :help "TodoList across all projects"))
			     "  "
			     (superman-make-button "Agenda"
						   '(:fun superman-agenda
							  :face superman-next-project-button-face
							  :help "Project-wide agenda"))
			     "  "
			     (superman-make-button "Projects"
						   '(:fun superman
							  :face superman-next-project-button-face
							  :help "List of projects"))
			     "\n"
			     (superman-make-button "\nAdd a Meeting\n"
						   '(:fun (lambda ()
							    (interactive)
							    (superman-capture-meeting
							     nil nil "Capture meeting for project: "))
							  :face superman-capture-button-face
							  :help "Add a meeting to calendar"))
			     )))))
    (push ?C unread-command-events)
    (call-interactively 'org-agenda)))

;;}}}
;;{{{ Cycle view 

(defvar superman-views nil)
(setq superman-views (list 'S 'S-todo 'S-todo-B 'S-agenda))
(defun superman-change-view  (&optional arg)
  (interactive "p")
  ;; cycle view list
  (when (or (eq major-mode 'org-agenda-mode)
	    superman-view-mode)
    (let ((current  (car superman-views))
	  (rest  (cdr superman-views)))
      (setq superman-views rest)
      (add-to-list 'superman-views current 'append)))
  (eval `(,(car superman-views)))
  (superman-view-mode-on)
  (superman-on))
;;}}}
;;{{{ superman-todo-mode-map
(defvar superman-todo-mode-map (make-sparse-keymap)
  "Keymap used for `superman-todo-mode' commands.")
   
(define-minor-mode superman-todo-mode
     "Toggle superman project todo mode.
With argument ARG turn superman-todo-mode on if ARG is positive, otherwise
turn it off.
                   
Enabling superman-todo mode electrifies the column todo for documents
for git and other actions like commit, history search and pretty log-todo."
     :lighter " *S-todo*"
     :group 'org
     :keymap 'superman-todo-mode-map)


(defun superman-todo-edit-item ()
  (interactive)
  (let ((marker (get-text-property (point-at-bol) 'org-hd-marker)))
    (if (not marker)
	(message "Nothing to do here: Missing value of org-hd-marker at beginning of line.")
	(switch-to-buffer (marker-buffer marker))
	(goto-char marker))))

(defun superman-todo-mode-on ()
  (interactive)
  (superman-todo-mode t))
(define-key superman-todo-mode-map "a" 'superman-todo-show-priority-all)
(define-key superman-todo-mode-map "A" 'superman-todo-show-priority-A)
(define-key superman-todo-mode-map "B" 'superman-todo-show-priority-B)
(define-key superman-todo-mode-map "C" 'superman-todo-show-priority-C)

(define-key superman-todo-mode-map "n" 'superman-next-entry)
(define-key superman-todo-mode-map "p" 'superman-previous-entry)
(define-key superman-todo-mode-map [(up)] 'superman-previous-entry)
(define-key superman-todo-mode-map [(down)] 'superman-next-entry)
(define-key superman-todo-mode-map "e" 'superman-todo-edit-item)
(define-key superman-todo-mode-map [return] 'superman-hot-return)
(define-key superman-todo-mode-map "R" 'superman-redo)

(define-key superman-todo-mode-map "t" 'superman-view-toggle-todo)
(define-key superman-todo-mode-map  [(shift up)] 'superman-view-priority-up)
(define-key superman-todo-mode-map [(shift down)] 'superman-view-priority-down)
(define-key superman-todo-mode-map "N" #'(lambda () (interactive)
					   (if (eq (car (get-text-property (point-min) 'redo-cmd))
						   'superman-todo)
					       (superman-capture-task nil nil "Capture task for project: ")
					     (superman-capture-meeting nil nil "Capture meeting for project: "))))
(define-key superman-todo-mode-map "P" 'superman-pretty-agenda)

;;}}}
;;{{{ superman-mode-map

(require 'superman-views)

(defvar superman-mode-map (copy-keymap superman-view-mode-map)
  "Keymap used for `superman-mode' commands.")
   
(define-minor-mode superman-mode 
  "Toggle org projectmanager document superman mode.
With argument ARG turn superman-mode on if ARG is positive, otherwise
turn it off.

Enabling superman mode electrifies the superman buffer for project management."
     :lighter " *S*"
     :group 'org
     :keymap 'superman-mode-map)

(defun superman-on ()
  (interactive)
  (when superman-hl-line (hl-line-mode 1))
  (superman-mode t))


(defun superman-update-project-overview ()
  (save-excursion
    (if (get-buffer "*S*")
	(switch-to-buffer (get-buffer "*S*"))
      (superman))
    (superman-redo)))

(setq superman-agenda-balls
      '((index ("width" 23) ("face" font-lock-keyword-face) ("name" "File"))
	(todo ("width" 7) ("face" superman-get-todo-face))
	(hdr ("width" 23) ("face" font-lock-function-name-face) ("name" "Description"))
	("DEADLINE" ("fun" superman-trim-date) ("face" superman-warning-face))
	(".*Date" ("fun" superman-trim-date) ("regexp" t) ("face" font-lock-string-face))
	("FileName" ("fun" superman-dont-trim))))

;; FIXME: It requires some efforts to associate the project with a given index file ...
;;        Making PROJECT-ALIST a hash table may improve efficiency? 
(defun superman-trim-project-attribute (marker attribute &optional dont-trim args)
  (if (markerp marker)
      (let* ((pro-list (mapcar
			(lambda (p)
			  (let ((index (cdr (assoc "index" (cadr p)))))
			    (if (or (bufferp index)
				    (not (file-exists-p index)))
				""
			      (cons (expand-file-name index)
				    (if (string= attribute "nickname") (car p)
				      (cdr (assoc attribute (cadr p))))))))
			superman-project-alist))
	     (ifile (buffer-file-name (marker-buffer marker)))
	     (attr (or (cdr (assoc (expand-file-name ifile) pro-list)) "--")))
	(if dont-trim
	    attr
	  (superman-trim-string attr args)))
    (if dont-trim
	marker
      (superman-trim-string marker args))))

(defun superman-make-project-button (nick &optional args)
  (let* ((ismarker (markerp nick))
	 (nickname (if ismarker
		       (superman-get-property nick "nickname")
		     (superman-trim-string nick args)))
	 (button (superman-make-button
		  (if ismarker
		      (superman-trim-string nickname args)
		    nickname)
		  `(:fun (lambda () (interactive)
			   (superman-switch-to-project ,(if ismarker
							    nickname
							  nick)))
			 :help (concat "Switch to "
				       (if ismarker nickname nick))))))
    button))

(defun superman-trim-project-nickname  (marker &optional args)
  (if (not (markerp marker));; column name
      (superman-trim-string marker args)
    (let* ((nick (superman-trim-project-attribute marker "nickname" 'dont args))
	   (nickname (superman-trim-string nick args))
	   (button (superman-make-button
		    nickname
		    `(:fun (lambda () (interactive) (superman-switch-to-project ,nick))
			   :help (concat "Switch to " ,nick)))))
      button)))
				  

(defun superman-trim-project-others  (marker attribute &optional args)
  (superman-trim-project-attribute marker "others" args))

(defun superman-trim-project-cat  (marker attribute &optional args)
  (superman-trim-project-attribute marker "category" args))

(defun superman-todo-show-more-todo-features ()
  (interactive)
  (setq superman-todolist-balls superman-more-todolist-balls)
  (superman-todo))

(defun superman-todo-show-less-todo-features ()
  (interactive)
  (setq superman-todolist-balls superman-less-todolist-balls)
  (superman-todo))

(defun superman-todo-show-priority-A ()
  (interactive)
  (setq superman-todo-tags "PRIORITY<>\"C\"+PRIORITY<>\"B\"")
  (superman-todo))

(defun superman-todo-show-priority-B ()
  (interactive)
  (setq superman-todo-tags "PRIORITY=\"B\"")
  (superman-todo))

(defun superman-todo-show-priority-C ()
  (interactive)
  (setq superman-todo-tags "PRIORITY=\"C\"")
  (superman-todo))

(defun superman-todo-show-priority-all ()
  (interactive)
  (setq superman-todo-tags nil)
  (superman-todo))

(setq superman-more-todolist-balls
      '((org-hd-marker ("width" 33)
		       ("name" "Project")
		       ;; ("face" superman-next-project-button-face)
		       ("face" superman-next-project-button-face)
		       ("fun" superman-trim-project-nickname))
	(todo ("width" 7) ("face" superman-get-todo-face))
	(priority ("width" 8) ("face" superman-get-priority-face))
	(hdr ("width" 53) ("face" font-lock-function-name-face) ("name" "Description"))
	("DEADLINE" ("fun" superman-trim-date) ("width" 12) ("face" superman-warning-face))
	(".*Date" ("fun" superman-trim-date) ("width" 12) ("regexp" t) ("face" font-lock-string-face) ("name" "Date"))
	(org-hd-marker ("width" 23) ("name" "Others") ("fun" superman-trim-project-others))
	(org-hd-marker ("width" 23) ("name" "Cat") ("fun" superman-trim-project-cat))
	;; (index ("width" 23) ("face" font-lock-keyword-face) ("name" "File"))
	("FileName" ("fun" superman-dont-trim))))

(setq superman-less-todolist-balls 
      '((".*Date" ("fun" superman-trim-date) ("width" 12) ("regexp" t) ("face" font-lock-string-face) ("name" "Date"))
	(priority ("width" 8) ("face" superman-get-priority-face))
	(todo ("width" 7) ("face" superman-get-todo-face))
	(org-hd-marker ("width" 23)
		       ("name" "Project")
		       ("face" superman-next-project-button-face)
		       ("fun" superman-trim-project-nickname))
	;; (org-hd-marker ("width" 23) ("name" "Cat") ("fun" superman-trim-project-cat))
	(hdr ("width" 100) ("face" font-lock-function-name-face) ("name" "Description"))))

(setq superman-todolist-balls superman-less-todolist-balls)

(defun superman-capture-appointments ()
  "Capture today's appointments from all projects."
  (interactive)
  (let ((org-agenda-buffer-name "*SuperMan-Appointments*")
	(org-agenda-sticky nil)
	(org-agenda-custom-commands nil))
    (add-to-list 'org-agenda-custom-commands
		 '("A" "Superman appointments"
		   ((agenda "" ((org-agenda-files (superman-index-list)))))
		   ((org-agenda-compact-blocks nil)
		    (org-agenda-span 1)
		    (org-agenda-buffer-name "*SuperMan-Appointments*")
		    (org-agenda-this-buffer-name "*SuperMan-Appointments**")
		    (org-agenda-window-setup 'current-window)
		    (org-agenda-finalize-hook 'superman-add-appointments))))
    (push ?A unread-command-events)
    (call-interactively 'org-agenda)))

(defun superman-add-appointments ()
  "Go through calendar buffer and add today's appointments."
  (let* ((key (next-single-property-change (point-min) 'org-today))
	 (end (when key (next-single-property-change key 'day)))
	 props
	 marker)
    (while key
      (setq marker (next-single-property-change key 'org-hd-marker nil end))
      (when marker
	(setq props (superman-parse-properties
		     (get-text-property marker 'org-hd-marker)
		     t
		     nil))
	(let* ((case-fold-search t)
	       (meetingdate (plist-get props :meetingdate))
	       (time (when meetingdate (org-parse-time-string meetingdate 'no)))
	       (hour (nth 2 time))
	       (minutes (nth 1 time))
	       (msg (plist-get props :HEADING)))
	  (when (and hour minutes)
	    (setq minutes (number-to-string minutes))
	    (setq hour (number-to-string hour))
	    (when (= (length hour) 1) (setq hour (concat "0" hour)))
	    (when (= (length minutes) 1) (setq minutes (concat "0" minutes)))
	    (appt-add (concat hour ":" minutes) msg 900)))
	;; move key to end of line
	(setq key
	      (if (and marker (< marker end))
		  (next-single-property-change marker 'org-hd-marker nil end)
		nil))))))

(defun superman-format-agenda (&optional balls redo title buttons by pretty)
  "Workhorse for `superman-todo'."
  (let ((balls (or balls superman-agenda-balls))
	;; (redo-cmd org-agenda-redo-command)
	agenda-buffers)
    (save-excursion
      (org-mode);; major
      (font-lock-mode -1)
      (font-lock-default-function nil)
      (goto-char (point-min))
      (delete-region (point-at-bol) (1+ (point-at-eol)))
      (insert "\n")
      (goto-char (point-min))
      (insert (or title "* SupermanAgenda"))
      ;; (put-text-property (point-at-bol) (point-at-eol) 'face 'org-level-2)
      (put-text-property (point-at-bol) (point-at-eol) 'redo-cmd redo)
      (put-text-property (point-at-bol) (point-at-eol) 'cat t)
      (put-text-property (point-at-bol) (point-at-eol) 'balls balls)
      (if buttons (insert buttons))
      (end-of-line)
      (insert "\n\n") 
      (superman-todo-mode-on)
      (if pretty 
	  (superman-pretty-agenda)
	(insert "\n" (superman-make-button
		      "Pretty display (P)" 
		      '(:fun superman-pretty-agenda
			     :face 'superman-header-button-face :help "Prettify display using columns")))
	(put-text-property (point-at-bol) (point-at-eol) 'superman-pretty-button t)
	(insert "\n")))))

(defvar superman-pretty-agenda t
 "If non-nil turn on `superman-pretty-agenda' else show a button which turns it on.") 

(defun superman-pretty-agenda (&optional balls)
  (interactive)
  (if (not superman-todo-mode)
      (error "Works only in agenda buffers where `superman-todo-mode' is switched on")
    (let ((balls (or balls 
		     (get-text-property (point-min) 'balls)
		     superman-todolist-balls))
	  (count 0)
	  (buffer-read-only nil))
      ;; delete button which involved this function
      (when (next-single-property-change (point-min) 'superman-pretty-button)
	(goto-char (next-single-property-change (point-min) 'superman-pretty-button))
	(delete-region (point-at-bol) (point-at-eol)))
      (superman-view-insert-action-buttons
       '(("More columns" :fun superman-todo-show-more-todo-features :help "Show more columns")
	 ("Less columns" :fun superman-todo-show-less-todo-features :help "Show less columns")
	 ("[#A]" :fun superman-todo-show-priority-A  :help "Limit to priority A and no-priority tasks")
	 ("[#B]" :fun superman-todo-show-priority-B  :help "Limit to priority B tasks")
	 ("[#C]" :fun superman-todo-show-priority-C   :help "Limit to priority B tasks")
	 ("All(a)" :fun superman-todo-show-priority-all  :help "Show all priorities")))
      (when (next-single-property-change (point) 'org-hd-marker)
	(goto-char (next-single-property-change (point) 'org-hd-marker))
	(beginning-of-line)
	(insert "\n" (superman-column-names balls) "\n") )
      ;; (superman-view-mode-on) ;; minor modes
      ;; (setq org-agenda-this-buffer-name org-agenda-buffer-name)
      (while (ignore-errors
	       (goto-char (next-single-property-change (point) 'org-hd-marker)))
	(setq count (+ count 1))
	(let* ((buffer-read-only nil)
	       (pom (get-text-property (point-at-bol) 'org-hd-marker))
	       ;; (buffer-live-p (marker-buffer pom))
	       (line
		(org-with-point-at pom
		  (superman-format-thing pom balls))))
	  (delete-region (point-at-bol) (1+ (point-at-eol)))
	  (insert line)
	  (put-text-property
	   (point-at-bol) (1+ (point-at-bol))
	   'superman-project-file
	   (org-with-point-at pom (buffer-file-name)))
	  (put-text-property
	   (point-at-bol) (1+ (point-at-bol))
	   'superman-project-file-marker
	   (marker-position pom))
	  (insert "\n")))
      (put-text-property (- (point-at-eol) 1) (point-at-eol) 'tail 'todo-end)
      (goto-char (next-single-property-change (point-min) 'face))
      (insert " [" (number-to-string count) "]"))))

(defun superman-visit-project ()
  "Goto the definition of the project in `superman-profile'"
  (interactive)
  (let* ((pom (get-text-property (point-at-bol) 'superman-item-marker))
	 (home superman-profile)
	 (ibuf (if pom (marker-buffer pom)
		 (get-file-buffer home)))
	 (iwin (when ibuf (get-buffer-window ibuf nil))))
    (if (and ibuf iwin)
	(select-window (get-buffer-window ibuf nil))
      (split-window-vertically)
      (other-window 1)
      (if ibuf (switch-to-buffer ibuf)
	(find-file home)))
    (when pom (goto-char pom))))

(defun superman-return ()
  "Switch to project at point."
  (interactive)
  (let ((pro (superman-property-at-point "nickname" nil)))
    (superman-switch-to-project pro)))

(define-key superman-mode-map [return] 'superman-return) 
(define-key superman-mode-map "i" 'superman-visit-project)
(define-key superman-mode-map "x" 'superman-delete-project)
(define-key superman-mode-map "V" 'superman-change-view)
(define-key superman-mode-map "N" 'superman-new-project)
(define-key superman-mode-map "?" 'supermanual)
;;}}}  

(provide 'superman)
;;; superman.el ends here


