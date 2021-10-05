;;; superman-manager.el --- org project manager

;; Copyright (C) 2013-2015  Thomas Alexander Gerds

;; Authors:
;; Thomas Alexander Gerds <tag@biostat.ku.dk>
;; Klaus Kähler Holst <kkho@biostat.ku.dk>
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

;; An emacs orgmode based project manager for applied statisticians

;;; Code:

(defconst superman-version "infinite-earths"
  "Version number of this package.")

;; External dependencies
(require 'org)  
(require 'deft nil t) ;; http://jblevins.org/git/deft.git
(require 'popup nil t) ;; https://github.com/auto-complete/popup-el.git
(require 'winner) 
(require 'ido)
;; (require 'org-colview)
(require 'ox-publish)
(require 'vc)
(require 'cl)

;; Loading extensions

	     

(defvar superman-default-directory (expand-file-name "~")
  ;; (file-name-as-directory
  ;; (expand-file-name (file-name-directory superman-profile)))
  "Default place for new projects.")

(require 'superman) ;; a project to manage projects
(require 'superman-views)    ;; project views
(require 'superman-capture)  ;; capture information
(require 'superman-git)      ;; git control,
(require 'superman-config)   ;; saving and setting window configurations
(require 'superman-pub)      ;; publication manager
(require 'superman-export)   ;; org export help
(require 'superman-google)   ;; google calendar support
(require 'superman-faces)    ;; highlighting
(require 'superman-file-list);; work with lists of files 
(if (featurep 'deft)
    (require 'superman-deft))     ;; selecting projects via deft

;;{{{ reload
(defun superman-reload ()
  "Re-load all superman lisp files."
  (interactive)
  (require 'loadhist)
  (let* ((dir (file-name-directory (locate-library "superman")))
	 (exts (list "" 
		     "-capture" 
		     "-config" 
		     "-deft"
		     "-display-file-list"
		     "-export"
		     "-faces"
		     "-file-list"
		     "-git" 
		     "-google" 
		     "-manager" 
		     "-pub" 
		     "-views" 
		     )))
    (while exts
      (load (concat "superman" (car exts)) 'noerror)
      (setq exts (cdr exts)))
    (message "Successfully reloaded SuperMan")))
;;}}}

;;{{{ variables and user options

(defvar superman-item-level 3
  "Outline level for items in project column views.
Level 1 is used to indicate sections, all levels between
1 and `superman-item-level' to indicate subsections.")
(make-variable-buffer-local 'superman-item-level)

(defvar superman-empty-line-before-cat t
  "Option for superman-view buffers: If non-nil insert an empty line before the category heading.")

(defvar superman-empty-line-after-cat t
  "Option for superman-view buffers: If non-nil insert an empty line after the category heading
before the column names.")


(defvar superman-profile "~/.SuperMan.org"
  "File for managing projects.")

(defvar superman-default-directory
  (file-name-as-directory
   (expand-file-name (file-name-directory superman-profile)))
  "Default place for new projects.")

(defvar superman-default-category "Krypton" 
  "Default category for new projects and uncategorized projects.")

(defvar superman-ual
  (expand-file-name
   (concat
    (file-name-directory (locate-library "superman"))
    "../Kal-El/supermanual/" "Supermanual.org"))
  "File with instructions for using superman.")

(defvar superman-help-fun 'superman-popup-tip 
  "Function used to display help. Possible values 'tooltip-show or 'popup-tip (depends on popup.el)") 

(defun superman-popup-tip (msg)
  (save-excursion
    (goto-char (point-min))
    (tooltip-show msg)))

(defvar superman-gitworkflow
  (expand-file-name
   (concat (file-name-directory
	    (locate-library "superman")) "../Kal-El/supermanual/" "git-workflow.png")
   "File with instructions for using superman."))

(defun superman-gitworkflow ()
  (interactive)
  (find-file superman-gitworkflow))

(defvar superman-default-content "" "Initial contents of org project index file.")
(defvar superman-project-subdirectories nil)
(defvar superman-project-level 4
"Subheading level at which projects are defined in `superman-profile'.")
(defvar superman-manager-mode-map (make-sparse-keymap)
  "Keymap used for `superman-manager-mode' commands.")
(defvar superman-project-alist nil
  "Alist of projects associating the nickname of the project
    with information like the location of the project, the index file,
    collaborator names, a category, the publishing directory, etc.")
(defvar superman-current-project nil "The currently selected project.")
(defvar superman-project-categories nil
  "List of categories for sorting projects.")
(defvar superman-org-location ""
    "Relative to the project directory this defines
  the path to the index file of a project. If set to
  'org' then the index file will be placed
  in a subdirectory 'org' of the project directory.
 The project directory is set by the property LOCATION in
the `superman-profile'.")
(defvar superman-select-project-completion-format
  "%c/%o/%n"
  "Format of the entries of the completion-list when selecting a project. ")
;; (setq superman-select-project-completion-format "%n %c -- %o")
;; (setq superman-select-project-completion-format "%n %o")
(defvar superman-frame-title-format nil
  "If non-nil add the nickname of the active project to frame-title")
(defvar superman-save-buffers "ask"
    "String or function to be called to save
   buffers before switching to a project. If a string, it can be 
   'no-questions-asked' then buffers are saved silently.")

(defvar superman-config-alist '(("supermanual" . "PROJECT / SUPERMANUAL")))

;; config

(defvar superman-config-action-alist
  '(("INDEX" . superman-find-index)
    ("TODO" . superman-project-todo)
    ("TIMELINE" . superman-project-timeline)
    ("LOCATION" . superman-location)
    ("FILELIST" . superman-view-file-list)
    ("PROJECT" . superman-view-project)
    ("SUPERMANUAL" . supermanual)
    ("GIT" . superman-git-display)
    ("recent.org" . superman-recent-org)
    ("*shell*" . superman-start-shell)
    ("*S*" . '(lambda (&optional project) superman))
    ("*S-todo*" . superman-todo)
    ("*S-agenda*" . superman-agenda)
    ("*ielm*" . 
     (lambda (project) 
       (if (get-buffer "*ielm*") 
	   (switch-to-buffer "*ielm*") 
	 (ielm))))
    ("*R*" . superman-find-R-function)
    ("" . (lambda (project))))
  "Alist used by `superman-find-thing' to associate actions with keys
for setting window configurations.

For example, the element

 (\"TIMELINE\" . superman-project-timeline)

will be chosen when thing is \"TIMELINE\" and then the function
`superman-project-timeline' will be called with one argument, 
a project, i.e., an element of `superman-project-alist'.

Generally, a key is a string which must not start or end with a number,
and an action a one-optional-argument function which must return a buffer.")

;; TODO Add description
(defvar superman-default-config "PROJECT" "default window configuration") 
(defvar superman-sticky-config nil "sticky window configuration")
;; (setq superman-sticky-config "recent.org / *R* | TODO")

(defvar superman-file-manager "file-list")
(defvar superman-find-R-function
  "Function used to find *R*"
  (lambda (project) (if (get-buffer "*R*") (switch-to-buffer "*R*") (R))))

(defvar superman-switch-always t
  "If nil 'superman-switch-to-project' will
 switch to current project unless the last command also was 'superman-switch-to-project'.
 Setting this variable to non-nil (the default) will force 'superman-switch-to-project'
 to always prompt for new project")
(defvar superman-human-readable-ext "^[^\\.].*\\.org\\|\\.[rR]\\|\\.tex\\|\\.txt\\|\\.el$" "Extensions of human readable files")
(defvar superman-config-cycle-pos 0 "Position in the current window configuration cycle. Starts at 0.")
(defvar superman-export-subdirectory "export")
(defvar superman-public-directory "~/public_html/")
(defvar superman-public-server "" "Place on the web where pages are published.")
(defvar superman-export-base-extension "html\\|png\\|jpg\\|org\\|pdf\\|R")
;; (setq org-agenda-show-inherited-tags (list))

;;}}}
;;{{{ the pro-file in manager-mode

;; The project manager is in org-mode (major-mode). To bind specific
;; keystrokes differently in this file, the current solution is to put
;; a minor-mode on top of it.

(define-minor-mode superman-manager-mode 
  "Toggle org projectmanager document view mode.
                  With argument ARG turn superman-docview-mode on if ARG is positive, otherwise
                  turn it off.
                  
                  Enabling superman-view mode electrifies the column view for documents
                  for git and other actions like commit, history search and pretty log-view."
  :lighter " manager"
  :group 'org
  :keymap 'superman-manager-mode-map
  (setq superman-manager-mode
	(not (or (and (null arg) superman-manager-mode)
		 (<= (prefix-numeric-value arg) 0))))
  (add-hook 'after-save-hook 'superman-refresh nil 'local))

(define-key superman-manager-mode-map [(meta return)] 'superman-return)
(define-key superman-manager-mode-map [f1] 'superman-manager)

(add-hook 'find-file-hook 
	  (lambda ()
	    (let ((file (buffer-file-name)))
	      (when (and file (equal file (expand-file-name superman-profile)))
		;; (setq org-todo-keywords-1 '("ACTIVE" "PENDING" "WAITING" "SLEEPING" "DONE" "CANCELED" "ZOMBI"))
		(superman-manager-mode)))))

(defun superman-goto-project-manager ()
  (interactive)
  (find-file superman-profile))

(defun superman-project-at-point (&optional noerror)
  "Check if point is at project heading and return the project,
                      i.e. its entry from the 'superman-project-alist'.
                      Otherwise return error or nil if NOERROR is non-nil. "
  (interactive)
  ;; (org-back-to-heading)
  (if (or (org-before-first-heading-p)
	  (not (org-at-heading-p))
	  (not (= superman-project-level
		  (- (match-end 0) (match-beginning 0) 1))))
      (if noerror nil
	(error "No project at point"))
    (or (org-entry-get nil "NICKNAME")
	(progn (superman-set-nickname)
	       (save-buffer) ;; to update the project-alist
	       (org-entry-get nil "NICKNAME")))))

(defun superman-goto-profile (project-or-nickname)
  "Open the file `superman-profile' and leave point at entry of PROJECT-OR-NICKNAME. 
PROJECT-OR-NICKNAME is either a project, i.e., a list whose first element is the nickname 
or the nickname."
  (let ((case-fold-search t)
	(nick (if (listp project-or-nickname) (car project-or-nickname) project-or-nickname)))
    (find-file superman-profile)
    (unless (superman-manager-mode 1))
    (goto-char (point-min))
    (or (re-search-forward (concat "^[ \t]*:NICKNAME:[ \t]*" nick) nil t)
	(error (concat "Cannot locate project " nick)))))

(defun superman-project-at-point (&optional pom)
  "Return project at point."
  (let* ((pom (or pom (org-get-at-bol 'org-hd-marker)))
	 (nickname (superman-get-property pom "NickName"))
	 (pro (assoc nickname superman-project-alist)))
    pro))

(defun superman-forward-project ()
  "Move to next project."
  (interactive)
  (re-search-forward
   (format "^\\*\\{%d\\} " superman-project-level) nil t))

(defun superman-backward-project ()
  "Move to previous project."
  (interactive)
  (re-search-backward
   (format "^\\*\\{%d\\} " superman-project-level) nil t))

;;}}}
;;{{{ parsing dynamically updating lists

(defun superman-get-matching-property (pom regexp &optional nth)
  "Return properties at point that match REGEXP."
  (org-with-point-at pom
    (let* ((case-fold-search t)
	   (proplist (org-entry-properties nil nil))
	   (prop (cdr (assoc-if #'(lambda (x) (string-match regexp x)) proplist))))
      (if (stringp prop)
	  (replace-regexp-in-string "[ \t]+$" "" prop)))))

(defun superman-get-text-property (marker property)
  "Return text-property at marker."
  (if (markerp marker)
      (with-current-buffer (marker-buffer marker)
	(get-text-property (marker-position marker)
			   property))
    (progn
      (message "Cannot see a marker here")
      nil)))

(defun org-property--local-values (property literal-nil)
  "Return value for PROPERTY in current entry.
Value is a list whose care is the base value for PROPERTY and cdr
a list of accumulated values.  Return nil if neither is found in
the entry.  Also return nil when PROPERTY is set to \"nil\",
unless LITERAL-NIL is non-nil."
  (let ((range (org-get-property-block)))
    (when range
      (goto-char (car range))
      (let* ((case-fold-search t)
            (end (cdr range))
            (value
             ;; Base value.
             (save-excursion
               (let ((v (and (re-search-forward
                              (org-re-property property nil t) end t)
                             (org-match-string-no-properties 3))))
                 (list (if literal-nil v (org-not-nil v)))))))
       ;; Find additional values.
       (let* ((property+ (org-re-property (concat property "+") nil t)))
         (while (re-search-forward property+ end t)
           (push (org-match-string-no-properties 3) value)))
       ;; Return final values.
       (and (not (equal value '(nil))) (nreverse value))))))

(defun superman-get-property (pom property &optional inherit literal-nil)
  "Read property and remove trailing whitespace."
  (let ((prop
	 (cond ((markerp pom)
		(org-with-point-at pom
		  (car (org-property--local-values property literal-nil))))
	       (pom 
		(save-excursion (goto-char pom)
				(car (org-property--local-values property literal-nil))))
	       (t (car (org-property--local-values property literal-nil))))))
    (if (stringp prop)
	(progn
	  (setq prop (replace-regexp-in-string "[ \t]+$" "" prop))
	  (if (string= prop "") nil prop))
      nil)))

;; (defvar superman-project-kal-el nil
  ;; "If non-nil add the Kal-El project to project alist.
;; Kal-El is the planet where superman was born. It is there
;; we find the `supermanual' and other helpful materials.")

(defun superman-initialize ()
  "Start the super manager."
  (interactive)
  (if (not (file-exists-p superman-profile))
      (superman-capture-superman)
    (superman-parse-projects)
    (superman)))

;; add project Kal-El
;; (if superman-project-kal-el
;; (let ((superman-loc
;; (expand-file-name
;; (concat (file-name-directory (locate-library "superman")) ".."))))
;; (setq superman-project-alist
;; `(("Kal-El"
;; (("location" . ,superman-loc)
;; ("index" .  ,(concat superman-loc "/Kal-El/Kal-El.org"))
;; ("category" . "Krypton")
;; ("others" . "Jor-El, SuperManual")
;; (hdr . "Kal-El"))))))

(defun superman-parse-projects ()
  "Parse the file `superman-profile' and update `superman-project-alist'. 
;; If `superman-project-kal-el' is non-nil also add the Kal-El project.
"
  (interactive)
  (save-excursion
    (setq superman-project-alist nil)
    (set-buffer (find-file-noselect superman-profile))
    (show-all)
    (widen)
    (superman-manager-mode 1)
    (save-buffer)
    (goto-char (point-min))
    (unless (re-search-forward "^\\#\\+TODO:" nil t)
      (insert "\n#+TODO: ACTIVE | PENDING WAITING SLEEPING DONE CANCELED ZOMBI\n\n")
      (org-ctrl-c-ctrl-c))
    (goto-char (point-min))
    (let ((error-buf (get-buffer-create "*Superman-parse-errors*")))
      (save-excursion (set-buffer error-buf) (erase-buffer))
      (kill-buffer error-buf))
    (while (superman-forward-project)
      (unless (and (org-get-todo-state) (string-match (org-get-todo-state) "ZOMBI") )
	(let* ((name (or (superman-get-property nil "nickname"  nil)
			 (nth 4 (org-heading-components))))
	       (loc (let ((loc (superman-get-property nil "location" nil)))
		      (if (not loc) 
			  (let ((error-buf (get-buffer-create "*Superman-parse-errors*")))
			    (save-excursion
			      (set-buffer error-buf)
			      (goto-char (point-max))
			      (insert
			       "\n"
			       (superman-make-button
				(concat "Project " name " does not have a location")
				`(:fun (lambda () (interactive) (superman-goto-profile ,name)))) "\n"))
			    (setq loc ""))
			(if (string-match org-bracket-link-regexp loc)
			    (setq loc (org-match-string-no-properties 1 loc))
			  (unless loc
			    (message (concat 
				      "project: " name
				      " unspecified location set to " 
				      superman-default-directory))
			    (setq loc superman-default-directory)))
			loc)))
	       (category
		(capitalize (cond
			     ((superman-get-property nil "category" nil))
			     (t (message (concat "SuperMan project " name
						 " unspecified category set to " 
						 superman-default-category))
				(or superman-default-category "Krypton")))))
	       (others (superman-get-property nil "others" nil))
	       (publish-dir (superman-get-property nil "publish" nil))
	       (marker
		(save-excursion
		  (org-back-to-heading)
		  (point-marker)))
	       (hdr (org-get-heading t t))
	       (lastvisit (let ((lvisit (superman-get-property nil "LastVisit" nil)))
			    (if lvisit lvisit
			      (let ((error-buf (get-buffer-create "*Superman-parse-errors*")))
				(save-excursion
				  (set-buffer error-buf)
				  (goto-char (point-max))
				  (insert
				   "\n"
				   (superman-make-button
				    (concat "Project " name " does not have a time-stamp")
				    `(:fun (lambda () (interactive) (superman-goto-profile ,name)))) "\n"))
				"<2013-09-01 Sun 08:>"))))
	       (config (superman-get-property nil "config" nil))
	       (todo (or (org-get-todo-state) ""))
	       (index (or
		       (let ((link (superman-get-property nil "index" nil)))
			 (when (and (stringp link) (string-match org-bracket-link-regexp link))
			   (setq link (org-match-string-no-properties 1 link)))
			 link)
		       ;; loc/name.org
		       (concat (file-name-as-directory loc)
			       (file-name-as-directory superman-org-location)
			       name ".org"))))
	  ;; remove text properties
	  (set-text-properties 0 (length hdr) nil hdr)
	  (set-text-properties 0 (length todo) nil todo)
	  (unless (file-name-absolute-p index)
	    (setq index
		  (expand-file-name index (file-name-as-directory loc))))
	  ;; categories
	  (add-to-list 'superman-project-categories category)
	  ;; project alist
	  (add-to-list 'superman-project-alist
		       (list name
			     (list (cons "location"  loc)
				   (cons "index" index)
				   (cons "category" category)
				   (cons "others" others)
				   (cons 'hdr hdr)
				   (cons "marker" marker)				 
				   (cons "lastvisit" lastvisit)
				   (cons "config" config)
				   (cons 'todo todo)
				   (cons "publish-directory" publish-dir)))))))
    ;; sort by last visit
    (setq superman-project-alist
	  (sort superman-project-alist (lambda (x y)
					 (> 
					  (org-time-stamp-to-now (cdr (assoc "lastvisit" (cadr x))) 'seconds)
					  (org-time-stamp-to-now (cdr (assoc "lastvisit" (cadr y))) 'seconds)))))
    
    (when (get-buffer "*Superman-parse-errors*")
      (pop-to-buffer  "*Superman-parse-errors*"))
    superman-project-alist))


(defun superman-view-directory (&optional dir)
  (interactive)
  (let* ((dir (or dir (read-directory-name "Create temporary project for directory: ")))
	 (name (file-name-nondirectory (replace-regexp-in-string "/$" "" dir)))
	 (index-buffer (get-buffer-create (concat "*Superman-" name "*.org")))
	 pro)
    (set-text-properties 0 (length name) nil name)
    (set-buffer index-buffer)
    (org-mode)
    ;; (add-to-list 'superman-project-alist
    (setq pro (list name
		    (list (cons "location"  dir)
			  (cons "index" index-buffer)
			  (cons "category" "Temporary")
			  (cons "others" nil)
			  (cons 'hdr nil)
			  (cons "marker" nil)				 
			  (cons "lastvisit" nil)
			  (cons "config" nil)
			  (cons 'todo nil)
			  (cons "publish-directory" nil))))
    (superman-view-project pro t)
    ;; (assoc name superman-project-alist))
    (if (superman-git-p dir) (superman-git-display)
      (superman-display-file-list dir (file-list-select nil "." nil nil dir)
				  nil nil nil
				  nil nil
				  'no-project))))
	 

(defun superman-property-values (key)
  "Return a list of all values of property KEY in the current buffer or region. This
function is very similar to `org-property-values' with two differences:
1) values are returned without text-properties.
2) The function does not call widen and hence search can be restricted to region."
  (save-excursion
    (save-restriction
      ;; (widen)
      (goto-char (point-min))
      (let ((re (org-re-property key))
	    values)
	(while (re-search-forward re nil t)
	  (add-to-list 'values
		       (org-trim (match-string-no-properties 3))))
	(delete "" values)))))


(defun superman-property-keys (&optional include-specials include-defaults)
  "Get all property keys in the current buffer or region.
This is basically a copy of `org-buffer-property-keys'.

With INCLUDE-SPECIALS, also list the special properties that reflect things
like tags and TODO state.

With INCLUDE-DEFAULTS, also include properties that has special meaning
internally: ARCHIVE, CATEGORY, SUMMARY, DESCRIPTION, LOCATION, and LOGGING
and others."
  (let (rtn range cfmt s p)
    (save-excursion
      (save-restriction
	;; (widen)
	(goto-char (point-min))
	(while (re-search-forward org-property-start-re nil t)
	  (setq range (org-get-property-block))
	  (goto-char (car range))
	  (while (re-search-forward
		  org-property-re
		  ;; (org-re "^[ \t]*:\\([-[:alnum:]_]+\\):")
		  (cdr range) t)
	    (add-to-list 'rtn (org-match-string-no-properties 2)))
	  (outline-next-heading))))
    (when include-specials
      (setq rtn (append org-special-properties rtn)))
    (when include-defaults
      (mapc (lambda (x) (add-to-list 'rtn x)) org-default-properties)
      (add-to-list 'rtn org-effort-property))
    (sort rtn (lambda (a b) (string< (upcase a) (upcase b))))))


(defun superman-refresh ()
  "Parses the categories and projects in file `superman-profile' and also
             updates the currently selected project."
  (interactive)
  ;; (superman-parse-project-categories)
  (superman-parse-projects)
  (when superman-current-project
    (setq superman-current-project
	  (assoc (car superman-current-project) superman-project-alist))))

;;}}}
;;{{{ Adding, (re-)moving, projects

(defun superman-create-project (project &optional ask)
  "Create the index file, the project directory, and subdirectories if
                                    'superman-project-subdirectories' is set."
  (interactive)
  (let ((pro (if (stringp project)
		 (assoc project superman-project-alist)
	       project)))
    (when pro
      (let ((dir (superman-get-location pro))
	    (index (superman-get-index pro)))
	(when (and index (not (file-exists-p index)))
	  (unless (file-exists-p (file-name-directory index))
	    (make-directory (file-name-directory index) t))
	  (find-file index)
	  (unless (file-exists-p index)
	    (insert "*** Index of project " (car pro) "\n:PROPERTIES:\n:ProjectStart: "
		    (format-time-string "<%Y-%m-%d %a %H:%M>")
		    "\n:END:\n")
	    (save-buffer)))
	;; (append-to-file superman-default-content nil index)
	(unless (or (not dir) (file-exists-p dir) (not (and ask (y-or-n-p (concat "Create directory (and default sub-directories) " dir "? ")))))
	  (make-directory dir)
	  (loop for subdir in superman-project-subdirectories
		do (unless (file-exists-p subdir) (make-directory (concat path subdir) t))))
	(find-file superman-profile)
	(unless (superman-manager-mode 1))
	(goto-char (point-min))
	(re-search-forward (concat (make-string superman-project-level (string-to-char "*")) ".*" (car pro)) nil )))))

(defun superman-move-project (&optional project)
  (interactive)
  (let* ((pro (or project (superman-get-project project)))
	 (index (superman-get-index pro))
	 (dir (superman-get-location pro))
	 (target  (read-directory-name (concat "Move all files below " dir " to: " )))
	 (new-index (unless (string-match dir (file-name-directory index))
		      (read-file-name (concat "Move " index " to ")))))
    (if (string= (file-name-as-directory target) target)
	(setq target (concat target (file-name-nondirectory dir))))
    (unless (file-exists-p (file-name-directory target)) (make-directory (file-name-directory target)))
    (when (yes-or-no-p (concat "Move " dir " to " target "? "))
      (rename-file dir target)
      (if (and new-index (yes-or-no-p (concat "Move " index " to " new-index "? ")))
	  (rename-file index new-index))
      (superman-goto-profile pro)
      (org-set-property "location"
			(file-name-directory target))
      (org-set-property "index"
			(or new-index
			    (replace-regexp-in-string
			     (expand-file-name (file-name-directory dir))
			     (expand-file-name (file-name-directory target))
			     (expand-file-name index))))
      (save-buffer))))

(defun superman-delete-project (&optional project)
  "Delete the project PROJECT from superman control. This includes
cutting the heading in `superman-profile', removing the project
from `superman-project-history', and killing the associated buffers.

Optionally (the user is prompted) move also the whole
project directory tree to the trash."
  (interactive)
  (let* ((marker (org-get-at-bol 'org-hd-marker))
	 (scene (current-window-configuration))
	 (pro (or project (superman-get-project project 'ask)))
	 ;; (or project (superman-project-at-point)))
	 (dir (superman-get-location pro))
	 (index (superman-get-index pro))
	 (ibuf (get-file-buffer index)))
    ;; switch to entry in superman-profile
    (if superman-mode
	(superman-view-index)	
      (superman-go-home (car pro)))
    (org-narrow-to-subtree)
    (when (yes-or-no-p (concat "Delete project " (car pro) " from SuperMan control? "))
      ;; remove entry from superman-profile
      (org-cut-subtree)
      (widen)
      (save-buffer)
      (delete-blank-lines)
      ;; delete from project-history
      (delete (car pro) superman-project-history)
      ;; kill buffers
      (when (buffer-live-p ibuf)
	(kill-buffer ibuf))
      (when (buffer-live-p (get-buffer (concat "*Project[" (car pro) "]*")))
	(kill-buffer (concat "*Project[" (car pro) "]*")))
      ;; update superman buffer
      (superman))
    ;; remove directory tree and index file
    (when (and (file-exists-p dir)
	       (yes-or-no-p (concat "Remove project directory tree? " dir " ")))
      (when (yes-or-no-p (concat "Are you sure? "))
	(move-file-to-trash dir)))
    (when (and (file-exists-p index)
	       (yes-or-no-p (concat "Remove index file? " index " ")))
      (move-file-to-trash index))
    (set-window-configuration scene)))

      

;;}}}
;;{{{ setting project properties

(defun superman-set-nickname ()
  (interactive)
  (org-set-property "nickname"
   (read-string "NickName for project: "
		(nth 4 (org-heading-components)))))

(defun superman-set-others (&optional project ask)
  (interactive)
  (let* ((pro (or project
		  (superman-get-project project ask)))
	 marker
	 (others  (superman-get-others pro))
	 (init (if others (concat others ", ") ""))
	 (others (read-string (concat "Set collaborators for " (car pro) ": ") init)))
    (superman-goto-profile (car pro))
    (org-back-to-heading)
    (setq marker (point-marker))
    (org-set-property "others"
		      (replace-regexp-in-string
		       "[,\t ]+$" ""
		       others))
    (superman-view-edit-item nil marker)))


(defun superman-fix-others ()
  "Update the others property (collaborator names) of all projects in `superman-profile'."
  (interactive "P")
  (set-buffer (find-file-noselect superman-profile))
  (unless (superman-manager-mode 1))
  (goto-char (point-min))
  (while (superman-forward-project)
	(superman-set-others (superman-project-at-point))))

;;}}}
;;{{{ listing projects

(defvar superman-ignore-index-buffers t "If non-nil add index buffers to `ido-ignore-buffers'.")
(defvar superman-has-ignored-index-buffers nil "User should not set this variable. Function `superman-index-list' sets this variable to avoid
 checking index buffers multiple times into `ido-ignore-buffers'.")

(defun superman-index-list (&optional category state extension not-exist-ok update exclude-regexp)
  "Return a list of project specific indexes.
Projects are filtered by CATEGORY unless CATEGORY is nil.
Projects are filtered by the todo-state regexp STATE unless STATE is nil.
Only existing files are returned unless NOT-EXIST-OK is non-nil.
Only files ending on EXTENSION are returned unless EXTENSION is nil.
Only files not matching EXCLUDE-REGEXP are included.a

If UPDATE is non-nil first parse the file superman.
Examples:
 (superman-index-list nil \"ACTIVE\")
 (superman-index-list nil \"DONE\")
"
  (interactive "P")
  (when update
    (superman-refresh))
  (let* ((testfun
	  (lambda (p)
	    (let ((p-cat (superman-get-category p)))
	      (when (and
		     (or (not category)
			 (not p-cat)
			 (string= category p-cat))
		     (or (not state)
			 (string-match state (superman-get-state p))))
		p))))
	 (palist (if (or category state)
		     (delq nil (mapcar testfun superman-project-alist))
		   superman-project-alist))
	 (index-list
	  (delete-dups
	   (delq nil
		 (mapcar
		  #'(lambda (x)
		      (let ((f (superman-get-index x)))
			(unless (bufferp f)
			  (when (and (or  not-exist-ok (file-exists-p f))
				     (or (not exclude-regexp) (not (string-match exclude-regexp f)))
				     (or (not extension)
					 (string= extension (file-name-extension f))))
			    f))))
		  palist)))))
    ;; see if user want's to hide the index buffers
    (when (and superman-ignore-index-buffers 
	       (not superman-has-ignored-index-buffers))
      ;; (setq ido-ignore-buffers  '("\\` "))
      (mapcar #'(lambda (file) (add-to-list 'ido-ignore-buffers
					    (concat "^" (file-name-nondirectory file))))
	      index-list))
    index-list))


;;}}}
;;{{{ selecting projects

(defun superman-format-project (entry)
  (let* ((cat (or (superman-get entry "category") ""))
	 (coll (or (superman-get entry "others") ""))
	 (todo (or (superman-get entry 'todo) ""))
	 (nickname (car entry))
	 (string (replace-regexp-in-string "%c" cat superman-select-project-completion-format))
	 (string (replace-regexp-in-string "%o" coll string))
	 (string (replace-regexp-in-string "%t" todo string))
	 (string (replace-regexp-in-string "%n" (car entry) string)))
    (cons string (car entry))))

(defun superman-select-project (&optional prompt)
  "Select a project from the project alist, 
The list is re-arranged such that 'superman-current-project'
is always the first choice. 
If PROMPT is a string use it to ask for project."
  (let* ((plist superman-project-alist)
	 (project-array (mapcar 'superman-format-project
				(if (not superman-current-project)
				    plist
				  (setq plist (append (list superman-current-project)
						      (remove superman-current-project plist))))))
	 (completion-ignore-case t)
	 (key (progn
		(sort plist (lambda (x y)
			      (> 
			       (org-time-stamp-to-now (cdr (assoc "lastvisit" (cadr x))) 'seconds)
			       (org-time-stamp-to-now (cdr (assoc "lastvisit" (cadr y))) 'seconds))))
	  (ido-completing-read (if (stringp prompt) prompt "Project: ")
				   (mapcar 'car project-array))))
	 (nickname (cdr (assoc key project-array))))
    (assoc nickname superman-project-alist)))

(defun superman-set-frame-title ()
  (let* ((old-format (split-string frame-title-format "Project:[ \t]+[^ \t]+[ \t]+"))
        (keep (if (> (length old-format) 1) (cadr old-format) (car old-format))))
    (setq frame-title-format
          (concat "Project: " (or (car superman-current-project) "No active project") " " keep))))

(defun superman-activate-project (project)
  "Sets the current project and updates the LastVisit field of the project manager.
 Also, adds the project location to `file-name-history'"
  (setq superman-current-project project)
  (let ((dir  (superman-get-location project)))
    (add-to-history 'file-name-history dir nil nil))
  (if superman-frame-title-format (superman-set-frame-title))
  (with-current-buffer (or (find-buffer-visiting superman-profile)
			   (find-file-noselect superman-profile))
    ;; in case that the profile was edited elsewhere
    (revert-buffer t t t)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (concat ":NICKNAME:[ \t]?" (car project)) nil t)
	(org-entry-put (point) "LastVisit"
		       (format-time-string "<%Y-%m-%d %a %H:%M>"))
	(save-buffer)))))

(defun superman-save-some-buffers (&optional arg pred)
  "Wrapper for `save-some-buffers' which does the same as `save-some-buffers' but never fails."
  (interactive)
  (ignore-errors (save-some-buffers arg pred)))

(defun superman-save-project (project)
  (interactive)
  (unless
      (string=
       (superman-get-category project) "Temporary")
    (save-excursion
      (let ((pbuf (get-file-buffer
		   (superman-get-index project))))
	(when pbuf
	  (switch-to-buffer pbuf)
	  (save-buffer))))
    (cond ((functionp superman-save-buffers)
	   (funcall superman-save-buffers))
	  ((string= superman-save-buffers "no-questions-asked")
	   (superman-save-some-buffers t))
	  (superman-save-buffers 
	   (superman-save-some-buffers nil)))))

;;}}}
;;{{{ switching projects (see also superman-config)

(defun superman-switch (&optional arg)
  "If ARG switch project else switch config."
  (interactive "P")
  (if arg
      (superman-switch-to-project)
    (superman-switch-config)))

(defun superman-switch-to-project (&optional project noselect)
  "Select project via `superman-select-project', activate it
 via `superman-activate-project',  find the associated index file.

Unless NOSELECT is nil, set the next window config of project.
If NOSELECT is set return the project."
  (interactive "P")
  (let* ((curpro superman-current-project)
	 (pro
	  (if (and (not project)
		   (get-text-property (point-min) 'project-view))
	      (superman-select-project)
	    (superman-get-project project 'ask)))
	 (stay (eq pro curpro)))
    (unless stay
      (if (member (car pro) superman-project-history)
	  (progn
	    (setq superman-project-history
		  (cons (car pro) superman-project-history))
	    (delete-dups superman-project-history))
	(setq superman-project-history
	      (cons (car pro) superman-project-history)))
      ;; (add-to-list 'superman-project-history (car pro))
      (when curpro
	(superman-save-project curpro))
      (superman-activate-project pro))
    (if noselect
	superman-current-project
      (if stay 
	  (superman-switch-config pro nil)
	;; the next command 
	;; re-sets superman-config-cycle-pos 
	(superman-switch-config pro 0))
      )))

(defun superman-list-files (dir ext sort-by)
  (if (featurep 'file-list)
      (mapcar 'file-list-make-file-name
	      (file-list-sort-internal
	       (file-list-select nil ext nil nil dir nil 'dont)
	       (or sort-by "time") nil t))
    (directory-files dir nil ext t)))

;;}}}
;;{{{ publishing project contents

(defvar superman-public-server-home nil "String indicating a place on the web where org to html exports are published.")

(defun superman-browse-this-file (&optional arg)
  "Browse the html version of the current file using `browse-url'. If
        prefix arg is given, then browse the corresponding file on the superman-public-server"
  (interactive "P")
  (let ((superman-candidate (intern (concat "superman-browse-org-export-target-" superman-org-export-target))))
    (cond ((and (not arg) (functionp superman-candidate))
	   (funcall superman-candidate))
	  ((or (string= superman-org-export-target "html")
	       (string= superman-org-export-target "exercise")
	       (string= superman-org-export-target "opgave"))
	   (let* ((bf (buffer-file-name (current-buffer)))
		  (server-home (if (and arg (not superman-public-server-home))
				   (read-string "Specify address on server: " "http://")
				 superman-public-server-home))
		  (html-file (if arg
				 (concat (replace-regexp-in-string
					  (expand-file-name superman-public-directory)
					  server-home
					  (file-name-sans-extension bf))
					 ".html")
			       (concat "file:///" (file-name-sans-extension bf) ".html"))))
	     ;; fixme superman-browse-file-hook (e.g. to synchronize with public server)
	     (message html-file)
	     (browse-url html-file)))
	  (t 
	   (let ((target (concat (file-name-sans-extension (buffer-file-name)) "." superman-org-export-target)))
	     (if (file-exists-p target)
		 (org-open-file target)
	       (message (concat "No such file: " target))))))))


(defun superman-set-publish-alist ()
  (interactive)
  (let ((p-alist superman-project-alist))
    (while p-alist
      (let* ((pro  (car p-alist))
	     (nickname (car pro))
	     (base-directory (superman-get-location pro))
	     (export-directory
	      (concat base-directory "/"
		      superman-export-subdirectory))
	     (public-directory
	      (or (superman-get-publish-directory pro)
		  (concat (file-name-as-directory superman-public-directory)
			  nickname))))
	;;(replace-regexp-in-string superman-public-directory (getenv "HOME") (expand-file-name export-directory))))
	(add-to-list 'org-publish-project-alist
		     `(,(concat nickname "-export")
		       :base-directory
		       ,base-directory
		       :base-extension "org"
		       :publishing-directory
		       ,base-directory
		       :headline-levels 4
		       :auto-preamble t
		       :recursive t
		       :publishing-function
		       org-publish-org-to-html))
	(add-to-list 'org-publish-project-alist
		     `(,(concat nickname "-copy")
		       :base-directory
		       ,export-directory
		       :base-extension
                       ,superman-export-base-extension
		       :publishing-directory
		       ,public-directory
		       :recursive t
		       :publishing-function
		       org-publish-attachment))
	(add-to-list 'org-publish-project-alist
		     `(,nickname
		       :components (,(concat nickname "-export") ,(concat nickname "-copy")))))
      (setq p-alist (cdr p-alist)))))

;;}}}
;;{{{ extracting properties from a project 
(defun superman-get (project el)
  "Return element named EL from PROJECT."
  (cdr (assoc el (cadr project))))

(defun superman-get-index (project)
  "Extract the index file of PROJECT."
  (cdr (assoc "index" (cadr project))))

(defun superman-get-git (project)
  (or (cdr (assoc "git" (cadr project))) ""))

(defun superman-go-home (&optional nickname)
  "Visit the file superman-profile and leave point at PROJECT."
  (find-file superman-profile)
  (goto-char (point-min))
  (let* ((case-fold-search t) 
	 (regexp (concat ":nickname:[ \t]*" nick-or-heading)))
    (re-search-forward regexp nil t)))
  

(defun superman-project-home (project)
   (superman-get-location project))

(defun superman-get-location (project)
  "Get the directory associated with PROJECT."
  (file-name-as-directory (cdr (assoc "location" (cadr project)))))

(defun superman-get-config (project)
  (cdr (assoc "config" (cadr project))))

(defun superman-get-publish-directory (project)
  (cdr (assoc "publish-directory" (cadr project))))

(defun superman-get-category (project)
  (cdr (assoc "category" (cadr project))))

(defun superman-get-others (project)
  (cdr (assoc "others" (cadr project))))

(defun superman-get-lastvisit (project)
  (cdr (assoc "lastvisit" (cadr project))))

(defun superman-get-state (project)
  (cdr (assoc 'todo (cadr project))))
;;}}}




(provide 'superman-manager)
;;; superman-manager.el ends here

