;;; superman-config.el --- Project specific window configurations

;; Copyright (C) 2012-2016  Thomas Alexander Gerds

;; Author: Thomas Alexander Gerds <tag@biostat.ku.dk>
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

;; 
;; It may be counterintuitive, but saving a window configuration is not
;; so easy. One reason are windows showing unsaved, temporary stuff such
;; as file-unrelated buffers. Also, the dimensions of
;; frames and windows depend on the current screen and most people will
;; at least occasionally work on different screens.

;; What we could do is restore from saved files and certain
;; file-unrelated buffers, as for example a buffer showing a shell. We
;; can also save the number of windows and the horizontal and vertical
;; splits in the current frame. 

;;; Code:


;;{{{ finding buffers

(defun superman-find-thing (thing project)
  "Extract THING from `superman-config-action-alist' and apply it to PROJECT.
Returns the corresponding buffer."
  (save-window-excursion
    (let* ((case-fold-search t)
	   (action (cdr
		    (assoc
		     (replace-regexp-in-string "^[ \t\n]+\\|[ \t\n]+$" ""  thing)
		     superman-config-action-alist))))
      (cond
       ((not thing) nil)
       ((functionp action) (funcall action project))
       ((string= (substring thing 0 1) "!")
	(superman-start-shell (substring thing 1 (length thing))))
       ((file-exists-p thing) (find-file thing))
       ((string-match org-bracket-link-regexp thing)
	(find-file (org-match-string-no-properties 1 thing)))
       ((file-name-directory thing)
	(find-file
	 (expand-file-name
	  thing
	  (superman-get-location project))))
       (t (switch-to-buffer thing))))
    (current-buffer)))
;;}}}
;;{{{ saving window configs

(defun superman-capture-config (&optional project)
  (interactive)
  (let ((pro (superman-get-project project)))
    (superman-capture
     pro
     "Configuration"
     "The window configuration is saved."
     nil
     `(("Config" :value ,(superman-current-config))))))
     
(fset 'superman-save-config 'superman-capture-config)

(defun superman-current-config ()
  (let* ((windata (winner-win-data))
	 config
	 prev-row)
    (while windata
      (let* ((buf (cdr (car windata)))
	     (pos (car (car windata)))
	     (row (nth 1 pos))
	     (bname (buffer-name buf))
	     (bfile (buffer-file-name buf))
	     (thing
	      (cond 
	       (bfile
		(concat "[["
			(replace-regexp-in-string (getenv "HOME") "~"  bfile)
			"]" "[" (file-name-nondirectory bfile) "]]"))
	       ((string-match "\\*Documents\\[.*\\]\\*" bname) "DOCUMENTS")
	       ((string-match "\\*Project\\[.*\\]\\*" bname) "PROJECT")
	       (t bname))))
	(setq config (concat config (when prev-row (if (< prev-row row) " / " " | ")) thing))
	(setq windata (cdr windata))
	(setq prev-row row)))
    config))


;;}}}
;;{{{ reading window configs

(defun superman-read-config (project)
  (let* ((config (superman-get-config project))
	 (config (if superman-sticky-config
		     (if config 
		       (concat superman-sticky-config " : " config)
		     superman-sticky-config)
		   config))
	 (case-fold-search t))
    (save-window-excursion
      (save-restriction
	(when (superman-goto-project project "Configuration" nil nil 'narrow nil)
	  (goto-char (point-min))
	  (while (outline-next-heading)
	    (let* ((this-config
		    (or (superman-get-property (point) "Config")
			(cdr (assoc (downcase
				     (or
				      (org-get-heading t t) "Untitled")) superman-config-alist)))))
	      (when this-config
		(if config
		    (setq
		     config
		     (concat config " : " this-config))
		  (setq config this-config)))))
	  (when (not config)
	    (setq config superman-default-config))
	  (widen))
	config))
    config))
;;}}}
;;{{{ smashing window configs

(defun superman-distangle-config-list (string)
   ;; return a list of lists with vertical splits 
  ;; where each element can have horizontal splits
  (split-string string "[ \t]+:[ \t]+"))

(defun superman-distangle-config (config)
  ;; return a list with horizontal splits 
  ;; where each element can have vertical splits
  ;;
  ;; example: (superman-distangle-config "a / b | c | d")
  (let* ((vlist (split-string config "[ \t]+|[ \t]+"))
	 (hlist (mapcar #'(lambda (x) (split-string x "[ \t]+/[ \t]+")) vlist)))
    hlist))

(defun superman-smash-windows (window-config project)
  "Smash windows according to the WINDOW-CONFIG and
then fill the windows relative to project. See `superman-set-config' for
the syntax of superman window configurations."
  (let ((ncolumns (length window-config))
	(nrows (mapcar 'length window-config))
	top-windows)
    (delete-other-windows)
    (setq top-windows (list (selected-window)))
    ;; first create the windows 
    (loop for c from 1 to (- ncolumns 1) do
	  (split-window-horizontally)
	  (other-window 1)
	  (setq top-windows (append top-windows (list (selected-window)))))
    (loop for c from 0 to (- ncolumns 1) do
	  (select-window (nth c top-windows)) ;; switch to the top-window in this column
	  (let ((nrow (nth c nrows)));; number of rows in this column
	    (loop for r from 0 to (- nrow 1) do
		  (let ((thing (nth r (nth c window-config)))
			width height)
		    (if (string-match ":width[ \t]*\\([0-9]*\\)[ \t]+\\(.*\\)" thing)
			(setq width (string-to-number (match-string 1 thing))
			      thing (match-string 2 thing))
		      (setq width 0))
		    (if (string-match ":height[ \t]*\\([0-9]*\\)[ \t]+\\(.*\\)" thing)
			(setq height (string-to-number (match-string 1 thing))
			      thing (match-string 2 thing))
		      (setq height 0))
		    ;; (string-match ":height\\([0-9]*\\)-?\\([0-9]*\\)[ ]*\\(.*\\)" thing)
		    ;; (setq width (string-to-number (match-string 1 thing))
		    ;; height (string-to-number (match-string 2 thing))
		    ;; thing (match-string 3 thing))
		    (switch-to-buffer
		     (superman-find-thing thing project))
		    ;; maybe shrink window
		    (let ((w-height (window-height (selected-window)))
			  (w-width  (window-width (selected-window))))
		      ;; The aim to shrink the window can fail, e.g.,
		      ;; root window cannot be shrunken. We prefer
		      ;; to do nothing and not to signal an error
		      (when (and (> height 0) (< height w-height))
			(ignore-errors (shrink-window (- w-height height))))
		      (when (and (> width 0) (< width w-height))
			(ignore-errors (shrink-window (- w-height width) 'horizontal)))))
		  (when (and (< r (- nrow 1)) (> nrow 1 ))
		    (split-window-vertically)
		    (other-window 1)))))
    (select-window (nth 0 top-windows))))

(defun superman-set-config (config &optional project)
  "Distangle and set superman-window-configuration CONFIG.
PROJECT if given should be an element of the `superman-project-alist'. 

A superman window configuration is a string which looks like this:

'THING1 / THING2 | THING3'

In this case the current emacs frame will show three windows:

-----------------------
        THING1

-----------------------
           |
THING2     |    THING3    
           |
The contents of the three windows will be the buffers
returned by the function `superman-find-thing' according
to the meaning of THING which is identified by looking THING
up in `superman-config-action-alist' relative to PROJECT:

  'file-name' refers to finding the file 'file-name'
   (either file-name is absolute or relative to the location
   of the project)
  'buffer-name' refers to the buffer named 'buffer-name'
  'PROJECT' show the project overview
  'TODO' show the project specific task list
  'INDEX' show the project index file
  'TIMELINE' show the project specific time line
  'FILELIST' show the project specific file-list
  'LOCATION' show the project directory (dired)
  'recent.org' show the most recent org file in project
  '*S*' show the superman manager
  '*S-todo*' show the overall project task list
  '*S-agenda*' show the overall project agenda
  '*shell*' show a shell buffer
  '*ielm*' show ielm
  '*R*' show the *R* buffer (start one if necessary)
  '' show the current buffer

For example,

PROJECT / ./lecturenotes | TODO / TIMELINE

will show four windows: the project overview,
the sub-directory lecturenotes of the project location
the task list and the timeline.

Experimental: control of width and height of some of the windows
as follows:

THING1 / :width 8 THING2 | :height 4 THING3

In this case the window showing THING2 will be shrinked
by delta=8 horizontally (see `shrink-window') and the window showing
THING2 will be shrinked by delta=4 vertically.
"
  (superman-smash-windows
   (superman-distangle-config config)
   project))
  
(defun superman-switch-config (&optional project position config)
  "Switch to the next user defined window configuration of PROJECT.

If no window configuration exists, as specified in the subtree
Configuration in index file of PROJECT, use
 `superman-default-config' instead.

If POSITION is an integer n then switch to the nth window configuration
of PROJECT and set `superman-default-config',
else cycle the value `superman-config-cycle-pos' and use it to
find the next window configuration.

If CONFIG is non-nil it should be a window configuration
given in superman notation."
  (interactive)
  (let ((pro (or project
		 superman-current-project
		 (and (not config)
		      (superman-switch-to-project nil t)))))
    (if config
	(superman-smash-windows
	 (superman-distangle-config config)
	 pro)
      (let* ((position (when (integerp position) position)) 
	     (proconfig (superman-read-config pro))
	     (config-list (if proconfig (progn (superman-distangle-config-list
						proconfig)) '("PROJECT")))
	     window-config)
	(if position
	    (setq superman-config-cycle-pos position)
	  (if (> (length config-list) (1+ superman-config-cycle-pos));; cycle-pos starts at 0
	      (setq superman-config-cycle-pos (1+ superman-config-cycle-pos))
	    (setq superman-config-cycle-pos 0)))
	(setq window-config
	      (superman-distangle-config
	       (nth superman-config-cycle-pos config-list)))
	(superman-smash-windows window-config pro)))))


;;}}}
;;{{{ functions that find things

(defun superman-find-index (project)
  (let* ((index (superman-get-index project)))
    (unless (file-exists-p index)
      (unless (file-exists-p (file-name-directory index))
	(make-directory (file-name-directory index) 'with-parents))
      (make-directory (file-name-directory index) 'with-parents))
    (find-file index)))


(defun superman-location (project)
  (find-file (superman-get-location project)))

(defun superman-project-timeline (&optional project)
  "Display a project specific timeline based on the index file."
  (interactive)
  (let* ((project (superman-get-project project))
	 (index (superman-get-index project))
	 (loc (superman-project-home project))
	 (nick (car project))
	 (org-agenda-window-setup 'current-window)
	 (org-agenda-sticky nil)
	 (org-agenda-redo-command 'superman-project-timeline)
	 (org-agenda-buffer-name (concat "*Timeline[" (car project) "]*"))
	 (org-agenda-finalize-hook
	  (lambda ()
	    (save-excursion
	      (goto-char (point-min))
	      (delete-region (point-at-bol) (1+ (point-at-eol)))
	      (insert 
	       "Project ToDo list")
	      (put-text-property (point-at-bol) (point-at-eol) 'redo-cmd `(superman-project-timeline ,nick))
	      (put-text-property (point-at-bol) (point-at-eol) 'git-dir (superman-git-toplevel loc))
	      (put-text-property (point-at-bol) (point-at-eol) 'dir loc)
	      (put-text-property (point-at-bol) (point-at-eol) 'nickname nick)
	      (put-text-property (point-at-bol) (point-at-eol) 'index index)
	      (insert
	       "  " (superman-make-button "Project view" '(:fun superman-view-back :face superman-next-project-button-face  :help "Back to project view."))
	       "  " (superman-make-button "Git" '(:fun superman-git-display :face superman-next-project-button-face :help "Control project's git repository."))
	       "  " (superman-make-button "File-list" '(:fun superman-view-file-list :face superman-next-project-button-face :help "View project's file-list."))
	       "  " (superman-make-button "Todo" '(:fun superman-project-todo :face superman-next-project-button-face :help "View project's todo list."))
	       "\n")))))
    (if (file-exists-p index)
	(progn
	  ;; we need to be 100% sure that the agenda is not accidentally 
	  ;; written to the index file, but how?
	  (switch-to-buffer
	   (get-buffer-create org-agenda-buffer-name))
	  (find-file index)
	  (push ?L unread-command-events)
	  (call-interactively 'org-agenda))
      (switch-to-buffer
       (get-buffer-create org-agenda-buffer-name))
      (insert "Empty time-line (project index file does not yet exist."))))


(defun superman-recent-org (project)
  (car (superman-list-files
	(superman-get-location project)
	"^[^\\.].*\\.org$" "time")))


(setq superman-project-todolist-balls
      '((todo ("width" 7) ("face" superman-get-todo-face))
	(priority ("width" 8) ("face" superman-get-priority-face))
	(index ("width" 23) ("face" font-lock-keyword-face) ("name" "File"))
	(hdr ("width" 23) ("face" font-lock-function-name-face) ("name" "Description"))
	("DEADLINE" ("fun" superman-trim-date) ("width" 12) ("face" superman-warning-face))
	("CaptureDate" ("fun" superman-trim-date) ("width" 12) ("face" font-lock-string-face))))

(defun superman-project-todo (&optional project)
  "Display a project specific todo-list based on all org files."
  (interactive)
  (let* ((project (superman-get-project project))
	 (nick (car project))
	 (title  (concat nick ": Todo-list"))
	 (loc (superman-project-home project))
	 (index (superman-get-index project)))
    (put-text-property 0 (length title) 'git-dir (superman-git-toplevel loc) title)
    (put-text-property 0 (length title) 'dir loc title)
    (put-text-property 0 (length title) 'nickname nick title)
    (put-text-property 0 (length title) 'index index title)
    (if (file-exists-p loc)
	(let* ((org-agenda-buffer-name  (concat "*Todo[" (car project) "]*"))
	       (org-agenda-sticky nil)
	       (org-agenda-redo-command 'superman-project-todo)
	       (org-agenda-custom-commands
		(when (file-exists-p loc)
		  `(("T" "Projects-TODO"
		     ((todo ""
			    ((org-agenda-files 
			      (mapcar 'file-list-make-file-name
				      (file-list-select
				       nil
				       (or "^[^\\.#].*org$" ".") nil nil
				       loc
				       nil t))))))
		     ((org-agenda-window-setup 'current-window)
		      (org-agenda-finalize-hook
		       (lambda ()
			 (superman-format-agenda
			  superman-project-todolist-balls
			  '(superman-project-todo)
			  title
			  (concat "  " (superman-make-button "Project view" '(:fun superman-view-back :face superman-next-project-button-face  :help "Back to project view."))
				  "  " (superman-make-button "Git" '(:fun superman-git-display :face superman-next-project-button-face :help "Control project's git repository."))
				  "  " (superman-make-button "File-list" '(:fun superman-view-file-list :face superman-next-project-button-face :help "View project's file-list."))
				  ;; "  " (superman-make-button "Todo" '(:fun superman-project-todo :face superman-next-project-button-face :help "View project's todo list."))
				  "  " (superman-make-button "Time-line" '(:fun superman-project-timeline :face superman-next-project-button-face :help "View project's timeline.")))
			  nil 'pretty)))))))))
	  ;; to be 100% sure that the agenda is not accidentally written
	  ;; to the index file
	  (switch-to-buffer (get-buffer-create org-agenda-buffer-name))
	  (push ?T unread-command-events)
	  (call-interactively 'org-agenda))
      (switch-to-buffer (get-buffer-create (concat "*Todo[" (car project) "]*")))
      (erase-buffer)
      (insert "Superman Project TODO:\n\n Directory " loc " does not exist or is not readable."))))
      

;;}}}
;;{{{ superman-shell
(defun superman-goto-shell ()
  "Switches to *shell* buffer and. "
  (interactive)
  (let ((sbuf (get-buffer "*shell*"))
	(cmd (concat "cd " default-directory))
	input)
    (if sbuf
	(switch-to-buffer-other-window sbuf)
      (split-window-vertically)
      (shell))
    (goto-char (point-max))
    (comint-bol)
    (when (looking-at ".*")
      (setq input (match-string 0))
      (replace-match ""))
    (insert cmd)
    (comint-send-input)
    (if input (insert input))))
;;}}}
;;{{{ synchronization

(defun superman-read-rsync (project)
  (let* (rsync)
    (save-window-excursion
      (superman-goto-project project "Configuration" nil)
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*rsync:[ \t]*" (point-max) t)
	(if rsync
	    (setq rsync (concat rsync " ; "
				(replace-regexp-in-string
				 "[ \t]*$" ""
				 (buffer-substring-no-properties (point) (point-at-eol)))))
	  (setq rsync
		(replace-regexp-in-string
		 "[ \t]*$" ""
		 (buffer-substring-no-properties (point) (point-at-eol)))))))
    rsync))
(defun superman-save-rsync (&optional config project)
  (interactive)
  (let* ((pro (superman-get-project project))
	 (org-capture-mode-hook 'org-narrow-to-subtree)
	 (org-capture-templates `(("s" "save" plain
				   (file+headline (superman-get-index pro) "Configuration")
				   ,(concat "rsync:" "%(read-directory-name \"Rsync from: \") "
					    "%(read-directory-name \"Rsync to: \")") :unnarrowed t))))
    (org-capture nil "s")))

(defun superman-synchronize-project (&optional project)
  (interactive)
  (let* ((pro (or project
		  superman-current-project
		  (superman-switch-to-project nil t)))
	 (rsync-list (superman-distangle-config
		      (superman-read-rsync pro)))
	 (rsync-cmd (when rsync-list
		      (if (eq 1 (length rsync-list))
			  (caar rsync-list)
			(caar (completing-read "Choose rsync: " rsync-list)))))
	 (cmd (concat "rsync -e ssh -avzAHX --delete-after " rsync-cmd)))
    (when (yes-or-no-p (concat "Do this? " cmd))
      (shell-command-to-string cmd))))


;;}}}
(provide 'superman-config)
;;; superman-config.el ends here
