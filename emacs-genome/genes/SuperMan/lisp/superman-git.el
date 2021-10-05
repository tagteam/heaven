;;; superman-git.el --- Summary of project contents and adding information to projects

;; Copyright (C) 2012-2017  Klaus Kähler Holst, Thomas Alexander Gerds

;; Authors: Klaus Kähler Holst <kkho@biostat.ku.dk>
;;          Thomas Alexander Gerds <tag@biostat.ku.dk>
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

;;; Code:

;;{{{ variables


(defvar superman-cmd-git "git")
(defvar superman-git-ignore "" "Decides about which files to include and which to exclude.
See M-x manual-entry RET gitignore.
By default we set this to '' which means that no files are ignored.")

;;}}}
;;{{{ superman run cmd

(defun superman-run-cmd (cmd buf &optional intro redo-buf pre-hook post-hook)
  "Execute CMD with `shell-command-to-string' and display
result in buffer BUF. Optional INTRO is shown before the
result. PRE-HOOK and POST-HOOK are functions that are called before and after CMD, respectively."
  (let ((intro (or intro "Superman returns:\n\n"))
	(msg (shell-command-to-string cmd))
	(cur-point (point)))
    (if redo-buf
	(with-current-buffer redo-buf
	  (superman-redo)))
    ;; (when superman-view-mode (superman-redo)))
    (delete-other-windows)
    (split-window-vertically)
    (other-window 1)
    (switch-to-buffer (get-buffer-create buf))
    (unless (eq major-mode 'diff-mode)
      (diff-mode))
    ;;(unless (assoc 'orgstruct-mode minor-mode-alist)
    ;; (orgstruct-mode t)
    (font-lock-mode 1)
    (setq buffer-read-only t)
    (let ((buffer-read-only nil))
      (when pre-hook (funcall pre-hook))
      (goto-char (point-max))
      (put-text-property 0 1 'scroll-position 1 intro)
      (insert (concat "\n*** " (format-time-string "<%Y-%m-%d %a %H:%M>") " "
		      intro msg))
      (goto-char (previous-single-property-change (point) 'scroll-position))
      (let ((this-scroll-margin
	     (min (max 0 scroll-margin)
		  (truncate (/ (window-body-height) 4.0)))))
	(recenter this-scroll-margin))
      (when post-hook (funcall post-hook)))
    (other-window 1)
    (goto-char cur-point)))

;;}}}
;;
;;{{{ initialize project and directory

(defun superman-git-init ()
  (interactive)
  (or (get-text-property (point-min) 'git-dir)
      (let ((pro (superman-view-current-project)))
	(superman-git-init-directory (superman-get-location pro))
	(superman-redo))))

(defun superman-git-init-project (&optional project)
  "Put project under git control."
  (interactive)
  (let* ((pro (superman-get-project project))
	 (index (superman-get-index pro))
	 (loc (superman-get-location pro)))
    (if (not index)
	(error (concat "Trying to superman-git-init-project: Project " (car pro) " has no index file"))
      (superman-git-init-directory loc)
      (if (string-match loc index)
	  (superman-git-add
	   (list index) loc
	   'commit (concat "Initial commit of project " (car pro)))))))

(defun superman-git-init-directory (dir)
  "Put directory DIR under git control."
  (if (superman-git-p dir)
      (message (concat "Directory " dir " is under git control."))
    (shell-command-to-string (concat "cd " dir ";" superman-cmd-git " init"))
    (let ((buffer-read-only nil))
      (put-text-property (point-min) (1+ (point-min)) 'git-dir dir))
    (with-current-buffer
	(get-buffer (get-text-property (point-min) 'project-buffer))
      (let ((buffer-read-only nil))
	(put-text-property (point-min) (1+ (point-min)) 'git-dir dir)))
    (append-to-file superman-git-ignore nil (concat (file-name-as-directory dir) ".gitignore"))))

;;}}}
;;{{{ push, pull, diff, branch and other global git actions 

(defun superman-git-root (file)
  (interactive)  
  "Return git root of FILE"
  (replace-regexp-in-string
   "\n$" "" 
   (shell-command-to-string
    (concat "cd " (file-name-directory (or file (buffer-file-name))) "; "
	    superman-cmd-git " rev-parse --show-toplevel"))))
  
(defun superman-git-p (dir)
  "Test if directory DIR is under git control."
  (when (and dir (file-exists-p dir))
    (string=
     "true\n"
     (shell-command-to-string
      (concat "cd " dir ";"
	      superman-cmd-git " rev-parse --is-inside-work-tree ")))))


(defun superman-list-project-subdirs (dir)
  "List subdirectories of directory DIR"
  (let ((dir-list (file-name-all-completions "" dir)))
    (setq dir-list
	  (delq nil
		(mapcar #'(lambda (file)
			    (when (and (directory-name-p file) (not (member file '("./" "../"))))
			      file)) dir-list)))
    dir-list))

(defun superman-list-git-subdirs (dir)
  "List subdirectories of directory DIR"
  (let ((dir-list (superman-list-project-subdirs dir)))
    (setq dir-list
	  (delq nil
		(mapcar 'superman-git-toplevel dir-list)))
	  dir-list))

(defun superman-git-action (action &optional dir buf)
  "Run a git command ACTION in directory DIR and display result. Optional argument BUF is
passed to `superman-run-cmd'."
  (let ((dir (or dir (if superman-view-mode
			 (get-text-property (point-min) 'git-dir)
		       (if superman-current-project
			   (superman-get-location superman-current-project)
			 (read-directory-name "Path to git repository: "))))))
    (save-excursion
      (superman-run-cmd
       (concat "cd " dir "; " superman-cmd-git " " action "\n")
       (or buf "*Superman-returns*")
       (concat "git " action " '" dir "' returns:\n\n")))))


(defun superman-git-status ()
  "Run \"git status\" on current project."
  (interactive)
  (let ((dir (get-text-property (point-min) 'git-dir)))
    (when dir
      (superman-git-action "status" dir))))

(defun superman-git-commit-project ()
  "Run \"git commit\" on current project."
  (interactive)
  (superman-git-status)
  (let* ((dir (get-text-property (point-min) 'git-dir))
	 (message
	  (read-string 
	   (concat "Commit message" ": ")))
	 (action (concat " commit -m \"" message "\" ")))
    (when dir
      (superman-git-action action dir)
      (superman-redo))))


(defun superman-git-push (&optional dir)
  "Pull to remote at DIR."
  (interactive)
  (let ((dir (get-text-property (point-min) 'git-dir)))
    (when dir
      (superman-git-action "push"  dir))))

(defun superman-git-pull (&optional dir)
  "Pull from remote at DIR."
  (interactive)
  (let ((dir (get-text-property (point-min) 'git-dir)))
    (superman-save-some-buffers nil)
    (when dir
      (superman-git-action "pull" dir))))

(defun superman-git-toplevel (file)
  "Check if the toplevel directory DIR is under git control."
  (when (and file (file-exists-p file))
    (let ((dir (if (file-directory-p file) file (file-name-directory file))))
      (if (superman-git-p dir)
	  (replace-regexp-in-string
	   "\n" ""
	   (shell-command-to-string
	    (concat "cd " dir "; " superman-cmd-git " rev-parse --show-toplevel ")))))))

(defun superman-git-list-branches (dir)
  (let* ((branch-list
	  (mapcar #'(lambda (x)
		      (replace-regexp-in-string
		       "^[ \t\n]+\\|[ \t\n]+$" "" x nil t))
		  (delete "" (split-string
			      (shell-command-to-string
			       (concat "cd " dir "; " superman-cmd-git " branch -a ")) "\n"))))
	 (current (if branch-list (car (member-if (lambda (x) (string-match "^\\*" x)) branch-list))
		    "master"))
	 (others (when branch-list (delete current branch-list))))
    (cons current others)))


(defun superman-git-merge-branches (dir)
  (let* ((branch-list (superman-git-list-branches dir))
	 (current-branch (car branch-list))
	 (other-branches (cdr branch-list))
	 (m-branch (completing-read
		    (concat "Merge current branch (" current-branch ") with: ")
		    (mapcar* 'cons other-branches
			     (make-list (length other-branches) `())))))
    (save-excursion
      (superman-run-cmd
       (concat "cd " dir ";" superman-cmd-git " merge " m-branch  "\n")
       "*Superman-returns*"
       (concat "git merge returns:\n\n")))))


(defun superman-git-new-branch (&optional dir)
  "Create a new branch in git directory DIR. If DIR is nil
use the location of the current project, if no project is current prompt for project."
  (interactive)
  (let ((dir (or dir
		 (superman-project-home
		  (superman-get-project nil)))))
    (message
     (shell-command-to-string
      (concat "cd " dir "; " superman-cmd-git " branch " (read-string "Name of new branch: ") "\n")))
    (when superman-view-mode (superman-redo))))

(defun superman-git-checkout-branch (&optional dir branch)
  "Checkout branch in git directory DIR. If DIR is nil
use the location of the current project, if no project is current prompt for project."
  (interactive)
  (let* ((dir (or dir
		  (superman-project-home
		   (superman-get-project nil))))
	 (branch (or branch
		     (let ((branches (superman-git-list-branches dir)))
		       (completing-read "Choose branch to checkout: "
					(mapcar* 'cons branches (make-list (length branches) `()))
					nil t)))))
    (save-excursion
      (superman-run-cmd 
       (concat "cd " dir "; " superman-cmd-git " checkout " branch "\n")
       "*Superman-returns*"
       (concat "Superman git checkout branch '" branch "' returns:\n\n")))
    (when superman-view-mode (superman-redo))))


(defun superman-relative-name (file dir)
  "If filename FILE is absolute return the relative filename w.r.t. dir,
Else return FILE as it is."
  (if (file-name-absolute-p file)
      (file-relative-name (expand-file-name file) (expand-file-name dir))
    file))


(defun superman-read-git-date (git-date-string &optional no-time)
  "Transform git date to org-format"
  (with-temp-buffer
    (org-insert-time-stamp 
     (date-to-time git-date-string) (not no-time))))
;;      (set-time-zone-rule t) ;; Use Universal time.
;;      (prog1 (format-time-string "%Y-%m-%d %T UTC" time)
;;        (set-time-zone-rule nil))))

;;}}}
;; 
;;{{{ get and set git status 

(defun superman-git-status-file ()
  "Show git status of the file at point"
  (interactive)
  (let* ((file (superman-filename-at-point))
	 (dir (file-name-directory file)))
    (save-excursion
      (superman-run-cmd
       (concat "cd " dir "; " superman-cmd-git " status "
	       (file-name-nondirectory file) "\n")
       "*Superman-returns*"
       (concat "git status '" file "' returns:\n\n")))))

(defun superman-git-set-status (pom file)
  (interactive)
  (let* ((status (superman-git-XY-status file))
	 (label (superman-label-status
		 (concat (nth 1 status) (nth 2 status)))))
    (org-entry-put pom "gitstatus" label)))

(defun superman-git-XY-status (file)
  "Finds the git status of file FILE as XY code, see M-x manual-entry RET git-status RET,
 and returns a list with 3 elements: the file-name relative to the path of the git-repository,
 the index-status (X) and the work-tree-status (Y)."
  (let* ((dir (file-name-directory file))
	 (raw-status (shell-command-to-string
		      (concat
		       "cd " dir ";"
		       superman-cmd-git
		       " status --ignored --porcelain "
		       "\"" (file-name-nondirectory file) "\"")))
	 (len  (length raw-status))
	 (index-status (if (> len 1)
			   (substring-no-properties raw-status 0 1)
			 ""))
	 (work-tree-status (if (> len 2)
			       (substring-no-properties raw-status 1 2)
			     ""))
	 (fname (if (> len 3)
		    (substring-no-properties raw-status 3 (length raw-status))
		  (file-name-nondirectory file))))
    (list (replace-regexp-in-string "\n" "" fname) index-status work-tree-status)))
  
(defun superman-label-status (XY)
  "Replace git status --  index-status (X) and the work-tree-status (Y) -- by a human readable label."
  (cond ((string= "" XY)
	 "Committed")
	((string= "M " XY)
	 "Modified (staged)")
	((string= " M" XY)
	 "Modified (unstaged)")
	((string= "MM" XY)
	 "Added to index, modified in Worktree")
	((string= "??" XY)
	 "Untracked")
	((string= " D" XY)
	 "Deleted")
	((string= " R" XY)
	 "Renamed")
	((string= " U" XY)
	 "Unmerged")
	((string= "A " XY)
	 "New in git-index")
	((string= "AM" XY)
	 "New in git-index, modified in Worktree")
	((string= "UU" XY)
	 "unmerged, both modified")
	((string= "DD" XY)
	 "unmerged, both deleted")
	((string= "AU" XY)
	 "unmerged, added by us")
	((string= "UD" XY)
	 "unmerged, deleted by them")
	((string= "UA" XY)
	 "unmerged, added by them")
	((string= "DU" XY)
	 "unmerged, deleted by us")
	((string= "AA" XY)
	 "unmerged, both added")
	(t "Unknown")))

;; (defun superman-git-get-commit (arg file &optional dir)
  ;; "Run git log and report the date and message of the n'th commit of
;; file FILE in directory DIR where n is determined by ARG."
  ;; (let* ((dir (cond (dir)
		    ;; ((file-name-absolute-p file) (file-name-directory file))
		    ;; (t (read-directory-name (concat "Find the git directory of file " file ": ")))))
	 ;; (file (superman-relative-name file dir))
	 ;; (date-string
	  ;; (shell-command-to-string
	   ;; (if (string= arg "first")
	       ;; (concat  "cd " dir ";" superman-cmd-git " log --date=local --pretty=format:\"%ad\" --reverse -- "
			;; file "  | head -1")
	     ;; (concat "cd " dir ";" superman-cmd-git " log --date=local -" arg " --pretty=format:\"%ad\" -- " file))))
         ;; (date (if (or (string= date-string "") (string-match "^fatal:" date-string)) "" (superman-read-git-date date-string)))
	 ;; (mess (shell-command-to-string
		;; (if (string= arg "first")
		    ;; (concat "cd " dir ";" superman-cmd-git " log --reverse --pretty=format:\"%s\" -- " file " | head -1")
		  ;; (concat "cd " dir ";"  superman-cmd-git " log -" arg " --pretty=format:\"%s\" -- " file)))))
    ;; (concat date " " (replace-regexp-in-string "\n+" "" mess))))

;;}}}
;;{{{ actions log/search/add/commit/delete on file-at-point

(defun superman-git-log-decoration-file (&optional arg)
  (interactive "p")
  (let* ((limit (if (or (not arg) (= arg 1)) superman-git-log-limit (or arg superman-git-log-limit)))
	 (file (superman-filename-at-point)))
    (superman-git-log file limit nil t)))

(defun superman-git-search-log-of-file (&optional arg)
  (interactive "p")
  (let* ((limit (if (or (not arg) (= arg 1)) superman-git-log-limit (or arg superman-git-log-limit)))
	 (file (superman-filename-at-point)))
    (superman-git-log file limit (read-string "Search string: ")) nil))

(defun superman-git-log-file (&optional arg)
  (interactive "p")
  (let* ((limit (if (or (not arg) (= arg 1))
		    superman-git-log-limit
		  (or arg superman-git-log-limit)))
	 (file (or (condition-case nil (superman-filename-at-point) (error nil)) (buffer-file-name))))
    (superman-git-log file limit nil nil)))

(defun superman-git-last-log-file (&optional arg)
  "Retrieves last commit message(s) of file"
  (interactive "p")
  (let*
      ((filename (superman-filename-at-point))
       (file (file-name-nondirectory filename))
       (dir (if filename (expand-file-name (file-name-directory filename))))
       (n (or arg 1))
       (cmd (concat superman-cmd-git " log -n" (number-to-string n) " -- " filename)))
    (superman-run-cmd (concat "cd " dir ";" cmd) 
		      "*Superman-returns*" (concat "log -- " file "\n"))))

(defun superman-git-add-file ()
  "Add the file at point to the git repository."
  (interactive)
  (let* ((filename (superman-filename-at-point))
	 (file (file-name-nondirectory filename))
	 (dir (if filename (expand-file-name (file-name-directory filename))))
	 (fbuf (get-file-buffer file)))
    (when (and fbuf
	       (with-current-buffer fbuf (buffer-modified-p))
	       (y-or-n-p (concat "Save buffer " fbuf "?")))
      (with-current-buffer fbuf (save-buffer)))
    (superman-git-add (list file) dir nil nil)
    (superman-git-set-status (org-get-at-bol 'org-hd-marker) filename)
    (superman-view-redo-line)
    (forward-line 1)))

(defun superman-git-commit-file ()
  "Add and commit the file given by the filename property
of the item at point."
  (interactive)
  (let* ((filename (superman-filename-at-point))
	 (file (file-name-nondirectory filename))
	 (dir (if filename (expand-file-name (file-name-directory filename))))
	 (fbuf (get-file-buffer filename)))
    (when (and fbuf
	       (with-current-buffer fbuf (buffer-modified-p))
	       (y-or-n-p (concat "Save buffer " (buffer-name fbuf) "?")))
      (with-current-buffer fbuf (save-buffer)))
    (superman-git-add (list file) dir 'commit nil)
    (superman-git-set-status (org-get-at-bol 'org-hd-marker) filename)
    (superman-view-redo-line)))

(defun superman-git-reset-file ()
  "Reset (unstaged) changes via \"git checkout HEAD file\" of the file
given by the filename property of the item at point."
  (interactive)
  (let* ((filename (superman-filename-at-point))
	 (file (file-name-nondirectory filename))
	 (dir (if filename (expand-file-name (file-name-directory filename))))
	 (fbuf (get-file-buffer file)))
    (when (and fbuf
	       (with-current-buffer fbuf (buffer-modified-p))
	       (y-or-n-p (concat "Save buffer " fbuf "?")))
      (with-current-buffer fbuf (save-buffer)))
    (when (y-or-n-p (concat "Really reset file " file "? "))
      (save-excursion
	(superman-run-cmd (concat "cd " dir ";" superman-cmd-git " checkout HEAD -- " file)
			  "*Superman-returns*"))
      (superman-view-redo-line))))


(defun superman-git-ignore-file ()
  "Add the file at point to .gitignore."
  (interactive)
  (let* 
      ((dir (get-text-property (point-min) 'git-dir))
       (filename (superman-filename-at-point 'no-error))
       (gitignore (concat dir "/.gitignore")))
    (if filename
	(progn
	  (append-to-file (concat  (replace-regexp-in-string (concat dir "/") "" (expand-file-name filename)) "\n") nil gitignore)
	  (let ((buffer-read-only nil))
	    (beginning-of-line)
	    (delete-region (point-at-bol) (1+ (point-at-eol)))))
      (find-file gitignore))))

(defun superman-git-delete-file ()
  "Delete the file at point by calling `git rm'."
  (interactive)
  (when (superman-view-delete-entry 'dont 'dont)
    (let ((buffer-read-only nil))
      (delete-region (point-at-bol) (1+ (point-at-eol))))))

;;}}}
;;{{{ actions add/commit/delete on all marked files

(defun superman-check-if-saved-needed ()
  (member (expand-file-name (buffer-file-name))
	  (superman-view-marked-files)))

(defun superman-git-add-marked ()
  "Call git add on the list of marked files."
  (interactive)
  (let* ((dir (get-text-property (point-min) 'git-dir))
	 (files
	  (mapcar 'expand-file-name 
		  (superman-view-marked-files))))
    (if (not files)
	(message "Apparently, no files are marked.")
      ;; prevent committing unsaved buffers
      ;; (superman-save-some-buffers nil 'superman-check-if-saved-needed)
      (superman-save-some-buffers nil)
      (when dir
	(superman-git-add files dir nil nil)
	(superman-redo)))))

(defun superman-git-commit-marked (&optional commit)
  "Call git commit on the list of marked files."
  (interactive)
  (let* ((dir (get-text-property (point-min) 'git-dir))
	 (files
	  (mapcar 'expand-file-name 
		  (superman-view-marked-files))))
    (if (not files)
	(message "Apparently, no files are marked.")
      ;; prevent committing unsaved buffers
      ;; (superman-save-some-buffers nil 'superman-check-if-saved-needed)
      (superman-save-some-buffers nil)
      (when dir (superman-git-add files dir 'commit nil)
	    ;; move point inside cat to the first marked entry
	    ;; FIXME: it would be safer to have a property 'marked
	    (when (next-single-property-change (point-min) 'type)
	      (goto-char (next-single-property-change (point-min) 'type))) ;; org-marked-entry-overlay
	    (when (next-single-property-change (point) 'cat)
	      (goto-char (next-single-property-change (point) 'cat))
	      (superman-redo-cat))))))

(defun superman-git-add (file-list dir &optional commit message)
  "Call git add on the files given by FILE-LIST. If DIR is nil,
prompt for project and use the associated git repository.
If FILE-LIST is nil then read file name below DIR.

If COMMIT is non-nil prompt for commit message and
commit the file to the git repository."
  (interactive)
  (let* ((dir (or dir
		  (let ((pro (superman-get-project nil)))
		    (superman-get-location pro))))
	 (file-list (or file-list
			(list (read-file-name "Git add file: " dir nil t))))
	 (file-list-string
	  (apply 'concat
		 (mapcar `(lambda (f) (concat " " (superman-relative-name f ,dir))) file-list)))
	 (cmd (concat "cd " dir ";" superman-cmd-git " add -f " file-list-string))
	 (message (if commit (or message (read-string (if (= (length file-list) 1)
							  (concat "Commit message for file:" file-list-string ": ")
							(concat "Commit message for files:" file-list-string ": ")))))))
    (if message (setq cmd (concat cmd  ";" superman-cmd-git " commit -m \"" message "\" " file-list-string)))
    (superman-run-cmd cmd "*Superman-returns*" cmd)))

;;}}}
;;{{{ git diff and annotate

(defvar superman-git-diff-context-lines nil
  "Number of context lines for diff. Passed to -U flag of git diff,
see M-x manual-entry RET git-diff RET.")

(defun superman-git-diff-file (&optional file dir hash ref config marker)
  (interactive)
  (let* ((file (or file (superman-filename-at-point)))
	 (dir (or dir (superman-git-toplevel file)))
	 (hash (or hash
		   (superman-get-property (get-text-property (point-at-bol) 'superman-item-marker) "commit")
		   ;; (get-text-property (point-at-bol) 'hash)
		   ))
	 (ref (if hash (or ref (concat hash "^")) "HEAD"))
	 (cmd (concat "cd " (file-name-directory file)
		      ";" superman-cmd-git " diff "
		      ref " " hash " "
		      (when superman-git-diff-context-lines
			(concat "-U" superman-git-diff-context-lines " "))
		      "./" (file-name-nondirectory file) "\n"))
	 (msg (concat
	       "diff "
	       (if hash (concat ref " " hash " ") "HEAD\n")
	       (file-relative-name file (superman-git-toplevel file)) "\n")))
    (superman-run-cmd cmd "*Superman:Git-diff*" msg nil
		      'erase-buffer `(lambda ()
				       (interactive)(superman-prepare-git-diff-buffer ,ref ,hash dir)))
    (when config (superman-switch-config nil 0 config)
	  (when marker
	    (select-window (get-buffer-window (marker-buffer marker) nil))
	    (goto-char (marker-position marker))))))


(defun superman-git-diff (&optional dir hash ref config)
  (interactive)
  (let* ((ref (or ref (if hash (concat hash "^") "HEAD")))
	 (hash (or hash ""))
	 (dir (or dir (get-text-property (point-min) 'git-dir)))
	 (cmd (concat "cd " dir
		      ";" superman-cmd-git " diff "
		      (unless (string= hash "") (concat hash "^ " hash " "))
		      (when superman-git-diff-context-lines
			(concat "-U" superman-git-diff-context-lines " "))
		      "\n"))
	 (msg (concat
	       "diff "
	       (if hash (concat ref " " hash " ") "HEAD")"\n")))
    (superman-run-cmd
     cmd "*Superman:Git-diff*" msg nil
     'erase-buffer '(lambda ()
		      (superman-prepare-git-diff-buffer ref hash dir)))
    (when config (superman-switch-config nil 0 config))))

(defun superman-prepare-git-diff-buffer (a b dir)
  (let ((buffer-read-only nil))
    (goto-char (point-min))
    (while (re-search-forward "^\\-\\-\\-[ ]*" nil t)
      (let* ((file (progn (looking-at ".*$")
			  (match-string-no-properties 0))))
	(beginning-of-line)
	(insert "*** " file "\n")
	(insert (superman-make-button
		 "Highlight differences"
		 `(:fun (lambda () (interactive)
			  (superman-compare-files
			   ,(concat default-directory 
				    (substring file
					       1 (length file)))
			   ,a
			   ,b))
			:face superman-next-project-button-face
			:help "Show these versions with differences highlighted"))
		"\n")
	(forward-line 2)))
    (goto-char (point-min))))

(defun superman-compare-files (file a b)
  (interactive)
  (let ((buf-a (get-buffer-create
		(concat (file-name-nondirectory file) "_" a)))
	(buf-b (get-buffer-create
		(concat (file-name-nondirectory file) "_" (if (string= b "") "WorkSpace" b)))))
    (set-buffer buf-a)
    (erase-buffer)
    (insert
     (shell-command-to-string 
      (concat "cd " (file-name-directory file)
	      ";" superman-cmd-git
	      " show " a ":./"
	      (file-name-nondirectory file))))
    (set-buffer buf-b)
    (erase-buffer)
    (if (string= b "")
	(insert-file file)
      (insert
       (shell-command-to-string 
	(concat "cd " (file-name-directory file)
		";" superman-cmd-git
		" show " b ":./"
		(file-name-nondirectory file)))))
    (highlight-compare-buffers buf-a buf-b)
    (set-buffer buf-b)
    (highlight-changes-next-change)
    (set-buffer buf-a)
    (highlight-changes-next-change)
    (superman-set-config
     (concat
      (buffer-name buf-a)
      " | "
      (buffer-name buf-b)))))


(defun superman-annotate-version (&optional version)
  (interactive)
  (font-lock-mode -1)
  (save-excursion
    (let ((version
	   (or version
	       (buffer-substring
		(point-at-bol)
		(progn (goto-char (point-at-bol))
		       (forward-word)
		       (point)))))
	  (buffer-read-only nil))
      (goto-char (point-min))
      (while (re-search-forward version nil t)
	(put-text-property (point-at-bol) (+ (point-at-bol) (length version))
			   'face 'superman-warning-face)
	(put-text-property
	 (progn (skip-chars-forward "^\\)")
		(+ (point) 1))
	 (point-at-eol)
	 'face 'superman-warning-face)))))


(defun superman-git-annotate (&optional file)
  "Annotate file"
  (interactive)
  (let* ((file (or file
		   (get-text-property (point-min) 'filename)
		   (condition-case nil (superman-filename-at-point) (error nil))
		   (buffer-file-name)))
	 (nick (get-text-property (point-min) 'nickname))
	 (index (get-text-property (point-min) 'index))
	 (git-dir (or (get-text-property (point-min) 'git-dir)
		      (superman-git-toplevel file)))
	 (bufn (concat "*Superman-annotate:" (file-name-nondirectory file) "*")))
    ;; (bufn))
    ;; (save-window-excursion
    ;; (find-file file)
    ;; (vc-annotate (org-link-display-format file) "HEAD")
    ;; (setq bufn (buffer-name)))
    (switch-to-buffer bufn)
    (let ((buffer-read-only nil))
      (erase-buffer)
      ;; (goto-char (point-min))
      (insert "* Annotation of " (file-name-nondirectory file))
      ;; (put-text-property (point-at-bol) (point-at-eol) 'face 'org-level-1)
      (put-text-property (point-at-bol) (point-at-eol) 'filename file)
      (put-text-property (point-at-bol) (point-at-eol) 'index index)
      (put-text-property (point-at-bol) (point-at-eol) 'nickname nick)
      (put-text-property (point-at-bol) (point-at-eol) 'git-dir git-dir)
      (insert "  " (superman-make-button "Project view" '(:fun superman-view-back :face superman-next-project-button-face  :help "Back to project view."))
	      "  " (superman-make-button "Git overview" '(:fun superman-git-display :face superman-next-project-button-face :help "Control project's git repository."))
	      ;; "  " (superman-make-button "annotate" '(:fun superman-git-annotate :face superman-next-project-button-face :help "Annotate."))
	      "  " (superman-make-button "versions" '(:fun superman-git-log :face superman-next-project-button-face :help "Show git log.")))
      (insert "\n")
      (insert (shell-command-to-string (concat "cd " git-dir ";" superman-cmd-git " blame " file))))))

;;}}}
;;
;;{{{ git display cycle

(defvar superman-git-display-cycles nil
  "Keywords to match the elements in superman-git-display-command-list")
(make-variable-buffer-local 'superman-git-display-cycles)
(setq superman-git-display-cycles nil)
(setq superman-git-default-displays
      '("versions" "modified" "tracked" "untracked" "stash"))
;; "submodule"
(defvar superman-git-display-diff-balls
  '(("filename" ("width" 14) ("fun" superman-make-git-keyboard) ("name" "git-keyboard") ("face" "no-face"))
    ("GitStatus" ("width" 20) ("face" superman-get-git-status-face) ("name" "What happened"))
    (hdr ("width" 34) ("face" font-lock-function-name-face) ("name" "Filename"))
    ("Directory" ("width" 25) ("face" superman-subheader-face)))
  "Balls to format git diff views.")

(defun superman-trim-hash (hash &rest args)
  (superman-make-button
   (superman-trim-string hash (car args))
   '(:fun superman-choose-entry :help "Run git diff")))

(defvar superman-git-display-command-list
  '(("versions"
     "log -n 25 --skip 0 --name-status --date=short --pretty=format:\"** %h\n:PROPERTIES:\n:Commit: %h\n:Author: %an\n:Tag: %d\n:Date: %cd\n:Message: %s\n:END:\n\""
     ((hdr ("width" 9) ("face" font-lock-function-name-face) ("name" "Commit") ("fun" superman-trim-hash))
      ("Author" ("width" 10) ("face" superman-get-git-status-face))
      ("Tag" ("width" 10))
      ("Date" ("width" 13) ("fun" superman-trim-string) ("face" font-lock-string-face))
      ("Message" ("width" 63)))
     superman-git-log-pre-display-hook
     superman-git-log-post-display-hook)
    ("tracked"
     "ls-files --full-name"
     (("filename" ("width" 14) ("fun" superman-make-git-keyboard) ("name" "git-keyboard") ("face" "no-face"))
      ("Directory" ("width" 25) ("face" superman-subheader-face))
      ("GitStatus" ("width" 20) ("face" superman-get-git-status-face))
      (hdr ("fun" superman-dont-trim) ("face" font-lock-function-name-face) ("name" "Filename")))
     superman-git-files-pre-display-hook)
    ;; ("submodule"
    ;; "submodule"
    ;; (("filename" ("width" 14) ("fun" superman-make-git-keyboard) ("name" "git-keyboard") ("face" "no-face"))
    ;; (hdr ("width" 34) ("face" font-lock-function-name-face) ("name" "Filename"))
    ;; ("Directory" ("width" 25) ("face" superman-subheader-face))
    ;; ("GitStatus" ("width" 20) ("face" superman-get-git-status-face)))
    ;; superman-git-submodule-pre-display-hook)
    ("untracked"
     (concat "ls-files --full-name " (unless superman-git-show-ignored "--exclude-standard") " --others")
     (("filename" ("width" 14) ("fun" superman-make-git-keyboard) ("name" "git-keyboard") ("face" "no-face"))
      ("Directory" ("width" 25) ("face" superman-subheader-face))
      ("GitStatus" ("width" 20) ("face" superman-get-git-status-face))
      (hdr  ("fun" superman-dont-trim) ("face" font-lock-function-name-face) ("name" "Filename")))
     superman-git-untracked-pre-display-hook
     superman-git-untracked-post-display-hook)
    ("modified"
     "ls-files --full-name -m"
     (("filename" ("width" 14) ("fun" superman-make-git-keyboard) ("name" "git-keyboard") ("face" "no-face"))
      ("Directory" ("width" 25) ("face" superman-subheader-face))
      ("GitStatus" ("width" 20) ("face" superman-get-git-status-face))
      (hdr  ("fun" superman-dont-trim) ("face" font-lock-function-name-face) ("name" "Filename")))
     superman-git-files-pre-display-hook)
    ("diff"
     "diff --name-status"
     (("filename" ("width" 14) ("fun" superman-make-git-keyboard) ("name" "git-keyboard") ("face" "no-face"))
      ("GitStatus" ("width" 20) ("face" superman-get-git-status-face))
      ("Directory" ("width" 25) ("face" superman-subheader-face))
      (hdr  ("fun" superman-dont-trim) ("face" font-lock-function-name-face) ("name" "Filename")))
     superman-git-diff-pre-display-hook)
    ("stash"
     "stash show --name-status"
     (("filename" ("width" 14) ("fun" superman-make-git-keyboard) ("name" "git-keyboard") ("face" "no-face"))
      ("GitStatus" ("width" 20) ("face" superman-get-git-status-face))
      ("Directory" ("width" 25) ("face" superman-subheader-face))
      (hdr  ("fun" superman-dont-trim) ("face" font-lock-function-name-face) ("name" "Filename")))
     superman-git-diff-pre-display-hook))
  "List of git-views. Each entry has 5 elements: (key git-switches balls pre-hook post-hook):

1) key is a string to identify the element
2) git-switches are the switches passed to git
3) balls are used to define the columns
4) pre-hook is is called before `superman' plays the balls.
5) post-hook is is called after `superman' has played the balls.
")

(defun superman-git-display (&optional project)
  "Display git control for the current project's directory
 (if it is git controlled). This function inserts the header
and then calls `superman-redo-cat' to format the results of an appropriate
git command."
  (interactive)
  (when project
    (superman-view-project project))
  (when (get-text-property (point-min) 'dir)
    ;; open git view buffer
    (let* ((index (get-text-property (point-min) 'index))
	   (pbuf (buffer-name))
	   (nickname (get-text-property (point-min) 'nickname))
	   (git-display-buf (concat "*Git[" nickname "]*"))
	   (dir (get-text-property (point-min) 'dir))
	   (git-dir (get-text-property (point-min) 'git-dir)))
      (unless (get-buffer git-display-buf)
	(get-buffer-create git-display-buf))
      (switch-to-buffer git-display-buf)
      (setq buffer-read-only t)
      (let ((buffer-read-only nil))
	(if (get-text-property (point-min) 'git-display)
	    nil ;; buffer already showing git-display
	  (erase-buffer) ;; probably unnecessary
	  (goto-char (point-min))
	  (insert (superman-make-button
		   (concat "* Git: " nickname)
		   '(:fun superman-redo :face superman-project-button-face)))
	  (insert "  " (superman-make-button "Project view" '(:fun superman-view-back :face superman-next-project-button-face  :help "Back to project view."))
		  "  " (superman-make-button "File-list" '(:fun superman-view-file-list :face superman-next-project-button-face :help "View project's file-list."))
		  "  " (superman-make-button "Todo" '(:fun superman-project-todo :face superman-next-project-button-face :help "View project's todo list."))
		  "  " (superman-make-button "Time-line" '(:fun superman-project-timeline :face superman-next-project-button-face :help "View project's timeline.")))
	  (insert "\n\n")
	  (when git-dir
	    (superman-view-insert-git-branches git-dir)
	    (superman-view-insert-git-buttons)
	    (insert "\n"))
	  (insert "\n")
	  (put-text-property (point-min) (1+ (point-min)) 'redo-cmd '(superman-redo-git-display))
	  (put-text-property (point-min) (1+ (point-min)) 'region-start t)
	  (put-text-property (point-min) (1+ (point-min)) 'project-buffer pbuf)
	  (put-text-property (point-min) (1+ (point-min)) 'nickname nickname)
	  (put-text-property (point-min) (1+ (point-min)) 'git-dir git-dir)
	  (put-text-property (point-min) (1+ (point-min)) 'index index)
	  (put-text-property (point-min) (1+ (point-min)) 'git-display "versions")
	  (superman-view-mode-on)
	  (superman-git-mode-on)
	  (insert "** Git")
	  (put-text-property (point-at-bol) (1+ (point-at-bol)) 'cat 'git)
	  (if (not git-dir)
	      (insert (superman-initialize-git-control-string dir))
	    (superman-redo-cat)))))))

(defun superman-set-git-cycle (value)
  (let ((buffer-read-only nil))
    (put-text-property (point-min) (1+ (point-min)) 'git-display value)))
;; (org-with-point-at (get-text-property (point-at-bol) 'org-hd-marker)
;; (org-set-property "git-display" value))

(defun superman-cycle-git-display ()
  "Cycles to the next value in `superman-git-display-cycles'.
This function should be bound to a key or button."
  (interactive)
  (let* ((cycles superman-git-default-displays)
	 (current (or (get-text-property (point-min) 'git-display)
		      (car cycles)))
	 (rest (member current cycles))
	 (next (if (> (length rest) 1) (cadr rest) (car cycles))))
    (superman-set-git-cycle next)
    (superman-redo-cat)))

(defun superman-redo-git-display ()
  "Update the header and then call `superman-redo-cat'."
  (interactive)
  (let ((git-dir (get-text-property (point-min) 'git-dir))
	(branch-start (next-single-property-change (point-min) 'git-branches))
	(remote-start (next-single-property-change (point-min) 'git-remote))
	(buttons (next-single-property-change (point-min) 'git-buttons))
	(buffer-read-only nil))
    ;; delete remote line
    (when remote-start
      (goto-char remote-start)
      (delete-region (point-at-bol) (1+ (point-at-eol))))
    ;; delete branch line
    (goto-char branch-start)
    (delete-region (point-at-bol) (1+ (point-at-eol)))
    (when git-dir
      (superman-view-insert-git-branches git-dir)
      (if buttons
	  (insert "\n")
	(superman-view-insert-git-buttons)
	)))
  (when (and superman-git-mode
	     (next-single-property-change (point-min) 'cat))
    (goto-char (next-single-property-change (point-min) 'cat))
    (superman-redo-cat)))


(defun superman-initialize-git-control-string (dir)
  (concat "\n\nDirectory " dir " is not yet under git control.\n"
	  "press `I' or this button to: " 
	  (superman-make-button
	   "initialize git control"
	   '(:fun superman-git-init
		  :face superman-capture-button-face
		  (concat
		   :help "Initialize git repository at " dir)))))

(defvar superman-git-log-limit 25 "Limit on number of previous versions shown by default in git log view")
(defvar superman-git-search-limit 250)
(defvar superman-git-log-skip 0 "Skip this many previous versions in git log view")

(defun superman-git-format-display (view-buf dir props index-buf name)
  "Usually called via `superman-git-display' and
`superman-redo-cat' by `superman-format-cat' to format git displays.

If directory is not yet git controlled provide a button which when pressed initializes git control.

Argument VIEW-BUF is the buffer which shows the results. DIR is a directory, the git repository.
PROPS is a plist with properties which should contain the keywords git-cycle and git-display.
The 'active display' is the first existing thing of 
 a) text-property 'git-display at point-min in VIEW-BUF
 b) the value of keyword 'git-display' in PROPS 
 c) the car of the value of key 'git-cycle' in PROPS. I.e., this can be set as a property 
    of heading git-repository in a project's index buffer).

The actual git command, balls, pre- and post-hooks are obtained from value of the
'active display' in `superman-git-display-command-list'. 

INDEX-BUF is the buffer which contains the pre-formatted results of the git command.
NAME is used to make the section heading.
 "
  (set-buffer view-buf)
  (setq view-point (point))
  (if (not (get-text-property (point-min) 'git-dir))
      (progn
	(switch-to-buffer view-buf)
	(goto-char view-point)
	(insert "** Git")
	(put-text-property (point-at-bol) (1+ (point-at-bol)) 'cat 'git)
	(insert (superman-initialize-git-control-string dir)))
    (let* ((git-dir (get-text-property (point-min) 'git-dir))
	   (cycles-given (plist-get props :git-cycle))
	   (cycles (cond ((listp cycles-given) cycles-given)
			 (t (split-string
			     cycles-given
			     "[ \t]*,[ \t]*"))))
	   (cycle (or
		   (with-current-buffer view-buf
		     (get-text-property (point-min) 'git-display))
		   (plist-get props :git-display)
		   (car cycles)))
	   (limit (with-current-buffer view-buf
		    (or (get-text-property (point-min) 'limit) superman-git-log-limit)))
	   (skip (with-current-buffer view-buf
		   (or (get-text-property (point-min) 'skip) superman-git-log-skip)))
	   ;; distinguishing git-log of a single file and git control
	   ;; of the whole project/directory:
	   (file (get-text-property (point-min) 'filename))
	   (rest (if file
		     (assoc "versions" superman-git-display-command-list)
		   (assoc cycle superman-git-display-command-list)))
	   (balls (or (nth 2 rest) superman-default-balls))
	   (pre-hook (nth 3 rest))
	   (post-hook (nth 4 rest))
	   (count 0)
	   (cmd (concat "cd " dir ";" superman-cmd-git " "
			(with-current-buffer view-buf
			  (eval (nth 1 rest)))
			(when file
			  (concat " -- "
				  (superman-relative-name file git-dir))))))
      ;; prepare the git output buffer
      (set-buffer (get-buffer-create
		   (concat "*Git output[" git-dir "]*")))
      (erase-buffer)
      (insert "git-output")
      (put-text-property (point-at-bol) (point-at-eol) 'git-dir git-dir)
      (when file (put-text-property (point-at-bol) (point-at-eol) 'filename file))
      (insert "\n")
      (org-mode)
      ;; for the first time ...
      (with-current-buffer view-buf
	(unless (get-text-property (point-min) 'git-display)
	  (put-text-property (point-min) (1+ (point-min)) 'git-display cycle)))
      (unless superman-git-display-cycles (setq superman-git-display-cycles cycles))
      ;; limit on number of revisions
      (when limit
	(setq cmd (replace-regexp-in-string "-n [0-9]+ " (concat "-n " (number-to-string limit) " ") cmd)))
      (when skip
	(setq cmd (replace-regexp-in-string "--skip [0-9]+ " (concat "--skip " (number-to-string skip) " ") cmd)))
      ;; insert the result of git command
      (insert (shell-command-to-string cmd))
      (goto-char (point-min))
      ;; prepare buffer 
      (when pre-hook (funcall pre-hook))
      (goto-char (point-min))
      (while (outline-next-heading)
	(setq count (+ count 1))
	(setq line (superman-format-thing (copy-marker (point-at-bol)) balls))
	(with-current-buffer view-buf (insert line "\n")))
      (set-buffer view-buf)
      (when superman-empty-line-before-cat (insert "\n"))
      (goto-char view-point)
      ;; section names
      (when (and
	     superman-empty-line-before-cat
	     (save-excursion (beginning-of-line 0)
			     (not (looking-at "^[ \t]*$"))))
	(insert "\n"))
      (superman-view-insert-section-name
       name count balls nil 
       'superman-cycle-git-display "Cycle git display (TAB)")
      (insert "\n")
      (end-of-line 0)
      (unless file
	(let ((cycle-strings cycles))
	  (while cycle-strings
	    (let ((cstring (car cycle-strings)))
	      (set-text-properties 0 (length cstring) nil cstring)
	      (insert " >> ")
	      (insert (superman-make-button
		       cstring
		       `(:fun (lambda () (interactive)
				(superman-set-git-cycle ,cstring)
				(superman-redo-cat))
			      :face ,(if (string= cycle cstring)
					 'superman-next-project-button-face nil)
			      :help (concat "Cycle display to git " cstring))))
	      (setq  cycle-strings (cdr cycle-strings))))))
      (forward-line 1)
      ;; insert the column names
      (when superman-empty-line-after-cat (insert "\n"))
      (insert (superman-column-names balls))
      (goto-char (1- (or (next-single-property-change (point) 'cat) (point-max))))
      (put-text-property (- (point-at-eol) 1) (point-at-eol) 'tail name)
      (goto-char (previous-single-property-change (point) 'cat))
      (beginning-of-line)
      (when (re-search-forward "\[[0-9]*\]" nil t)
	(replace-match
	 (superman-make-button (concat "[Set limit: " (number-to-string limit) "]")
			       '(:fun superman-git-set-limit :face superman-default-button-face :help "Set limit of logs"))))
      (when post-hook 
	(goto-char (point-max))
	(funcall post-hook)))))

(defun superman-git-set-limit ()
  "Re-set limit on number of items and redisplay"
  (interactive)
  (if (not (or superman-git-mode superman-git-log-mode))
      (error "Currently this works only in `superman-git-log-mode' and `superman-git-mode'")
    (let ((buffer-read-only nil)
	  (new-limit (string-to-number
		      (read-string "Limit on number of revisions (leave empty to cancel): ")))
	  (new-skip (string-to-number
		      (read-string "Skip this many revisions (default 0): " nil nil "0"))))
      (when (integerp new-limit)
	(put-text-property (point-min) (+ (point-min) 1) 'limit new-limit))
      (when (integerp new-skip)
	(put-text-property (point-min) (+ (point-min) 1) 'skip new-skip))
      (superman-redo-cat))))
	
(defun superman-git-display-diff (commit ref dir file project)
  "Display differences between the versions COMMIT and REF of the git
repository of PROJECT which is located at DIR."
  (let* ((balls superman-git-display-diff-balls)
	 (count 0)
	 (name "Git-diff")
	 (nickname (get-text-property (point-min) 'nickname))
	 (git-dir (or dir (get-text-property (point-min) 'git-dir)))
	 (file (or file (get-text-property (point-min) 'filename)))
	 next-item
	 (ref (cond ((string= commit "Workspace") "HEAD") (t ref)))
	 (commit (cond ((string= commit "Workspace") "") (t commit)))
	 (cmd (concat "cd " git-dir ";"
		      superman-cmd-git " diff "
		      commit " " ref " --name-status"
		      (when file (concat " -- " file))))
	 (log-buf (current-buffer))
	 (commit-string (superman-make-button
			 (if (string= commit "") "Workspace (now)"
			   (concat commit
				   " (" (superman-get-property
					 (get-text-property (point-at-bol) 'superman-item-marker)
					 "date") ")"))
			 '(:fun superman-git-diff-switch-commit
				:face superman-capture-button-face
				:help "Change active version.")))
	 (ref-string (superman-make-button
		      (concat ref
			      " (" (save-excursion
				     (forward-line 1)
				     (or (superman-get-property
					  (get-text-property (point-at-bol) 'superman-item-marker)
					  "date") "unknown")) ")")
		      '(:fun superman-git-diff-switch-ref
			     :face superman-capture-button-face :help "Change reference version.")))
	 (header (concat "Changes in " commit-string " since " ref-string))
	 (index-buf (get-buffer-create (concat "*[" (if file (file-name-nondirectory file)
						      project) ":index" "]*")))
	 (view-buf (get-buffer-create (concat "*[" (if file (file-name-nondirectory file)
						     project) ":diff" "]*")))
	 (config (if file
		     (concat (buffer-name log-buf)  " | *Superman:Git-diff*")
		   (concat (buffer-name log-buf) " / " (buffer-name view-buf) " | *Superman:Git-diff*"))))
    (set-buffer view-buf)
    (org-mode)
    (font-lock-mode -1)
    (superman-view-mode-on)
    (superman-git-mode-on)
    (setq buffer-read-only t)
    (let ((buffer-read-only nil))
      (erase-buffer)
      ;; insert the result of git command
      (set-buffer index-buf)
      (org-mode)
      (erase-buffer)
      (insert (shell-command-to-string cmd))
      (goto-char (point-min))
      (insert "git-output\n")
      (put-text-property (point-min) (1+ (point-min)) 'git-dir git-dir)
      ;; prepare buffer if necessary
      (funcall 'superman-git-diff-pre-display-hook)
      (goto-char (point-min))
      (while (outline-next-heading)
	(setq count (+ count 1))
	(setq line (superman-format-thing (copy-marker (point-at-bol)) balls))
	(with-current-buffer view-buf (insert line "\n")))
      (set-buffer view-buf)
      (when superman-empty-line-before-cat (insert "\n"))
      (goto-char (point-min))
      ;; section names
      (when (and
	     superman-empty-line-before-cat
	     (save-excursion (beginning-of-line 0)
			     (not (looking-at "^[ \t]*$"))))
	(insert "\n"))
      (superman-view-insert-section-name
       name count balls
       nil
       nil)
      (insert "\n")
      (end-of-line 0)
      (forward-line 1)
      ;; insert the column names
      (when superman-empty-line-after-cat (insert "\n"))
      (insert (superman-column-names balls))
      (goto-char (1- (or (next-single-property-change (point) 'cat) (point-max))))
      (put-text-property (- (point-at-eol) 1) (point-at-eol) 'tail name)
      ;; (goto-char (previous-single-property-change (point) 'cat))
      (goto-char (point-min))
      (insert header "\n")
      (when (string= commit "")
	(superman-view-insert-git-buttons)
	(insert "\n"))
      (put-text-property
       (point-min) (+ (point-min) 1) 'redo-cmd
       `(lambda () (superman-git-display-diff ,commit ,ref ,git-dir ,file ,project)))
      (put-text-property (point-min) (+ (point-min) (length header)) 'region-start t)
      (put-text-property (point-min) (+ (point-min) (length header)) 'nickname nickname)
      (put-text-property (point-min) (+ (point-min) (length header)) 'git-dir git-dir)
      ;; prepare the file diffs
      (switch-to-buffer view-buf)
      (goto-char (point-min))
      (set-text-properties 0 (length commit) nil commit)
      (set-text-properties 0 (length ref) nil ref)
      ;; prepare the list of files which have changed
      (let (next)
	(while (setq next (next-single-property-change (point-at-eol) 'superman-item-marker))
	  (goto-char next)
	  (let* ((cmd `(lambda () (superman-git-diff-file
				   ,file ,git-dir
				   ,commit ,ref ,config (point-marker)))))
	    (put-text-property (point-at-bol) (1+ (point-at-bol)) 'superman-choice cmd))))
      (setq next-item (next-single-property-change (point-min) 'superman-item-marker))
      (when next-item ;; is nil when nothing changed between workspace and HEAD
	(goto-char next-item)
	(previous-line))
      (if file (superman-git-diff-file file git-dir commit ref config)
	(superman-git-diff git-dir commit ref config)))))

(defun superman-list-to-alist (list)
  (mapcar* 'cons list (make-list (length list) `())))

(defun superman-git-diff-switch-commit ()
  (interactive)
  (let* ((redo-cmd (get-text-property (point-min) 'redo-cmd))
	 (dir (nth 3 (caddr redo-cmd)))
	 (dir (nth 4 (caddr redo-cmd)))
	 ;; (commit (nth 1 (caddr redo-cmd)))
	 (hash-list (split-string
		     (shell-command-to-string
		      (concat "cd " dir ";" superman-cmd-git " --no-pager log --full-history --pretty=\"%h\""))
		     "\n" t))
	 (hash-alist (superman-list-to-alist hash-list))
	 (commit (completing-read "Choose commit" hast-alist))
	 (ref (nth 2 (caddr redo-cmd)))
	 (nick (nth 5 (caddr redo-cmd))))
    (superman-git-display-diff commit ref dir file nick)))

(defun superman-view-back ()
  "Kill current buffer and return to project view."
  (interactive)
  (let ((pbuf (get-text-property (point-min) 'project-buffer)))
    (if pbuf
	(progn
	  (kill-buffer (current-buffer))
	  (switch-to-buffer pbuf)
	  (superman-redo))
      (superman-view-project))))

;;}}}
;;{{{ preparing git displays

(defun superman-get-git-status-face (str)
  (cond ((string-match "Committed" str ) 'font-lock-type-face)
	((string-match  "Modified" str) 'superman-warning-face)
	(t 'font-lock-comment-face)))

(defun superman-git-untracked-pre-display-hook ()
  (let* ((git-dir (get-text-property (point-min) 'git-dir)))
    (goto-char (point-min))
    (while (re-search-forward "^[^ \t\n]+" nil t)
      (let* ((ff (buffer-substring (point-at-bol) (point-at-eol)))
	     (dname (file-name-directory ff))
	     (fname (file-name-nondirectory ff))
	     (fullname (concat git-dir "/" ff))
	     (status "Untracked"))
	(replace-match
	 (concat "** "
		 fname
		 "\n:PROPERTIES:\n:GitStatus: " status
		 "\n:Directory: " (cond (dname) (t "."))
		 "\n:Filename-nondirectory: " (file-name-nondirectory fullname)
		 "\n:FILENAME: [[" fullname "]]\n:END:\n\n")
	 'fixed)))))

(defun superman-git-submodule-pre-display-hook ()
  (while (re-search-forward "^[ -][^ \t]+" nil t)
    (kill-region (point-at-bol) (1+ (point)))
    (re-search-forward "[^ \t]+" (point-at-eol) t)
    (kill-region (point) (point-at-eol))
    (end-of-line))
  (superman-git-files-pre-display-hook))


(defun superman-git-files-pre-display-hook ()
  (let* ((git-dir (get-text-property (point-min) 'git-dir))
	 (git-status
	  (shell-command-to-string
	   (concat "cd " git-dir ";" superman-cmd-git " status --porcelain -uno")))
	 (status-list
	  (mapcar (lambda (x)
		    (let ((index-status (substring-no-properties x 0 1))
			  (work-tree-status (substring-no-properties x 1 2))
			  (fname  (substring-no-properties x 3 (length x))))
		      (list fname index-status work-tree-status)))
		  (delete-if (lambda (x) (string= x ""))
			     (split-string git-status "\n"))))
	 (flist))
    (goto-char (point-min))
    (when (looking-at "git-output") (forward-line 1))
    (while (re-search-forward "^[^ \t\n]+" nil t)
      (let* ((match (match-string-no-properties 0))
	     (ff (buffer-substring (point-at-bol) (point-at-eol)))
	     (dname (file-name-directory ff))
	     (fname (file-name-nondirectory ff))
	     (fullname (concat git-dir "/" ff))
	     (status (assoc ff status-list)))
	(if (member fullname flist)
	    (delete-region (point-at-bol) (point-at-eol))
	  (setq flist (append (list fullname) flist))
	  (replace-match
	   (concat "** "
		   fname
		   "\n:PROPERTIES:\n:GitStatus: "
		   (cond ((not status) "Committed")
			 (t
			  (let* ((X (or (nth 1 status) " "))
				 (Y (or (nth 2 status) " "))
				 (XY (concat X Y)))
			    (superman-label-status XY))))
		   "\n:Directory: " (cond (dname) (t "."))
		   "\n:Filename-nondirectory: " (file-name-nondirectory fullname)
		   "\n:FILENAME: [[" fullname "]]\n:END:\n\n")
	   'fixed))))))

(defvar superman-git-diff-status-letters
  '(("A" "addition" "addition of a file")
    ("C" "copy" "copy of a file into a new one")
    ("D" "deletion" "deletion of a file")
    ("M" "modification" "modification of the contents or mode of a file")
    ("R" "renaming" "renaming of a file")
    ("T" "type-change" "change in the type of the file")
    ("U" "unmerged" "file is unmerged (you must complete the merge before it can be committed)")
    ("X" "unknown" "unknown change type (most probably a bug, please report it)"))
  "Explanation of status letters copied from man page, see M-x manual-entry RET git-diff RET.")

(defun superman-git-diff-pre-display-hook ()
  (let* ((git-dir (get-text-property (point-min) 'git-dir)))
    (goto-char (point-min))
    (while (re-search-forward "^\\([ACDMRTUX]\\)[ \t\n]+\\(.*\\)\n" nil t)
      (let* ((status (match-string-no-properties 1))
	     (ff (match-string-no-properties 2))
	     (dname (file-name-directory ff))
	     (fname (file-name-nondirectory ff))
	     (fullname (concat git-dir "/" ff)))
	(replace-match
	 (concat "** "
		 fname
		 "\n:PROPERTIES:\n:GitStatus: "
		 (cadr (assoc status superman-git-diff-status-letters))
		 "\n:Directory: " (cond (dname) (t "."))
		 "\n:Filename-nondirectory: " (file-name-nondirectory fullname)
		 "\n:FILENAME: [[" fullname "]]\n:END:\n\n")
	 'fixed)))))


(defun superman-git-log-pre-display-hook ()
  (goto-char (point-min))
  (end-of-line)
  (insert (concat "\n\n** Workspace\n:PROPERTIES:\n:Commit: Workspace\n:Author: "
		  (user-full-name)
		  "\n:Date:"
		  "\n:Message: current state on disk\n:END:\n\n")))

;;}}}
;;{{{ git display post-hooks

(defun superman-git-log-post-display-hook ()
  (let* ((tail (point))
	 (cat-point (superman-cat-point))
	 (column-point (previous-single-property-change cat-point 'column-names))
	 (dir (get-text-property (point-min) 'git-dir))
	 (file (get-text-property (point-min) 'filename))
	 (nickname (get-text-property (point-min) 'nickname))
	 next)
    (goto-char cat-point)
    (while (setq next (next-single-property-change (point-at-eol) 'superman-item-marker))
      (goto-char next)
      (let* ((commit (superman-get-property (get-text-property (point-at-bol) 'superman-item-marker) "commit"))
	     (ref (if (string= commit "Workspace") "HEAD" (concat commit "^")))
	     (cmd
	      (if file
		  `(lambda () (superman-git-log-open-commit-at-point ,file ,commit))
		`(lambda () (superman-git-display-diff ,commit ,ref ,dir ,file ,nickname)))))
	(put-text-property (point-at-bol) (1+ (point-at-bol)) 'superman-choice cmd)))))

(defvar superman-git-show-ignored nil
  "If non-nil show files that are otherwise ignored by git in list of untracked files.")
(make-variable-buffer-local 'superman-git-show-ignored)

(defun superman-git-toggle-show-ignored ()
  (interactive)
  (setq superman-git-show-ignored (not superman-git-show-ignored))
  (superman-redo))

(defun superman-git-untracked-post-display-hook ()
  (let ((current superman-git-show-ignored))
  (goto-char (superman-cat-point))
  (end-of-line)
  (insert "\n\n" (superman-make-button
	   (if current "Hide ignored" "Show ignored")
	   '(:fun superman-git-toggle-show-ignored
	   :face superman-capture-button-face
	   :help "Toggle show/hide ignored")))))

;;}}}
;;
;;{{{ filter
(defvar superman-git-filter nil
  "List of currently selected lines.")
(make-variable-buffer-local 'superman-git-filter)


(defun superman-git-set-filter ()
  "In git displays, match lines regexp against value of one of the columns
and keep only the matching lines. Works only if the column name of the
 display is equal to the name of the property."
  (interactive)
  (let* ((cstart (next-single-property-change (point-min) 'column-names))
	 (column-names (superman-list-to-alist
			(get-text-property cstart 'column-names)))
	 (filenamep (assoc "Filename" column-names))
	 (balls (get-text-property (superman-cat-point) 'balls))
	 (col
	  (completing-read
	   (concat "Filter on column"
		   (if filenamep " (default: Filename): " ": "))
	   column-names nil t nil nil
	   (when filenamep "Filename")))
	 ;; works only if the column name of the display is equal to the
	 ;; name of the property 
	 ;; search filename without the path
	 (prop (if (string= col "Filename") "Filename-nondirectory" col))
	 (regexp 
	  (read-string "Filter regexp: "))
	 (filter-name (concat col " matches " regexp))
	 catch
	 next
	 val
	 (buffer-read-only nil))
    (goto-char cstart)
    (while (setq next (next-single-property-change (point-at-eol) 'org-hd-marker))
      (goto-char next)
      ;; match regexp against value of column
      (if (and (setq
		val (superman-get-property
		     (get-text-property next 'org-hd-marker)
		     prop))
	       (string-match regexp val))
	  nil
	(beginning-of-line)
	(setq catch (append catch
			    (list
			     (buffer-substring (point) (point-at-eol)))))
	(delete-region (point-at-bol) (1+ (point-at-eol)))
	(forward-line -1)))
    ;; in case we remove the last line
    (unless (next-single-property-change (point-min) 'tail)
      (let ((end (previous-single-property-change (point-max) 'org-hd-marker)))
	(if end
	    (put-text-property end (1+ end) 'tail t)
	  (goto-char (next-single-property-change (point-min) 'column-names))
	  (put-text-property (point-at-eol) (- (point-at-eol) 1) 'tail t))))
    (if (not catch)
	(message "No lines are deleted by this filter.")
      (setq superman-git-filter
	    (append superman-git-filter
		    (list (cons filter-name catch))))
      (goto-char (superman-cat-point))
      (if (next-single-property-change (point) 'git-filter)
	  (progn 
	    (goto-char (next-single-property-change (point) 'git-filter))
	    (end-of-line))
	(end-of-line)
	(insert "\n\n" (superman-make-button
			"Filter:"
			'(:fun superman-git-set-filter
			       :face superman-header-button-face
			       :help "Set filter")))
	(put-text-property (point-at-bol) (+ (point-at-bol) 1) 'git-filter t)
	(end-of-line))
      (insert "\t" (superman-make-button
		    filter-name
		    `(:fun (lambda () (interactive)
			     (superman-git-remove-filter ,filter-name))
			   :face superman-next-project-button-face
			   :help "Press button to remove filter"))))))


(defun superman-git-remove-filter (filter-name)	
  (interactive)
  (let* ((filter (assoc filter-name superman-git-filter))
	 (filter-contents (cdr filter))
	 (buffer-read-only nil))
    ;; first remove the button
    (kill-region (previous-single-property-change (point) 'button)
		 (next-single-property-change (point) 'button))
    (goto-char (next-single-property-change (point-min) 'tail))
    (while filter-contents
      (insert "\n" (car filter-contents))
      (setq filter-contents (cdr filter-contents)))
    (setq superman-git-filter
	  (delete filter superman-git-filter))
    (goto-char (next-single-property-change
		(point-min)
		'git-filter))))
;; (if (re-search-forward filter-name nil t)
;; (progn
;; (replace-match "") (just-one-space 0))
;; (message filter-name))))

;;}}}
;;
;;{{{ superman-git-mode

(defvar superman-git-mode-map (make-sparse-keymap)
  "Keymap used for `superman-git-mode' commands.")
   
(define-minor-mode superman-git-mode
     "Toggle superman git mode.
With argument ARG turn superman-git-mode on if ARG is positive, otherwise
turn it off.
                   
Enabling superman-git mode enables the git keyboard to control single files."
     :lighter " *SG*"
     :group 'org
     :keymap 'superman-git-mode-map)

(defun superman-git-mode-on ()
  (interactive)
  (when superman-hl-line (hl-line-mode 1))
  (superman-git-mode t))

(define-key superman-git-mode-map "q" 'superman-view-back)
(define-key superman-git-mode-map "t" 'superman-git-tag)
(define-key superman-git-mode-map "c" 'superman-git-commit-file)
(define-key superman-git-mode-map "a" 'superman-git-add-file)
(define-key superman-git-mode-map "F" 'superman-git-pull)
(define-key superman-git-mode-map "P" 'superman-git-push)
(define-key superman-git-mode-map "s" 'superman-git-status-file)
(define-key superman-git-mode-map "x" 'superman-git-delete-file)
(define-key superman-git-mode-map "d" 'superman-git-diff-file)
(define-key superman-git-mode-map "r" 'superman-git-reset-file)
(define-key superman-git-mode-map "l" 'superman-git-log-file)
(define-key superman-git-mode-map "#" 'superman-git-ignore-file)
(define-key superman-git-mode-map " " 'superman-git-last-log-file) 
(define-key superman-git-mode-map "/" 'superman-git-set-filter)
;;}}}
;;{{{ superman-git-keyboard

(defun superman-make-git-keyboard (f &rest args)
  (if (string-match org-bracket-link-regexp f)
      (let ((diff (superman-make-button "d"
					'(:fun superman-git-diff-file
					       :face superman-git-keyboard-face-d
					       :help "git diff file")))
	    (log (superman-make-button "l"
				       '(:fun superman-git-log-file
					      :face superman-git-keyboard-face-l
					      :help "git log file")))
	    (add (superman-make-button "a"
				       '(:fun superman-git-add-file
					      :face superman-git-keyboard-face-a
					      :help "git add file")))
	    (commit (superman-make-button "c"
					  '(:fun superman-git-commit-file
						 :face superman-git-keyboard-face-c
						 :help "git commit file")))
	    (reset (superman-make-button "r"
					 '(:fun superman-git-reset-file
						:face superman-git-keyboard-face-r
						:help "git checkout (reset) file")))
	    (delete (superman-make-button "x"
					  '(:fun superman-git-delete-file
						 :face superman-git-keyboard-face-x
						 :help "git rm file")))
	    (ignore (superman-make-button "#"
	     				  '(:fun superman-git-ignore-file
						 :face superman-git-keyboard-face-i
						 :help "add to .gitignore")))
	    )
	(concat diff " " log  " " add  " " delete " " reset " " commit " " ignore " " " "))
    ;; for the column name
    (superman-trim-string f (car args))))

;;}}}
;;
;;{{{ git search and history

(defun superman-git-search (&optional arg)
  (interactive "p")
  (superman-git-search-file (or arg superman-git-search-limit)))


(defun superman-git-history (&optional arg)
  (interactive)
  (let* ((file (or (superman-filename-at-point t)
		   (get-text-property (point-min) 'git-dir)
		   (buffer-file-name)))
	 (dir (if (file-directory-p file) (file-name-as-directory file) (file-name-directory file)))
	 (curdir default-directory)
	 (bufn (concat "*history: " file "*")))
    (when dir
      (save-window-excursion
	(setq default-directory dir)
	(vc-git-print-log file bufn t nil (or arg superman-git-log-limit)))
      (setq default-directory curdir)
      (switch-to-buffer bufn)
      (vc-git-log-view-mode))))

;;}}}
;;{{{ git log-view

(defvar superman-git-log-mode-map (copy-keymap superman-view-mode-map)
  "Keymap used for `superman-git-log-mode' commands.")

(define-key superman-git-log-mode-map [return] 'superman-git-log-open-commit-at-point)
(define-key superman-git-log-mode-map "D" (lambda () (interactive) (superman-git-log-open-commit-at-point 1)))
(define-key superman-git-log-mode-map "t" 'superman-git-tag)
(define-key superman-git-log-mode-map "?" 'superman-git-show-help)
(define-key superman-git-log-mode-map "q" 'kill-this-buffer)
(define-key superman-git-log-mode-map "r" (lambda () (interactive) (org-agenda-redo) (superman-git-log-mode-on)))
(define-key superman-git-log-mode-map "!" 'superman-start-shell)
(define-key superman-git-log-mode-map [(down)] 'next-line)
(define-key superman-git-log-mode-map [(up)] 'previous-line)
(define-key superman-git-log-mode-map " " (lambda () (interactive) (funcall superman-help-fun (superman-git-comment-at-point))))

(define-minor-mode superman-git-log-mode 
  "Toggle org projectmanager document view mode.
                        With argument ARG turn superman-docview-mode on if ARG is positive, otherwise
                        turn it off.
                        
                        Enabling superman-view mode electrifies the column view for documents
                        for git and other actions like commit, history search and pretty log-view."
  :lighter " S-log"
  :group 'org
  :keymap 'superman-git-log-mode-map)

(defun superman-git-log-mode-on ()
  (interactive)
  (hl-line-mode 1)
  (superman-git-log-mode t))

(defun superman-git-log-format (hdr level category tags-list prop-list)
  (concat " " 
	  (let* ((cprops prop-list)
		 (pstring "")
		 (ntrim))
	    (while cprops
	      (let ((val (cdr (car cprops))))
		(cond ((string= (downcase (caar cprops)) "decoration")
		       (setq ntrim 22)
		       (if (string= val "not set") (setq val " ")))		      
		      ((string= (downcase (caar cprops)) "date")
		       (setq ntrim 10))
		      (t (setq ntrim 7)))
		;; (cond ((string= (downcase (caar cprops)) (down-case)"filename")
		;;        (setq val (file-name-nondirectory (org-link-display-format val)))))
		(setq pstring (concat pstring "  " (superman-trim-string val ntrim))))
	      (setq cprops (cdr cprops)))
	    pstring) "    " (superman-trim-string hdr 70)))


(setq superman-log-balls
      '(("Date" ("width" 10) ("face" font-lock-string-face))
	("Hash" ("width" 10) ("face" font-lock-comment-face))
	("Author" ("width" 20) ("face" font-lock-function-name-face))
	("Tag" ("width" 10) ("face" font-lock-comment-face))
	("Comment" ("fun" superman-dont-trim) ("face" font-lock-keyword-face))))



;; (defvar superman-git-display-cycles "versions"
;; "Keywords to match the elements in superman-git-display-command-list")
;; (make-variable-buffer-local 'superman-git-display-cycles)
;; (setq superman-git-display-cycles nil)
(setq superman-git-log-displays '("versions" "search" "backup" "stash"))

(defun superman-git-log (&optional file limit search-string decoration-only)
  "Similar to `superman-git-display' but here for a single file instead of
the git directory."
  (interactive)
  (catch 'no-file
    (let* ((file (cond (file)
		       ((condition-case nil (superman-filename-at-point) (error nil)))
		       ((get-text-property (point-min) 'filename))
		       ((buffer-file-name))
		       (t (throw 'no-file (message "No file-name at point and buffer not associated with a file.")))))
	   (git-dir (or (get-text-property (point-min) 'git-dir)
			(superman-git-root file)))
	   (rel-file (superman-relative-name file git-dir))
	   (index (or (get-text-property (point-min) 'index) rel-file))
	   (git-log-display-buf (concat "*git-log[" (file-name-nondirectory file) "]*"))
	   (log-buf  (concat "*Log[" (file-name-nondirectory file) "]*"))
	   log-strings)
      (unless (get-buffer git-log-display-buf)
	(get-buffer-create git-log-display-buf))
      (switch-to-buffer git-log-display-buf)
      (setq buffer-read-only t)
      (let ((buffer-read-only nil))
	(if (get-text-property (point-min) 'git-display)
	    nil ;; buffer already showing git-log
	  ;; insert header
	  (erase-buffer) ;; probably unnecessary
	  (goto-char (point-min))
	  (insert (superman-make-button
		   (concat "* Git-log: " (file-name-nondirectory file))
		   `(:fun superman-redo :face superman-project-button-face
			  :help (concat "Git-directory: " ,(superman-git-root file)
					"\nPress this button to reload\nPress q to go back"))))
	  (insert "  " (superman-make-button "Project view" '(:fun superman-view-back :face superman-next-project-button-face  :help "Back to project view."))
		  "  " (superman-make-button "Git overview" '(:fun superman-git-display :face superman-next-project-button-face :help "Control project's git repository."))
		  "  " (superman-make-button "annotate" '(:fun superman-git-annotate :face superman-next-project-button-face :help "Annotate."))
		  "  " (superman-make-button "File-list" '(:fun superman-view-file-list :face superman-next-project-button-face :help "View project's file-list.")))
	  (insert "\n\n")
	  (when git-dir
	    (superman-view-insert-git-branches git-dir)
	    (superman-view-insert-git-buttons)
	    (insert "\n"))
	  (insert "\n")
	  (put-text-property (point-min) (1+ (point-min)) 'redo-cmd '(superman-redo-git-display))
	  (put-text-property (point-min) (1+ (point-min)) 'region-start t)
	  (put-text-property (point-min) (1+ (point-min)) 'git-dir git-dir)
	  (put-text-property (point-min) (1+ (point-min)) 'git-display "versions")
	  ;; (put-text-property (point-min) (1+ (point-min)) 'face 'org-level-1)
	  (put-text-property (point-min) (1+ (point-min)) 'filename file)
	  (put-text-property (point-min) (1+ (point-min)) 'index index)
	  (put-text-property (point-min) (1+ (point-min)) 'limit limit)
	  (put-text-property (point-min) (1+ (point-min)) 'search-string search-string)
	  (put-text-property (point-min) (1+ (point-min)) 'decoration-only decoration-only)
	  (superman-view-mode-on)
	  (superman-git-mode-on)
	  (insert "** Git")
	  (put-text-property (point-at-bol) (1+ (point-at-bol)) 'cat 'git)
	  (superman-redo-cat))))))

(defun superman-git-log-1 (&optional file limit search-string decoration-only)
  (interactive)
  (let* ((file (or file
		   (condition-case nil (superman-filename-at-point) (error nil))
		   (get-text-property (point-min) 'filename)
		   (buffer-file-name)))
	 (limit (or limit superman-git-log-limit))
	 (gitsearch (if search-string (concat " -G\"" search-string "\"") ""))
	 (nick (get-text-property (point-min) 'nickname))
	 (git-switches (concat " --no-pager log --full-history --pretty=\"%h:#:%s:#:%ad:#:%an:#:%d\" --date=short "
			       gitsearch  " "
			       (if limit (concat "-n " (number-to-string limit)))))
	 (dir (or (get-text-property (point-min) 'git-dir) (superman-git-root file)))
	 (rel-file (superman-relative-name file dir))
	 (index (or (get-text-property (point-min) 'index) rel-file))
	 (cmd (concat "cd " dir "; " superman-cmd-git git-switches " -- " rel-file))
	 (gitlog (shell-command-to-string cmd))
	 (log-buf  (concat "*Log[" (file-name-nondirectory file) "]*"))
	 log-strings)
    (when (string= gitlog "")
      (error (concat "No search results in file history or file " rel-file " not (not yet) git controlled")))
    (switch-to-buffer log-buf)
    (setq buffer-read-only nil)
    (erase-buffer)
    (font-lock-mode -1)    
    (insert "* Git Log of " (file-name-nondirectory file))
    ;; (put-text-property (point-at-bol) (point-at-eol) 'face 'org-level-1)
    (put-text-property (point-at-bol) (point-at-eol) 'filename file)
    (put-text-property (point-at-bol) (point-at-eol) 'nickname nick)
    (put-text-property (point-at-bol) (point-at-eol) 'index index)
    (put-text-property (point-at-bol) (point-at-eol) 'git-dir dir)
    (put-text-property (point-at-bol) (point-at-eol) 'limit limit)
    (put-text-property (point-at-bol) (point-at-eol) 'search-string search-string)
    (put-text-property (point-at-bol) (point-at-eol) 'decoration-only decoration-only)
    (insert "  " (superman-make-button "Project view" '(:fun superman-view-back :face superman-next-project-button-face  :help "Back to project view."))
	    "  " (superman-make-button "Git overview" '(:fun superman-git-display :face superman-next-project-button-face :help "Control project's git repository."))
	    "  " (superman-make-button "annotate" '(:fun superman-git-annotate :face superman-next-project-button-face :help "Annotate."))
	    ;; "  " (superman-make-button "versions" '(:fun superman-git-log :face superman-next-project-button-face :help "Show git log."))
	    )
    (insert "\n\n")
    ;; column names
    (insert (superman-column-names superman-log-balls) "\n")
    (setq log-strings (split-string (substring gitlog 0 -1) "\n"))
    (while log-strings
      (let* ((log (split-string (car log-strings) ":#:"))
	     (deco (nth 4 log))
	     (item (superman-format-thing
		    (list (nth 0 log)
			  (list 
			   (cons "Comment" (nth 1 log))
			   (cons "Hash" (nth 0 log))
			   (cons "Tag" deco)
			   (cons "Date" (nth 2 log))
			   (cons "Author"  (nth 3 log))))
		    superman-log-balls)))
	(put-text-property 0 (length item) 'hash (nth 0 log) item)
	(if (or (not decoration-only) (not (string= deco "")))
	    (progn
	      (put-text-property 0 (length item) 'decoration (nth 4 log) item)
	      (insert item "\n")))
	(setq log-strings (cdr log-strings))))
    (superman-git-log-mode-on)
    (goto-char (point-min))
    ;; (setq truncate-lines t
    ;; truncate-partial-width-windows nil)
    (superman-next-entry)
    (setq buffer-read-only t)))



(defun superman-git-tag ()
  "Set git tag"
  (interactive)
  ;; tag commit in git log-view
  (let* ((marker (get-text-property (point-at-bol) 'org-hd-marker))
	 (oldtag (superman-get-property
		  marker
		  "tag"))
	 (dir (if (or superman-git-mode
		      superman-git-log-mode)
		  (get-text-property (point-min) 'git-dir)
		(get-text-property (point-min) 'filename)))
	 (hash (superman-get-property
		marker
		"commit"))
	 (tag (read-string "Tag (empty to clear): " oldtag))
	 (linenum (line-number-at-pos)))
    ;; run git tag
    (if (string-equal tag "")
	(when oldtag
	  (shell-command-to-string
	   (concat "cd " dir ";"
		   superman-cmd-git " tag -d "
		   (replace-regexp-in-string "\\(\(tag:[ ]*\\|\(\\|\)\\)" "" oldtag))))
      (superman-run-cmd (concat
			 "cd " dir ";"
			 superman-cmd-git " tag -a " tag " "
			 hash " -m \"\"")
			"*Superman-returns*"))
    (if (or superman-git-mode
	    superman-git-log-mode)
	(progn
	  (org-entry-put marker "Tag" tag)
	  (superman-view-redo-line))
      (superman-git-log file (get-text-property (point-min) 'limit)
			(get-text-property (point-min) 'search-string)
			(get-text-property (point-min) 'decoration-only))
      (goto-char (point-min))
      (forward-line (1- linenum)))))

(defun superman-git-log-open-commit-at-point (&optional file commit diff)
  "Shows version of the document at point "
  (interactive)
  ;; (superman-git-revision (get-text-property (point-at-bol) 'hash) diff))
  ;; (defun superman-git-revision (pom &optional diff)
  ;; "Shows version of the document at point "
  (catch 'work-space
    (let* ((file (or file (get-text-property (point-min) 'filename)
		     (superman-get-property (get-text-property (point-at-bol) 'superman-item-marker) "filename")))
	   (hash (or commit
		     (superman-get-property (get-text-property (point-at-bol) 'superman-item-marker) "commit")))
	   (ext (if (string= hash "Workspace")
		    (throw 'work-space (find-file-other-window file))
		  (file-name-extension file)))
	   (filehash
	    (concat
	     (file-name-sans-extension
	      (file-name-nondirectory file))
	     "_" hash (if ext (concat "." ext))))
	   (str (shell-command-to-string 
		 (concat "cd " (file-name-directory file)
			 ";" superman-cmd-git
			 " show " hash ":./"
			 (file-name-nondirectory file)))))
      (if diff (find-file file))
      (switch-to-buffer-other-window filehash) 
      (setq buffer-file-name filehash)
      (normal-mode) ;; Get default major-mode 
      (erase-buffer)  
      (insert str)
      (setq buffer-file-name nil)
      (goto-char (point-min))
      (if diff (ediff-buffers (file-name-nondirectory file) filehash)))))

;;}}}
;;{{{ git grep

(defun superman-git-grep (&optional arg)
  (interactive)
  (let ((dir (get-text-property (point-min) 'git-dir)))
    (when dir
;;      (if arg
      (compilation-start (concat "cd " dir "; git grep -n -e " (read-string "Grep: ") " -- *") 'grep-mode))))
;;	(vc-git-grep (read-string "Grep: "))
;;	(vc-git-grep (read-string "Grep: ") "*" dir)))))
;;}}}

(provide 'superman-git)
;;; superman-git.el ends here

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End: 

