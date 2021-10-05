;; superman-display-file-list.el --- List files in projects

;; Copyright (C) 2014-2016  Thomas Alexander Gerds

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

;;; Code:

(defun superman-view-file-list ()
  (interactive)
  (when superman-view-mode
    (superman-file-list (superman-view-current-project))))


(defun superman-file-list (project &optional ext)
  "List files in project's location that match extension EXT"
  (if (featurep 'superman-file-list)
      (let ((dir (superman-project-home project)))
	(cond ((file-list-select nil (or ext ".")
				 nil nil dir (concat "*FileList[" (car project) "]*") nil))
	      (t
	       (switch-to-buffer (concat "*FileList[" (car project) "]*"))
	       (let ((read-only nil))
		 (erase-buffer)
		 (insert "FILE-LIST: No files in project")))))
    (error "superman-file-list.el not loaded")))

(defun superman-file-capture-button (c &rest args)
  (superman-make-button "track"
			'(:fun superman-capture-file-at-point
			       :face superman-next-project-button-face
			       :help "Capture file for project control")))

(defvar superman-file-list-balls
  '(("FileName" ("fun" superman-trim-string) ("width" 88) ("face" superman-file-name-face))
    ("Path" ("fun" superman-dont-trim) ("face" superman-directory-name-face)))
  ;; ("Capture" ("fun" superman-file-capture-button) ("width" 7) ("face" "no-face") ("face" superman-next-project-button-face))
  "balls used to display file-lists.")


(defun superman-file-list-display-buffer-p (&optional dir)
  "Return non-nil if current buffer is a file-list display buffer. 
If optional argument DIR is a directory-file-name nil is returned also in 
file-list display buffers unless DIR matches the directories associated with
 the buffer."
  (and file-list-mode
       (or (not dir)
	   (string= (expand-file-name dir)
		    (expand-file-name (get-text-property (point-min) 'dir))))))

(defun superman-display-file-list (dir &optional list filter sort balls view-buffer no-project)
  "Format file-list displays."
  (let* ((project (if no-project nil
		    (ignore-errors (superman-view-current-project))))
	 (nick (if project (car project)
		 (or (get-text-property (point-min) 'nickname)
		     file-list-display-buffer)))
	 (pbuf (if file-list-mode
		   (get-text-property (point-min) 'project-buffer)
		 (buffer-name)))
	 (sort (or sort (get-text-property (point-min) 'sort-key)))
	 (dir (or dir (get-text-property (point-min) 'dir)
		  (when project
		    (expand-file-name (superman-project-home project)))
		  (error "Missing location")))
	 (view-buf (or view-buffer (get-buffer-create (concat "*FileList[ " nick "]*"))))
	 (list (cond (list)
		     ((eq list 'empty) 'empty)
		     (file-list-current-file-list)))
	 ;; when balls are specified force level 0
	 ;; and provide file-name and directory name
	 (level (if balls 0 file-list-display-level))
	 (balls (if (= level 0)
		    '(("FileName" ("fun" superman-trim-string) ("width" 88) ("face" superman-file-name-face))
		      ("Path" ("fun" superman-dont-trim) ("face" superman-directory-name-face)))
		  '(("FileName" ("fun" superman-dont-trim)))))
	 (new-buffer (not (superman-file-list-display-buffer-p dir))))
    (switch-to-buffer view-buf)
    (setq buffer-read-only t)
    (let ((buffer-read-only nil))
      (if (eq list 'empty)
	  (setq file-list-current-file-list nil)
	(setq file-list-current-file-list list))
      (run-hooks 'superman-file-list-pre-display-hook)
      (if new-buffer
	  (progn
	    (erase-buffer)
	    (org-mode)
	    (font-lock-mode -1)
	    (file-list-mode)
	    ;; FIXME: since modes kill local variables
	    ;; we set again
	    (if (eq list 'empty)
		(setq file-list-current-file-list nil)
	      (setq file-list-current-file-list list))
	    (goto-char (point-min))
	    ;; create header (but only in new buffers)
	    ;; prepare buffer if necessary
	    (goto-char (point-min))
	    (insert (superman-make-button (concat "* " (or project "Directory") " files:")
					  '(:fun file-list-reload
						 :face superman-project-button-face
						 :help "Refresh file-list")))
	    (unless no-project
	      (put-text-property (point-min) (1+ (point-min)) 'nickname nick)
	      (put-text-property (point-min) (1+ (point-min)) 'project-buffer pbuf)
	      (put-text-property (point-min) (1+ (point-min)) 'git-dir (superman-git-toplevel dir)))
	    (put-text-property (point-min) (1+ (point-min)) 'dir (expand-file-name dir))
	    (insert " " dir " ")
	    (unless no-project
	      (put-text-property (point-min) (1+ (point-min)) 'index (superman-get-index project))
	      (insert "  " (superman-make-button "Project view" '(:fun superman-view-back :face superman-next-project-button-face :help "Back to :width view."))
		      "  " (superman-make-button "Git" '(:fun superman-git-display :face superman-next-project-button-face :help "Control project's :width repository."))
		      "  " (superman-make-button "Todo" '(:fun superman-project-todo :face superman-next-project-button-face :help "View project's :width list."))
		      "  " (superman-make-button "Time-line" '(:fun superman-project-timeline :face superman-next-project-button-face :help "View project':width timeline."))))
	    (insert "\n\n"))
	;; end of header (new buffers)
	;; 
	;; clear file-list section in existing buffer
	(delete-region 
	 (previous-single-property-change (point-max) 'point-file-list-start)
	 (point-max))
	(goto-char (previous-single-property-change (point-max) 'point-file-list-start))
	(end-of-line)
	(insert "\n"))
      ;; tools
      (when new-buffer
	(file-list-tools-button)
	(insert "\n\n")
	(file-list-show-tools)
	(forward-line 2))
      ;; set filter at point-min
      (put-text-property (point-min) (1+ (point-min)) 'filter-list filter)
      ;; insert or update filter 
      (if (eq list 'empty)
	  (file-list-update-filter-line filter 0)
	(file-list-update-filter-line filter (length list)))
      (if new-buffer
	  (progn
	    (forward-line 1)
	    (insert "\n"))
	(goto-char (previous-single-property-change (point-max) 'point-file-list-start))
	(delete-region (point-at-bol) (1+ (point-at-eol))))
      ;; set file list section header
      (insert  (superman-make-button  "Update list"
				      '(:fun file-list-reload
					     :face superman-capture-button-face
					     :width 13
					     :help "Update files in mother directory and reload file-list")))
      ;; set sorted
      (if sort (insert " Sorted by " (nth 0 sort) " "
		       (if (nth 1 sort) 
			   (superman-make-button "(decending)"
						 `(:fun file-list-sort-ascending
							:face file-list-action-button-face
							:width 13
							:help ,(concat "Change order to ascending by " 
								       (nth 0 sort) "\n" 
								       file-list-sort-keys-help-string)))
			 (superman-make-button "(ascending)"
					       `(:fun file-list-sort-descending
						      :face file-list-action-button-face
						      :width 13
						      :help ,(concat "Change order to descending by " 
								     (nth 0 sort) "\n"
								     file-list-sort-keys-help-string)))))
	(insert " " (superman-make-button "(unsorted)"
					  '(:fun file-list-sort-descending
						 :face file-list-action-button-face
						 :width 13
						 :help (concat "Change to ascending file name order.\n" file-list-sort-keys-help-string)))))
      (insert " ")
      ;; display mode button
      (insert (superman-make-button (concat "[mode " (number-to-string level) "]")
				    '(:fun file-list-toggle-display-mode
					   :face file-list-action-button-face
					   :width 13
					   :help "Toggle display mode")))
      (insert " ")      
      ;; display attributes button      
      (insert (superman-make-button "Attributes"
				    '(:fun file-list-attributes
					   :face file-list-action-button-face
					   :width 13
					   :help "Toggle attributes mode")))
      (put-text-property (point-at-bol) (point-at-eol) 'point-file-list-start t)
      (end-of-line)
      (insert "\n")
      ;; find maximal length of file-name
      (if (eq list 'empty)
	  (insert "\nNo files match")
	(when (= level 0)
	  (let ((maxfile (apply 'max (mapcar #'(lambda (x) (length (car x))) list))))
	    (setcdr (assoc "width" (assoc "FileName" balls)) `(,maxfile))))
	;; insert the file-list 
	;; (when (= level 5)
	;; (setq list (file-list-attributes list t)))
	(dolist (el list)
	  (let* ((file (car el))
		 (path (cadr el))
		 (rest (caddr el))
		 file-path
		 line
		 appendix)
	    (put-text-property 0 (length file) 'face 'superman-file-name-face file)
	    (put-text-property 0 (length path) 'face 'superman-directory-name-face path)
	    (setq file-path (cond ((= level 0)
				   `(list (("FileName" . ,file) ("Path" . ,path))))
				  ((= level 1)
				   `(list (("FileName" . ,(concat path file)))))
				  ((= level 2)
				   `(list (("FileName" . ,(file-list-make-file-name~ el)))))
				  ((= level 3)
				   `(list (("FileName" . ,(concat "[[" path file "]]")))))
				  ((= level 4)
				   `(list (("FileName" . ,(concat "[[" (file-list-make-file-name~ el) "]]")))))))
	    (setq line (superman-format-thing file-path balls 'no-face))
	    (insert line)
	    ;; results of grep and ls 
	    (while rest
	      (let ((key (caar rest))
		    (val (cdar rest)))
		(put-text-property 0 (length key) 'face 'font-lock-warning-face key)
		(setq appendix
		      (concat appendix
			      "\n"
			      (let ((line
				     (concat (format "%13s" (caar rest))
					     " : " (cdar rest))))
				(put-text-property 0 (length line) 'appendix t line)
				line)))
		(setq rest (cdr rest))))
	    ;; each beginning line has the filename saved as text-property
	    (put-text-property (point-at-bol) (1+ (point-at-bol)) 'filename (file-list-make-file-name el))
	    (put-text-property (point-at-bol) (1+ (point-at-bol)) 'superman-item-marker t)
	    (when appendix
	      (insert appendix))
	    (insert "\n"))))
	;; clear appendix button
	(goto-char (next-single-property-change (point-min) 'point-file-list-start))
	(if (next-single-property-change (point-min) 'appendix)
	    (unless  (next-single-property-change (point-min) 'clear-appendix)
	      (goto-char (point-at-eol))
	      (insert " "(superman-make-button "Clear appendix" 
					       '(:fun file-list-clear-display
						      :face file-list-clear-button-face
						      :props '(clear-appendix t)
						      :help "Remove search results and file attributes from display.")))
	      (put-text-property  (point-at-bol) (point-at-eol) 'point-file-list-start t))
	  (when (next-single-property-change (point-min) 'clear-appendix)	
	    (delete-region (next-single-property-change (point-min) 'clear-appendix) (point-at-eol))
	    (put-text-property  (point-at-bol) (point-at-eol) 'point-file-list-start t)))
	(goto-char (1+ (or (previous-single-property-change (point-max) 'point-file-list-start) 0)))
	(run-hooks 'superman-file-list-pre-display-hook))))


;; tools

(defun file-list-tools-button ()
  (insert (superman-make-button
	   "Tools"
	   '(:fun file-list-show-tools
		  :face superman-capture-button-face
		  :help "Show file-list tools" :width 13)))
  (put-text-property (point-at-bol) (point-at-eol) 'tools t))

(defun file-list-hide-tools ()
  (interactive)
  (let ((buffer-read-only nil))
    (goto-char (next-single-property-change (point-min) 'tools))
    (delete-region (point-at-bol) (point-at-eol))
    (file-list-tools-button)))

(defun file-list-show-tools ()
  (interactive)
  (let ((buffer-read-only nil))
    (goto-char (next-single-property-change (point-min) 'tools))
    (delete-region (point-at-bol) (point-at-eol))
    (insert
     (superman-make-button "Tools"  '(:width 13 :fun file-list-hide-tools :face superman-capture-button-face :help "Hide tools"))
     " "
     (superman-make-button "grep" '(:fun file-list-grep :width 9 :face file-list-action-button-face :help "Run `grep -n' on all files"))     
     " "
     (superman-make-button "Rename" '(:fun file-list-rename :width 9 :face file-list-action-button-face :help "Replace a substring of all files by a new string.\nUseful to remove or replace white-space or other non-letters.\n\n\nTo rename only one file. Put cursor on file-name, then press 'r'."))
     " "
     (superman-make-button "Move" '(:fun file-list-move :width 9 :face file-list-action-button-face :help "Move all files to a different directory"))
     " "
     (superman-make-button "Copy" '(:fun file-list-copy :width 9 :face file-list-action-button-face :help "Copy all files to a different directory"))
     " "
     (superman-make-button "Del" '(:fun file-list-remove :width 9 :face file-list-action-button-face :help "Delete all files"))
     " "
     (superman-make-button "Find-Repl" '(:fun file-list-query-replace :width 9 :face file-list-action-button-face :help "Run interactive query replace through all files"))
     " "
     (superman-make-button "Shell" '(:fun file-list-shell-command :width 9 :face file-list-action-button-face :help "Run the same shell command on all files"))
     ;; (superman-make-button (concat "Format" (cond ((= level 0) "file | path")
     ;; ((= level 1) "/path/file")
     ;; ((= level 2) "~/path/file")
     ;; ((= level 3) "[[org-link]]")
     ;; ((= level 4) "[[relative org-link]]")))
     ;; '(:fun file-list-toggle-display-mode
     ;; :face superman-default-button-face
     ;; :help "Change format used to show file-names"))
     " "
     ;; (superman-make-button "Attributes" '(:fun file-list-attributes :face file-list-action-button-face :help "Show file attributes."))
     ;; " "
     ;; (superman-make-button "$ls -lh" '(:fun file-list-ls :face file-list-action-button-face :help "Remove search results and file attributes from display."))
     )
    (put-text-property (point-at-bol) (point-at-eol) 'tools t)
    (goto-char (point-at-bol))))


(defvar file-list-sort-keys-help-string
"Change order of file-list
                        Sort by name: S-f 
                        Sort by path: S-p
                        Sort by time: S-t
                        Sort by size: S-s
                        Sort by .ext: S-e
               
                Sort descending: Crtl-u followed by S-* key stroke")

(defun file-list-update-filter-line (filter n)
  "Insert list of filters according to FILTER, and show number of files N."
  (save-excursion
    (let ((f-point (next-single-property-change (point-min) 'filter-line)))
      ;; remove existing filter line
      (when f-point
	(goto-char f-point)
	(delete-region (point-at-bol) (1+ (point-at-eol))))
      (insert 
       (superman-make-button
	"Filters" 
	'(:fun file-list-remove-all-filters
	       :face superman-capture-button-face
	       :width 13
	       :help "Press this button to remove all filters.
         Filter file-list
                        by name: /-f 
                        by path: /-p
                        by time: /-t
                        by size: /-s
                        by .ext: /-e
               
         Inverse filter: Crtl-u followed by /-* key stroke")))
      (insert " Listed: (n=" (number-to-string n) ") ")
      (when filter
	(let ((count 0))
	  (dolist (x filter nil)
	    (insert " " (superman-make-button
			 (concat (plist-get x :name) " (n="
				 (number-to-string 
				  (length (plist-get x :filtered-files))) ")")
			 `(:fun (lambda () (interactive)
				  (file-list-remove-filter ,(plist-get x :name)))
				:face file-list-active-filter-button-face
				:help "Press button to remove this filter"
				:props (filter ,count))))
	    (setq count (1+ count)))))
      (insert " "
	      (superman-make-button "+" '(:fun file-list-filter
					       :face file-list-active-filter-button-face
					       :help "Press button to add a filter")))
      (put-text-property (point-at-bol) (point-at-eol) 'filter-line t)
      (insert "\n"))))



(defun file-list-button-sort-by-name ()
  (interactive)
  (let ((buffer-read-only nil)
	(reverse (get-text-property (previous-single-property-change (point) 'button) 'reverse))
	(here (point)))
    (when (get-text-property (point-at-bol) 'point-file-list-start)
      (put-text-property
       (previous-single-property-change (point) 'button)
       (next-single-property-change (point) 'button)
       'reverse (not reverse)))
    (file-list-sort-by-name reverse)
    (goto-char here)))

(defun file-list-button-sort-by-ext ()
  (interactive)
  (let ((buffer-read-only nil)
	(reverse (get-text-property (previous-single-property-change (point) 'button) 'reverse))
	(here (point)))
    (when (get-text-property (point-at-bol) 'point-file-list-start)
      (put-text-property
       (previous-single-property-change (point) 'button)
       (next-single-property-change (point) 'button)
       'reverse (not reverse)))
    (file-list-sort-by-ext reverse)
    (goto-char here)))

(defun file-list-button-sort-by-path ()
  (interactive)
  (let ((buffer-read-only nil)
	(reverse (get-text-property (previous-single-property-change (point) 'button) 'reverse))
	(here (point)))
    (when (get-text-property (point-at-bol) 'point-file-list-start)
      (put-text-property
       (previous-single-property-change (point) 'button)
       (next-single-property-change (point) 'button)
       'reverse (not reverse)))
    (file-list-sort-by-path reverse)
        (goto-char here)))

(defun file-list-button-sort-by-time ()
  (interactive)
  (let ((buffer-read-only nil)
	(reverse (get-text-property (previous-single-property-change (point) 'button) 'reverse))
	(here (point)))
    (when (get-text-property (point-at-bol) 'point-file-list-start)
      (put-text-property
       (previous-single-property-change (point) 'button)
       (next-single-property-change (point) 'button)
       'reverse (not reverse)))
    (file-list-sort-by-time reverse)
    (goto-char here)))

(defun file-list-button-sort-by-size ()
  (interactive)
  (let ((buffer-read-only nil)
	(reverse (get-text-property (previous-single-property-change (point) 'button) 'reverse))
	(here (point)))
	(when (get-text-property (point-at-bol) 'point-file-list-start)
	  (put-text-property
	   (previous-single-property-change (point) 'button)
	   (next-single-property-change (point) 'button)
	   'reverse (not reverse)))
    (file-list-sort-by-size reverse)
    (goto-char here)))

(defun superman-file-list-refresh-display (&optional file-list)
  (interactive)
  (if (not file-list-mode)
      (error "Works only in file-list-mode")
    (superman-display-file-list
     (get-text-property (point-min) 'dir)
     (or file-list file-list-current-file-list)
     (get-text-property (point-min) 'filter-list)
     (get-text-property (point-min) 'sort-key)
     (get-text-property (point-min) 'balls)
     (current-buffer)
     (not (get-text-property (point-min) 'project-buffer)))))

(defun file-list-reload ()
  "Update below the directory which is stored at point-min.
 Then remove non-existing files and redisplay file-list."
  (interactive)
  (let* ((dir (get-text-property (point-min) 'dir))
	 (active-filter-list (get-text-property (point-min) 'filter-list))
	 (sort (get-text-property (point-min) 'sort-key))
	 flist)
    (file-list-update-below-dir dir)
    ;; (main-filter (get-text-property (point-min) 'filter))
    (while active-filter-list
      (let* ((filter (car active-filter-list))
	     (regexp (plist-get filter :regexp))
	     (by (plist-get filter :by))
	     (inverse (plist-get filter :inverse)))
	(setq flist
	      (file-list-select flist regexp by inverse dir (current-buffer) t 'force)))
      (setq active-filter-list (cdr active-filter-list)))
    (when sort
      (setq flist (file-list-sort-internal flist (nth 0 sort) (nth 1 sort) t)))
    ;; (file-list-select-existing-files)
    (setq file-list-current-file-list flist)
    (superman-file-list-refresh-display)))

(defun file-list-clear-display ()
  (interactive)
  (when file-list-mode
    (dolist (entry file-list-current-file-list)
      (when (> (length (cdr entry)) 1)
	(setcdr entry (list (car (cdr entry))))))
    ;; (setq file-list-display-level 0)
    (superman-file-list-refresh-display)))

(defun file-list-remove-all-filters ()
  (interactive)
  (let* ((filter-list (get-text-property (point-min) 'filter-list))
	 (new-filter-list '((:name "file match '.'" :test "." :by "name" :regexp "." :inverse nil))))
    (while filter-list
      (setq file-list-current-file-list
	    (append file-list-current-file-list
		    (plist-get (car filter-list) :filtered-files))
	    filter-list (cdr filter-list)))
    (superman-display-file-list
     (get-text-property (point-min) 'dir)
     file-list-current-file-list
     new-filter-list
     (get-text-property (point-min) 'sort-key)
     (get-text-property (point-min) 'balls)
     (current-buffer)
     (not (get-text-property (point-min) 'project-buffer)))))
  
(defun file-list-remove-filter (filter)
  (let* ((old-filter-list (get-text-property (point-min) 'filter-list))
	 (this-filter (nth (get-text-property (point) 'filter) old-filter-list))
	 (new-filter-list (delete this-filter old-filter-list)))
    (unless new-filter-list
      (setq new-filter-list  '((:name "file match '.'" :test "." :by "name" :regexp "." :inverse nil))))
    (setq file-list-current-file-list
	  (append file-list-current-file-list
		  (plist-get this-filter :filtered-files)))
    (superman-display-file-list
     (get-text-property (point-min) 'dir)
     file-list-current-file-list
     new-filter-list
     (get-text-property (point-min) 'sort-key)
     (get-text-property (point-min) 'balls)
     (current-buffer)
     (not (get-text-property (point-min) 'project-buffer)))))

(defun file-list-remove-filter-1 ()
  (interactive)
  (file-list-remove-nth-filter 0))

(defun file-list-remove-filter-2 ()
  (interactive)
  (file-list-remove-nth-filter 1))

(defun file-list-remove-filter-3 ()
  (interactive)
  (file-list-remove-nth-filter 2))

(defun file-list-remove-filter-4 ()
  (interactive)
  (file-list-remove-nth-filter 3))

(defun file-list-remove-filter-5 ()
  (interactive)
  (file-list-remove-nth-filter 4))

(defun file-list-remove-filter-6 ()
  (interactive)
  (file-list-remove-nth-filter 5))

(defun file-list-remove-filter-7 ()
  (interactive)
  (file-list-remove-nth-filter 6))

(defun file-list-remove-nth-filter (n)
  (let* ((old-filter-list (get-text-property (point-min) 'filter-list))
	 (this-filter (nth n old-filter-list))
	 (new-filter-list (delete this-filter old-filter-list)))
    (unless new-filter-list
      (setq new-filter-list  '((:name "file match '.'" :test "." :by "name" :regexp "." :inverse nil))))
    (setq file-list-current-file-list
	  (append file-list-current-file-list
		  (plist-get this-filter :filtered-files)))
    (superman-display-file-list
     (get-text-property (point-min) 'dir)
     file-list-current-file-list
     new-filter-list
     (get-text-property (point-min) 'sort-key)
     (get-text-property (point-min) 'balls)
     (current-buffer)
     (not (get-text-property (point-min) 'project-buffer)))))

  
(defun file-list-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map file-list-mode-map)
  (setq major-mode 'file-list-mode)
  (setq mode-name "file-list")
  (make-local-variable 'file-list-current-file-list)
  (make-local-variable 'file-list-display-level)
  (make-local-variable 'file-list-mode)
  (make-local-variable 'file-list-match-history)
  (setq file-list-mode t)
  (run-hooks 'file-list-display-hook))


(defvar file-list-display-help-string
  (concat ""
	  ;; (substitute-command-keys "list of matching files. here are some commands and keys. M-x `describe-bindings' shows all key-bindings\n\n`\\<file-list-mode-map>\\[file-list-choose-file]' find file `\\<file-list-mode-map>\\[file-list-choose-file-other-window]' in other window;`\\<file-list-mode-map>\\[file-list-choose-magic]' open, i.e. find with external program\n`S f', `S p', `S t', `S s' sorting and `/f' `/p' `/s' `/t' sub-selection by name, path, size, time\n `g' grep; `x' shell command; `d' dired; `t' toggle display\n\n")))
	  (substitute-command-keys "finding files: RET, SPACE, M-RET; sorting list: S f, S p, S t, S s; selecting from list: /f /p /s /t (by name, path, size, time)\nother commands: g grep; x shell command; d dired; t toggle display ...\n\n")))


(defun file-list-current-display-buffer ()
  (if file-list-mode
      (buffer-name (current-buffer))
    nil))


(defun file-list-quit (&optional force)
  "Close file-list window and switch to another buffer."
  (interactive)
  (let ((pbuf (get-text-property (point-min) 'project-buffer)))
    (if pbuf
	(progn
	  (kill-buffer (current-buffer))
	  (switch-to-buffer pbuf)
	  (superman-redo))
      (if (or force (not (one-window-p)))
	  (delete-window))
      (switch-to-buffer (other-buffer)))))
;; (file-list-mode
;; (superman-view-back))
;; (t (error "Not in file-list"))))

(defvar file-list-mode nil "Variable to indicate file-list-mode")


(defun file-list-switch-to-file-list ()
  (interactive)
  (let (w)
    (when (setq w (get-buffer-window file-list-display-buffer t))
      (select-window w)
      (file-list-beginning-of-file-list))))
	
(defun file-list-toggle-display-mode ()
  "Toggles display between the 4 stages
 (0) file | path
 (1) /path/file fullname
 (2) ~/path/to/file home replaced by ~
 (3) [[/path/file]] orgmode link
"
  (interactive)
  (setq file-list-display-level
	(cond 
	 ;; ((= file-list-display-level 5) 4)
	 ((= file-list-display-level 0) 1)
	 ((= file-list-display-level 1) 2)
	 ((= file-list-display-level 2) 3)
	 ((= file-list-display-level 3) 4)
	 ((= file-list-display-level 4) 0)))
  (superman-file-list-refresh-display))


(defun file-list-beginning-of-file-name ()
  "Find the beginning of the file-name at point"
  (unless (and (looking-at "[^ \t\n]")
	       (save-excursion
		 (and
		  (progn (backward-char 1) (looking-at "[ \t\n]"))
		  (progn (backward-char 1) (not (looking-at "\\\\"))))))
    (let ((pos (previous-single-property-change (point) 'filename)))
      (when pos (goto-char (1+ pos))))))
    ;; (let ((found nil)
	  ;; (pmin (- (save-excursion
		     ;; (file-list-beginning-of-file-list)) 1)))
      ;; (if (and (= file-list-display-level 2)
	       ;; (save-excursion (beginning-of-line) (looking-at "[ \t\n]+")))
	  ;; (re-search-backward "^[^ \t\n]" pmin t)
	;; (skip-chars-backward " \t\n")
	;; (re-search-backward "[ \t\n]" pmin t)
	;; (while (and (not found) (not (bobp)))
	  ;; (backward-char 1)
	  ;; (if (looking-at "\\\\")
	      ;; (re-search-backward "[ \t\n]" pmin t)
	    ;; (forward-char 1)
	    ;; (setq found 'yes)))
	;; (skip-chars-forward "\t\n ")))))

(defun file-list-find-end-of-file-name ()
  "Find the end of file-name.
Works only if the point is at the beginning of a file-name.
Returns the point at the end of the file-name."
  (let (found)
    (skip-chars-forward "^ \t\n")
    (while (and (not found) (not (eobp)))
      (if (save-excursion
	    (backward-char 1)
	    (looking-at "\\\\"))
	  (progn
	    (skip-chars-forward " \t\n")
	    (skip-chars-forward "^ \t\n"))
	(setq found 'yes)))
    (point)))

(defun file-list-end-of-file-name ()
  "Goto end of file-name. Works only if the point is at the beginning of a file-name."
  (file-list-beginning-of-file-name)
  (goto-char (file-list-find-end-of-file-name)))


(defun file-list-file-at-point (&optional exists-p)
  "Return the absolute file-name at point and error if there is none."
  (let (fname)
    (cond (file-list-mode
	   (or (setq fname (get-text-property (point-at-bol) 'filename))
	       (let ((pos (previous-single-property-change (point)  'filename)))
		 (when pos
		   (setq fname (get-text-property (previous-single-property-change pos 'filename) 'filename))))))
	  (t (error (format "Works only in %s buffers." file-list-display-buffer))))
    (if (not fname) (error "No absolute filename at point!")
      (set-text-properties 0 (length fname) nil fname)
      (if (or (not exists-p)
	      (file-exists-p fname))
	  fname
	(error "file %s does not exist on disk" fname)))))
	

(defun file-list-end-of-file-list ()
  "Return the point of the end of displayed file-list."
  (interactive)
  (when file-list-mode
    ;; (set-buffer file-list-display-buffer)
    (goto-char (point-max))
    (goto-char (previous-single-property-change (point) 'filename))
    (end-of-line)
    (point)))

(defun file-list-beginning-of-file-list ()
  "Return the point of the beginning of displayed file-list."
  (interactive)
  (when file-list-mode
    (goto-char (previous-single-property-change (point-max) 'point-file-list-start))
    (forward-line 1)
    (beginning-of-line)
    (point)))

(defun file-list-next-file (arg)
  "Move to the next item in the file list.
With prefix argument arg, move arg items (negative arg means move backward)."
  (interactive "p")
  (let ((beg (point-min)) (end (point-max)))
    (while (and (> arg 0) (not (eobp)))
      ;; If in a completion, move to the end of it.
      (when (get-text-property (point) 'filename)
	(goto-char (next-single-property-change (point) 'filename nil end)))
      ;; Move to start of next one.
      (unless (get-text-property (point) 'filename)
	(goto-char (next-single-property-change (point) 'filename nil end)))
      (setq arg (1- arg)))
    (while (and (< arg 0) (not (bobp)))
      (let ((prop (get-text-property (1- (point)) 'filename)))
	;; If in a completion, move to the start of it.
	(when (and prop (eq prop (get-text-property (point) 'filename)))
	  (goto-char (previous-single-property-change
		      (point) 'filename nil beg)))
	;; Move to end of the previous completion.
	(unless (or (bobp) (get-text-property (1- (point)) 'filename))
	  (goto-char (previous-single-property-change
		      (point) 'filename nil beg)))
	;; Move to the start of that one.
	(goto-char (previous-single-property-change
		    (point) 'filename nil beg))
	(setq arg (1+ arg))))))


(defun file-list-previous-file (arg)
  "Move to the previous item in file-list.
With prefix argument arg, move arg items backward."
  (interactive "p")
  (file-list-next-file (- arg)))
  
(defun file-list-nth-in-list (entry file-list)
  "Return the position of entry in file-list."
  (let ((ename (if (stringp entry) entry (file-list-make-file-name entry))))
    (if (string= ename (file-list-make-file-name (car file-list)))
	0
      (let ((rest (cdr file-list))
	    (pos 1))
	(while 
	    (not
	     (cond ((string= (file-list-make-file-name (car rest)) ename)
		    pos)
		   (rest (setq rest (cdr rest)
			       pos (+ pos 1))
			 nil)
		   (t (setq pos nil)
		      t))))
	pos))))


(provide 'superman-display-file-list)
;;; superman-display-file-list.el ends here

