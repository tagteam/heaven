;;{{{ Header

;;; superman-file-list.el --- working with a list of filenames
;;
;; Copyright (C) 2002-2016, Thomas A. Gerds <tag@biostat.ku.dk>
;; Version: 1.1.5 (11 August 2016)
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;;}}}
;;{{{ Description:
;; The aim is to create a facility which allows to interactively
;; find and work with files, which does not require repeated and
;; time-consuming calls of e.g., the lunix-command `find'.
;;
;; One of the best features is that the file-list-alist is automatically
;; updated when a new file is added to one of the listed directories.
;; 
;; An alist with entries of the form (filename . /path/to/filename)
;; enables the following features
;;
;; select and display files in a buffer. then operate on the files
;; file-list: sorting (by time, name, or size), move, delete, copy, grep, dired, ...
;;
;; Examples: M-x file-list-by-name RET *\.tex$ RET
;;           M-x file-list-by-size-below-directory RET ~/Mail/ RET 1000000 RET
;;
;;}}}
;;{{{ Usage:
;;; ----------------------------------------------------------------
;; 
;; (require 'file-list)
;; (file-list-initialize)
;;
;; you may also like
;;
;; (file-list-default-keybindings)
;;
;; this binds commands to keys as follows:
;; 
;;C-x f		<< Prefix Command >>
;;C-x f L		file-list-by-name-below-directory
;;C-x f P		file-list-by-path-below-directory
;;C-x f U		file-list-update-below-dir
;;C-x f d		file-list-iswitchf-below-directory
;;C-x f f		file-list-iswitchf-file
;;C-x f l		file-list-by-name
;;C-x f m		file-list-iswitchf-magic
;;C-x f p		file-list-by-path
;;C-x f s		file-list-by-size
;;C-x f t		file-list-by-time
;;C-x f u		file-list-update
;;C-x f 4		<< Prefix Command >>
;;C-x f 5		<< Prefix Command >>
;;C-x f 4 d	file-list-iswitchf-below-directory-other-window
;;C-x f 4 f	file-list-iswitchf-file-other-window
;;C-x f 5 d	file-list-iswitchf-below-directory-other-frame
;;C-x f 5 f	file-list-iswitchf-file-other-frame
;;}}}
;;{{{ TODO
;;
;; connect file-list-find-magic to system wide mime type handling
;; file-list-grep: restrict file-list to files with (or without) hits
;; handling of tar files
;;; BUGS
;;; there are many. please let me know by email: tag@biostat.ku.dk
;;
;;}}}
;;{{{ autoload
(require 'superman-display-file-list)
;;}}}
;;{{{ variables

(defgroup file-list nil
  "Listing of filenames below directories."
  :group 'extensions
  :link '(emacs-commentary-link :tag "Commentary" "file-list.el")
  :link '(emacs-library-link :tag "Lisp file" "file-list.el"))


(defcustom file-list-max-length 200000
  "Maximal number of file-names that can be stored in a single entry of file-list-alist."
  :type 'integer
  :group 'file-list)

(defcustom file-list-home-directory
  (file-name-as-directory (getenv "HOME"))
  "Default directory for all file-list-iswitchf and file-list-list commands."
  :type '(repeat regexp)
  :group 'file-list)

(defcustom file-list-default-directories
  (list file-list-home-directory)
  "List of directories that are initialized by the command file-list-initialize."
  :type '(repeat regexp)
  :group 'file-list)

(defcustom file-list-directory-filter (list "^[[:punct:]]")
  "List of regular expressions to match sub-directories
 which should be filtered by default in file listings."
  :type 'list
  :group 'file-list)

(defvar file-list-list-files-regexp "^.*[^\.]+$" 
  "Regular expression used to match file names for file listings.")

(defcustom file-list-file-name-filter (list "~$")
  "Regexp matching file-names that should be filtered by default."
  :type 'list
  :group 'file-list)

;; (defcustom file-list-exclude-dir-regexp nil
  ;; "Alist where each car is a regexp to be matched against directory-names
 ;; and the cdr a regexp that is matched against the names of all subdirectories
 ;; which should be omitted when listing files. For example
 ;; (setq file-list-exclude-dir-regexp
      ;; (list
       ;; (cons file-list-home-directory
	     ;; \"\\(^\\\.[a-w]\\|^auto$\\)\")))
 ;; excludes all directories below file-list-home-directory that start with `.' and
 ;; also those that are called `auto'. 
;; "
 ;; :type 'alist
 ;; :group 'file-list)

(defcustom file-list-iswitchf-prompt "iswitchf "
  "Prompt-string for file-list-iswitchf."
  :type 'string
  :group 'file-list)

(defcustom file-list-format-time-string "%a %d %b %Y, %T %Z"
  "Format of time string. See format-time-string for details."
  :type 'string
  :group 'file-list)



(defcustom file-list-clear-home-regexp
  "\\(\\\.~[0-9]?[0-9]?~$\\|\\\.aux\\)"
  "Regexp that matches files that should be deleted (frequently)."
  :type '(repeat regexp)
  :group 'file-list)

(defcustom file-list-follow-links nil
  "If non-nil follow linked directories when building or updating file-lists."
  :type 'boolean
  :group 'file-list)

(defcustom file-list-verbose nil
  "If non-nil echo names of expanded directories when building/updating of file-lists."
  :type 'boolean
  :group 'file-list)


(defcustom file-list-update t
  "If non-nil update file-list-alist just before file-list-iswitchf or file-list-list commands."
  :type 'boolean
  :group 'file-list)

;;  variables 
;;  ------------------------------------------------------------------

(defvar file-list-alist nil
  "Alist where the car of each entry is a directory name and the
cdr is the corresponding file-(a)list for that directory.")

(defvar file-list-current-file-list nil
  "List of currently selected file-names.")

(make-variable-buffer-local 'file-list-current-file-list)

(defvar file-list-excluded-directories nil
  "List of directories for which files are excluded.")

(defvar file-list-dir-list nil
  "Alist of directories for which files are listed.
The cdr of each entry is the modification time.")

(defvar file-list-display-buffer "*File-list*"
  "Buffer for displaying matching files.")

(defvar file-list-dirlist-buffer "*Dir-list*")

(defvar file-list-reference-buffer nil)

(defvar file-list-file-name-regexp
  "\\([\n\t ]/[^\t\n ]+/\\)\\([^/\t\n ]+\\)")

(defvar file-list-file-info-regexp
  "^ +\\(.*\\) : \\(.*\\)$")

(defvar file-list-iswitchf-history nil
  "History for file-list-iswitchf commands.")

(defvar file-list-regexp-history nil
  "History for regexp used by file-list commands.")

(defvar file-list-grep-history nil
  "History for regexp used by file-list command grep.")

(defvar file-list-match-history nil
  "History for matching file-list.")

(defvar file-list-display-level 1
  "In file-list-display-buffer:\n\n0 - display non-absolute file names (i.e. without path)\n
1 - display absolute file names\n
2 - display absolute file names and extra information for file (ie the result file-list-grep or file-list-attributes)")

(defvar file-list-display-hook nil
  "Normal hook run at the end of setting up text in file-list-display-buffer.
Good for binding keys.")

(defvar file-list-magic-alist
  '((".dvi"      . "xdvi")
    (".eps"      . "evince")
    (".prn"      . "evince")
    (".doc"      . "oowriter")
    (".xls"      . "oocalc")
    (".gif"      . "geeqie")
    (".icon"     . "geeqie")
    (".ief"      . "geeqie")
    (".jpe"      . "geeqie")
    (".jpg"      . "geeqie")
    (".pdf"      . "evince")
    (".ps"       . "evince")
    (".eps"       . "evince")
    (".psF"      . "evince")
    (".tif"      . "geeqie")
    (".tiff"     . "geeqie")
    (".xbm"      . "geeqie")
    (".xpm"      . "geeqie")
    (".jpg"      . "geeqie")
    (".jpeg"     . "geeqie"))
  "*An assoc list of file extensions and the program called by the choose magic function.")

(defvar file-list-mode-map
  (let ((map (make-sparse-keymap 'file-list-mode-map)))
    map)
  "Default keymap to use when choosing action for file
list in completion buffer.")

;;}}}
;;{{{ functions that create and modify the file-list-alist 

(defun file-list-replace-in-string (str regexp newtext &optional literal fixedcase)
  (if (featurep 'xemacs)
      (replace-in-string str regexp newtext)
    (replace-regexp-in-string regexp newtext str fixedcase literal)))

(defun file-list-list-internal (dir &optional dont-exclude recursive exclude-handler)
  "Lists filenames below DIR and subdirectories of DIR.
The value is an alist of file-names where each entry has the form (filename . path-to-filename).

The directory DIR and its subdirectories with their last modification time are added 
to the variable file-list-dir-list in order to be able later to test if updating is necessary.

If DONT-EXCLUDE is nil, then the function EXCLUDE-HANDLER is called to decide
if the directory should be listed. If EXCLUDE-HANDLER is nil
then the build-in function file-list-exclude-p is used. If RECURSIVE is nil, then
subdirectories of DIR are not listed. If MATCH is non-nil, then only those subdirectories
of DIR are listed that match the regular expression MATCH."
  (let ((dir-list (directory-files dir t  "^.*[^\.]+$" t)) ;; don't match ../ and ./
	file-list
	(exclude-handler (or exclude-handler 'file-list-exclude-p)) 
	(oldentry (assoc dir file-list-dir-list)))
    ;; update the file-list-dir-list
    (if oldentry
	(plist-put (cdr oldentry) :time (nth 5 (file-attributes dir)))
      (add-to-list 'file-list-dir-list
		   (cons (file-name-as-directory dir)
			 (list :time (nth 5 (file-attributes dir))))))
    ;;
    (while dir-list
      (let ((entry (car dir-list)))
	(if (file-directory-p entry)
	    (if (and (not dont-exclude)
		     (funcall exclude-handler (file-name-nondirectory entry) dir))
		(plist-put (cdr (assoc entry file-list-dir-list)) :filtered entry)
	      ;; (add-to-list
	      ;; 'file-list-excluded-directories
	      ;; (cons entry "matched file-list-directory-filter"))
	      (let ((exists
		     (assoc
		      (setq
		       entry
		       (file-name-as-directory entry))
		      file-list-dir-list)))
		(if file-list-verbose
		    (message "file-list: expanding %s (abort: C-g)" entry))
		(cond
		 ;; directory not accessible
		 ((not (file-accessible-directory-p entry))
		  (add-to-list
		   'file-list-excluded-directories
		   (cons entry "not accessible"))
		  nil)
		 ;; directory not readable
		 ((not (file-readable-p entry))
		  (add-to-list
		   'file-list-excluded-directories
		   (cons entry "not readable"))
		  nil)
		 ;; directory name is a link
		 ((and (file-symlink-p entry)
		       (not file-list-follow-links))
		  (add-to-list 'file-list-excluded-directories
			       (cons entry
				     (format "symlink to %s"
					     (file-symlink-p entry))))
		  nil)
		 ;;
		 ((and (not recursive) exists) nil)
		 ;; 
		 (t
		  (setq file-list
			(append
			 file-list (file-list-list-internal
				    entry dont-exclude recursive)))
					; 		    (message "length dir %i" (length file-list))
					; 		    (sit-for 1)
		  (when (> (length file-list) file-list-max-length)
		    (error (format "file-list below %s reached maximum length %i (user-option file-list-max-length) " dir file-list-max-length)))
		  ))))
	  ;; 
					;	  (when (not (string-match file-list-exclude-files fname))
	  (setq file-list
		(append file-list
			(list
			 (list (file-name-nondirectory entry)
			       (file-name-directory entry))))))
	(setq dir-list (cdr dir-list))))
					;    (message "%s files listed below '%s'" (length file-list) dir)
    file-list))




(defun file-list-list (dir &optional update dont-exclude recursive remove exclude-handler)
  "Builds and updates the variable file-list-alist entry for directory DIR.

If UPDATE is nil, then existing list is returned. If DONT-EXCLUDE is nil then
subdirectories below DIR are excluded depending on what file-list-exclude-p returns."
  ;; check if dir is assessable 
  (when (not (and (file-readable-p dir)
		  (file-accessible-directory-p dir)))
    (error "Directory %s is either not accessible or not readable" dir))
  ;; initialize the file-list-alist 
  (when  (not file-list-alist)
    (setq file-list-alist
	  (append
	   file-list-alist
	   (list
	    (cons
	     file-list-home-directory nil))))
    ;; (when (y-or-n-p (concat "Read files below" file-list-home-directory "?"))
    ;; (file-list-initialize))
    )
  (let* (
	 (dir (file-name-as-directory (expand-file-name dir)))
	 (dir-list (file-list-assoc-dir dir file-list-alist))
	 (dir-headp (if dir-list
			(string= dir (file-name-as-directory (car dir-list)))))
	 file-list)
    (cond ((not dir-list)
	   (when file-list-verbose
	     (message "file-list: reading %s ... (abort: C-g)" dir))
	   (setq file-list-alist
		 (append
		  file-list-alist
		  (list
		   (cons
		    dir
		    (setq file-list
			  (file-list-list-internal
			   dir dont-exclude 'recursive exclude-handler)))))))
	  ((and dir-headp recursive)
	   (if update
	       (progn
		 (when file-list-verbose
		   (message "file-list: updating %s ..." dir))
		 (setcdr dir-list
			 (setq file-list
			       (file-list-list-internal
				dir dont-exclude recursive exclude-handler))))
	     (setq file-list (cdr dir-list))))
	  ((and update recursive)
	   (when file-list-verbose
	     (message "file-list: updating %s ..." dir))
	   (setcdr dir-list
		   (append
		    (delq nil
			  (mapcar
			   (lambda (entry)
			     (if (not (string-match dir (cadr entry)))
				 ;;FIXME (string= dir (substring (cadr entry) 0 (length dir)))
				 entry nil))
			   (cdr dir-list)))
		    (setq file-list (file-list-list-internal dir dont-exclude recursive exclude-handler)))))
	  (update
	   (when file-list-verbose
	     (message "file-list: updating %s ..." dir))
	   ;; not recursive!!
	   (setcdr dir-list
		   (append
		    (delq nil
			  (mapcar
			   (lambda (entry)
			     (if (not (string= dir (cadr entry)))
				 entry nil))
			   (cdr dir-list)))
		    (setq file-list (file-list-list-internal dir dont-exclude nil exclude-handler)))))
	  ((not
	    (setq file-list
		  (delq nil
			(mapcar
			 (lambda (entry)
			   (if (string-match dir (cadr entry))
			       entry nil))
			 (cdr dir-list)))))
	   (when file-list-verbose
	     (message "file-list: reading %s ... (abort: C-g)" dir)	   )
	   (setcdr dir-list
		   (append (cdr dir-list)
			   (setq file-list (file-list-list-internal dir dont-exclude 'recursive exclude-handler))))))
    (when file-list-verbose
      (message "%s files %s below '%s'" (length file-list) "listed" dir))
    file-list))


;; (defun file-list-initialize ()
  ;; "Initialize the file-list-home-directory entry to file-list-alist,
;; by listings all the entries of file-list-default-directories."
  ;; (interactive)
  ;; ;; first initialize an empty file-list-alist, then
  ;; ;; list the subdirs specified by file-list-default-directories.
  ;; (setq file-list-alist (list (cons file-list-home-directory nil)))
  ;; ;; initialize all files sitting in the home directory
  ;; (file-list-list file-list-home-directory 'yes nil nil nil nil)
  ;; ;; adding the home directory to the dir-list
  ;; (add-to-list 'file-list-dir-list
	       ;; (cons (file-name-as-directory file-list-home-directory)
		     ;; (nth 5 (file-attributes file-list-home-directory))))
  ;; (let ((initDirs file-list-default-directories)
	;; subDir)
    ;; (while initDirs
      ;; (setq subDir (car initDirs))
      ;; (file-list-update-below-dir subDir)
;; ;       (file-list-list subDir 'update nil 'recursive nil)
      ;; (setq initDirs (cdr initDirs)))))

(defun file-list-remove-dir (entry)
  "Argument ENTRY is an element of file-list-dir-list.
Remove all filenames below dir (car entry) from file-list-alist and ENTRY from file-list-dir-list."
  (let* ((dir (car entry))
	 (dir-list (file-list-assoc-dir dir file-list-alist)))
    ;; remove dir from file-list-dir-list
    (setq file-list-dir-list (delq entry file-list-dir-list))
    ;; remove files from file-list-alist
    (setcdr dir-list (delq nil
			   (mapcar
			    (lambda (entry)
			      (if (not (string= dir (cadr entry)))
				  entry nil))
 			    (cdr dir-list))))))

(defun file-list-update (&optional dir force dont-exclude recursive)
  "Re-read filenames below dir from disk if changed on disk."
  (interactive "i\nP")
  (let* ((dir (if dir (file-name-as-directory
		       (expand-file-name dir))
		file-list-home-directory))
	 (dirinlist (assoc dir file-list-dir-list))
	 (dlist (delq nil (mapcar
			   (lambda (x)
			     (if (string-match dir (car x)) x nil))
			   file-list-dir-list)))
	 update-info)
    (if (or (not dirinlist) force)
	(file-list-list dir t dont-exclude recursive nil)
      (while dlist
	(let* ((dircons (car dlist))
	       (fis (file-exists-p (car dircons))))
	  (if (not fis)
	      ;; remove directory from list
	      (progn
		(file-list-remove-dir dircons)
		(setq update-info (append (list (concat (car dircons) " *removed* ")))))
	    ;; check modification time of dir 
	    (when (file-list-time-less-p (plist-get (cdr dircons) :time) (nth 5 (file-attributes (car dircons))))
	      ;; update
	      (file-list-list (car dircons) t nil nil)
	      (setq update-info (append (list (car dircons)) update-info))))
	  (setq dlist (cdr dlist))))
      (if (> (length update-info) 0)
	  (if (= (length update-info) 1)
	      (when file-list-verbose
		(message "%s updated" (car update-info)))
	    (save-window-excursion
	      (pop-to-buffer (get-buffer-create "*file-list-update*"))
	      (erase-buffer)
	      (insert "Updated directories: \n\n")
	      (mapcar (lambda (x) (insert x "\n")) update-info)))
	(when file-list-verbose
	  (message "No directory below %s has changed" dir))))))

(defun file-list-update-below-dir (dir)
  "Re-read filenames below dir from disk."
  (interactive "DUpdate filenames below dir ")
  (file-list-update dir t))


(defun file-list-update-file-alist (&optional file delete)
  (let* ((file-name (expand-file-name
		     (or file
			 (buffer-file-name (current-buffer)))))
	 (file-cons (file-list-make-entry file-name))
	 (file-dir (cadr file-cons)))
    (dolist (dir-list file-list-alist)
      (when (string-match (car dir-list) file-dir)
	(if delete (delete file-cons dir-list)
	  (when (not (member file-cons dir-list))
	    (setcdr dir-list (cons file-cons (cdr dir-list)))))))))

;;(add-hook 'after-save-hook 'file-list-update)	

(defun file-list-update-current-file-list (oldname newname)
  (let ((oldentry (cl-find-if
		   (lambda (item) 
		     (string= oldname (file-list-make-file-name item))) 
		   file-list-current-file-list))
	(newentry (file-list-make-entry newname)))
    (setcar oldentry (car newentry))
    (setcdr oldentry (cdr newentry))))


;;}}}
;;{{{ internal functions that alter the file-list-alist

(defun file-list-assoc-dir (dir list)
  (cl-assoc-if '(lambda (entry)
	       (or (string-match entry dir)
		   (string-match (file-list-replace-in-string
				  (file-list-replace-in-string entry "\\[" "\\[")
				  "\\]" "\\]") dir)))
	    list))

(defun file-list-exclude-p (subdir dir &optional regexp)
  "Decide if the directory SUBDIR below DIR should be excluded
 when listing files. If REGEXP is given then the SUBDIR is excluded if 
 REGEXP matches the part of the path between DIR and SUBDIR."
  (let ((filter file-list-directory-filter)
	hit)
    (while (and filter (not hit))
      (if (string-match (car filter) subdir) 
	  (setq hit (car filter))
	(setq filter (cdr filter))))
    hit))

 ;; (let* ((regexp
 ;; (or regexp
;; (cdr (assoc-if (lambda (entry)
  ;; (string-match entry dir))
  ;; file-list-exclude-dirs))))
  ;; (subDir (file-name-as-directory
  ;; (expand-file-name subdir)))
  ;; decision)
  ;; (when regexp
  ;; (setq decision (string-match regexp subDir)))
  ;; (if include (not decision) decision)))
    

(defun file-list-extract-sublist (file-list regexp-or-test &optional dont-match)
  ;; if regexp-or-test is not a string it must be a function taking one argument
  ;; which is then applied to each entry in file-list.
  ;; default is string-match regexp against filename
  (let ((test (if (stringp regexp-or-test)
		  '(lambda (entry)
		     (let ((match-p (string-match regexp-or-test (car entry))))
		       (if dont-match
			   (when (not match-p) entry)
			 (when match-p entry))))
		regexp-or-test)))
    (delete nil (mapcar test file-list))))

(defun file-list-filter-file-list (file-list filter &optional dont-match)
  "Split FILE-LIST according to FILTER into matching files and non-matching files. Return a plist
with elements :yin and :yang. :yin is the list of matching files and :yang the filter which also is
 a plist now enlarged with a new property :filtered-files, the list of not matching files.

If DONT-MATCH is non-nil do the inverse operation, i.e., split into non-matching and matching files."
  (let* (yin
	 yang
	 fl-result
	 (regexp-or-test (plist-get filter :test))
	 (test (if (stringp regexp-or-test)
		   (if (string= regexp-or-test ".")
		       nil
		     '(lambda (entry)
			(string-match regexp-or-test (car entry))))
		 regexp-or-test))
	 (n (length file-list))
	 (i 0))
    (if test
	(while (< i n)
	  (let* ((entry (nth i file-list))
		 (in (funcall test entry)))
	    (if in
		(setq yin (append `(,entry) yin))
	      (setq yang (append `(,entry) yang))))
	  (setq i (1+ i)))
      (setq yin file-list yang nil))
    (cond (dont-match
	   (setq fl-result yang)
	   (plist-put filter :filtered-files yin))
	  (t
	   (setq fl-result yin)
	   (plist-put filter :filtered-files yang)))
    (unless fl-result 
      (message
       (concat "No files are matched by filter:" (plist-get filter :name))))
    (list :yin fl-result :yang filter)))

;;mapcar this function on a file-list ...
(defun file-list-make-file-name (entry)
  "Concats path-name and file-name of entry."
  (concat (cadr entry) (car entry)))


(defun file-list-make-file-name~ (entry)
  "Concats path-name and file-name of entry."
  (replace-regexp-in-string (getenv "HOME") "~"
			    (concat
			     (cadr entry) (car entry))))


(defun file-list-make-entry (filename)
  "Returns a cons where the car equals the nondirectory part of filename
and the cdr is the directory of filename."
  (list (file-name-nondirectory filename)
	(file-name-directory filename)))


(defun file-list-concat-file-names (file-list)
  "Returns a string consisting of all absolut filenames in file-list separated by blanks."
  (let (file-names-as-string)
    (dolist (file file-list file-names-as-string)
      (setq file-names-as-string
	    (concat file-names-as-string
		    "\'" (file-list-make-file-name file) "\' ")))
    file-names-as-string))

;;}}}
;;{{{ file-list-list

;; pre selection and sorting of files for display in
;; the file-list-display-buffer

(defun file-list-select-existing-files (&optional file-list)
  "Check existence (on disk) of all files in file-list-current-file-list.
Return  the sublist of the existing files. Does not re-display selected list."
  (interactive)
  (let (
	(file-list (or file-list file-list-current-file-list)))
    (setq file-list-current-file-list
	  (delete nil (mapcar (lambda (entry)
				(when (file-exists-p
				       (file-list-make-file-name entry))
				  entry))
			      file-list))))
  file-list-current-file-list)


(defun file-list-select (file-list regexp by inverse dir display-buffer dont-display &optional force-update)
  "Returns sublist of filenames in FILE-LIST matched by REGEXP according to BY. BY is either
of file-name, path, ext, time, or size. If INVERSE is
non-nil return the sublist which does not match.

DISPLAY-BUFFER should be the buffer in which the result should appear. 
This happens unless DONT-DISPLAY is non nil. 
If FILE-LIST is nil, the buffer local value
of the variable `file-list-current-file-list' will be used as input, unless FORCE-UPDATE is non nil, in which
case the file list is updated according to the current directory and filters.

This function sets the variable `file-list-current-file-list'. 
See also `file-list-add'.
"
  (setq file-list-reference-buffer (current-buffer))
  (let* ((fl-buffer-p (superman-file-list-display-buffer-p dir))
	 (update (or force-update
		     (file-list-time-less-p 
		      (plist-get (cdr (assoc dir file-list-dir-list)) :time)
		      (nth 5 (file-attributes dir)))))
	 (display-buffer (or display-buffer
			     (if fl-buffer-p
				 (current-buffer)
			       file-list-display-buffer)))
	 (file-list (cond (file-list)
			  ((and fl-buffer-p (not force-update))
			   ;; same directory as shown in current buffer
			   file-list-current-file-list)
			  (dir
			   (when file-list-update
			     (file-list-update dir nil))
			   (file-list-list dir nil nil 'recursive nil))
			  (file-list-mode file-list-current-file-list)
			  (t (if file-list-update
				 (file-list-update dir nil))
			     ;;FIXME: write function file-list-update-file-list-alist
			     (file-list-list
			      file-list-home-directory nil nil 'recursive nil))))
	 (prompt-string (format "Select files whose %s %s: "
				(cond ((not by) "filename")
				      ((string= by "path") "pathname")
				      ((string= by "ext") "extension")
				      ((string= by "time") "age in days")
				      ((string= by "size") "size in bytes")
				      (t "filename"))
				(cond ((not by) (format "is%s matched by regexp" (if inverse " *not*" "")))
				      ((string= by "time") (format "exceeds%s " (if (not inverse) " *not*" "")))
				      ((string= by "size") (format "exceeds%s " (if inverse " *not*" "")))
				      (t (format "is%s matched by regexp" (if inverse " *not*" ""))))))
	 (regexp (cond (regexp)
		       ((string= by "user-omit") nil)
		       (t (read-string
			   prompt-string
			   nil
			   'file-list-regexp-history))))
	 (filter-name (cond ((string= by "time")
			     (format (concat "" "%s '%s' day%s")
				     (if inverse
					 " < " " >")
				     regexp (if (string= regexp "1") "" "s")))
			    ((string= by "size")
			     (format "size %s '%s'" (if inverse "<" ">") regexp))
			    ((string= by "path")
			     (format "dir %s '%s'" (if inverse "not match:" "match:") regexp))
			    ((string= by "ext")
			     (format "ext %s '%s'" (if inverse "not match:" "match:") regexp))
			    (t (format "file %s '%s'" (if inverse "not match:" "match:") regexp))))
	 (test (cond 
		((not by) nil)
		((string= by "path")
		 '(lambda (entry) (string-match regexp (cadr entry))))
		((string= by "ext") 
		 '(lambda (entry)
		    (string-match (concat regexp "$")
				  (or (file-name-extension (car entry)) ""))))
		((string= by "size")
		 '(lambda (entry)
		    (< (string-to-number regexp)
		       (or (nth 7 (file-attributes
				   (file-list-make-file-name entry)))
			   0))))
		((string= by "time")
		 '(lambda (entry)
		    (file-list-time-less-p
		     (file-list-time-subtract (current-time) (file-list-days-to-time
							      (string-to-number regexp)))
		     (nth 5 (file-attributes (file-list-make-file-name entry))))))
		(t nil)))
	 (new-filter (list :name filter-name
			   :test (or test regexp)
			   :by by
			   :regexp regexp
			   :inverse inverse))
	 filter-result
	 (filter-list (when (superman-file-list-display-buffer-p dir)
			(get-text-property (point-min) 'filter-list)))
	 sub-file-list)
    ;; apply filter
    (setq filter-result
	  (file-list-filter-file-list file-list new-filter inverse))
    (setq
     sub-file-list (plist-get filter-result :yin)
     filter-list (add-to-list 'filter-list (plist-get filter-result :yang)))
    (unless dont-display
      (if (not sub-file-list)
	  (superman-display-file-list
	   dir
	   'empty
	   filter-list
	   nil
	   nil
	   display-buffer
	   t)
	(let ((sorted (get-text-property (point-min) 'sort-key)))
	  (when sorted
	    (setq sub-file-list
		  (file-list-sort-internal
		   sub-file-list
		   (nth 0 sorted)
		   (nth 1 sorted)
		   'dont)))
	  (superman-display-file-list
	   dir
	   sub-file-list
	   filter-list
	   sorted
	   nil
	   display-buffer
	   t))))
    sub-file-list))

(defun file-list-filter ()
  "Apply a filter to a file-list."
  (interactive)
  (let ((dbuf (current-buffer))
	(dir (get-text-property (point-min) 'dir))
	(inverse (get-text-property (point-min) 'inverse))
	(file-list file-list-current-file-list)
	(type (completing-read "Filter list by: " '(("file name")
						    ("directory name")
						    ("age")
						    ("file size")))))
    (file-list-select file-list nil (cdr (assoc type 
						'(("file name" "name")
						  ("directory name" "path")
						  ("time" "time")
						  ("file size" "size"))))
		      inverse dir dbuf nil)))

(defun file-list-by-name (&optional arg file-list dir)
  "Return sublist of filenames (in displayed file-list) whose path is matched by regexp."
  (interactive "P")
  (file-list-select file-list nil nil arg dir (current-buffer) nil))

(defun file-list-by-path (&optional arg file-list dir)
 "Returns sublist of filenames (in file-list) whose path-name is matched by regexp.
This changes the value of file-list-current-file-list."
  (interactive "P")
    (file-list-select file-list nil "path" arg dir (current-buffer) nil))

(defun file-list-by-time (&optional arg file-list dir)
  "Returns sublist of filenames (in file-list) whose size is lower than a given value.
This changes the value of file-list-current-file-list."
  (interactive "P")
  (file-list-select file-list nil "time" arg  dir (current-buffer) nil))

(defun file-list-by-size (&optional arg file-list dir)
 "Returns sublist of filenames (in file-list) whose size is lower than a given value.
This changes the value of file-list-current-file-list."
  (interactive "P")
  (file-list-select file-list nil "size" arg  dir (current-buffer) nil))

(defun file-list-by-ext (&optional arg file-list dir)
  "Returns sublist of filenames (in file-list) whose path is matched by regexp.
This changes the value of file-list-current-file-list."
  (interactive "P")
  (file-list-select file-list nil "ext" arg dir (current-buffer) nil))

(defun file-list-fresh-buffer (dir)
  "Create a fresh buffer for listing files below directory DIR. 
If such a buffer exists already, kill it first and return a fresh one."
  (let* ((fl-buf-name (concat "File-list: " (expand-file-name dir)))
	 (fl-buf (get-buffer fl-buf-name)))
    (when fl-buf (kill-buffer fl-buf))
    (get-buffer-create fl-buf-name)))

(defun file-list-by-name-below-directory (&optional arg file-list)
  "Read a directory name and return sublist of filenames below the  directory whose
file-name (without directory) matches a regexp."
  (interactive "P")
  (let ((dir (read-directory-name (format
				   "%s of files by name below directory: "
				   (if arg "Inverse selection" "Selection")))))
    (file-list-select file-list nil nil arg dir (file-list-fresh-buffer dir) nil)))

(defun file-list-by-ext-below-directory (&optional arg file-list)
  "Read directory name and return sublist of filenames below directory whose
file-name extension matches a regexp."
  (interactive "P")
  (let ((dir (read-directory-name (format
				   "%s of files by name below directory: "
				   (if arg "Inverse selection" "Selection")))))
    (file-list-select file-list nil "ext" arg dir (file-list-fresh-buffer dir) nil)))


(defun file-list-by-path-below-directory (&optional arg file-list)
 "Read a directory name and return sublist of filenames (in file-list) whose path
is matched by a regexp."
  (interactive "P")
  (let ((dir (read-directory-name (format
				   "%s of files by pathname below directory: "
				   (if arg "Inverse selection" "Selection")))))
    (file-list-select file-list nil "path" arg dir (file-list-fresh-buffer dir) nil)))


(defun file-list-by-time-below-directory (&optional arg file-list)
  "Read a directory name and return sublist of filenames (in file-list) whose age
is less or greater than a number of days which also has to be specified."
  (interactive "P")
  (let ((dir (read-directory-name (format
				   "%s of files by age below directory: "
				   (if arg "Inverse selection" "Selection")))))
  (file-list-select file-list nil "time" arg dir (file-list-fresh-buffer dir) nil)))

(defun file-list-by-size-below-directory (&optional arg file-list)
  "Read a directory name and return sublist of filenames (in file-list) whose size
is less or greater than a number in bytes which also has to be specified."
  (interactive "P")
  (let ((dir (read-directory-name (format
				   "%s of files by size below directory: "
				   (if arg "Inverse selection" "Selection")))))
    (file-list-select file-list nil "size" arg dir (file-list-fresh-buffer dir) nil)))


(defun file-list-add (&optional file-list dir regexp)
  "Finds sublist of filenames below dir matched by regexp.
The result is appended to the value file-list-current-file-list.
See also file-list-select."
  (interactive)
  (let* ((file-list (or file-list
			file-list-current-file-list))
	 (dir (or dir
		  (read-directory-name
		   (format "Directory for files to add (default: %s) "
			   file-list-home-directory)
		   file-list-home-directory
		   file-list-home-directory t)))
	 (regexp (or regexp
		     (read-string "Regexp to match filenames "
				  nil
				  'file-list-regexp-history)))
	 (add-file-list
	  (file-list-extract-sublist
	   (progn (when file-list-update (file-list-update dir nil))
		  (file-list-list dir nil nil 'recursive nil))
	   regexp))
	 new-list)
    (if (= (length add-file-list) 0)
	(message (format "No files added below %s according to match against %s." dir regexp))
      ;; update current file-list
      (let ((name-list (mapcar 'file-list-make-file-name file-list)))
	(dolist (entry add-file-list)
	  (unless (member (file-list-make-file-name entry) name-list)
	    (setq new-list (append new-list (list entry))))))
      (setq file-list-current-file-list (append file-list new-list))
      (superman-file-list-refresh-display file-list-current-file-list))))

;;  sorting file-lists 
;;  ------------------------------------------------------------------

;; next functions stolen from ognus' time-date.el
(defun file-list-time-less-p (t1 t2)
  "Say whether time value T1 is less than time value T2."
  (if (or (not t1) (not t2))
      nil
    (or (< (car t1) (car t2))
	(and (= (car t1) (car t2))
	     (< (nth 1 t1) (nth 1 t2))))))

(defun file-list-days-to-time (days)
  "Convert DAYS into a time value."
  (let* ((seconds (* 1.0 days 60 60 24))
	 (rest (expt 2 16))
	 (ms (condition-case nil (floor (/ seconds rest))
	       (range-error (expt 2 16)))))
    (list ms (condition-case nil (round (- seconds (* ms rest)))
	       (range-error (expt 2 16))))))

;; (defalias 'subtract-time 'time-subtract)

(defun file-list-time-subtract (t1 t2)
  "Subtract two time values.
Return the difference in the format of a time value."
  (let ((borrow (< (cadr t1) (cadr t2))))
    (list (- (car t1) (car t2) (if borrow 1 0))
	  (- (+ (if borrow 65536 0) (cadr t1)) (cadr t2)))))

(defun file-list-sort-by-size (&optional reverse file-list)
  "Sort file-list by file size"
  (interactive "P")
  (let ((file-list (or file-list file-list-current-file-list)))
    (file-list-sort-internal file-list "size" reverse)))

(defun file-list-sort-by-time (&optional reverse file-list)
  "Sort file-list by last save time"
  (interactive "P")
  (let ((file-list (or file-list file-list-current-file-list)))
     (file-list-sort-internal file-list "time" reverse)))

(defun file-list-sort-descending ()
  "Sort file-list in descending order by current sort-key (stored as text-property at point-min) 
or by file-name if there is no sort-key yet"
  (interactive)
  (let ((sort (get-text-property (point-min) 'sort-key)))
    (file-list-sort-internal file-list-current-file-list
			     (or (nth 0 sort) "name") 't nil)))

(defun file-list-sort-ascending ()
  "Sort file-list in ascending order by current sort-key (stored as text-property at point-min) 
or by file-name if there is no sort-key yet"
  (interactive)
  (let ((sort (get-text-property (point-min) 'sort-key)))
    (file-list-sort-internal file-list-current-file-list
			     (or (nth 0 sort) "name") nil nil)))

(defun file-list-sort-by-name (&optional reverse file-list)
  "Sort file-list by file-name"
  (interactive "P")
  (let ((file-list (or file-list file-list-current-file-list)))
    (file-list-sort-internal file-list "name" reverse)))

(defun file-list-sort-by-ext (&optional reverse file-list)
  "Sort file-list by extension"
  (interactive "P")
  (let ((file-list (or file-list file-list-current-file-list)))
     (file-list-sort-internal file-list "ext" reverse)))

(defun file-list-sort-by-path (&optional reverse file-list)
  "Sort file-list by directory-name"
  (interactive "P")
  (let ((file-list (or file-list file-list-current-file-list)))
     (file-list-sort-internal file-list "path" reverse)))

(defun file-list-parse-size (string)
  (let ((lst (mapcar 'string-to-number (delq "" (split-string string "G[ ]*\\|M[ ]*\\|K[ ]*\\|B[ ]*"))))
	terra giga mega kilo byte)
    ;; terra
    (if (nth 4 lst)
	lst
      ;; giga
      (if (nth 3 lst) (append (list 0) lst)
	;; mega
	(if (nth 2 lst) (append (list 0 0) lst)
	  ;; mega
	  (if (nth 1 lst) (append (list 0 0 0) lst)
	    ;; byte
	    (if (nth 0 lst) (append (list 0 0 0 0) lst)
	      (list 0 0 0 0 0))))))))


(defun file-list-sort-internal (&optional file-list by reverse dont-display)
  (let ((file-list (or file-list file-list-current-file-list))
	(sortfun (cond
		  ((string= by "name")
		   (lambda (a b)
		     (string-lessp (car a) (car b))))
		  ((string= by "ext")
		   (lambda (a b)
		     (string-lessp (file-name-extension (car a)) (file-name-extension (car b)))))
		  ((string= by "path")
		   (lambda (a b)
		     (string-lessp (cadr a) (cadr b))))
		  ((string= by "time")
		   (lambda (a b)
		     (let ((attr-a (file-attributes (file-list-make-file-name a)))
			   (attr-b (file-attributes (file-list-make-file-name b))))
		       (if (or (not attr-a) (not attr-b))
			   t
			 (not (file-list-time-less-p (nth 5 attr-a) (nth 5 attr-b)))))))
		  ((string= by "size")
		   (lambda (a b)
		     (let ((size-a (file-list-parse-size (cdr (assoc "size" (caddr a)))))
			   (size-b (file-list-parse-size (cdr (assoc "size" (caddr b)))))
			   stop
			   res)
		       (while (and (not stop) size-a)
			 (if (= (nth 0 size-a) (nth 0 size-b))
			     (setq size-a (cdr size-a) size-b (cdr size-b))
			   (setq stop t)
			   (setq res (> (nth 0 size-a) (nth 0 size-b)))))
		       res)))
		  ;; (> (string-to-number (cdr (assoc "size" (caddr a))))
		  ;; (string-to-number (cdr (assoc "size" (caddr b)))))))
		  (t nil)))
	sorted-list)
    (message "Sorting file list by %s" by)
    (when (string= by "size")
      (setq file-list (file-list-attributes file-list t)))
    (when sortfun
      ;; (setq file-list-current-file-list
      (setq sorted-list
	    (if reverse
		(reverse (sort file-list sortfun))
	      (sort file-list sortfun)))
      (unless dont-display
	(let ((buffer-read-only nil))
	  (put-text-property (point-min) (1+ (point-min)) 'sort-key `(,by ,reverse)))))
    (if dont-display
	sorted-list
      (setq file-list-current-file-list sorted-list)
      (message "File list sorted by %s%s" by (if reverse " in reverse order" ""))
      (superman-file-list-refresh-display file-list-current-file-list))))

;;}}}
;;{{{ file-list actions

(defun file-list-quote-filename (name)
  ;; for shell commands
  (concat "\"" name "\""))


(defun file-list-choose-file (arg &optional event extent buffer magic)
  (interactive "P")
  (let ((grep-line
	 (save-excursion
	   (beginning-of-line)
	   (if (looking-at "\\(^[ \t]+\\)\\([0-9]+\\)\\( : \\)")
	       (match-string 2) nil)))
	(file-name (file-list-file-at-point))
	fun)
    (cond ((and magic
		(if arg
		    (setq fun (read-shell-command (format "icommand on %s " file-name)
						  nil nil nil))
		  (setq fun (cdr (assoc-string
				  (concat "\." (file-name-extension
						file-name))
				  file-list-magic-alist 'case-fold)))))
	   (async-shell-command
	    (concat "PATH=~/bin:\"${PATH}\";" fun " "
		    (file-list-quote-filename file-name))))
					; (start-process-shell-command
	  ;; "file-list-find-magic"
	  ;; nil
	  ;; (concat fun " "
	  ;; (file-list-quote-filename file-name))))
	  (t
	   (cond ((not arg) nil)
		 ((= arg 4) (switch-to-buffer-other-window (current-buffer)))
		 ((= arg 5) (switch-to-buffer-other-frame (current-buffer)))
		 (t nil))
	   (if (functionp 'org-open-at-point)
	       (org-open-file file-name)
	     (find-file file-name))
	   (when grep-line
	     (goto-char (point-min))
	     (forward-line (1- (string-to-number grep-line))))))))

(defun file-list-choose-file-no-visit ()
  (interactive)
  (let ((cur-buf (current-buffer)))
    (file-list-choose-file nil)
    ;;(switch-to-buffer cur-buf)
    (switch-to-buffer-other-window cur-buf)
    (forward-line 1)))

(defun file-list-choose-file-other-window (&optional event extent buffer magic)
  (interactive)
  (file-list-choose-file 4 event extent buffer magic))
				   
(defun file-list-choose-magic (arg &optional event extent buffer)
  (interactive "P")
  (file-list-choose-file arg event extent buffer 'magic))

(defun file-list-omit-file-at-point (&optional arg)
  "Omit entry of filename at point from current file list.
If ARG keep only filename at point."
  (interactive "P")
  (when file-list-mode
    (let* ((ff (get-text-property (point-min) 'filter-list))
	   (buffer-read-only nil)
	   ;; manual filter is always in pole position
	   (is-manual (when (equal (plist-get (nth 0 ff) :name) "manual-filter") ff))
	   (hf (when is-manual (nth 0 ff)))
	   filtered-files
	   (c-list file-list-current-file-list)
	   (this-file (file-list-make-entry (file-list-file-at-point))))
      (if arg (setq filtered-files (delete this-file file-list-current-file-list)
		    file-list-current-file-list (list this-file))
	(setq filtered-files (list this-file)
	      file-list-current-file-list
	      (delete this-file c-list)))
      ;; set manual filter
      (if hf (setq hf (plist-put hf :filtered-files 
				 (append (plist-get hf :filtered-files) filtered-files)))
	(setq hf `((:name "manual-filter" 
			  :test nil :by "user-omit" :regexp nil :inverse nil :filtered-files ,filtered-files))))
      (if is-manual (setcar ff hf) (setq ff (append hf ff)))
      ;; set filter and adapt number of files
      (put-text-property (point-min) (1+ (point-min)) 'filter-list ff)
      (file-list-update-filter-line ff (length file-list-current-file-list))
      ;; modifiy file-list
      (if arg
	  (superman-display-file-list
	   (get-text-property (point-min) 'dir)
	   file-list-current-file-list
	   ff
	   (get-text-property (point-min) 'sort-key)
	   (get-text-property (point-min) 'balls)
	   (current-buffer)
	   (not (get-text-property (point-min) 'project-buffer)))
	;; manual removal of the file at point
	(delete-region (point-at-bol) (1+ (point-at-eol)))
	(while (get-text-property (point-at-bol) 'appendix)
	  (delete-region (point-at-bol) (1+ (point-at-eol)))))
      ;; move 
      (unless arg
	(unless (next-single-property-change (point) 'filename)
	  (forward-line -1))
	;; (previous-single-property-change (point-max) 'point-file-list-start))
	;; (goto-char (1+ (previous-single-property-change (point-max) 'point-file-list-start))))
	(when (< (point) 
		 (previous-single-property-change (point-max) 'point-file-list-start))
	  (goto-char (1+ (previous-single-property-change (point-max) 'point-file-list-start))))))))
  

(defun file-list-find (arg &optional file-list)
  (interactive "p")
  (cond ((not arg) nil)
	((= arg 4) (switch-to-buffer-other-window (current-buffer)))
	((= arg 5) (switch-to-buffer-other-frame (current-buffer)))
	(t nil))
  (let ((file-list (or file-list file-list-current-file-list)))
    (mapcar (lambda (entry)
	      (find-file (file-list-make-file-name entry)))
	    file-list)))


(defun file-list-dired (&optional file-list dir)
  (interactive)
  (let* ((file-list (or file-list file-list-current-file-list)))
    (dired (cons file-list-home-directory
		 (mapcar 'file-list-make-file-name file-list)))))


(defun file-list-shell-command-at-point (&optional file-list)
  (interactive)
  (let* ((fatp (file-list-file-at-point))
	 (command (read-shell-command "Shell-command "
				      nil
				      nil
				      shell-command-history)))
    (shell-command (concat "cd "
			   (file-name-directory fatp) ";"
			   command " " 	      (file-list-quote-filename fatp)))))
    
	

(defun file-list-shell-command (&optional file-list)
  "Shell command on all files in file-list-current-file-list.
Switches to the corresponding directory of each file."
  (interactive)
  (let ((file-list (or file-list file-list-current-file-list))
	(command (read-shell-command "Shell-command "
				     nil
				     nil
				     shell-command-history)))
    (dolist (f file-list)
      (let* ((fname (file-list-make-file-name f))
	     (fcommand (concat command " " (file-list-quote-filename fname))))
	(message fcommand)
	(shell-command
	 (concat "cd " (file-name-directory fname) ";" fcommand))))))


(defun file-list-pdf2eps (&optional file-list)
"Run ps2pdf x.pdf x.eps on all pdf files in file-list-current-file-list.
  Switches to the corresponding directory of each file."
  (interactive)
  (let ((file-list (or file-list file-list-current-file-list)))
    (dolist (f file-list)
      (if (string= (file-name-extension (file-list-make-file-name f)) "pdf")
      (let* ((fname (file-list-make-file-name f))
	     (epsname (file-list-quote-filename (concat (file-name-sans-extension fname) ".eps")))
	     (fcommand (concat "ps2ps" " " (file-list-quote-filename fname) " " (file-list-quote-filename epsname))))
	(message fcommand)
	(shell-command
	 (concat "cd " (file-name-directory fname) ";" fcommand)))))))


(defun file-list-rename-file-at-point (&optional newname)
  (interactive)
  (let* ((oldname (file-list-file-at-point))
	 (newname (or newname
		      (expand-file-name (read-file-name (format "Enter new name for %s " oldname))))))
    (rename-file oldname newname 'ok)
    (file-list-update-file-alist oldname 'delete)
    (file-list-update-file-alist newname)
    (file-list-update-current-file-list oldname newname)
    (superman-file-list-refresh-display)
    (re-search-forward newname nil t)))

(defun file-list-rename (&optional file-list one-by-one)
  (interactive)
  (if one-by-one
      (let ((file-list (or file-list file-list-current-file-list)))
	(dolist (entry file-list)
	  (let* ((oldname (file-list-make-file-name entry))
		 (newname
		  (read-file-name (format "Enter new name for %s " (car entry))
				  (cdr entry)))
		 (newdir (if (not (file-name-absolute-p newname))
			     (file-name-directory oldname)
			   (file-name-directory (expand-file-name newname))))
		 (filename (file-name-nondirectory newname)))
	    (unless (and newdir (file-directory-p newdir))
	      (if (yes-or-no-p
		   (format "Directory %s does not exist create it? "
			   newdir))
		  (make-directory newdir)))
	    (setq filename
		  (file-list-make-file-name
		   (cons filename newdir)))
					;	  (shell-command (concat "mv " oldname " " filename))
	    (rename-file oldname filename)
	    ;; update current file list
	    (dolist (entry file-list-current-file-list)
	      (setcar entry filename) (setcdr entry newdir)))))
    (let* ((file-list (or file-list file-list-current-file-list))
	   (old-regexp (read-string 
			"Regexp matching all filenames "))
	   (replacement (read-string
			 (format "Replace match ('%s') with " old-regexp))))
      (dolist (entry file-list)
	(let* ((old-name (car entry))
	       (new-name (file-list-replace-in-string old-name old-regexp replacement nil 'fixed)))
	  (setcar entry new-name)
	  (rename-file (concat (cadr entry) old-name)
		       (concat (cadr entry) new-name) t)))))
  (superman-file-list-refresh-display file-list-current-file-list))


(defun file-list-move (&optional ask file-list target copy)
  (interactive "P")
  (let* ((file-list-intern (or file-list file-list-current-file-list))
	 (nfiles (length file-list-intern))
	 (target (or target
		     (expand-file-name
		      (file-name-as-directory
		       (read-directory-name
			(if (eq 1 nfiles)
			    "Copy this file to directory: "
			  (concat "Copy " (int-to-string nfiles) " files to directory: "))
			nil nil nil)))))
	 (use-path (if ask (yes-or-no-p "Use path as part of file-name? "))))
    (cond ((file-directory-p target) nil)
	  ((yes-or-no-p (format "Directory %s does not exist create it? "
				target))
	   (make-directory target))
	  ((yes-or-no-p "Abort moving files ") (file-list-quit))
	  (t (file-list-move nil file-list-intern nil)))
    (while file-list-intern
      (let* ((entry (car file-list-intern))
	     (oldname (file-list-make-file-name entry))
	     (newcar (if use-path
			 (concat (replace-in-string (replace-in-string (car (cdr entry)) "^/" "") "/" "_") (car entry))
		       (car entry)))
	     (newname (file-list-make-file-name (cons newcar (list target))))
	     (ilist (mapcar 'car (setq file-list-intern (cdr file-list-intern)))))
	;; stop if there are duplicate (non-absolute) file-names in current file-list
	(if (member newcar ilist)
	    (error "Duplicate file-name '%s' in current file-list!" (car entry))
	  (if copy (progn
		     (condition-case nil
			 (copy-file oldname newname nil)
		       (error
			(progn
			  (message (concat "Could not move" oldname " to " newname ". File exists"))
			  nil))))
	    (if (condition-case nil
		    (rename-file oldname newname nil)
		  (error
		   (progn
		     (message (concat "Could not move" oldname " to " newname))
		     nil)))
		(message (concat "Successfully moved " oldname " to " newname))))
	  (if copy
	      (setq file-list-current-file-list
		    (append file-list-current-file-list (list (file-list-make-entry newname))))
	    (if file-list
		(setcar (cdr (assoc (car entry) file-list-current-file-list)) target)
	      (setcar (cdr entry) target))))))
    (superman-file-list-refresh-display file-list-current-file-list)))


(defun file-list-move-file-at-point (&optional ask)
  (interactive "P")
  (let ((file (file-list-file-at-point)))
    (file-list-move ask (list (file-list-make-entry file)) nil)))


(defun file-list-copy (&optional ask file-list target path)
  (interactive "P")
  (file-list-move ask file-list target 'copy))

(defun file-list-copy-file-at-point (&optional ask)
  (interactive "P")
  (let ((file (file-list-file-at-point)))
    (file-list-copy ask (list (file-list-make-entry file)) nil)))


(defun file-list-ls (&optional file-list nodisplay)
  (interactive)
  (let* ((file-list (or file-list file-list-current-file-list))
	 (attr-list
	  (mapcar
	   (lambda (attr)
	     (let* ((rest (split-string
			   (file-list-replace-in-string attr "[ \t]+" " ") " ")))
	       (cons (car (reverse rest))
		     (list (cons "mod" (car rest))
			   (cons "owner" (caddr rest))
			   (cons "group" (nth 3 rest))
			   (cons "size" (nth 4 rest))
			   (cons "created"
				 (concat (nth 5 rest)
					 " "
					 (nth 6 rest)
					 " "
					 (nth 7 rest)))))))
	   (split-string (shell-command-to-string
			  (concat "ls -lh "
				  (file-list-concat-file-names file-list)))
			 "\n"))))
    (setq file-list-current-file-list
	  (mapcar
	   (lambda (entry)
	     (let ((add (cdr (assoc
			      (file-list-make-file-name entry)
			      attr-list))))
	       (if add (list (car entry) (cadr entry) add)
		 entry)))
	   file-list))
    (unless nodisplay
      (superman-file-list-refresh-display file-list-current-file-list))))


(defun file-list-attributes (&optional file-list nodisplay)
  (interactive)
  (let ((file-list (file-list-select-existing-files
		    (or file-list file-list-current-file-list))))
    (setq file-list-current-file-list
	  (mapcar
	   (lambda (entry)
	     (let ((add (file-attributes (file-list-make-file-name entry))))
	       (list (car entry) (cadr entry)
		     (list (cons "mod" (nth 8 add))
			   (cons "time"
				 (format-time-string file-list-format-time-string
						     (nth 5 add)))
;			   (cons "status-time"
;				 (format-time-string file-list-format-time-string
;						     (nth 6 add)))
			   
;			   (cons "access-time"
;				 (format-time-string file-list-format-time-string
;						     (nth 4 add)))
			   
;     "Human-readable" output.  Use unit suffixes: Byte, Kilobyte,
;     Megabyte, Gigabyte, Terabyte and Petabyte in order to reduce the
;     number of digits to four or fewer using base 2 for sizes.
;     (FreeBSD man page of C<df>: http://www.freebsd.org/cgi/man.cgi?query=df)

;   byte      B
;   kilobyte  K = 2**10 B = 1024 B
;   megabyte  M = 2**20 B = 1024 * 1024 B
;   gigabyte  G = 2**30 B = 1024 * 1024 * 1024 B
;   terabyte  T = 2**40 B = 1024 * 1024 * 1024 * 1024 B

;   petabyte  P = 2**50 B = 1024 * 1024 * 1024 * 1024 * 1024 B
;   exabyte   E = 2**60 B = 1024 * 1024 * 1024 * 1024 * 1024 * 1024 B
;   zettabyte Z = 2**70 B = 1024 * 1024 * 1024 * 1024 * 1024 * 1024 * 1024 B
;   yottabyte Y = 2**80 B = 1024 * 1024 * 1024 * 1024 * 1024 * 1024 * 1024 * 1024 B
			   (cons "size"
; 				 (number-to-string (nth 7 add))
				 (file-list-convert-bytes (nth 7 add)))))))
	   file-list))
    (unless nodisplay
      (superman-file-list-refresh-display
       file-list-current-file-list)))
  file-list-current-file-list)

(defun file-list-convert-bytes (int)
  (let* ((string (number-to-string int))
	 (len (length string))
 	 mega kilo byte)
    (if (> len 10)
	"huge (> 1G)"
      (setq mega (/ int 1048576)
	    kilo (/ (mod int 1048576) 1024)
	    byte (if (> kilo 0) (mod int kilo) int))
      (concat (when (> mega 0) (concat (number-to-string mega) "M "))
	      (when (or (> mega 0) (> kilo 0)) (concat (number-to-string kilo) "K "))
	      (number-to-string byte) "B "))))
      
(defun file-list-remove-file-at-point ()
  (interactive)
  (let* ((fname (file-list-file-at-point))
	 (do-it (yes-or-no-p (format "Delete %s ? " fname))))
    (when do-it (delete-file fname))
    (file-list-update-file-alist fname 'delete)
    (setq file-list-current-file-list
	  (delete nil (mapcar (lambda (entry)
				(unless (string= (file-list-make-file-name entry) fname)
				  entry)) file-list-current-file-list)))
    (superman-file-list-refresh-display)
    (file-list-beginning-of-file-list)))
;; (file-list-switch-to-file-list)))

(defun file-list-remove (&optional file-list)
  (interactive)
  (let ((file-list (or file-list file-list-current-file-list)))
    ;; (superman-file-list-refresh-display file-list)
    (when (yes-or-no-p
	   "Really really move all these files to /dev/null in the sky? ")
      (file-list-select-existing-files file-list)
      (dolist (fn file-list)
	(delete-file (file-list-make-file-name fn)))
      ;; update file-list-current-file-list and file-list-alist
      (setq file-list-current-file-list nil)
      (superman-file-list-refresh-display))))

(defun file-list-grep (&optional file-list)
  (interactive)
  (when file-list-mode
    (let* ((file-list (or file-list file-list-current-file-list))
	   (grep-regexp (read-string
			 "Regexp for `grep -n' on current file-list: "
			 nil file-list-grep-history))
	   (files (file-list-concat-file-names file-list))
	   (file-list-buffer (current-buffer))
	   (hits-buf (get-buffer-create "*file-list-grep-hits*")))
      (message "Waiting for grep ...")
      (shell-command
       (concat "grep -n " grep-regexp " "  files " " "/dev/null")
       hits-buf)
      (if (not (buffer-live-p hits-buf))
	  (error "No grep hits for '%s'." grep-regexp))
      (with-current-buffer hits-buf
	(goto-char (point-min))
	(let (grep-hits grep-hit-list file-name)
	  (while (re-search-forward "\\(^/.*\\)\\(:[0-9]+:\\)\\(.*$\\)" nil t)
	    (setq file-name (match-string 1))
	    (setq grep-hits (list (cons (substring
					 (match-string 2) 1 -1)
					(match-string 3))))
	    ;; collect further hits for the same file 
	    (while (re-search-forward (concat "\\(" file-name "\\)\\(:[0-9]+:\\)\\(.*$\\)") nil t)
	      (setq grep-hits
		    (append grep-hits
			    (list (cons (substring (match-string 2) 1 -1)
					(match-string 3)))))
	      (forward-line 1))
	    (setq grep-hit-list (append (list (cons file-name grep-hits))
					grep-hit-list)))
	  (when (get-buffer-window hits-buf)
	    (delete-window (get-buffer-window hits-buf))
	    (kill-buffer hits-buf))
	  (switch-to-buffer file-list-buffer)
	  (file-list-beginning-of-file-list)
	  (if (not grep-hit-list)
	      (message "No grep hits for '%s'." grep-regexp)
	    ;;replace current entry with new entry
	    (setq file-list-current-file-list
		  (mapcar
		   (lambda (entry)
		     (let ((add (cdr
				 (assoc
				  (file-list-make-file-name entry)
				  grep-hit-list))))
		       (if add
			   (list (car entry) (cadr entry)
				 (if (> (length entry) 2)
				     (append (caddr entry) add)
				   add))
			 entry)))
		   file-list))
	    (setq file-list-current-file-list
		  (sort file-list-current-file-list
			(lambda (e f)
			  (> (length (caddr e)) (length (caddr f)))))))
	  (superman-file-list-refresh-display file-list-current-file-list))))))

;;}}}
;;{{{ isearch
(defun file-list-search ()
  (interactive)
  (when file-list-mode
    (let* ((buffer-read-only nil)
	   (string (read-string "Search string in file-list: "))
	   (display-buffer (current-buffer))
	   this-file this-line line-no)
      (goto-char (1+ (file-list-beginning-of-file-list)))
      (delete-other-windows)
      (split-window-vertically)
      (while (setq this-file (get-text-property (point-at-bol) 'filename))
	(other-window 1)
	(find-file this-file)
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  (while (search-forward string nil t)
	    (setq 
	     line-no (number-to-string (line-number-at-pos))
	     this-line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
	    (put-text-property 0 (length line-no) 'face font-lock-warning-face line-no)
	    (with-current-buffer display-buffer
	      (end-of-line)
	      (insert "\n  " line-no " : " this-line)
	      (put-text-property (point-at-bol) (point-at-eol) 'file-info t))))
	(file-list-next-file 1)))))

(defun file-list-replace-nonstop (&optional file-list)
  (interactive)
  (let* ((file-list (or file-list file-list-current-file-list))
	 (args (query-replace-read-args "Query-replace" nil t)))
    (dolist (file file-list)
      (save-window-excursion
	(find-file (file-list-make-file-name file))
	(widen)
	(save-restriction
	  (when (and (eq major-mode 'org-mode)
                     (not visible-mode))
	    (visible-mode 1))
	  (goto-char (point-min))
	  (while (re-search-forward (car args) nil t)
	    (replace-match (cadr args) nil nil))
	  (save-buffer)
	  )))))

(defun file-list-query-replace (&optional file-list args)
  "Apply `query-replace' to FILE-LIST which defaults to `file-list-current-file-list' current.
When ARGS is given it should have the same format as the result of `query-replace-read-args'. E.g., 
(list 'match' 'replacement' nil nil)."
  (interactive)
  (save-some-buffers)
  (let* ((file-list (or file-list file-list-current-file-list))
	 (args (or args (query-replace-read-args "Query-replace" nil t))))
    (dolist (file file-list)
      (save-window-excursion
	(find-file (file-list-make-file-name file))
	(save-restriction
	  (widen)
	  (when (and (eq major-mode 'org-mode)
                     (not visible-mode))
	    (visible-mode 1))
	  (goto-char (point-min))
	  (query-replace (car args) (cadr args))
	  (save-buffer)
	  ;; (kill-buffer)
	  )))))


(defun file-list-query-replace-regexp (&optional file-list)
  (interactive)
  (let* ((file-list (or file-list file-list-current-file-list))
	 (args (query-replace-read-args "Query-replace" nil t)))
    (dolist (file file-list)
      (save-window-excursion
	(find-file (file-list-make-file-name file))
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  (query-replace-regexp (car args) (cadr args))
	  (save-buffer)
	  )))))

(defun file-list-query-replace-ignore-case (&optional file-list)
  (interactive)
  (let* ((file-list (or file-list file-list-current-file-list))
	 (case-fold-search t)
	 (args (query-replace-read-args "Query-replace" nil t)))
    (dolist (file file-list)
      (save-window-excursion
	(find-file (file-list-make-file-name file))
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  (query-replace (car args) (cadr args))
	  (save-buffer)
	  ;; (kill-buffer)
	  )))))
;;}}}
;;{{{ iswitchf
 
(defun file-list-iswitchf-internal (&optional dir file-list fun prompt)
  (let* (
	 (prompt (or prompt file-list-iswitchf-prompt))
	 (minibuffer-completion-table file-list)
	 (fun (or fun 'find-file))
	 (dir (cond ((and dir
			  (file-directory-p dir)
			  (file-name-as-directory
			   (expand-file-name dir))))
	      (t file-list-home-directory)))
	 (file-list
	  (or file-list
	      (progn (if file-list-update (file-list-update dir nil))
		     (file-list-list dir nil nil 'recursive nil))))
	 (file (completing-read
		prompt
		file-list
		nil
		t
		nil
		'file-list-iswitchf-history))
	 (duplicates (file-list-extract-sublist 
		      file-list
		      (lambda (entry)
			(when (string= file (car entry))
			  entry)))))
    (if (= (length duplicates) 1)
	(funcall fun (file-list-make-file-name (car duplicates)))
      (funcall fun 
	       (completing-read
		"Duplicate names. Select one: "
		(mapcar (lambda (entry) (cons
					 (file-list-make-file-name entry)
					 ""))
			duplicates)
		nil 
		t
		dir)))))


(defun file-list-iswitchf-file ()
  "Switch to a file in the file-list of file-list-home-directory."
  (interactive)
  (unless file-list-alist
    (file-list-initialize))
  (file-list-iswitchf-internal))
;   (file-list-iswitchf-internal file-list-home-directory))

;; (defun file-list-find-magic (file-name &optional ask-for-prog)
  ;; "Open a file with the application found in file-list-magic-alist."
  ;; (let ((prog (if ask-for-prog
		  ;; (read-shell-command (format "icommand on %s " file-name)
				      ;; nil nil nil)
		;; (cdr (assoc-string
		      ;; (concat "\."
			      ;; (file-name-extension file-name))
		      ;; file-list-magic-alist 'case-fold)))))
    ;; (if prog (start-process-shell-command
	      ;; "file-list-find-magic"
	      ;; nil
	      ;; prog
	      ;; (file-list-quote-filename file-name))
      ;; (find-file file-name))))

;; (defun file-list-iswitchf-magic (arg)
  ;; "Switch to file in file-list of file-list-home-directory."
  ;; (interactive "P")
  ;; (file-list-iswitchf-internal
   ;; file-list-home-directory
   ;; nil
   ;; '(lambda (file)
      ;; (file-list-find-magic
       ;; file
       ;; arg))
       ;; "Find file magic "))

(defun file-list-iswitchf-file-other-window ()
  "See file-list-iswitchf-file."
  (interactive)
  (file-list-iswitchf-internal file-list-home-directory
		      nil
		      'find-file-other-window
		      (concat file-list-iswitchf-prompt "(other window) ")))

(defun file-list-iswitchf-file-other-frame ()
  "See file-list-iswitchf-file."
  (interactive)
  (file-list-iswitchf-internal
   file-list-home-directory
   nil
   'find-file-other-frame
   (concat file-list-iswitchf-prompt "(other frame) ")))


(defun file-list-iswitchf-below-directory (dir)
  "Like file-list-iswitchf-file but prompts for directory."
  (interactive "D iswitchf below directory ")
  (file-list-iswitchf-internal (file-name-as-directory
		       (expand-file-name dir))))

;; (defun file-list-iswitchf-below-directory-other-window (dir)
  ;; "See file-list-iswitchf-below-directory."
  ;; (interactive "D iswitchf below directory ")
  ;; (file-list-iswitchf-internal (file-name-as-directory
				;; (expand-file-name dir)
				;; nil
				;; 'find-file-other-window
				;; (concat file-list-iswitchf-prompt "(other window) "))))

;; (defun file-list-iswitchf-below-directory-other-frame (dir)
  ;; "See file-list-iswitchf-below-directory."
  ;; (interactive "D iswitchf below directory ")
  ;; (file-list-iswitchf-internal (file-name-as-directory
		       ;; (expand-file-name dir)
		       ;; nil
		      ;; 'find-file-other-frame
		      ;; (concat file-list-iswitchf-prompt "(other frame) "))))
;;}}}
;;{{{ keybindings for the file-list-display-buffer
(define-key file-list-mode-map [(return)] 'file-list-choose-file)
(define-key file-list-mode-map [(control return)]  'file-list-choose-file-no-visit)
(define-key file-list-mode-map [(meta return)] 'file-list-choose-magic)
(if (featurep 'xemacs)
    (define-key file-list-mode-map [(space)] 'file-list-choose-file-other-window)
  (define-key file-list-mode-map (kbd "SPC") 'file-list-choose-file-other-window))
(define-key file-list-mode-map "a" 'file-list-mml-attach-file-at-point)
(define-key file-list-mode-map "A" 'file-list-mml-attach)
(define-key file-list-mode-map "b" 'file-list-previous-file)
(define-key file-list-mode-map "c" 'file-list-copy-file-at-point)
(define-key file-list-mode-map "C" 'file-list-copy)
(define-key file-list-mode-map "d" 'file-list-dired)
(define-key file-list-mode-map "e" 'file-list-end-of-file-list)
(define-key file-list-mode-map "f" 'file-list-choose-file)
(define-key file-list-mode-map "F" 'file-list-find)
(define-key file-list-mode-map "g" 'file-list-grep)
(define-key file-list-mode-map "k" 'file-list-remove-file-at-point)
(define-key file-list-mode-map "K" 'file-list-remove)
(define-key file-list-mode-map "l" 'file-list-attributes)
(define-key file-list-mode-map "L" 'file-list-ls)
(define-key file-list-mode-map "m" 'file-list-move-file-at-point)
(define-key file-list-mode-map "M" 'file-list-move)
(define-key file-list-mode-map "\t" 'file-list-next-file)
(define-key file-list-mode-map "n" 'file-list-next-file)
(define-key file-list-mode-map "o" 'file-list-omit-file-at-point)
(define-key file-list-mode-map "p" 'file-list-previous-file)
(define-key file-list-mode-map "q" 'file-list-quit)
(define-key file-list-mode-map "r" 'file-list-rename-file-at-point)
(define-key file-list-mode-map "R" 'file-list-rename)
(define-key file-list-mode-map "t" 'file-list-toggle-display-mode)
(define-key file-list-mode-map "u" 'file-list-reload)
(define-key file-list-mode-map "x" 'file-list-shell-command-at-point)
(define-key file-list-mode-map "X" 'file-list-shell-command)
(define-key file-list-mode-map "y" 'file-list-add)
(define-key file-list-mode-map "\C-c" 'file-list-clear-display)
(define-key file-list-mode-map [(delete)] 'file-list-clear-display)
(define-key file-list-mode-map "\C-l" 'file-list-clear-display)
(define-key file-list-mode-map "\C-t" 'file-list-toggle-display-mode)
(define-key file-list-mode-map "Ss" 'file-list-sort-by-size)
(define-key file-list-mode-map "St" 'file-list-sort-by-time)
(define-key file-list-mode-map "Sf" 'file-list-sort-by-name)
(define-key file-list-mode-map "Sp" 'file-list-sort-by-path)
(define-key file-list-mode-map "Se" 'file-list-sort-by-ext)
(define-key file-list-mode-map "Uu" 'file-list-update)
(define-key file-list-mode-map "Ud" 'file-list-update-below-dir)
(define-key file-list-mode-map "/s" 'file-list-by-size)
(define-key file-list-mode-map "/t" 'file-list-by-time)
(define-key file-list-mode-map "/f" 'file-list-by-name)
(define-key file-list-mode-map "/e" 'file-list-by-ext)
(define-key file-list-mode-map "/p" 'file-list-by-path)
(define-key file-list-mode-map "/a" 'file-list-add)
(define-key file-list-mode-map "1" 'file-list-remove-filter-1)
(define-key file-list-mode-map "2" 'file-list-remove-filter-2)
(define-key file-list-mode-map "3" 'file-list-remove-filter-3)
(define-key file-list-mode-map "4" 'file-list-remove-filter-4)
(define-key file-list-mode-map "5" 'file-list-remove-filter-5)
(define-key file-list-mode-map "6" 'file-list-remove-filter-6)
(define-key file-list-mode-map "7" 'file-list-remove-filter-7)

(defun file-list-default-keybindings ()
  "Set up default keybindings'."
  (interactive)
  (global-unset-key "\C-xf")
  (global-set-key (read-kbd-macro "C-x f f")  'file-list-iswitchf-file)
  (global-set-key (read-kbd-macro "C-x f m") 'file-list-iswitchf-magic)
  
  (global-set-key (read-kbd-macro "C-x f 4 f") 'file-list-iswitchf-file-other-window)
  (global-set-key (read-kbd-macro "C-x f 5 f") 'file-list-iswitchf-file-other-frame)
  (global-set-key (read-kbd-macro "C-x f 4 d") 'file-list-iswitchf-below-directory-other-window)
  (global-set-key (read-kbd-macro "C-x f 5 d") 'file-list-iswitchf-below-directory-other-frame)

  (global-set-key (read-kbd-macro "C-x f d") 'file-list-iswitchf-below-directory)

  (global-set-key (read-kbd-macro "C-x f u") 'file-list-update)
  (global-set-key (read-kbd-macro "C-x f U") 'file-list-update-below-dir)

  (global-set-key (read-kbd-macro "C-x f s") 'file-list-by-size)
  (global-set-key (read-kbd-macro "C-x f t") 'file-list-by-time)
  (global-set-key (read-kbd-macro "C-x f p") 'file-list-by-path)
  (global-set-key (read-kbd-macro "C-x f P") 'file-list-by-path-below-directory)
  (global-set-key (read-kbd-macro "C-x f l") 'file-list-by-name)
  (global-set-key (read-kbd-macro "C-x f L") 'file-list-by-name-below-directory))
;;}}}
;;{{{ utility functions 

(defun file-list-time-stamp-file-at-point ()
  (interactive)
  (let ((f (file-list-file-at-point)))
    (file-list-rename-file-at-point
     (concat (file-name-sans-extension f)
	     "-"
	     (format-time-string "%Y%m%d")
	     (file-name-extension f 'p)))))

(defun file-list-clear-home ()
  "Delete all files whose names are matched by file-list-clear-home-regexp."
  (interactive)
  (setq file-list-current-file-list nil)
  (save-window-excursion
    (let ((death-row (file-list-select-existing-files
		      (file-list-extract-sublist
		       (file-list-list
			(getenv "HOME"))
		       file-list-clear-home-regexp))))
      (if (null death-row)
	  (message (format "Your $HOME is clean: there are no files matching %s"
			   file-list-clear-home-regexp))
	(superman-file-list-refresh-display death-row)
	(file-list-remove death-row)
	(message nil)))))


(defun file-list-tar-current-file-list () ;(file-list &optional untar)
  (interactive)
  (let ((file-list file-list-current-file-list)
	(tarfile
; 	 (unless untar
	 (expand-file-name
	  (read-file-name
	   "Name for new tar file': "
	   nil nil nil))))
;     )(if untar (while file-list
; 		(let ((filename (file-list-make-file-name
; 				 (car file-list))))
; 		  (when (string= (file-name-extension filename)
; 				 "tar")
; 		    (shell-command (concat
; 				    "tar xvf "
; 				    filename)))
; 		  (setq file-list (cdr file-list))))
    (shell-command (concat  "tar cvf " tarfile " "
			    (file-list-concat-file-names file-list)))
    (setq file-list-current-file-list
	  (add-to-list 'file-list-current-file-list
		       (file-list-make-entry tarfile) 'append))
    (superman-file-list-refresh-display file-list-current-file-list)
    (file-list-update-file-alist tarfile)
    ))


(defun file-list-mml-attach-file-at-point ()
  "Attach file at point to the outgoing MIME message."
  (interactive)
  (let* ((fatpoint (file-list-file-at-point))
	 (type (mm-default-file-encoding fatpoint))
	 (description nil)
	 (buf (if (with-current-buffer file-list-reference-buffer
		    (string-match "message" (symbol-name
					     major-mode)))
		  file-list-reference-buffer
		(read-buffer "attach to buffer "
			     (cl-find-if (lambda (b)
					(with-current-buffer 
					    b
					  (string= "message-mode" (buffer-name b))))
				      (buffer-list))
			     t))))
    (switch-to-buffer buf)
    (goto-char (point-max))
    (mml-insert-empty-tag 'part
			  'type type
			  'filename fatpoint
			  'disposition "attachment"
			  'description description))
  (file-list-switch-to-file-list))


(defun file-list-mml-attach (&optional file-list)
  "Attach files in file-list to the outgoing MIME message."
  (interactive)
					;  (save-window-excursion
  (let* ((file-list (or file-list file-list-current-file-list))
	 (flist (mapcar 'file-list-make-file-name file-list))
	 (active (string= (buffer-name (current-buffer))
			  file-list-display-buffer))
	 (buf (cond ((not active) (current-buffer))
		    ((string-match "mail" (buffer-name file-list-reference-buffer))
		     file-list-reference-buffer)
		    (t (read-buffer "Attach all these files to buffer " 
				    (cl-find-if (lambda (b)
						  (with-current-buffer b
						    (string= "message-mode" (buffer-name b))))
						(buffer-list)) t))))
	 file type description)
    (switch-to-buffer buf)
    (while flist
      (setq file (car flist)
	    type (mm-default-file-encoding file)
	    description nil)
      (mml-insert-empty-tag 'part
			    'type type
			    'filename file
			    'disposition "attachment"
			    'description description)
      (setq flist (cdr flist)))
    (file-list-quit t)))



(defun file-list-call-defun (&optional file-list)
  (interactive)
  (let* ((fun (read-command "Function to be called on all files: "))
	 (buffer-read-only nil)
	 (file-list (or file-list file-list-current-file-list)))
    (dolist (file file-list)
      (save-window-excursion
	(find-file (file-list-make-file-name file))
	(save-restriction
	  (widen)
	  (funcall fun)
	  (save-buffer))))))

(defun roxy-clean-export-lines ()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "@export " nil t)
    (kill-region (point) (point-at-eol))))

(defun file-list-replace (&optional file-list)
  (interactive)
  (let* ((file-list (or file-list file-list-current-file-list))
	 (string (read-string "String to replace: "))
	 (replacement (read-string "Replacement string: ")))
    (dolist (file file-list)
      (save-window-excursion
	(find-file (file-list-make-file-name file))
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  (while (re-search-forward string nil t)
	    (replace-match replacement))
	  (save-buffer)
	  (kill-this-buffer))))))
  

(defun file-list-downcase-filenames (&optional file-list)
  (interactive)
  (let ((file-list (or file-list file-list-current-file-list)))
    (dolist (entry file-list)
      (let* ((oldname (file-list-make-file-name entry))
	     (newname (file-list-make-file-name (cons (downcase (car entry)) (cdr entry)))))
	(rename-file oldname newname)))
    ;; update current file list
    (dolist (entry file-list-current-file-list)
      (setcar entry (downcase (car entry)))))
  (superman-file-list-refresh-display file-list-current-file-list))


;; (defun file-list-diagnosis (&optional dir)
  ;; (interactive)
  ;; (get-buffer-create "*file-list-diagnosis*")
  ;; (switch-to-buffer "*file-list-diagnosis*")
  ;; (erase-buffer)
  ;; (let* (
	 ;; (dir (or dir file-list-home-directory))
	 ;; (dirlist (file-list-list dir t t t))
	 ;; (overall 0) 
	 ;; (diaglist (sort (let ((tail dirlist) dlist entry size)
			   ;; (while tail 
			     ;; (if (setq entry (assoc (cadr (car tail)) dlist))
				 ;; (progn (setq size (nth 7 (file-attributes (file-list-make-file-name (car tail)))))
					;; ;; size can be out of range ...
					;; (if (= size -1) nil
					  ;; (setcdr entry (+ size (cdr entry)))
					  ;; (setq overall (+ size overall))))
			       ;; (setq dlist (cons (cons (cadar tail) 0) dlist)))
			     ;; (setq tail (cdr tail)))
			   ;; dlist)
			 ;; (lambda (a b) (> (cdr a) (cdr b)))))
	 ;; ;;1 KByte=2^10 Bytes
	 ;; ;;1 Mbyte=2^20 Bytes
	 ;; ;;1 GByte=2^30 Bytes
	 ;; ;;1 Tbyte=2^40 Bytes
	 ;; (breaks `(,(cons 1048576 "1 Gigabyte")
		   ;; ,(cons 512000 "500 Megabyte")
		   ;; ,(cons 204800 "200 Megabyte")
		   ;; ,(cons 102400 "100 Megabyte")
		   ;; ,(cons 1024 "1 Megabyte")
		   ;; ,(cons 0 "0 Byte")
		   ;; ,(cons -1 nil)))
	 ;; (break (car breaks))
	 ;; (first t)
	 ;; lastbreak)
    ;; (insert (make-string (window-width (selected-window)) (string-to-char "#")) 
	    ;; "\n\nSize of all assessable files below " dir "\n"
	    ;; "Gigabyte: " (number-to-string (/ overall (* 1024 1024 1024))) "\n"
	    ;; "Megabyte: " (number-to-string (/ overall (* 1024 1024))) "\n"
	    ;; "Kilobyte: " (number-to-string (/ overall 1024)) "\n\n"
	    ;; (make-string (window-width (selected-window)) (string-to-char "#")) "\n")
    ;; (while diaglist
      ;; (let* ((y (car diaglist))
	     ;; (name (car y))
	     ;; (size-in-kbytes (/ (cdr y) 1024)))
	;; (cond ((and first (>= size-in-kbytes (car break)))
	       ;; (insert "\n"
		       ;; (make-string (window-width (selected-window)) (string-to-char "~"))
		       ;; "\n"
		       ;; (cond ((= (car lastbreak) 0)
			      ;; "0 Bytes")
			     ;; (lastbreak
			      ;; (concat "Cumulative size (Kilobytes) between " (cdr break) " and " (cdr lastbreak)))
			     ;; (t (concat "Cumulative size (Kilobytes) >= " (cdr break))))
		       ;; "\n"
		       ;; (make-string (window-width (selected-window)) (string-to-char "~"))
		       ;; "\n\n")
	       ;; (insert name " : " (number-to-string size-in-kbytes) "\n")
	       ;; (setq first nil))
	      ;; ((>= size-in-kbytes (car break)) (insert name " : " (number-to-string size-in-kbytes) "\n"))
	      ;; (t (setq lastbreak break)
		 ;; (setq breaks (cdr breaks))
		 ;; (setq break (car breaks))
		 ;; (setq first t)))
	;; (if (not first) (setq diaglist (cdr diaglist))))))
  ;; (goto-char (point-min)))
;;}}}

(provide 'superman-file-list)

;; superman-file-list.el ends here

