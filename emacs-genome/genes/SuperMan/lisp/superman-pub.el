;;; superman-pub.el --- Superman views of project contents 

;; Copyright (C) 2013-2016  Thomas Alexander Gerds, Klaus Kaehler Holst

;; Authors: Thomas Alexander Gerds <tag@biostat.ku.dk>
;;          Klaus Kaehler Holst <kkho@biostat.ku.dk>
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

;; Code:

(defun superman-redo-bibtex-view ()
  "Redo the superman display of BibTeX file."
  (interactive)
  (let ((bib-marker (get-text-property (point-min) 'bib-marker))
	(bib-file  (get-text-property (point-min) 'bib-file)))
    (find-file bib-file)
    (superman-view-bibtex)))
    ;; (pro (get-text-property (point-min) 'pro))
    ;; (buffer-read-only nil))
  ;; (superman-prepare-bibtex-index bib-buf index-buf)
  ;; (superman-view-project pro t)
  ;; (superman-view-bibtex-mode)
  ;; (put-text-property (point-min) (1+ (point-min)) 'bib-marker bib-marker)))

(defun superman-view-bibtex ()
  "Show the contents of a BibTeX file in
electric table format."
  (interactive)
  (let* ((bib-buf (current-buffer))
	 (bib-file (buffer-file-name bib-buf))
	 (index-buf
	  (get-buffer-create
	   (concat
	    "*Superman:"
	    (file-name-sans-extension bib-file)
	    "*")))
	 (dir (file-name-directory bib-file))
	 (name (buffer-name bib-buf))
	 (redo 'superman-redo-bibtex-view)
	 (bib-marker (progn
		       (set-buffer bib-buf)
		       (goto-char (point-min))
		       (point-marker)))
	 pro)
    (superman-prepare-bibtex-index bib-buf index-buf)
    (set-buffer index-buf)
    (goto-char (point-min))
    (setq pro (list name
		    (list (cons "location" dir)
			  (cons "index" index-buf)
			  (cons "category" "Temporary")
			  (cons "others" nil)
			  (cons 'hdr nil)
			  (cons "marker" nil)
			  (cons "bibtex-file" bib-file)
			  (cons "lastvisit" nil)
			  (cons "config" nil)
			  (cons 'todo nil)
			  (cons "publish-directory" nil))))
    ;; (add-to-list 'superman-project-alist pro)
    (superman-view-project pro t redo)
    (superman-view-bibtex-mode)
    (let ((buffer-read-only nil))
      (put-text-property (point-min) (1+ (point-min)) 'bib-marker bib-marker)
      (put-text-property (point-min) (1+ (point-min)) 'bib-file bib-file))))


(defvar superman-view-bibtex-mode-map (make-sparse-keymap)
  "Keymap used for `superman-view-bibtex-mode' commands.")
   
(define-minor-mode superman-view-bibtex-mode
     "Toggle superman project view-bibtex mode.
With argument ARG turn superman-view-bibtex-mode on if ARG is positive, otherwise
turn it off.
                   
Enabling superman-view-bibtex mode electrifies the view buffer for bibtex files."
     :lighter " *S-view-bibtex*"
     :group 'org
     :keymap 'superman-view-bibtex-mode-map)

(defun superman-view-bibtex-mode-on ()
  (interactive)
  (superman-view-bibtex-mode t))
(define-key superman-view-bibtex-mode-map "N" 'superman-capture-bibtex)
(define-key superman-view-bibtex-mode-map "v" 'superman-bibtex-view-entry)
(define-key superman-view-bibtex-mode-map "e" 'superman-bibtex-edit-entry)

(defun superman-bibtex-view-entry ()
  (interactive)
  (let ((marker (superman-get-text-property
			  (get-text-property (point-at-bol) 'org-hd-marker) 'bib-marker)))
    (superman-view-edit-item t marker)))

(defun superman-bibtex-edit-entry ()
  (interactive)
  (let ((marker
	 (superman-get-text-property
	  (get-text-property (point-at-bol) 'org-hd-marker) 'bib-marker)))
    (superman-view-edit-item nil marker)))

(defun superman-capture-bibtex (&optional project marker ask)
  (interactive)
  (let* ((scene (current-window-configuration))
	 (bib-file (get-text-property (point-min) 'dir))
	 (bib-marker (or marker
			 (superman-get-text-property
			  (get-text-property (point-at-bol) 'org-hd-marker) 'bib-marker)
			 (progn
			   (find-file bib-file)
			   (goto-char (point-max))
			   (setq marker (point-marker)))))
	 ;; (pro (superman-get-project project ask))
	 (pro (superman-get-project nil))
	 (fields (or (superman-view-property-keys)
		     (list "title" "author" "journal" "year")))
	 (superman-setup-scene-hook
	  #'(lambda ()
	      (let ((inhibit-read-only t))
		(put-text-property (point-min) (1+ (point-min)) 'clean-hook
				   'superman-clean-bibtex))))
	 (template "@article{,"))
    ;; (bibtex-entry)
    (while fields
      (setq template (concat template "\n" (car fields) "={},")
	    fields (cdr fields)))
    (setq template (concat template "\n}"))
    (superman-capture
     pro bib-marker "bibtex"
     template nil 0 scene)))

(defun superman-prepare-bibtex-index (bib-buf index-buf)
  "Parse all bibtex entries in buffer BIB-BUF and
write the result to buffer INDEX-BUF."
  (let (fields
	bib-marker
	(pos -1))
    (set-buffer index-buf)
    (erase-buffer)
    (insert "*** Contents of BibTeX file " (buffer-file-name bib-buf) ": \n:PROPERTIES:\n:ProjectStart: now\n:END:\n")
    (insert "To add a new entry press `N'\n\n")
    (insert "* BibTeX\n"
	    ":PROPERTIES:\n"
	    ":CaptureButtons: New entry|superman-capture-bibtex,Visit bibtex|superman-visit-bibtex\n"
	    ":ConfigButtons: nil\n"
	    ;; ":Ball1: org-hd-marker :fun superman-show-plain\n"
	    ":Ball1:    year  :width 4\n"
	    ":Ball2:    author  :width 13\n"
	    ":Ball3:    journal :width 30\n"
	    ":Ball4:    title  :width 50\n"
	    ;; ":buttons: superman-pub-make-sort-buttons\n"
	    ":END:\n\n")
    (org-mode)
    (set-buffer bib-buf)
    ;; (insert-buffer bib-buf)
    (goto-char (point-min))
    (while (and (bibtex-skip-to-valid-entry)
		(> (point) pos))
      (setq bib-marker (point-marker))
      (setq pos (point-at-eol))
      ;; (ignore-errors
      (setq fields (superman-bibtex-parse-entry))
      (set-buffer index-buf)
      (goto-char (point-max))
      (insert "*** " (cdar fields))
      (put-text-property (point-at-bol) (point-at-eol) 'bib-marker bib-marker)
      (insert "\n" ":PROPERTIES:\n")
      (setq fields (cdr fields))
      (while fields
	(let ((prop (caar fields))
	      (val (cdar fields)))
	  (insert ":" prop ": ")
	  (insert val "\n")
	  (setq fields (cdr fields))))
      (insert ":END:\n")
      (set-buffer bib-buf))
    (set-buffer index-buf)
    (goto-char (point-min))))

(defun superman-visit-bibtex ()
  (interactive)
  (find-file
   (get-text-property (point-min) 'bib-file)))

(defun superman-clean-bibtex ()
  (message "Don't know how to clean yet."))
  ;; (goto-char (next-single-property-change (point-min) 'header-end))
  ;; (re-search-forward "@" nil t)
  ;; (while (re-search-forward "={}" nil t)
;; FIXME: this is a rather crude and time-consuming hack:
;; (set-buffer
;; (marker-buffer
;; (get-text-property
;; (next-single-property-change (point-min) 'destination) 'destination))))

(defun superman-bibtex-parse-entry ()
  (interactive)
  (let* ((start (bibtex-beginning-of-entry))
	 (end (bibtex-end-of-entry))
	 (type (progn
		 (goto-char start)
		 (looking-at bibtex-entry-head)
		 (setq type (match-string-no-properties 1))))
	 (bibkey (match-string-no-properties 2))
	 ;; (bib  (buffer-substring start end))
	 done
	 previous
	 next
	 fields)
    (goto-char start)
    (while (re-search-forward "^[ \t\n]*\\(.*\\)[ \t\n]*=[ \t\n]*\\(.*\\)[,]?$" end t)
      (let ((field-key
	     (replace-regexp-in-string "^[ \t]*\\|[ \t]*$" ""
				       (match-string-no-properties 1)))
	    (field-val
	     (replace-regexp-in-string "[{},]*" ""
				       (match-string-no-properties 2))))
	(setq fields
	      (append fields
		      (list (cons field-key field-val))))
	(setq previous next)
	(if (> (point) end)
	    (setq done t))))
    (append `(("key" . ,bibkey)) fields)))

    ;; (while (and (not done)
		;; (setq next (bibtex-next-field 1))
		;; (setq next (bibtex-next-field 1))
		;; (not (string= next previous))
		;; (not (string= next "Entry key")))
      ;; (let* ((field-info (bibtex-find-text-internal t nil t))
	     ;; (field-key (when field-info
			  ;; (downcase
			   ;; (car field-info))))
	     ;; (end-of-field (nth 2 field-info))
	     ;; (field-val (when field-info
			  ;; (replace-regexp-in-string
			   ;; "[\n}{]*" ""
			   ;; (buffer-substring-no-properties
			    ;; (nth 1 field-info)
			    ;; end-of-field)))))
	;; (setq fields
	      ;; (append fields
		      ;; (list (cons field-key field-val))))
	;; (setq previous next)
	;; (if (> (point) end)
	    ;; (setq done t))))
    ;; (append `(("key" . ,bibkey)) fields)))

;; (when plain
;; (insert "\n**** Plain\n"
;; (replace-regexp-in-string "\\[1\\]" ""
;; (car (split-string plain "=+")))))
;; (insert "\n**** BibTeX \n")))
;; (if (eq cmode 'org-mode)
;; (let ((parsed-text (buffer-string)))
;; (switch-to-buffer cbuf)
;; (insert parsed-text))
;; (copy-region-as-kill (point-min) (point-max))
;; (pop-to-buffer cbuf)
;; (message "Buffer-string copied to kill-ring"))))
      
  
(defun superman-bibtex2text (bib-string)
  (interactive)
  (save-window-excursion
    (find-file "/tmp/superman-pub.bib")
    (erase-buffer)
    (insert bib-string)
    (save-buffer)
    (shell-command  "bibtex2html -nobibsource -o /tmp/superman-pub /tmp/superman-pub.bib")
    (shell-command-to-string "html2text -ascii /tmp/superman-pub.html")))
  
(defun superman-show-bibtex (&optional marker &rest args)
  (org-with-point-at (or marker (point))
    (save-restriction
      (org-narrow-to-subtree)
      (re-search-forward "BibTeX" nil t)
      (forward-line 1)
      (replace-regexp-in-string
       "^[ \t]*" ""
       (buffer-substring
       (point)
       (progn (forward-paragraph) (point)))))))

(defun superman-show-plain (&optional marker &rest args)
  "Show citation in plain format."
  (if (markerp marker)
      (let* ((pdf (superman-get-property marker "pdf"))
	     (pdf-string (when pdf
			   (string-match org-bracket-link-regexp pdf)
			   (org-make-link-string
			    (org-match-string-no-properties 1 pdf)
			    "pdf")))
	     (plain (org-with-point-at marker
		      (save-restriction
			(org-narrow-to-subtree)
			(re-search-forward "Plain" nil t)
			(forward-line 1)
			(replace-regexp-in-string
			 "^[ \t]*" ""
			 (buffer-substring
			  (point)
			  (progn (forward-paragraph) (point))))))))
	(if pdf
	    (concat plain pdf-string)
	  plain))
    marker))

(defun superman-pub-make-sort-buttons (&optional names)
  (let ((names (or names '("year" "author" "journal" "title"))))
    (while names
      (let* ((name (car names))
	     (sort-cmd (concat "sort-publications-by-" name))
	     (map (make-sparse-keymap)))
	(define-key map [mouse-2]
	  `(lambda () (interactive)
	     (superman-sort-publications-by ,name)))
	(define-key map [return]
	  `(lambda () (interactive)
	     (superman-sort-publications-by ,name)))
	(add-text-properties
	 0 (length name) 
	 (list
	  'button (list t)
	  'face 'superman-warning-face
	  'keymap map
	  'mouse-face 'highlight
	  'follow-link t
	  'help-echo sort-cmd)
	 name)
	(insert name " "))
      (setq names (cdr names)))))

(defun superman-sort-publications-by (&optional column)
  "Idea is simple but not very efficient:
first produce a single column view of the section, then
sort the section, and finally play the original balls.
"
  (let ((cat-head (superman-cat-point))
	(column (or column "year"))
	(buffer-read-only nil))
    (if (not cat-head)
	(message "Nothing to sort here")
      (let ((cat-tail (or (next-single-property-change (point) 'cat) (point-max)))
	    (balls (get-text-property cat-head 'balls))
	    (temp-balls `((,column ("width" 4)))))
	;; (delete-region cat-head cat-tail)
	(save-excursion
	  (superman-change-balls temp-balls)
	  (superman-refresh-cat temp-balls)
	  (goto-char (previous-single-property-change (point) 'columns))
	  (superman-sort-section)
	  (superman-change-balls balls)
	  (superman-refresh-cat balls))))))
				 
  
(defun superman-find-bibtex (&optional regexp)
  (interactive)
  (let ((regexp (or regexp
		    (read-string "Re-search-forward: " "year.*=.*2013"))))
    (get-buffer-create "*Superman-bibtex-match*")
    (with-current-buffer "*Superman-bibtex-match*"
      (erase-buffer)
      (unless (eq major-mode 'bibtex-mode)
	(bibtex-mode)))
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (bibtex-mark-entry)
      (call-interactively 'copy-region-as-kill)
      (with-current-buffer "*Superman-bibtex-match*"
	(yank)
	(insert "\n\n")))
    (switch-to-buffer  "*Superman-bibtex-match*")))

(provide 'superman-pub)

;;; superman-pub.el ends here
