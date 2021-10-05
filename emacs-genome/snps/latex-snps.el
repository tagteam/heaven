;;; latex-snps.el --- custom latex for emacs-genome 

;; Copyright (C) 2012  Thomas Alexander Gerds

;; Author: Thomas Alexander Gerds <tag@biostat.ku.dk>
;; Keywords: tex, convenience

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

;; LaTeX and reftex settings
;; Use M-j to latex or latexmk a document

;;; Code:

;;{{{ load auctex
;; (add-to-list  'load-path (concat emacs-genome "/genes/auctex/style"))
;; (add-to-list  'load-path (concat emacs-genome "/genes/auctex/preview"))

;; (unless (ignore-errors (load "auctex.el" nil t t))
;; (message "Auctex not loaded"))
;; (ignore-errors (load "preview-latex.el" nil t t))
;;}}}
;;{{{ LaTeX mode hook
(add-hook 'LaTeX-mode-hook
	  '(lambda ()
	     (LaTeX-math-mode)
	     (TeX-source-correlate-mode)
	     (setq TeX-master t)
	     (TeX-PDF-mode t)
	     (define-key LaTeX-mode-map "\C-ce" 'TeX-next-error)
	     (define-key LaTeX-mode-map "\M-k" 'save-buffer)
	     (define-key LaTeX-mode-map "\M-q" 'eg/indent-paragraph)
	     (define-key LaTeX-mode-map "\M-j" 'eg/latex-save-and-run)))
;;}}}
;;{{{ TeX shell and master and custom 
(setq TeX-shell "/bin/bash")
(setq-default TeX-master nil) ; Query for master file.
(setq TeX-parse-self t) 
(setq TeX-auto-save t)
(setq TeX-file-extensions
      '("Snw" "Rnw" "nw" "tex" "sty" "cls" "ltx" "texi" "texinfo"))
(setq reftex-try-all-extensions t)
;;}}}
;;{{{ reftex
(autoload 'reftex-mode    "reftex" "RefTeX Minor Mode" t) 
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil) 
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq reftex-file-extensions '(("Snw" "Rnw" "nw" "tex" ".tex" ".ltx") ("bib" ".bib")))
;;}}}
;;{{{ bibtex mode
(setq bibtex-autokey-edit-before-use nil)
(setq bibtex-autokey-prefix-string ""
      bibtex-autokey-names 3
      bibtex-autokey-names-stretch 3
      bibtex-autokey-name-length 'infty
      bibtex-autokey-name-separator "_"
      bibtex-autokey-name-case-convert 'capitalize
      bibtex-autokey-name-year-separator "_"
      bibtex-autokey-year-title-separator "_"
      bibtex-autokey-year-length 4 
      bibtex-autokey-titleword-case-convert 'capitalize
      bibtex-autokey-titlewords 3
      bibtex-autokey-titlewords-stretch 3
      bibtex-autokey-titleword-length 10
      bibtex-maintain-sorted-entries nil)

(defun fix-umlaute-in-string (string)
  (interactive "p")
  (setq string (replace-regexp-in-string "√∏\\|√∏\\|Å¯\\|\370\\|\366" "oe" string))
  (setq string (replace-regexp-in-string "√ò÷\\|Åÿ\\|\330" "Oe" string))
  (setq string (replace-regexp-in-string "√¶\\|\344\\|\201\346\\|\346\\|\303\246" "ae" string))
  (setq string (replace-regexp-in-string "√Ü\\|ƒ\\|\306" "AE" string))
  (setq string (replace-regexp-in-string "√•\\|\201\345\\|\345\\|\305" "aa" string))
  (setq string (replace-regexp-in-string "√Ö\\|\201\345\\|\345\\|\305" "Aa" string))
  (setq string (replace-regexp-in-string "√§\\|¸" "ae" string))
  (setq string (replace-regexp-in-string "√Ñ\\|‹" "Ae" string))
  (setq string (replace-regexp-in-string "√∂\\|¸" "oe" string))
  (setq string (replace-regexp-in-string "√ñ\\|‹" "Oe" string))
  (setq string (replace-regexp-in-string "√º\\|¸" "ue" string))
  (setq string (replace-regexp-in-string "√ú\\|‹" "Ue" string))
  (setq string (replace-regexp-in-string "√ü\\|ﬂ" "ss" string)))

(defun my-bibtex-generate-autokey ()
  (let* ((names (ignore-errors (bibtex-autokey-get-names)))
         (year (bibtex-autokey-get-year))
         ;; (title (bibtex-autokey-get-title))
         (autokey (concat year
                          (unless (or (equal (or names "UNKNOWN") "")
                                      (equal year ""))
                            bibtex-autokey-name-year-separator)
                          names)))
    (fix-umlaute-in-string autokey)))

(defun my-bibtex-clean-entry (&optional new-key called-by-reformat)
  "copy of bibtex-clean-entry"
  (interactive "P")
  (let ((case-fold-search t)
        (start (bibtex-beginning-of-entry))
        (_ (or (looking-at bibtex-any-entry-maybe-empty-head)
	       (error "Not inside a BibTeX entry")))
        (entry-type (bibtex-type-in-head))
        (key (bibtex-key-in-head)))
    (bibtex-format-entry)
    ;; set key
    (if (or new-key (not key))
        (save-excursion
          ;; First delete the old key so that a customized algorithm
          ;; for generating the new key does not get confused by the
          ;; old key.
          (re-search-forward (if (eq entry-type 'string)
                                 bibtex-string-maybe-empty-head
                               bibtex-entry-maybe-empty-head))
          (if (match-beginning bibtex-key-in-head)
              (delete-region (match-beginning bibtex-key-in-head)
                             (match-end bibtex-key-in-head)))
          (setq key (my-bibtex-generate-autokey))
          ;; Sometimes `bibtex-generate-autokey' returns an empty string
          (if (or bibtex-autokey-edit-before-use (string= "" key))
              (setq key (if (eq entry-type 'string)
                            (bibtex-read-string-key key)
                          (bibtex-read-key "Key to use: " key))))
	  (let ((i 0) (test-key key))
	    (while
		;; (assoc-string test-key bibtex-reference-keys)
		(save-excursion (goto-char (point-min))
				(and (search-forward test-key nil t)
				     (looking-at ",")))
	      (setq i (1+ i) test-key (concat key "-" (int-to-string i)))
	      )
	    (setq key test-key))
          (insert key)))
    (unless called-by-reformat
      (let* ((end (save-excursion
                    (bibtex-end-of-entry)
                    (if (re-search-forward
                         bibtex-entry-maybe-empty-head nil 'move)
                        (goto-char (match-beginning 0)))
                    (point)))
             (entry (buffer-substring start end))
             ;; include the crossref key in index
             (index (let ((bibtex-maintain-sorted-entries 'crossref))
                      (bibtex-entry-index))) ; moves point to end of head
             error)
        ;; sorting
        ;; (if (and bibtex-maintain-sorted-entries
	;; (not (and bibtex-sort-ignore-string-entries
	;; (eq entry-type 'string))))
	;; (progn
	;; (delete-region start end)
	;; (setq error (not (bibtex-prepare-new-entry index))
	;; start (point)) ; update start
	;; (save-excursion (insert entry)))
	;; (bibtex-search-entry key)
	;; (setq error (or (/= (point) start)
	;; (bibtex-search-entry key nil end))))
        ;; (if error
	;; (error "New inserted entry yields duplicate key"))
        (dolist (buffer (bibtex-initialize))
          (with-current-buffer buffer
            (if (cdr (assoc-string key bibtex-reference-keys))
                (error "Duplicate key in %s" (buffer-file-name)))))
        ;; Only update `bibtex-strings' and `bibtex-reference-keys'
        ;; if they have been built already.
        (cond ((eq entry-type 'string)
               ;; We have a @String entry.
               (unless (or (functionp bibtex-strings)
                           (assoc key bibtex-strings))
                 (push (cons key (bibtex-text-in-string
                                  (bibtex-parse-string) t))
                       bibtex-strings)))
              ;; We have a normal entry.
              ((not (functionp bibtex-reference-keys))
               (let ((found (assoc key bibtex-reference-keys)))
                 (cond ((not found)
                        (push (cons key t) bibtex-reference-keys))
                       ((not (cdr found))
                        ;; Turn a crossref key into a header key
                        (setq bibtex-reference-keys
                              (cons (cons key t)
                                    (delete (list key) bibtex-reference-keys))))))
               ;; If entry has a crossref key, it goes into the list
               ;; `bibtex-reference-keys', too.
               (if (and (nth 1 index)
                        (not (assoc (nth 1 index) bibtex-reference-keys)))
                   (push (list (nth 1 index)) bibtex-reference-keys))))))))


;; (load-file  "~/research/Admin/AnnualReport/lisp/BibtexToWiki.el")
(defun my-bibtex-autogenerate-keys ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "@\\([a-z]+\\)[ \t]*{" nil t)
      (looking-at ".*$")
      (message (match-string-no-properties 0)) 
      ;; insert new key
      (my-bibtex-clean-entry t)
      (goto-char (bibtex-end-of-entry)))))
;; (bib2wiki-fix-key name type year pages journal))))

(defun custom-set-difference (a b)
  (remove-if
     #'(lambda (x) (and (member x a) (member x b)))
     (append a b)))



;; (my-bibtex-diff "Pure publications - 29052017.bib" "tag-publications.bib")


(defun my-bibtex-diff (&optional buf-1 buf-2)
  (let* ((buf-1 (or (get-buffer buf-1) (read-buffer "Compare bibtex in buffer 1: ")))
	 (keys-1 (progn (set-buffer buf-1) (bibtex-parse-keys)))
	 (buf-2 (or (get-buffer buf-2) (read-buffer "With bibtex in buffer 2: ")))
	 (keys-2 (progn (set-buffer buf-2) (bibtex-parse-keys)))
	 (diff (custom-set-difference keys-1 keys-2))
	 (res (get-buffer-create "diff.bib"))
	 entry)
    (set-buffer res)
    (erase-buffer)
    (bibtex-mode)
    (while diff 
      (set-buffer buf-2)
      (goto-char (point-min))
      (if (or (bibtex-search-entry (caar diff))
	      (bibtex-search-entry (fix-umlaute-in-string (caar diff))))
	  (save-excursion
	    (setq entry (buffer-substring (bibtex-beginning-of-entry) (bibtex-end-of-entry)))
	    (set-buffer res)
	    (insert "@Comment only in " (buffer-name buf-2) "\n" entry "\n"))
	(set-buffer buf-1)
	(goto-char (point-min))	
	(when (or (bibtex-search-entry (caar diff))
		  (bibtex-search-entry (fix-umlaute-in-string (caar diff))))
	  (save-excursion
	    (setq entry (buffer-substring (bibtex-beginning-of-entry) (bibtex-end-of-entry)))
	    (set-buffer res)
	    (insert "@Comment only in " (buffer-name buf-1) "\n" entry "\n"))))
      (setq diff (cdr diff)))
    (switch-to-buffer res)))
;;}}}
;;{{{ latex run command
(defun eg/latex-save-and-run ()
  "Efficiently combine two actions:

 (1) save the current buffer
 (2) run the TeX-command-master.

"
  (interactive)
  (save-buffer)
  (TeX-command-master))
;;}}}
;;{{{ latexmk
(add-hook 'LaTeX-mode-hook
	  '(lambda ()
	     (add-to-list 'TeX-command-list '("make" "latexmk -f %t" TeX-run-TeX  nil "nil") t)
	     ;; (add-to-list 'TeX-command-list '("make-dvi" "latexmk -pvc -dvi -f %t" TeX-run-TeX  nil "nil") t)
	     ;; (add-to-list 'TeX-command-list '("make-ps"  "latexmk -cd -pvc -ps -f %t" TeX-run-TeX  nil "nil") t)
	     (add-to-list 'TeX-command-list '("make-pdf" "latexmk -pvc -pdf -f %t" TeX-run-TeX  nil "nil") t)
	     (add-to-list 'TeX-command-list '("make-pdf-cd" "latexmk -pvc -cd -pdf -f %t" TeX-run-TeX  nil "nil") t)
	     (add-to-list 'TeX-command-list '("make-ps2pdf" "latexmk -pvc -pdfps -f %t" TeX-run-TeX  nil "nil") t)
	     ;; (add-to-list 'TeX-command-list '("make-dvi-landscape" "latexmk -pvc -l -dvi -f %t" TeX-run-TeX  nil "nil") t)
	     ;; (add-to-list 'TeX-command-list '("make-ps-landscape" "latexmk  -pvc -l -ps -f %t" TeX-run-TeX  nil "nil") t)
	     (add-to-list 'TeX-command-list '("make-pdf-landscape" "latexmk -pvc -cd -l -pdf -f %t" TeX-run-TeX  nil "nil") t)
	     (add-to-list 'TeX-command-list '("make-ps2pdf-landscape" "latexmk -pvc -l -pdfps -f %t" TeX-run-TeX  nil "nil") t)))
;;}}}


(provide 'latex-snps)
;;; latex-snps.el ends here
