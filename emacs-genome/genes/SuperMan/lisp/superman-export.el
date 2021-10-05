;;; superman-export.el --- Buttonized org to latex export

;; Copyright (C) 2014-2020  Thomas Alexander Gerds

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

;; Org provides a series of export functions that are invoked
;; via C-c C-e. The case where an org document
;; is exported to pdf via latex is special because the exported
;; latex document needs to be compiled and thus errors can occur
;; on different stages. The superman feature defined below
;; provides a control buffer with buttons which guide to:
;;
;;  - the next LaTeX error
;;  - the first LaTeX error
;; 
;; and for documents that include R-code there are further buttons
;; to find the next/previous error in the inferior ESS buffer.
;; 
;;

;;; Code:

;; see also superman-run-R-or-export-as in org-snps.el
(defun superman-control-export (&optional arg)
  "If inside babel src code block, evaluate block
with `superman-ess-eval-and-go' and otherwise export buffer
with `superman-org-export-as'."
  (interactive "p")
  (if (and (string= (car (org-babel-get-src-block-info)) "R")
	   ;; since org-element-at-point finds the nearest element,
	   ;; need to test if really sitting inside the block:
	   (org-babel-where-is-src-block-head))
      (superman-ess-eval-and-go arg)
    ;; (progn
    ;; (ess-switch-to-end-of-ESS)
    ;; (when arg (erase-buffer) (inferior-ess-send-input)))
    (cond ((save-excursion
	     (goto-char (point-min))
	     (when (re-search-forward "^\\#\\+superman-export-target:[ \t]*" nil t)
	       (let ((this (buffer-substring-no-properties
			    (point)
			    (progn (skip-chars-forward "[a-zA-z/]")
				   (point)))))
		 (setq superman-org-export-target this))))
	   (superman-org-export-as superman-org-export-target arg))
	  ((and (boundp 'superman-org-export-target) (string= superman-org-export-target "pdf"))
	   (superman-export-as-latex arg))
	  (t (superman-org-export-as "pdf" arg)))))

;; See library tex-buf for help on TeX-process.
(defun superman-export-as-latex (&optional debug)
  "Export current org document to latex and further to pdf via latexmk.
If no process is running start the pearl script latexmk.

This function works outside R src blocks. Inside R src block
 it calls `superman-ess-eval-and-go'."
  (interactive "P")
  (let* ((org-buf (current-buffer))
	 (raw-file (superman-file-name nil nil t ""))
	 (tex-file (concat raw-file ".tex"))
	 (org-file (concat raw-file ".org"))
	 (tex-buf (get-file-buffer tex-file))
	 ;; (help-buf (concat "*Superman-export-control: " (file-name-nondirectory org-file) "*"))
	 (wconf (current-window-configuration))
	 (process (TeX-process tex-file))
	 R-buf
	 R-proc)
    (save-buffer)
    ;; we silently assume that the user wants to overwrite
    ;; the current tex file and to avoid that the user has
    ;; to confirm this -- in case of an open tex-buffer -- we
    ;; kill a possibly existing tex-buffer
    (when tex-buf
      (save-excursion
	(set-buffer tex-buf)
	(if (not (file-exists-p tex-file))
	    (kill-buffer tex-buf)
	  (revert-buffer t t t)
	  (kill-buffer (get-file-buffer tex-file)))))
    ;; find R process
    (when debug
      (switch-to-buffer org-buf)
      (setq R-proc (and ess-current-process-name (get-process ess-current-process-name)))
      (if R-proc (setq R-buf (buffer-name (process-buffer R-proc)))
	(save-excursion
	  (goto-char (point-min))
	  (while (and (not R-buf)
		      (re-search-forward org-block-regexp nil t))
	    (beginning-of-line) ;; move inside block
	    (let ((info (org-babel-get-src-block-info)))
	      (and (string= (car info) "R")
		   (setq R-buf (cdr (assoc ':session (caddr info))))))))))
    (when (and R-buf (buffer-live-p R-buf))
      (with-current-buffer R-buf
	(let ((ess-current-process-name R-buf))
	  (ess-switch-to-end-of-ESS)
	  (put-text-property (point-at-bol) (1+ (point-at-bol)) 'eval-point t))))
    ;; export to latex or beamer
    (if org-beamer-mode
	(org-beamer-export-to-latex)
      (org-latex-export-to-latex))      
    ;; check/start latexmk process
    ;; (message "export")
    (if process
	(cond (debug (delete-process process)
		     (find-file tex-file)
		     (TeX-command "LaTeX" 'TeX-master-file nil))  ;; run latex to identify problems
	      ((not (eq (process-status process) 'run));; kill not running process
	       (delete-process process)
	       (TeX-run-TeX "make-pdf" (concat "latexmk -pvc -pdf -f " tex-file) tex-file))
	      (t (message (concat "Currently running process: " (process-name process)))))
      (if debug ;; run latex to identify problems
	  (progn
	    (find-file tex-file)
	    (superman-export-header-mode)
	    (TeX-command "LaTeX" 'TeX-master-file nil))
	(TeX-run-TeX "make-pdf" (concat "latexmk -pvc -pdf -f " tex-file) tex-file)))
    ;; (message "process")
    (when debug
      (if  R-buf
	  (superman-set-config
	   (concat (buffer-name org-buf) " | " tex-file " / " R-buf))
	(superman-set-config
	 (concat (buffer-name org-buf) " | " tex-file))))))
(fset 'superman-export-as-pdf 'superman-export-as-latex)

;;{{{ superman org headline buttons

(defvar superman-org-headline-map (make-sparse-keymap)
  "Keymap for `superman-org-headline-mode', a minor mode.
Use this map to set additional keybindings for when Org-mode is used.")

(defvar superman-org-headline-mode-hook nil
  "Hook for the minor `superman-org-headline-mode'.")

(defvar superman-org-export-target-list '("pdf" "html" "docx")
  "Export targets.")

(defvar superman-org-export-target "pdf" "current export target")

(defvar superman-babel-target-list '("this-block" "all-blocks")
  "Babel R-block targets.")
(defvar superman-babel-target "all-blocks" "Either 'all-blocks': action on all R-blocks\n or 'this-block': action on current R-block.")

(defun superman-org-export-change-target ()
  (interactive)
  (setq
   superman-org-export-target
   (cadr superman-org-export-target-list)
   superman-org-export-target-list
   (append (cdr superman-org-export-target-list)
	   (list (car superman-org-export-target-list))))
  (superman-org-headline-mode))


(defun superman-babel-change-target ()
  (interactive)
  (setq
   superman-babel-target
   (cadr superman-babel-target-list)
   superman-babel-target-list
   (append (cdr superman-babel-target-list)
	   (list (car superman-babel-target-list))))
  (superman-org-headline-mode))


(defun superman-org-export-as (&optional target arg)
  "Find and apply target specific export function. Targets are defined 
in `superman-org-export-target-list' and for target TARGET the export function 
is either called superman-export-as-TARGET or org-export-to-TARGET."
  (let* ((target superman-org-export-target)
	 (super-candidate (intern (concat "superman-export-as-" target)))
	 (org-candidate (intern (concat "org-" target "-export-to-" target))))
    (cond ((functionp super-candidate)
	   (funcall super-candidate arg))
	  ((functionp org-candidate)
	   (funcall org-candidate arg))
	  (t (message (concat "Don't know how to export to " target))))))

(define-minor-mode superman-org-headline-mode
  "Minor mode for headline buttons in header line in org buffers."
  nil "" superman-org-headline-map
  (make-local-variable 'superman-org-export-target)
  (make-local-variable 'superman-babel-target)
  (make-local-variable 'superman-org-export-target-list)
  (make-local-variable 'superman-babel-target-list)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\\#\\+superman-export-target:[ \t]*" nil t)
      (let ((this (buffer-substring-no-properties
		   (point)
		   (progn (skip-chars-forward "[a-zA-z/]")
			  (point)))))
	(setq superman-org-export-target this))))
  (setq-local
   header-line-format
   (concat (header-button-format (concat "M-J:" (or superman-org-export-target "not set")) :action
				 #'(lambda (&optional arg) (interactive)
				     (superman-org-export-change-target)))
	   " "
	   (header-button-format "export" :action
				 #'(lambda (&optional arg) (interactive)
				     (superman-org-export-as nil)))
	   (when (string= superman-org-export-target "pdf")
	     (concat
	      " "
	      (header-button-format "debug" :action
				    #'(lambda (&optional arg) (interactive)
					(superman-export-as-latex t)))))
	   " "
	   (header-button-format "view" :action
				 #'(lambda (&optional arg) (interactive)
				     (let* ((ext (or superman-org-export-target "pdf"))
					    (target (concat (file-name-sans-extension (buffer-file-name)) "." ext)))
				       (if (file-exists-p target)
					   (org-open-file target)
					 (message (concat "No such file: " target))))))
	   " | "
	   (header-button-format (concat "R:" superman-babel-target) :action
				 #'(lambda (&optional arg) (interactive)
				     (superman-babel-change-target)))
	   " "
	   (header-button-format "run" :action
				 #'(lambda (&optional arg) (interactive)
				     (cond ((string= superman-babel-target "this-block")
					    (beginning-of-line)
					    ;; (if (string= (car (org-babel-get-src-block-info)) "R")
					    (if (org-babel-get-src-block-info)
						(org-babel-execute-src-block)
					      ;; (superman-ess-eval-and-go)
					      (message "Cursor is not in src-block")))
					   ((string= superman-babel-target "all-blocks")
					    (org-babel-execute-buffer)))))
	   " "
	   (header-button-format "clear"  :action
				 #'(lambda (&optional arg) (interactive)
				     (cond ((string= superman-babel-target "all-blocks")
					    (org-babel-clear-all-results))
					   ((string= superman-babel-target "this-block")
					    (if (org-babel-get-src-block-info)
						(org-babel-remove-result)
					      ;; (superman-ess-eval-and-go)
					      (message "Cursor is not in src-block"))))))

	       " "
	       (header-button-format "new" :action
				     #'(lambda (&optional arg) (interactive)
					 (beginning-of-line)
					 (let ((buf  (buffer-name)))
					   (if (org-babel-get-src-block-info)
					       (progn (org-babel-goto-src-block-head)
						      (re-search-forward org-babel-src-block-regexp nil t)
						      (insert "\n"))
					     (unless (looking-at "^[ \t\n]*$")
					       (insert "\n")
					       (forward-line -1)))
					   (insert "<Rr")
					   (org-cycle))))
	       ;; (superman-create-R-block-help-buffer)
	       ;; (superman-set-config (concat buf " / *Superman:R-block help*")))))
	       " "
	       (header-button-format "graph" :action
				     #'(lambda (&optional arg) (interactive)
					 (beginning-of-line)
					 (let ((buf  (buffer-name)))
					   (if (org-babel-get-src-block-info)
					       (progn (org-babel-goto-src-block-head)
						      (re-search-forward org-babel-src-block-regexp nil t)
						      (insert "\n"))
					     (unless (looking-at "^[ \t\n]*$")
					       (insert "\n")
					       (forward-line -1)))
					   (insert "<Rg")
					   (org-cycle))))
	       ;; (superman-create-R-block-help-buffer)
	       ;; (superman-set-config (concat buf " / *Superman:R-block help*")))))
	       ;; " "
	       ;; (header-button-format "clear-all" :action
	       ;; #'(lambda (&optional arg) (interactive)
	       ;; (org-babel-clear-all-results)))
	       ;; " "
	       ;; (header-button-format "run-all" :action
	       ;; #'(lambda (&optional arg) (interactive)
	       ;; (beginning-of-line)
	       ;; (org-babel-execute-buffer)))
	       ;; " "
	       )))

(defun superman-create-R-block-help-buffer ()
  (unless (get-buffer "*Superman:R-block help*")
    (get-buffer-create "*Superman:R-block help*")
    (set-buffer  "*Superman:R-block help*")
    (insert "Header arguments:\n\n"
	    ":exports code\n"
	    "    The default in most languages. The body of the code block is exported, as described in Literal examples.\n\n"
	    ":exports results\n"
	    "    The code block will be evaluated and the results will be placed in the Org mode buffer for export,\n    either updating previous results of the code block located anywhere in the buffer\n    or, if no previous results exist, placing the results immediately after the code block. \n    The body of the code block will not be exported.\n\n"
	    ":exports both\n"
	    "    Both the code block and its results will be exported.\n\n"
	    ":exports none\n"
	    "    Neither the code block nor its results will be exported.\n\n"
	    ":results raw output drawer\n"
	    "    Interprete results as org-code (drawer will only be exported if d:t in file header) \n\n"
	    ":results output\n"
	    "    Do not interprete output.\n\n"
	    )
    (setq buffer-read-only t)))


(easy-menu-define superman-menu org-mode-map "Superman's org-mode"
  `("S-Org"
    [,(concat "Mode:" (or superman-org-export-target "not set")) nil nil]
    ["export"  (lambda (&optional arg) (interactive)
		 (superman-org-export-as nil)) t]
    ["view"
     (lambda (&optional arg) (interactive)
       (let* ((ext (or superman-org-export-target "pdf"))
	      (target (concat (file-name-sans-extension (buffer-file-name)) "." ext)))
	 (if (file-exists-p target)
	     (org-open-file target)
	   (message (concat "No such file: " target))))) t]
    ["-----" nil nil]
    ["R-target"
     (lambda (&optional arg) (interactive)
       (superman-babel-change-target)
       (message (concat "New target: " superman-babel-target)))  t]
    ["run"  (lambda (&optional arg) (interactive)
	      (cond ((string= superman-babel-target "this-block")
		     (beginning-of-line)
		     ;; (if (string= (car (org-babel-get-src-block-info)) "R")
		     (if (org-babel-get-src-block-info)
					 (org-babel-execute-src-block)
		       ;; (superman-ess-eval-and-go)
				       (message "Cursor is not in src-block")))
		    ((string= superman-babel-target "all-blocks")
		     (org-babel-execute-buffer)))) t]
    ["clear"
     (lambda (&optional arg) (interactive)
	 (cond ((string= superman-babel-target "all-blocks")
		(org-babel-clear-all-results))
	       ((string= superman-babel-target "this-block")
		(if (org-babel-get-src-block-info)
		    (org-babel-remove-result)
		  ;; (superman-ess-eval-and-go)
		  (message "Cursor is not in src-block"))))) t]))

  ;;}}}
;;{{{ superman latex header line buttons

(defvar superman-export-header-map (make-sparse-keymap)
  "Keymap for `superman-export-header-mode', a minor mode.
Use this map to set additional keybindings for when superman-export-header-mode is used.")


;; (defvar superman-export-header-mode-hook nil
;; "Hook for the minor `superman-export-header-mode'.")

(define-minor-mode superman-export-header-mode
  "Minor mode which shows export and evaluation header buttons in org buffers."
  nil "" superman-export-header-map
  (setq-local
   header-line-format
   (concat "Run: "
	   (header-button-format "LaTeX" :action 
				 #'(lambda (&optional arg) (interactive) 
				     (TeX-command "LaTeX" 'TeX-master-file nil)))
	   " "
	   (header-button-format "BibTeX" :action
				 #'(lambda (&optional arg) (interactive)
				     (TeX-command "BibTeX" 'TeX-master-file nil)))
	   " "
	   (header-button-format "Make-pdf" :action
				 #'(lambda (&optional arg) (interactive)
				     ;; (TeX-run-TeX "make-pdf" (concat "latexmk -pvc -pdf -f " tex-file) tex-file)))
				     (TeX-command "make-pdf" 'TeX-master-file nil)))
	   " Error: "
	   (header-button-format "next" :action
				 #'(lambda (&optional arg) (interactive)
				     (superman-next-latex-error)))
	   " "
	   (header-button-format "first" :action 
				 #'(lambda (&optional arg) (interactive)
				     (superman-next-latex-error 1)))
	   " "
	   (header-button-format "find-sec" :action 
				 #'(lambda (&optional arg) (interactive)
				     (superman-find-latex-error)))
	   " "
	   (header-button-format "find-frame" :action 
				 #'(lambda (&optional arg) (interactive)
				     (superman-find-latex-error 'frame)))
	   " View: "
	   (header-button-format "Start viewer" :action 
				 #'(lambda (&optional arg) (interactive)
				     (let ((pdf (concat (file-name-sans-extension (buffer-file-name)) ".pdf")))
				       (if (file-exists-p pdf)
					   (org-open-file pdf)
					 (message (concat "No such file: " pdf))))))
	   ;; (TeX-command "View" 'TeX-master-file nil)))
	   )))


;; (add-hook 'LaTeX-mode-hook #'(lambda ()
			       ;; (when (buffer-file-name)
				 ;; (superman-export-header-mode))))

;;}}}


(defun superman-ess-eval-and-go (arg)
  (interactive)
  (if arg 
      (save-excursion
	(ess-switch-to-end-of-ESS)
	(erase-buffer) (comint-send-input))
    (if (region-active-p)
	(let* ((start (region-beginning))
	       (end (region-end)))
	  ;; (code (buffer-substring start end))
	  ;; (cur-buf-name (buffer-name (current-buffer))))
	  (ess-eval-region-and-go start end  'nowait))
      ;; (visibly (< (length (buffer-substring-no-properties start end)) 300)))
      ;; (with-temp-buffer (R-mode) (insert code)
      ;; (ess-eval-buffer-and-go 'nowait)))
      ;; (ess-eval-region-and-go start end  'nowait))
      (save-excursion
	(ess-eval-line-and-step nil nil t)))))


;; (defun superman-control-export-back-to-org ()
  ;; (interactive)
  ;; (set-window-configuration
   ;; (get-text-property (point-min) 'wconf)))


(defun superman-file-name (&optional file buf full ext)
  "Function to turn name.xxx into name.org. When FILE is given
it must be regular file-name, it may be the absolute filename including
the directory. If FILE is nil and BUF is given, then use the filename of
the file visited by buffer BUF. If BUF is also nil then use 
'current-buffer'. If FULL is non-nil return the absolute filename.
If EXT is given then turn name.xxx into name.ext. EXT must be a string like '.tex'" 
  (let ((name (or file (buffer-file-name (or buf (current-buffer))))))
    (concat (file-name-sans-extension
	     (if full
		 name
	       (file-name-nondirectory name)))
	    (or ext ".org"))))
  


(defun superman-find-latex-error (&optional by-frame)
  (interactive)
  (let ((tex-file (buffer-file-name))
	(control-buf (buffer-name (current-buffer)))
	(log-buf (concat (file-name-sans-extension 
			  (buffer-name (current-buffer)))
			 ".log"))
	(last-pos (get-text-property (point-min) 'latex-pos))
	tex-buf
	pos)
    (if (get-file-buffer tex-file)
	(set-buffer (get-file-buffer tex-file))
      (find-file tex-file))
    (save-excursion (goto-char (point-min))
		    (while (re-search-forward "\\\\end{document}" nil t)
		      (replace-match "")))
    (setq tex-buf (current-buffer))
    (goto-char (or last-pos (point-min)))
    (if by-frame
	(re-search-forward "\\\\\\(end\\){frame" nil t)
      (re-search-forward "\\\\\\(sub\\)*section{" nil t)
      (previous-line 1))
    (end-of-line)
    (insert "\n\\end{document}")
    (when (get-buffer "*TeX Help*")
      (save-excursion (set-buffer "*TeX Help*")
		      (setq buffer-read-only nil)
		      (erase-buffer)))
    (save-buffer)
    (save-excursion
      (TeX-command "LaTeX" 'TeX-master-file nil))
    (beginning-of-line)
    (save-buffer)
    (forward-line 1)
    (setq pos (point))
    (if (get-buffer log-buf)
	(save-excursion 
	  (set-buffer log-buf)
	  (revert-buffer t t t)))
    (superman-switch-config
     nil nil
     (concat tex-file " / " log-buf " | *TeX Help* / " control-buf))
    (with-current-buffer control-buf
      (let ((buffer-read-only nil))
	(put-text-property (point-min) (1+ (point-min)) 'latex-pos pos)))))
    
;; (defun superman-latex-export ()
  ;; (interactive)
  ;; (let ((obuf (get-text-property (point-min) 'org-buffer)))
    ;; (with-current-buffer obuf
      ;; (superman-export-as-latex))))

;; (defun superman-run-latex ()
  ;; (interactive)
  ;; (let ((tex-file (get-text-property (point-min) 'tex-file)))
    ;; (save-window-excursion
      ;; (if (get-file-buffer tex-file)
	  ;; (set-buffer (get-file-buffer tex-file))
	;; (find-file tex-file))
      ;; ;; (set-buffer (find-buffer-visiting tex-file))
      ;; (TeX-command "LaTeX" 'TeX-master-file nil))))

;; (defun superman-start-viewer ()
  ;; (interactive)
  ;; (TeX-command "View" 'TeX-master-file nil))

;; (defun superman-run-bibtex ()
  ;; (interactive)
  ;; (let ((tex-file (get-text-property (point-min) 'tex-file)))
    ;; (save-excursion
      ;; (if (get-file-buffer tex-file)
	  ;; (set-buffer (get-file-buffer tex-file))
	;; (find-file tex-file))
      ;; (TeX-command "BibTeX" 'TeX-master-file nil))))

;; (defun superman-start-latexmk ()
  ;; (interactive)
  ;; (let ((tex-file (get-text-property (point-min) 'tex-file)))
    ;; (TeX-run-TeX "make-pdf" (concat "latexmk -pvc -pdf -f " tex-file) tex-file)))

(defun superman-first-latex-error ()
  (interactive)
  (superman-next-latex-error 'first))

(defun superman-next-latex-error (&optional first)
  (interactive)
  (let ((tex-file (buffer-file-name))
	(org-buf (buffer-name (get-text-property (point-min) 'org-buffer))))
    (when (buffer-live-p (get-buffer "*TeX Help*"))
      (with-current-buffer  "*TeX Help*"
	(erase-buffer)
	(insert "No more errors.")))
    (TeX-next-error first)
    (superman-set-config
     (concat tex-file " / :height 15 *TeX Help*"))
    (other-window 2)))
  
(defun superman-previous-R-error ()
  (interactive)
  "See `superman-next-R-error'."
  (superman-next-R-error 'back))

(defun superman-next-R-error (&optional backwards)
  "Search for Error terms in current R output console. If BACKWARDS
is non-nil, search backwards within the boundery set by last call to
`superman-latex-export'."
  (interactive)
  (let ((control-buf (buffer-name (current-buffer)))
	(R-buf (get-text-property (point-min) 'R-buf)))
    (when R-buf
      (switch-to-buffer R-buf))
    (unless
	(if backwards
	    ;; search for errors within the bound set by the last
	    ;; call to superman-export-as-latex 
	    (re-search-backward "^Error[: ]+" (previous-single-property-change (point) 'eval-point) t)
	  (re-search-forward "^Error[: ]+ " nil t))
      (message "No next error"))))


(defun org-turn-on-auto-export ()
  (interactive)
  (add-hook 'after-save-hook 'superman-export-as-latex))

(defun org-turn-off-auto-export ()
  (interactive)
  (remove-hook 'after-save-hook 'superman-export-as-latex))


(defun superman-save-and-run (&optional arg)
  (interactive)
  (save-buffer)
  (let ((cmd (completing-read "Command (default make-pdf): " 
			      '("make-pdf" "make-html")
			      nil 'must-match nil nil "make-pdf" nil))
	(dir (when arg (read-file-name "Publishing directory: " default-directory))))
    (cond ((string= cmd "make-pdf")
	   ;;	   (call-interactively 'org-export-as-latex)
	   (if org-beamer-mode
	       (org-beamer-export-as-latex 3 nil nil nil nil nil)
	     (org-latex-export-as-latex 3 nil nil nil nil nil)))
	  ((string= cmd "make-html")
	   (org-html-export-as-html-and-open 3)))))

(defun superman-export-as-docx (&optional dont-open)
  "Save current buffer, then export to docx via soffice."
  (interactive)
  (save-buffer)
  (let* ((org-odt-preferred-output-format "docx")
	 (old-proc (get-buffer-process (current-buffer)))
	 (name (file-name-sans-extension (buffer-name)))
	 (proc-name (concat "superman-opens:" name ".docx"))
	 (proc-buf (get-buffer-create (concat "superman-oo-run:" name)))
	 file)
    "/usr/bin/soffice /home/tag/tmp/u.docx"
    ;; (org-open-file file 'system)))
    (when (get-process proc-name)
      (kill-process proc-name))
    (setq file (org-odt-export-to-odt))
    (if dont-open
	file
    (start-process-shell-command
     proc-name
     proc-buf
     (concat "/usr/bin/soffice -norestore " file)))))

(provide 'superman-export)
;;; superman-export.el ends here
