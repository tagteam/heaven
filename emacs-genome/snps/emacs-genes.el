;;; emacs-genes.el

;; Copyright (C) 2015-2021  Thomas Alexander Gerds

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
;;----------------------------------------------------------------------
;; created: Apr 12 2015 (09:51) 
;; Version: 
;; last-updated: Apr  7 2021 (08:37) 
;;           By: Thomas Alexander Gerds
;;     Update #: 74
;;----------------------------------------------------------------------
;; 
;;; Commentary: Show important features {genes}. 
;;              Display either featured functions (buttons) and/or help on
;;              how to achieve functionality, e.g., install missing
;;              software.
;;; Change Log:
;;----------------------------------------------------------------------
;; 
;;; Code:
;;; Commentary:
;; 
;;; Code:


(defvar eg-alist '(
		   ;; ("emacs-genome"
		   ;; :test (:var emacs-genome)
		   ;; :fun (lambda (&optional arg) (interactive) (superman-view-directory emacs-genome)))
		   ("Recent files" :fun eg/recent-files)
		   ("Projects" :fun superman)
		   ("Calendar" :fun superman-calendar)
		   ("Todo-list" :fun superman-todo)
		   ("Black-Board" :fun superman-black-board)
		   ("Cheat sheet" :fun eg-cheat-sheet)
		   ;; ("org-mode" :test org)
		   ;; ("superman"
		   ;; :test (:genes superman-manager)
		   ;; :fun superman)
		   ("R" 
		    :test ess
		    :fun eg/start-R
		    #'(lambda ()
			(interactive)
			(and (featurep 'ess)
			     inferior-R-program-name
			     (ignore-errors
			       (R)
			       (eq major-mode 'inferior-ess-mode)))))
		   ;; ("LaTeX" :test tex-site)
		   ;; ("helm" :test helm
		   ;; :emacs-genes eg-helm-emacs-genes)
		   ;; ("recentf" :test recentf)
		   ;;("hippie" :test hippie-exp)
		   )
  "List of emacs-genome emacs-genes")

(defun eg/start-R ()
  (interactive)
  (r 1))

(defun eg/recent-files ()
  (interactive)
  (recentf-mode 1)
  (let ((files recentf-list))
    (unless files 
      (setq files `(,(expand-file-name "~/.emacs"))))
    (recentf-open-files files)))


(defvar eg-button-faces
  '(("superman" superman-project-button-face)))

(defface eg-default-genes-button-face
  '((t (:height 3.0
		:foreground "gray22"
		:background "gray90"
		:box (:line-width 1 :color "gray88" :style released-button))))
  "Default face used for emacs genome genes buttons."
  :group 'emacs-genome)

(defun eg (&optional test force)
  "Show and test emacs-genome functionality.
Illustrate functional emacs-genome emacs-genes and provide hints on how to install
yet non-functional emacs-genes."
  (interactive)
  (let* ((eg-buf-name "*Emacs Genome*")
	 (eg-buf (get-buffer eg-buf-name)))
    (unless (and (not force) eg-buf)
      (set-buffer
       (setq eg-buf (get-buffer-create eg-buf-name)))
      ;; (org-mode)
      (erase-buffer)
      (font-lock-mode -1)
      (emacs-genome-mode)
      (let* ((emacs-genes eg-alist)
	     (button-width 13))
	(insert "\nYour emacs has been genetically modified.\n\n")
	;; (put-text-property (point-min) (1+ (point-min)) 'redo-cmd '(eg t t))
	(while emacs-genes
	  (let* ((genes (car emacs-genes))
		 (this-gene (car genes))
		 (genes-attr (cdr genes))
		 (is-genes
		  (or (and (plist-get genes-attr :ftest)
			   (featurep (plist-get genes-attr :ftest)))
		      (and (plist-get genes :vtest)
			   (boundp (plist-get genes-attr :vtest)))
		      (and (plist-get genes :test)
			   (functionp (plist-get genes-attr :test))
			   (funcall (plist-get genes-attr :test)))
		      (featurep (intern this-gene))))
		 (genes-fun (or (plist-get genes-attr :fun)
				(intern (concat "eg-" this-gene "-fun"))))
		 (genes-nofun (or (plist-get genes-attr :nofun)
				  (intern (concat "eg-" this-gene "-nofun"))))
		 (genes-emacs-genes (plist-get genes-attr :emacs-genes))
		 (fun (cond
		       ((functionp genes-fun) genes-fun)
		       ((functionp genes-nofun) genes-nofun)
		       (t 'eg-missing-genes))))
	    (insert (superman-make-button
		     this-gene
		     `(:fun ,fun
			    :face eg-default-genes-button-face :width ,button-width)) "\n\n") 
	    (setq emacs-genes (cdr emacs-genes))))
	
(goto-char (point-min))
(re-search-forward "Projects")
	(setq buffer-read-only t)))
    (superman-set-config eg-buf-name)))

(defun eg-show-gene (file)
  (interactive)
  (get-buffer-create (concat "*" file "*"))
  (set-buffer (concat "*" file "*"))
  (erase-buffer)
  (insert-file file)
  (goto-char (point-min))
  (while (re-search-forward "^BUTTON[ \t]+" nil t)
    (replace-match "")
    (let* ((sexp-start (point))
	   (sexp-end (progn (forward-sexp) (point)))
	   (code  (buffer-substring sexp-start sexp-end t)
	   (result (eval-region sexp-start sexp-end t)))
      (kill-region sexp-start sexp-end)
      (insert result)))))


(defun eg-cheat-sheet ()
  "Show some keybindings."
  (interactive)
  (let* ((egcs-buf-name "*Emacs Genome Cheat Sheet*")
	 (egcs-buf (get-buffer egcs-buf-name)))
    (unless (and egcs-buf)
      (set-buffer
       (setq egcs-buf (get-buffer-create egcs-buf-name)))
      (erase-buffer)
      (emacs-lisp-mode)
      ;; cheat sheet
      (insert "\n
* Cheat sheet

** Notation

  \\C = control
  \\M = Alt
  SPC = space
  RET = Return
  TAB = Tab-key
 BACK = Back button
  DEl = Delete button
  up  = arrow up
 down = arrow down
 left = arrow left
right = arrow right
   eg = emacs genome modified

* Important bindings

 \\C-g : quit minibuffer/stop a command 
 \\M-x : evaluate commands: e.g., \\M-x eg RET (show the current buffer)
   F10 : undo/redo (eg)

* Walking the cursor 

       Beginning of line: \\C-a
            End of line : \\C-e
          Previous line : \\C-p
              Next line : \\C-n
     Previous paragraph : \\M-up
         Next paragraph : \\M-down
    Beginning of buffer : \\C-HOME
          End of buffer : \\C-END

           Word forward : \\M-f
          Word backward : \\M-b
  Expression({[ forward : \\M-f
 Expression({[ backward : \\M-b

* Highlighting text

             Mark line : \\M-l 
        Mark paragraph :  \\M-h
           Mark buffer : \\C-x h 
Start marking a region : \\C-\\M SPC (expand with arrow keys, \\C-x \\C-x change direction)

* Copying or killing and pasting text

 copy region to kill-ring : \\M-w
 kill region to kill-ring : \\C-w
                    paste : \\M-y (press again and again to get older elements from kill ring)

   kill rest of line : \\C-k
delete word backward : \\M-BACK

* Completing text and other things

       \\M-i : dabbrev-expand (expand text)
       \\M-e : hippie-expand (expand many things)
  
  Completing text dynamically

   Expands to the most recent, preceding word for which this is a prefix.
   Press SPACE after expansion and then \\M-i again to get expansion the following 
   expression from the source of the first expansion.
  

* Search & replace

                  Search forward : \\C-s
                 Search backward : \\C-b
  find all occurrences in buffer : \\M-x occur RET
       Search & replace (literal): \\M-% (literal)
       Search & replace (regexp) : \\C-\\M-% (regexp)
edit all occurences simultaneous : \\M-x iedit-mode RET

* Walking buffers 

            Switch buffer : \\C-x b
Previous buffer same mode :  \\M-p
    Next buffer same mode : \\M-n

* Walking Windows

                 F8 : restore previous window-configuration 
 delete this window : \\C-x 0
       delete other : \\C-x 1
 split horizontally : \\C-x 2
   split vertically : \\C-x 3 

* Projects

 \\M-x : Project overview
    F2 : switch to project
    F3 : switch to project window configuration

** In the project view
     u : show/create/apply unison file synchronisation rules
     f : list files
     g : show git repository
     N : associate document, link, task or meeting with project

* org-mode

     \\M-x : visible mode
       TAB : cycle through folding levels
     \\M-J : change export target (pdf, html, docx)
     \\M-j : export buffer to target
\\C-c\\C-e : export to other formats
\\C-c\\C-c : view export (pdf, html or docx)
     \\M-k : export buffer to pdf, show tex and R buffers for debugging

* R 

     \\M-k : switch to R (start R if not running) 
     \\M-j : eval region
     \\M-r : copy region 
 \\M-\\C-i : indent call sophisticatedly
     \\M-q : indent paragraph
     \\M-k : switch to R buffer (console)
    \\C-cf : insert call to a function
    \\C-cv : insert vector (any R vector, e.g. names(mydata))
    \\C-cp : insert path to current directory
    \\C-ch : mark function call
  \\C-cF : insert path to file name
")
      (goto-char (point-min)))
  (pop-to-buffer egcs-buf-name)
  ))


(defvar emacs-genome-mode-map (make-sparse-keymap)
  "Keymap used for `emacs-genome-mode' commands.")
   
(define-minor-mode emacs-genome-mode 
  "Toggle org projectmanager document emacs-genome mode.
With argument ARG turn emacs-genome-mode on if ARG is positive, otherwise
turn it off.

Enabling emacs-genome mode electrifies the emacs-genome buffer for project management."
     :lighter " *EG*"
     :group 'org
     :keymap 'emacs-genome-mode-map)

(defun emacs-genome-on ()
  (interactive)
  (when emacs-genome-hl-line (hl-line-mode 1))
  (emacs-genome-mode t))

(defun eg-next ()
  (interactive)
  (goto-char (or (next-single-property-change (point-at-eol) 'button)
		 (next-single-property-change (point-min) 'button)))
  (beginning-of-line))

(defun eg-previous ()
  (interactive)
  (goto-char (or (previous-single-property-change (point-at-bol) 'button)
		 (previous-single-property-change (point-max) 'button)))
  (beginning-of-line))

(define-key emacs-genome-mode-map "R" 'eg-redo)
(define-key emacs-genome-mode-map [(tab)] 'eg-next)
(define-key emacs-genome-mode-map [(backtab)] 'eg-previous)

(defun eg-redo ()
  "Refresh emacs genome buffer."
  (interactive)
  (eval (get-text-property (point-min) 'redo-cmd)))


(defun yank-or-pop (arg)
  "Combine `yank' with `yank-pop'."
  (interactive "*p") 
  (if (eq last-command 'yank)
      (yank-pop arg)
    (yank arg))
  nil)

(defun count-words (start end)
  "Print number of words in the region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (count-matches "\\sw+"))))

(defun looking-at-backward (regexp)
  (let* ((begin (point))
	 (found (re-search-backward regexp nil t)))
    (goto-char begin)
    (and found (= begin (match-end 0)))))


;; (defun fit-frame ()
  ;; "Fit the emacs frame to the current display"
  ;; (interactive)
  ;; (when (window-system)
    ;; (set-frame-position (selected-frame) 10 30)
    ;; (set-frame-height (selected-frame)
		      ;; (/ (* (/ (x-display-pixel-height) (frame-char-height)) 8) 9))
    ;; (set-frame-width (selected-frame)
		     ;; (/ (* (/ (x-display-pixel-width)
			      ;; ;; (if (> (length (terminal-list)) 1)
			      ;; ;; (* (frame-char-width) (length (terminal-list)))
			      ;; (if  (string-match "27909\\|29377\\|siam" (system-name))
				  ;; (* (frame-char-width) 3)
				;; (frame-char-width)))
			   ;; 12) 13))))

(defun mark-line (arg)
  "Push the mark in line.
The place mark goes is the same place \\[forward-line] would
move to with the same argument."
  (interactive "p")
  (beginning-of-line)  
  (push-mark
   (save-excursion
     (end-of-line arg)
     (point))
   nil t))

(defun winner-cycle (&optional backward)
  (interactive)
  (let ((n (ring-length winner-pending-undo-ring)))
   (if backward 
       (setq winner-undo-counter (max 1 (1- winner-undo-counter)))
     (setq winner-undo-counter (min n (1+ winner-undo-counter))))
  (winner-set (ring-ref winner-pending-undo-ring winner-undo-counter))
  (message "Winner undo ring (%d / %d)"
	   winner-undo-counter
	   (1- (ring-length winner-pending-undo-ring)))))

(defun winner-cycle-backwards ()
  (interactive)
  (winner-cycle t))

(defun comment-or-uncomment-line-or-region (&optional arg)
  "If region is active comment or uncomment region, see `comment-or-uncomment-region' 
else comment or uncomment current line. If ARG is non-nil uncomment region or current line."
  (interactive "P")
  (if arg
      (if (region-active-p)
	  (uncomment-region (region-beginning) (region-end))
	(uncomment-region (point-at-bol) (point-at-eol))
	(forward-line))
    (if (region-active-p)
	(comment-or-uncomment-region (region-beginning) (region-end))
      (while (looking-at "^[ \t\n]*$") (forward-line))
      (comment-or-uncomment-region (point-at-bol) (point-at-eol))
      (forward-line))))

(defun uncomment-line-or-region ()
  "If region is active uncomment the region, else uncomment 
the current line."
  (interactive)
  (comment-or-uncomment-line-or-region 1))


(defun eg/sort-region (&optional separator)
  (interactive)
  (let ((separator (or separator ","))
	(sort-fold-case nil))
    (narrow-to-region (region-beginning) (region-end))
    (goto-char (point-min))
    (while (re-search-forward (concat "[ \t\n]*" separator "[ \t\n]*") nil t)
      (replace-match "\n"))
    (sort-fields 1 (point-min) (point-max))
    (goto-char (point-min))
    (while (re-search-forward "\n" nil t)
      (replace-match ", "))
    (widen)))


(defun kill-entire-line ()
  (interactive)
  (progn
    (beginning-of-line)
    (kill-line)))

(defun eg/kill-this-buffer ()
  "Kill the current buffer and goto buffer with same mode."
  (interactive)
  (let ((cbuf (current-buffer)))
    (next-mode-buffer)
    (kill-buffer cbuf)))

(defun eg/mark-paragraph (&optional arg)
  (interactive "p")
  (cond 
   ((eq major-mode 'org-mode)
    (if (org-babel-where-is-src-block-head)
	(if (= arg 4)
	    (org-babel-mark-block)
	  (mark-paragraph))
      (org-mark-subtree)))
   (t (mark-paragraph))))

(defun eg/indent-paragraph ()
  (interactive)
  (cond 
   ((eq major-mode 'org-mode)
    (cond ((string= (car (org-babel-get-src-block-info)) "emacs-lisp")
	   (let* ((info (org-element-at-point))
		  (beg (plist-get (cadr info) :begin))
		  (end (plist-get (cadr info) :end)))
	     (narrow-to-region beg end)
	     (emacs-lisp-mode)
	     (indent-region beg end)
	     (org-mode)
	     (widen)))
	  ((string= (car (org-babel-get-src-block-info)) "R")
	   (let ((org-src-preserve-indentation t))
	     (org-edit-special)
	     (indent-region (point-min) (point-max))
	     (org-edit-src-exit)))
	  (t (fill-paragraph))))
   ((eq major-mode 'Rd-mode) nil)
   ((eq major-mode 'c++-mode)
    (progn (mark-paragraph) (indent-region (region-beginning) (region-end))
	   ))
   ((eq major-mode 'bibtex-mode)
    (save-excursion
      (let ((beg (progn (backward-paragraph 1) (point)))
	    (end (progn (forward-paragraph 1)
			(point))))
	(narrow-to-region beg end)
	(bibtex-reformat)
	(widen))))
   ((eq major-mode 'latex-mode)
    (unless (save-excursion
	      (re-search-forward "begin{document}" nil t))
      (LaTeX-fill-paragraph))) ;; in the preamble do nothing
   (t (save-excursion
	(let ((beg (progn (backward-paragraph 1) (point)))
	      (end (progn (forward-paragraph 1)
			  (point))))
	  (unless (string-match "ess-\\|c-\\|c++\\|emacs" (symbol-name major-mode))
	    (fill-region beg end nil nil nil))
	  (when (string-match "ess-\\|c-\||c++\\|emacs" (symbol-name major-mode))
	    (indent-region beg end nil)))))))

(provide 'emacs-genes)
;;; emacs-genes.el ends here

