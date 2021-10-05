;;{{{ Folding
(use-package folding :ensure t)
(use-package fold-dwim :ensure t)

(folding-add-to-marks-list 'mediawiki-mode "<!-- {{{" "<!-- }}}" t)
(folding-add-to-marks-list 'ess-mode "# {{{" "# }}}" t)
(folding-add-to-marks-list 'latex-mode "% {{{" "% }}}" t)
(folding-add-to-marks-list 'c-mode "// {{{" "// }}}" t)
(folding-add-to-marks-list 'muse-mode "; {{{" "; }}}" t)
(folding-add-to-marks-list 'ess-mode "# {{{" "# }}}" nil t)
(folding-add-to-marks-list 'ess-r-mode "# {{{" "# }}}" nil t)
(folding-add-to-marks-list 'css-mode "/* {{{ */" "/* }}} */" nil t)

; (add-to-list 'folding-mode-marks-alist '(ess-mode "#{{{" " #}}}"))

(defun insert-folds ()
  (interactive)
  (let ((beg (if (region-active-p) (region-beginning) (point)))
	(marks (cdr (assoc major-mode folding-mode-marks-alist))))
    (if marks
	(save-excursion
	  (goto-char beg)
	  (insert (car marks) " ")
	  (if (region-active-p)
	      (goto-char (region-end))
	    (insert "\n"))
	  (insert "\n")
	  (insert (cadr marks))))))

; (folding-mode-add-find-file-hook)

(remove-hook 'find-file-hooks 'folding-mode-find-file)

(add-hook 'folding-mode-hook
	  '(lambda nil
	     (font-lock-default-fontify-buffer)
	     (local-set-key "\C-z" 'fold-dwim-toggle)))

;;}}}
;;{{{ font-lock


(defadvice fold-dwim-toggle (after fontify first activate)
  (font-lock-default-fontify-buffer))

(defun folding-font-lock-support-instantiate (&optional mode)
  "Add fold marks with `font-lock-add-keywords'."
  (or mode
      (setq mode major-mode))
  (if  (eq mode 'muse-mode)
      nil
    ;;  Hide function from Byte Compiler.
    (let* ((function 'font-lock-add-keywords))
      (when (fboundp function)
	(funcall function
		 mode
		 (folding-font-lock-keywords mode))
	;; In order to see new keywords font lock must be restarted.
	(dolist (buffer (buffer-list))
	  (with-current-buffer buffer
	    (when (and (eq major-mode mode)
		       (or font-lock-mode
			   ;;  Hide variable from byte compiler.
			   (let ((sym 'global-font-lock-mode))
			     (and (boundp sym)
				  (symbol-value sym)))))
	      ;; #todo: should we use font-lock-fontify-buffer instead?
	      (font-lock-mode -1)
	      (font-lock-mode 1))))))))

; (folding-font-lock-support)

;; the infinite loop is from font-lock-set-defaults which is
;; called as part of find-file-hooks and calls font-lock-set-defaults-1 
;; which seems to call font-lock-set-defaults ...
; (require 'folding)
; (folding-font-lock-support 
; (defadvice folding-font-lock-keywords
; ;   folding-font-lock-support-instantiate
;   (after beware-of-lisp-eval-nesting first activate)
;   (if (eq major-mode 'muse-mode)
;       (setq font-lock-keywords-alist
; 	    (assq-delete-all 'muse-mode font-lock-keywords-alist))))
; (defadvice font-lock-mode
;   (before beware-of-lisp-eval-nesting first activate)
;   (if (eq major-mode 'muse-mode)
;       (setq font-lock-keywords-alist
; 	    (assq-delete-all 'muse-mode font-lock-keywords-alist))))
;;}}}

(defun kill-entire-line ()
  (interactive)
  (progn
    (beginning-of-line)
    (kill-line)))


;;{{{ latex

(defun R-fix-folds ()
  " for # --------------------------------------
        #                 headline
        # --------------------------------------"
  (interactive)
  (while (re-search-forward "#\\(.*\\)\\\n[ \t]*#[ \t]*[-]+\n" nil t)
    (let ((tit (match-string 1)))
      (previous-line 1)
      (kill-entire-line)
      (previous-line 1)
      (kill-entire-line)
      (insert "# }}}\n\n# {{{ " tit "\n"))))

(defun R-fix-folds2 ()
  " for # ----------------headline--------------"
  (interactive)
  (while (re-search-forward "#[ -]*\\(.*\\)\\\n" nil t)
    (let ((tit (match-string 1)))
      (beginning-of-line)
      (kill-entire-line)
      (insert "# }}}\n\n# {{{ " tit "\n"))))

(defun latex-fix-folds ()
  (interactive)
  (if folding-mode (folding-mode))
  (goto-char (point-min))
  (while (re-search-forward "% {{{\\|% }}}" nil t)
    (kill-entire-line))
  (let ((sec 0)
	(sub 0)
	(subsub 0))
    (goto-char (point-min))
    (insert "% {{{ Header\n")
    (re-search-forward "begin{document}")
    (beginning-of-line)
    (insert "% }}}\n")
    (forward-line 1)
    (insert "% {{{\n")
    (while (re-search-forward "section[*]?{\\(.*\\)}" nil t)
      (let ((sectitle (match-string 1))
	    (num (progn (beginning-of-line)
			(cond
			 ((looking-at "\\\\subsub")
			  (setq subsub (+ 1 subsub))
			  (concat (number-to-string sec)
				  "."
				  (number-to-string sub)
				  (number-to-string subsub)))
			 ((looking-at "\\\\sub")
			  (setq subsub 0)
			  (setq sub (+ 1 sub))
			  (concat (number-to-string sec)
				  "."
				  (number-to-string sub)))
			 ((looking-at "\\\\sec")
			  (setq sec (+ 1 sec))
			  (setq subsub 0 sub 0)
			  (concat (number-to-string sec)))
			 (t "")))))
	;; (unless (looking-at "[ \t]*$") (insert "\n"))
	(insert "% }}}\n% {{{" " *" num "* " sectitle "\n")
	(forward-line 2)))
    (re-search-forward "end{document}" nil t)
    (beginning-of-line)
    (insert "% }}}\n")))
;;}}}

(provide 'folding-snps)
