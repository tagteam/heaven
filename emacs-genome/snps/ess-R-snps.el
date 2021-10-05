;;; ess-R-snps.el --- ess setup file for statistical software R

;; Copyright (C) 2012-2018  Thomas Alexander Gerds

;; Author: Thomas Alexander Gerds <tag@biostat.ku.dk>
;; Keywords: convenience, tools

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

;; (customize-set-variable
 ;; 'display-buffer-alist '((".*" . (display-buffer-same-window . nil))))
;;{{{ Help mode
(add-hook 'ess-help-mode-hook #'(lambda () (setq truncate-lines t
						 truncate-partial-width-windows nil)))

;;{{{ global custom
(setq ess-use-tracebug t)

;; keep ess-buffer names simple 
(setq ess-gen-proc-buffer-name-function 'ess-gen-proc-buffer-name:simple)

(setq ess-display-buffer-reuse-frames nil)

;; I don't like if another frame pops up showing the inferior-ess buffer,
;; rather I want unconditionally that the buffer is shown in the current frame.
;; (defun ess-buffer-visible-other-frame (buf) nil)

;; (setq ess-long+replacement "")
(setq-default ess-ask-for-ess-directory nil)
(setq-default ess-directory (concat (getenv "HOME") "/R/"))
(setq-default ess-history-directory (concat (getenv "HOME") "/R/"))
(setq-default ess-language "R")
(setq-default ess-dialect "R")
(setq inferior-ess-font-lock-keywords nil)
(setq ess-eval-deactivate-mark t)
(setq ess-eval-visibly 't)
(setq ess-eval-visibly-p 't)
(setq ess-eval-visibly 'nowait)
(setq ess-eval-visibly-p 'nowait)
;;}}}
;;{{{ key bindings


(require 'ess-edit)
(add-hook 'ess-mode-hook
	  '(lambda () 
	     (ess-edit-default-keybindings)
	     (define-key ess-mode-map "\M-j" 'ess-eval-region-and-go)
	     (define-key ess-mode-map "\M-r" 'copy-region-as-kill)
	     (define-key ess-mode-map "\M-q" 'eg/indent-paragraph)
	     (define-key ess-mode-map "\M-l" 'mark-line)
	     (define-key ess-mode-map "\M-k" 'eg-switch-to-R)
	     (define-key ess-mode-map "\C-z" 'fold-dwim-toggle)
	     (define-key ess-mode-map [(meta return)] '(lambda () (interactive) (ess-eval-line)(forward-line 1)))
	     (define-key ess-mode-map "\C-\M-y" 'eg/ess-duplicate-line)  
	     (define-key ess-mode-map "_" 'eg/ess-smart-underscore)
	     (define-key ess-mode-map "\M-H" 'eg/ess-get-help-R-object)
	     (define-key ess-mode-map "\C-\M-k" #'(lambda () (interactive)(eg-switch-to-R 't)))
	     (define-key ess-mode-map [(backspace)] 'delete-backward-char)
	     (define-key ess-mode-map [(meta backspace)] 'backward-kill-word)
	     ))

;;}}}
;;{{{ expanding objects

(setq ess-tab-complete-in-script t)
(setq ess-first-tab-never-complete nil)
(add-hook 'ess-mode-hook
	  '(lambda ()
	     ;; (make-variable-buffer-local 'hippie-expand-try-functions-list)
	     (add-to-list 'hippie-expand-try-functions-list (lambda (old) (ess-complete-object-name)))))
;;}}}
;;{{{ R mode

(defun r (arg)
  (interactive "P")
  (cond ((not (one-window-p)) nil)
	(arg (split-window-horizontally))
	(t (split-window-vertically)))
  (other-window 1)
  (R))

;; allow to duplicate a line - from http://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs
(defun eg/ess-duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let

  ;; put the point in the lowest line and return
  (next-line arg))


(defun eg/R-mode ()
  (interactive)
  (setq split-width-threshold nil)
  (setq dabbrev-abbrev-skip-leading-regexp "\\$")
  (setq ess-indent-with-fancy-comments nil))
(add-hook 'ess-r-mode-hook 'eg/R-mode)

(defun dont-like-fancy ()
  (setcdr (assoc 'ess-indent-with-fancy-comments (cdr (assoc 'RRR ess-style-alist))) nil)
  )
;; (defun dont-like-fancy ()
       ;; (setq ess-indent-with-fancy-comments nil))
(add-hook 'ess-mode-hook 'dont-like-fancy)
(add-hook 'ess-r-mode-hook 'dont-like-fancy)

;;}}}
;;{{{ scrolling iess window on output
(setq comint-scroll-to-bottom-on-output 'this)
(setq comint-scroll-to-bottom-on-input 'this)
;;}}}
;;{{{ command history
(setq comint-input-ring-size 5000)
;;(add-hook 'ess-send-input-hook
;;	  '(lambda nil
;;	     (if (>= (point) (marker-position
;;			      (process-mark
;;			       (get-buffer-process (current-buffer)))))
;;		 (comint-send-input)
;;	       (comint-copy-old-input))
;;	     (setq ess-object-list nil)
;;	     (let ((debug-on-error nil))
;;	       (error ""))))

;;The function ess-eval-line-and-step (^c^n) calls
;;ess-eval-linewise with the line to evaluate.

;You can add a defadvice for ess-eval-linewise that explicitly calls
;comint-add-to-input-history with the command.

;Something like (untested) should do what you want.
;; (defadvice ess-eval-linewise (before smart-toggle-visibly first activate)
  ;; (and (not (eq major-mode 'inferior-ess-mode)) (< (length text-withtabs) 300))
      ;; (setq invisibly nil)
 ;; (setq invisibly t))

;; (defadvice ess-eval-region (before smart-toggle-visibly first activate)
  ;; (if (and (not (eq major-mode 'inferior-ess-mode)) (< (length (buffer-substring-no-properties start end)) 300))
      ;; (setq toggle t)
    ;; (setq toggle nil)))

;; (defun ess-show-buffer (buf &optional visit)
  ;; (pop-to-buffer buf t (selected-frame)))

;; (defadvice ess-eval-linewise (after add-history first activate)
  ;; (if (and (not (eq major-mode 'inferior-ess-mode)) (< (length text-withtabs) 300))
      ;; (save-excursion
	;; (set-buffer (process-buffer
		     ;; (get-ess-process
		      ;; ess-current-process-name)))
	;; (comint-add-to-input-history
	 ;; (let* ((str text-withtabs)
		;; (pos (string-match "\n" str)))
	   ;; (while pos
	     ;; (setq str (concat (substring str 0 pos)
			       ;; (substring str (+ 1 pos))))
	     ;; (setq pos (string-match "\n" str)))
	   ;; str)))))

;;Your other idea of not adding long commands to the history can be
;;handled with comint-input-filter.  For example, the default filter
;;rejects short commands.
;; (setq comint-input-filter
      ;; #'(lambda (str)
	  ;; (and (not (string-match "\\`\\s *\\'" str))
	       ;; ;; Ignore '!!' and kin
	       ;; (> (length str) 2)
	       ;; (< (length str) 300))))

;; (defun comint-add-to-input-history (cmd)
  ;; "Maybe add CMD to the input history.  
;; CMD is only added to the input history if `comint-input-filter'
;; returns non-nil when called on CMD. If `comint-input-ignoredups' is
;; non-nil then duplicates are ignored."
  ;; (if (and (funcall comint-input-filter cmd)
	   ;; (or (null comint-input-ignoredups)
	       ;; (not (ring-p comint-input-ring))
	       ;; (ring-empty-p comint-input-ring)
	       ;; (not (string-equal (ring-ref comint-input-ring 0)
				  ;; cmd))))
      ;; (ring-insert comint-input-ring cmd)))

;;}}}
;;{{{ smart underscore

(defun eg/ess-smart-underscore ()
  (interactive)
  (if (not (eq last-command 'eg/ess-smart-underscore))
      (insert " <- ")
    (undo)
    (insert "_")))
;;}}}
;;{{{ clearing the inferior window
(defun eg-switch-to-R (&optional arg)
  "Goto the end of the R console with argument ARG erase its contents.
See `ess-switch-to-ESS' and `ess-show-buffer' for buffer behaviour.
"
  (interactive "P")
  (ess-switch-to-end-of-ESS)
  (when arg (erase-buffer) (comint-send-input)))
;;}}}


;;{{{ ess-rmarkdown
(defun ess-rmarkdown ()
  "Compile R markdown (.Rmd). Should work for any output type."
  (interactive)
					; Check if attached R-session
  (condition-case nil
      (ess-get-process)
    (error
     (ess-switch-process)))
  (let* ((rmd-buf (current-buffer)))
    (save-excursion
      (let* ((sprocess (ess-get-process ess-current-process-name))
	     (sbuffer (process-buffer sprocess))
	     (buf-coding (symbol-name buffer-file-coding-system))
	     (R-cmd
	      (format "library(rmarkdown); rmarkdown::render(\"%s\")"
		      buffer-file-name)))
	(message "Running rmarkdown on %s" buffer-file-name)
	(ess-execute R-cmd 'buffer nil nil)
	(switch-to-buffer rmd-buf)
	(ess-show-buffer (buffer-name sbuffer) nil)))))
;;}}}

  
(provide 'ess-R-snps)
;;; ess-R-snps.el ends here
