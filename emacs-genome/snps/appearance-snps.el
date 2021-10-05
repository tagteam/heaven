;;; appearance-snps.el --- emacs-genome flavor

;; Copyright (C) 2012-2021  Thomas Alexander Gerds

;; Author: Thomas Alexander Gerds<tag@biostat.ku.dk>
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

;;{{{warnings 
(setq warning-suppress-types nil)
(setq debug-on-error t)	
;;}}}
;;{{{ prefer horizontal splits also on a wide screen
(setq split-width-threshold nil)
;;}}}
;;{{{ avoid pop-ups
(setq use-file-dialog nil)
(setq font-lock-verbose nil) ;; get rid of the stupid pop-up box
;;}}}
;;{{{ no blinking cursor!
(setq blink-cursor-mode nil)
(blink-cursor-mode)
(blink-cursor-mode)
;;}}}
;;{{{ open multiple buffers with the same file
(setq find-file-existing-other-name t)
;;}}}
;;{{{ minibuffer
(setq
 minibuffer-confirm-incomplete t
 minibuffer-max-depth nil
 enable-recursive-minibuffers t)
   
;; 10.15 Why doesn't SPC complete file names anymore?
;; ==================================================

;; Starting with Emacs 22.1, `SPC' no longer completes file names in the
;; minibuffer, so that file names with embedded spaces could be typed
;; without the need to quote the spaces.
   ;; You can get the old behavior by binding `SPC' to
;; `minibuffer-complete-word' in the minibuffer, as follows:

(define-key minibuffer-local-filename-completion-map (kbd "SPC")
  'minibuffer-complete-word)

(define-key minibuffer-local-must-match-filename-map (kbd "SPC")
  'minibuffer-complete-word)
;;}}}
;;{{{ case-fold search
(setq-default case-fold-search t)
(setq case-fold-search t)
;;}}}
;;{{{ fill and truncate 

(setq kill-whole-line t
      default-overwrite-mode nil
      truncate-lines t
      default-truncate-lines t
      auto-fill-mode 1
      scroll-step 1)

;;}}}
;;{{{ delete and overwrite 
;; (pending-delete-mode) xemacs
(require 'delsel nil t)
(delete-selection-mode 1)
(put 'erase-buffer 'disabled nil)
(put 'overwrite-mode 'disabled t)
(setq delete-key-deletes-forward t)

;;}}}
;;{{{ horizontal scrolling
(if (boundp 'truncate-lines)
    (setq-default truncate-lines t) ; always truncate
  (progn
    (hscroll-global-mode t)
    (setq hscroll-margin 1)
    (setq auto-hscroll-mode 1)
    (setq automatic-hscrolling t)
   ))
;;}}}
;;{{{ autosave, version control, backup

(setq backup-by-copying t   ; don't clobber symlinks
      version-control t     ; use versioned backups
      delete-old-versions t
      make-backup-files t
      kept-new-versions 6
      kept-old-versions 2)
;;}}}
;;{{{ save minibuffer history
(savehist-mode 1)
;;}}}
;;{{{ visual line mode

(visual-line-mode 1)

;;}}}
;;{{{ paren mode

(if (require 'mic-paren nil t)
    (paren-activate)
  (show-paren-mode 1))

(require 'uniquify nil t)       
(setq uniquify-buffer-name-style 'post-forward)
;;}}}
;;{{{ time
(require 'time nil t)
;;}}}
;;{{{ frame title
(setq frame-title-format (concat " %b" " %f " (user-login-name) "@" (shell-command-to-string "echo $HOSTNAME")))
;;}}}
;;{{{ mode line
(setq line-number-mode t
      column-number-mode t)
(setq
 display-time-mode t
 calendar-time-display-form (quote (24-hours ":" minutes))
 display-time-24hr-format t
 display-time-day-and-date t)
(display-time)

(setq-default mode-line-format
      '(
	(" " display-time-string)
	;; "%e"
	;; #("-" 0 1
	;; (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display")
	;; )
	;; mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification
	#(" " 0 1
	  (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))
	(list
	 'line-number-mode "L%l ")
	(list
	 'column-number-mode "C%c ")
	;; mode-line-position
	(vc-mode vc-mode)
	;; #("  " 0 2
	;; (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))
	mode-line-modes
	;; default-directory
	(which-func-mode
	 ("" which-func-format
	  #("--" 0 2
	    (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))))
	;; ("" win:mode-string display-time-string)
	;; (global-mode-string
	;; (#("--" 0 2
	;; (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))
	;; global-mode-string))
	mode-line-buffer-identification
	mode-line-remote
	;; (concat " buffer: %b" " %f")
	#("-%-" 0 3
	  (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))))

;;}}}

(provide 'appearance-snps)
;;; appearance-snps.el ends here

