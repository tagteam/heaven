;;; shell-snps.el --- Shell settings for emacs

;; Copyright (C) 2013  Thomas Alexander Gerds

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

;;; Commentary

;; Use f6 to open a shell in other buffer and also f6 to close the buffer again.
;; Use f7 to open ielm in other buffer and also f7 to close the buffer again.

;;; Code 

(add-to-list 'load-path (concat (getenv "HOME") "/emacs-genome/genes/ssh-el/"))
(require 'ssh nil 'noerror)

(setq comint-scroll-to-bottom-on-input t)

(setq process-coding-system-alist 
      (cons '("bash" . raw-text-unix)
	    process-coding-system-alist))

(add-to-list 'debug-ignored-errors "^Marker does not point anywhere$")

(add-hook 'shell-mode-hook
	  '(lambda ()
	     (ansi-color-for-comint-mode-on)
	     (setq comint-scroll-to-bottom-on-input 'all)
	     (define-key shell-mode-map [f6] 'delete-window)
	     (define-key shell-mode-map "\M-k" 'clear-shell)
	     (define-key shell-mode-map "\M-r" 'copy-region-as-kill)
	     (define-key shell-mode-map "\M-y" 'yank-or-pop)
	     (define-key shell-mode-map "\M-p" 'comint-previous-matching-input-from-input)
	     (define-key shell-mode-map "\M-n" 'comint-next-matching-input-from-input)
	     (define-key shell-mode-map "\M-e" 'comint-dynamic-complete)))


(add-hook 'eshell-mode-hook '(lambda ()
			       ;; (define-key eshell-mode-map [(control f6)] 'delete-window)
			       (define-key eshell-mode-map [(control f6)] 'shell-toggle)
			       (define-key eshell-mode-map "\M-k" 'clear-eshell)
			       (define-key eshell-mode-map "\M-e" 'pcomplete)))
(add-hook 'ielm-mode-hook '(lambda ()
			     (local-set-key [(delete)] 'delete-char)
			     (local-set-key [f7] 'delete-window)))

(defun shell-window ()
  (interactive)
  (split-window)
      (shell))

(defun clear-shell ()
  (interactive)
  (erase-buffer)			
  (comint-send-input))

(defun eshell-window ()
  (interactive)
  (split-window)
      (eshell))

(defun clear-eshell ()
  (interactive)
  (erase-buffer)			
  (eshell-send-input))


(defun ielm-window ()
  (interactive)
  (split-window)
      (ielm))

(provide 'shell-snps)
;;; shell-snps.el ends here
