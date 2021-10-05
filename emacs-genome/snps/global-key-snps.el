;;; global-key-snps.el --- keybindings for emacs-genome

;; Copyright (C) 2012-2021  Thomas Alexander Gerds

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

(global-set-key [f10] 'undo)
(global-set-key [f2] 'superman-switch-to-project)
(global-set-key [f3] 'superman-switch-config)
;; buffer switching/cycling
(global-set-key "\M-p" 'next-mode-buffer-backward)
(global-set-key "\M-n" 'next-mode-buffer)
;; (global-set-key "\C-xb" 'iswitchb-buffer)
;; (global-set-key "\C-xb" '-buffer)
;; commentary
(global-set-key "\M-;" 'comment-or-uncomment-line-or-region)
;; major-mode specific indentation
(global-set-key "\M-q" 'eg/indent-paragraph) 
(global-set-key "\M-Q" '(lambda () (interactive) (mark-paragraph) (fill-region-as-paragraph (region-beginning) (region-end))))
;; marking text
(global-set-key "\M-l" 'mark-line)
(global-set-key "\M-\C-l" 'mark-end-of-line)
;; expanding text
(global-set-key "\M-e" 'hippie-expand)
(global-set-key "\M-i" 'dabbrev-expand)
;; searching for anything
;; (global-set-key "\C-v" 'help-recoll-search)
;; copy-and-paste 
(global-set-key "\M-y" 'yank-or-pop)
(global-set-key "\M-r" 'copy-region-as-kill)
;;; window cycling
(global-set-key [(f8)] 'winner-undo)
(global-set-key [(meta f8)] 'winner-cycle)
(global-set-key [(shift f8)] 'winner-cycle-backwards)
(global-set-key "\M-o" 'other-window)          ; move to next window clockwise
(global-set-key "\M-O" '(lambda ()(interactive) (other-window -1))) ; move to next window anti-clockwise
;; (global-set-key [M-left] 'windmove-left)          ; move to left windnow
;; (global-set-key [M-right] 'windmove-right)        ; move to right window
;; (global-set-key [M-up] 'windmove-up)              ; move to upper window
;; (global-set-key [M-down] 'windmove-down)          ; move to downer window
;; folding
(global-set-key [(f12)] 'folding-mode)
(global-set-key [(meta f12)] 'fold-dwim-toggle)

(provide 'global-key-snps)
