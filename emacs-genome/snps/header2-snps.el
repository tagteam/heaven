;;; header2-snps.el --- 

;; Copyright (C) 2015-2021  Thomas Alexander Gerds

;; Author: Thomas Alexander Gerds <tag@biostat.ku.dk>
;; Keywords: 

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
(use-package header2)
(autoload 'auto-update-file-header "header2")
(add-hook 'write-file-hooks 'auto-update-file-header)
(autoload 'auto-make-header "header2")
;;(add-hook 'emacs-lisp-mode-hook 'auto-make-header)
(add-hook 'R-mode-hook 'auto-make-header)
;;(add-hook 'org-mode-hook 'auto-make-header)
(setq header-date-format  "%b %e %Y (%R) ")

(defsubst header-end-line ()
  "Insert a divider line."
  (insert (cond (comment-end-p comment-end)
                ((and comment-start (= 1 (length comment-start)))
		 (concat comment-start "" (make-string 70 (aref "-" 0))))
		;; (make-string 70 (aref comment-start 0)))
		(comment-start-p comment-start)
		(t (make-string 70 ?\;)))
	  "\n"))
(setq comment-end-p nil)
(setq make-header-hook '(
                         ;;header-mode-line
                         header-title
			 header-end-line
                         ;;header-blank
					;header-file-name
					;header-description
                         ;;header-status
                         ;;header-copyright
                         header-author
                         ;;header-maintainer
                         ;;header-blank
                         header-creation-date
                         ;;header-rcs-id
                         header-version
                         ;;header-sccs
			 header-modification-date
			 header-modification-author
			 header-update-count
			 header-end-line
                         ;;header-url
                         ;;header-keywords
                         ;;header-compatibility
                         ;;header-free-software
                         ;;header-blank
                         ;;header-lib-requires
					;header-blank
					;header-end-line
                         header-commentary
                         ;;header-blank
                         ;;header-blank
                         ;;header-blank
                         ;;header-end-line
                         header-history
                         ;;header-rcs-log
                         ;;header-blank
                         ;;header-blank
                         header-end-line
                         ;;header-free-software
                         header-code
                         header-eof
                         ))
(provide 'header2-snps)
;;; header2-snps.el ends here
