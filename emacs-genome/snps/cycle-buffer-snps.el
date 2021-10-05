;;; cycle-buffer-snps.el --- cycle through buffers by major-mode

;; Copyright (C) 2012  Thomas Alexander Gerds

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

;; cycle through buffers by major mode

(defun next-mode-buffer (&optional backwards)
  (interactive "P")
  (let* ((curr-mode major-mode)
	 (buffers (if backwards
		      (reverse (buffer-list))
		    (progn (bury-buffer) (buffer-list)))))
    (switch-to-buffer
     (find-if #'(lambda (buf)
		  (set-buffer buf)
		  (and (eq major-mode curr-mode)
		       (not (string-match "\\` " (buffer-name)))))
	      buffers))))

(defun next-mode-buffer-backward ()
  (interactive) (next-mode-buffer t))

(provide 'cycle-buffer-snps)
;;; cycle-buffer-snps.el ends here