;;; superman-deft.el --- hacking deft for org project manager 

;; Copyright (C) 2012-2016  Thomas Alexander Gerds, Klaus Kähler Holst

;; Authors: Thomas Alexander Gerds <tag@biostat.ku.dk>
;;          Klaus Kähler Holst <kkho@biostat.ku.dk>
;; Keywords: tools

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
;; Some crude hacks to search for project index files
;; and to start new projects using deft 
;; 

;;; Code:

(require 'deft)

(defun deft-local-setup ()
  ;; (kill-all-local-variables)
  (make-variable-buffer-local 'deft-buffer)
  (make-variable-buffer-local 'deft-directory)
  (make-variable-buffer-local 'deft-current-files)
  (make-variable-buffer-local 'deft-all-files)
  (make-variable-buffer-local 'deft-filter-regexp)
  (make-variable-buffer-local 'deft-find-all-files-function)
  (make-variable-buffer-local 'deft-new-file-function)
  (make-variable-buffer-local 'deft-filter-match-file-function)
  (make-variable-buffer-local 'deft-hash-mtimes)
  (make-variable-buffer-local 'deft-hash-contents)
  (make-variable-buffer-local 'deft-hash-titles)
  (make-variable-buffer-local 'deft-hash-summaries)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq default-directory deft-directory)
  (use-local-map deft-mode-map)
  (deft-cache-initialize)
  (deft-cache-update-all)
  (deft-filter-initialize)
  (setq major-mode 'deft-mode)
  (deft-set-mode-name)
  (deft-buffer-setup) ;; calls deft-refresh
  (when (> deft-auto-save-interval 0)
    (run-with-idle-timer deft-auto-save-interval t 'deft-auto-save))
  (run-mode-hooks 'deft-mode-hook))


(defun deft-local-mode ()
  ;; (kill-all-local-variables)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq default-directory deft-directory)
  (use-local-map deft-mode-map)
  (deft-cache-initialize)
  (deft-cache-update-all)
  (deft-filter-initialize)
  (setq major-mode 'deft-mode)
  (deft-set-mode-name)
  (deft-buffer-setup) ;; calls deft-refresh
  (when (> deft-auto-save-interval 0)
    (run-with-idle-timer deft-auto-save-interval t 'deft-auto-save))
  (run-mode-hooks 'deft-mode-hook))

(defvar deft-new-file-function 'deft-new-file-local)
(defvar deft-find-all-files-function 'deft-find-all-files-local)
(defvar deft-filter-match-file-function 'deft-filter-match-file-local)

(defun deft-filter-match-file (file &optional batch)
  (funcall deft-filter-match-file-function file batch))
(defun deft-find-all-files ()
  (funcall deft-find-all-files-function))
(defun deft-new-file ()
  (funcall deft-new-file-function))

;; copy of original deft-find-all-files
(defun deft-find-all-files-local (&optional dir)
  "Return a list of all files in the Deft directory."
  (let ((dir (or dir deft-directory)))
    (if (file-exists-p dir)
	(let (files result)
	  ;; List all files
	  (setq files
		(directory-files dir t
				 (concat "\." deft-extension "$") t))
	  ;; Filter out files that are not readable or are directories
	  (dolist (file files)
	    (when (and (file-readable-p file)
		       (not (file-directory-p file)))
	      (setq result (cons file result))))
	  result))))

;; copy of original deft-filter-match-file
(defun deft-filter-match-file-local (file &optional batch)
  "Return FILE if FILE matches the current filter regexp."
  (let ((dfr deft-filter-regexp)
        (dhc deft-hash-contents)
        (dhs deft-hash-summaries)
        (dht deft-hash-titles)
        (dhm deft-hash-mtimes))
    (with-temp-buffer
      (make-variable-buffer-local 'deft-filter-regexp)
      (make-variable-buffer-local 'deft-hash-summaries)
      (make-variable-buffer-local 'deft-hash-titles)
      (make-variable-buffer-local 'deft-hash-contents)
      (make-variable-buffer-local 'deft-hash-mtimes)
      (setq deft-filter-regexp dfr
            deft-hash-contents  dhc 
	    deft-hash-summaries dhs
	    deft-hash-titles dht
	    deft-hash-mtimes dhm)
      (setq deft-filter-regexp dfr)
      (insert file)
      (insert (deft-file-title file))
      (insert (deft-file-contents file))
      (if batch
	  (if (every (lambda (filter)
		       (goto-char (point-min))
		       (deft-search-forward filter))
		     deft-filter-regexp)
	      file)
	(goto-char (point-min))
	(if (deft-search-forward (car deft-filter-regexp))
	    file)))))

;; hack for new files
(defun deft-file-mtime (file)
  "Retrieve modified time of FILE from cache."
  (when deft-hash-mtimes
  (gethash file deft-hash-mtimes)))

;; copy of original def-new-file
(defun deft-new-file-local ()
  "Create a new file quickly, with an automatically generated filename
or the filter string if non-nil and deft-use-filename-as-title is set.
If the filter string is non-nil and title is not from filename,
use it as the title."
  (interactive)
  (let (filename)
    (if (and deft-use-filename-as-title deft-filter-regexp)
	(setq filename (concat (file-name-as-directory deft-directory) (deft-whole-filter-regexp) "." deft-extension))
      (let (fmt counter temp-buffer)
	(setq counter 0)
	(setq fmt (concat "deft-%d." deft-extension))
	(setq filename (concat (file-name-as-directory deft-directory)
			       (format fmt counter)))
	(while (or (file-exists-p filename)
		   (get-file-buffer filename))
	  (setq counter (1+ counter))
	  (setq filename (concat (file-name-as-directory deft-directory)
				 (format fmt counter))))
	(when deft-filter-regexp
	  (write-region (concat (deft-whole-filter-regexp) "\n\n") nil filename nil))))
    (deft-open-file filename)
    (with-current-buffer (get-file-buffer filename)
      (goto-char (point-max)))))

(defun superman-deft-new-project ()
  "Create a new project quickly."
  (interactive)
  (superman-new-project (deft-whole-filter-regexp)))

(defun superman-deft ()
  (interactive)
  (switch-to-buffer "*deft projects*")
  (deft-mode)
  (deft-local-setup)
  (setq deft-directory superman-default-directory)
  (setq deft-find-all-files-function 'superman-index-list)
  (setq deft-new-file-function 'superman-deft-new-project)
  (setq deft-buffer (current-buffer))
  (deft-local-mode))

(provide 'superman-deft)
;;; superman-deft.el ends here



