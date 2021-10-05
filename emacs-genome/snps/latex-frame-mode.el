;;; latex-frame-mode.el --- minor mode for latex beamer geeks equipped with folding power

;; Copyright (C) 2011  Thomas Alexander Gerds

;; Author: Thomas Alexander Gerds <tagteam@sund.ku.dk>
;; Keywords: convenience, tex
;; Version: 0.0.1 (12 Oct 2011)

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

;;;  Installation
;;;
;;; save the file somewhere in the load-path of your (X)emacs and add the
;;; line
;;;    (require 'latex-frame-mode)
;;; to your ~/.xemacs/init.el or ~/.emacs file
;;; if you do not know what the load-path is you may say
;;;    (load-file "/path/to/latex-frame-mode.el")
;;; in your ~/.xemacs/init.el or ~/.emacs file
;;;
;;;  Usage
;;;
;;; In your latex buffer say first
;;;   M-x latex-frame-mode
;;; and somewhere between \begin{document} and \end{document}
;;;   hit `C-c SPACE'
;;;
;;; Code:

(defvar latex-frame-mode nil)
(make-variable-buffer-local 'latex-frame-mode)

(defvar latex-frame-map (make-sparse-keymap)
  "Keymap used for `latex-frame-mode' commands.")

(define-key latex-frame-map "\C-c " 'latex-frame-new-frame)
(define-key latex-frame-map "\C-cn" 'latex-frame-forward-frame)
(define-key latex-frame-map "\C-cp" 'latex-frame-backward-frame)
(define-key latex-frame-map "\C-cb" 'latex-frame-backward-frame)
(define-key latex-frame-map "\C-c1" 'latex-frame-goto-first-frame)
(define-key latex-frame-map "\C-ch" 'latex-frame-mark-this-frame)
(define-key latex-frame-map "\C-cm" 'latex-frame-mark-this-frame)
(define-key latex-frame-map "\C-j" 'newline)
(define-key latex-frame-map "\C-cc" 'latex-frame-refresh)

(or (assq 'latex-frame-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (append minor-mode-map-alist
		  (list (cons 'latex-frame-mode latex-frame-map)))))

(defun latex-frame-mode (&optional arg)
  "A minor mode for latex beamer documents
Activate:        M-x latex-frame-mode
Insert a frame:  \C-c SPACE
Mark a frame:  \C-c m
Goto first frame:  \C-c 1
Goto next frame:  \C-c n
Goto previous frame:  \C-c b
"
  (interactive "P")
  (setq latex-frame-mode
	(not (or (and (null arg) latex-frame-mode)
		 (<= (prefix-numeric-value arg) 0)))))


(or (assq 'latex-frame-mode minor-mode-alist)
              (setq minor-mode-alist
                    (cons '(latex-frame-mode " Beamer") minor-mode-alist)))

(defun latex-frame-goto-first-frame ()
  (interactive)
  (goto-char (point-min))
  (latex-frame-forward-frame))

(defun latex-frame-forward-frame ()
  (interactive)
  (when (re-search-forward "\\\\end{frame}" nil t)
    (re-search-forward "\\\\begin{frame}" nil t)
    ))

(defun latex-frame-beginning () 
  (re-search-backward "\\\\begin{frame}" nil t)
  (beginning-of-line -1))

(defun latex-frame-end () 
  (re-search-forward "\\\\end{frame}" nil t)
  (beginning-of-line 2))

(defun latex-frame-delete-frame ()
  (interactive)
  (latex-frame-mark-this-frame)
  (delete-region (region-beginning) (region-end))
  (latex-frame-refresh-labels))

(defun latex-frame-backward-frame ()
  (interactive)
  (latex-frame-beginning)
  (latex-frame-beginning)
  (forward-line -1))

(defun latex-frame-mark-this-frame ()
  (interactive)
  (latex-frame-beginning)
  (push-mark
   (save-excursion
     (latex-frame-end)
     (point))
   nil t))

(defun latex-frame-folding-top-mark ()
  (or
   (car (cdr (assoc major-mode folding-mode-marks-alist)))
   "% "))

(defun latex-frame-folding-bottom-mark ()
  (or
   (caddr (assoc major-mode folding-mode-marks-alist))
   "% "))

(defun latex-frame-new-frame ()
  (interactive)
  (LaTeX-insert-environment "frame" "\\frametitle{ }")
  (save-excursion
    (beginning-of-line 0)
    (newline)
    (beginning-of-line 0)
    (insert "\n" (latex-frame-folding-top-mark) " Frame 17 \n%%" 
	    (make-string (- fill-column 2) (string-to-char "-")))
    (re-search-forward "\\\\end{frame}" nil nil)
    (insert "\n" (latex-frame-folding-bottom-mark))
    (latex-frame-refresh))
    (when (folding-mode)
      (fold-dwim-show)))

(defun latex-frame-refresh ()
  (interactive)
  (save-excursion
    (latex-frame-refresh-labels)
    (latex-frame-refresh-titles)))

(defun latex-frame-refresh-titles ()
  (interactive)
  (let ((fmode folding-mode))
    (when fmode (folding-mode))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (concat "\\(^" (latex-frame-folding-top-mark) "[ \t]*Frame[ \t]*[0-9]+\\)\\(.*\\)$") nil t)
	(replace-match (match-string 1))
	(insert " "
		(save-excursion
		  (let* ((fb (re-search-forward "\\\\begin{frame}" nil t))
			 (fe (re-search-forward "\\\\end{frame}" nil t)))
		    (latex-frame-get-title fb fe))))))
    (when fmode (folding-mode))))

       
(defun latex-frame-refresh-labels ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((counter 1))
      (while (re-search-forward (concat "^" (latex-frame-folding-top-mark) "[ \t]*Frame[ \t]*\\([0-9]+\\)") nil t)
	(replace-match (concat (latex-frame-folding-top-mark) " Frame " (int-to-string counter)))
	(setq counter (1+ counter))))
    (save-buffer)))

;; (defun latex-frame-insert-graph ()
  ;; (interactive)
  ;; (LaTeX-insert-environment "center" (concat "\n\\includegraphics[width=8cm,angle=-90]{" (expand-file-name (default-directory)) "figure.pdf}")))

(defun latex-frame-fix-old-style (&optional ignore-header)
  (interactive)
  (goto-char (point-min))
  (unless ignore-header
    (insert (latex-frame-folding-top-mark) "Header\n")
    (re-search-forward "begin{document}")
    (beginning-of-line)
    (insert (latex-frame-folding-top-mark) "\n"))
  (while (re-search-forward "%% Frame" nil t)
    (replace-match "Frame")
    (beginning-of-line)
    (insert  (latex-frame-folding-bottom-mark) " ")
    (let ((ppp (progn (end-of-line) (point)))
	  (title (if (re-search-forward "frametitle{\\(.*\\)}" nil t)
		     (match-string 1)
		   "")))
      (goto-char ppp)
      (insert " " title))
    (latex-frame-end)
    (insert (latex-frame-folding-bottom-mark))))

(defun latex-frame-get-title (&optional frame-beg frame-end)
  (save-excursion
    (goto-char frame-beg)
    (cond
     ((re-search-forward "\\\\frametitle{\\(.*\\)" frame-end t)
      (replace-regexp-in-string "}$" "" (match-string 1)))
     ((progn
	(goto-char frame-beg)
	(re-search-forward "\\\\titlepage" frame-end t))
      "Titlepage")
     ((progn
	(goto-char frame-beg)
	(re-search-forward "\\\\includegraphics.*{\\(.*\\.ps\\)}" frame-end t)
	(match-string 1)))
     (t "Untitled"))))
     
(defun latex-frame-make-folds (&optional ignore-header)
  (interactive)
  (let* ((marks (cdr (assoc major-mode folding-mode-marks-alist)))
	 (folding-start (car marks))
	 (folding-end (cadr marks)))
    (unless ignore-header
      (goto-char (point-min))
      (if (re-search-forward (concat "^" folding-start ".*Header.*$") nil t)
	  (re-search-forward (concat "^" folding-end))
	(insert folding-start " Header\n")
	(re-search-forward "begin{document}")
	(beginning-of-line)
	(insert (concat folding-end "\n"))))
    (while (re-search-forward "\\\\begin{frame}" nil t)
      (let ((fb (re-search-backward "\\\\begin{frame}" nil t))
	    (fe (re-search-forward "\\\\end{frame}" nil t)))
	(goto-char fb)
	(insert "\n" (latex-frame-folding-top-mark) " Frame 1 " (latex-frame-get-title fb fe) "\n") 
	(goto-char (re-search-forward "\\\\end{frame}" nil t))
	(insert "\n" (latex-frame-folding-bottom-mark) " ")))
    (latex-frame-refresh-labels)))

(defun latex-frame-fix-folds (&optional ignore-header)
  (interactive)
  (let* ((marks (cdr (assoc major-mode folding-mode-marks-alist)))
	 (folding-start (car marks))
	 (folding-end (cadr marks)))
    (unless ignore-header
      (goto-char (point-min))
      (if (re-search-forward (concat "^" folding-start ".*Header.*$") nil t)
	  (re-search-forward (concat "^" folding-end))
	(insert folding-start " Header\n")
	(re-search-forward "begin{document}")
	(beginning-of-line)
	(insert (concat folding-end "\n"))))
    (while (re-search-forward "\\\\begin{frame}" nil t)
      (let ((fb (re-search-backward "\\\\begin{frame}" nil t))
	    (fe (re-search-forward "\\\\end{frame}" nil t)))
	(goto-char fb)
	(insert "\n" (latex-frame-folding-top-mark) " Frame 1 " (latex-frame-get-title fb fe) "\n") 
	(goto-char (re-search-forward "\\\\end{frame}" nil t))
	(insert "\n" (latex-frame-folding-bottom-mark) " ")))
    (latex-frame-refresh-labels)))

(defun latex-frame-remove-whitelines ()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "^[ \t]*$" nil t)
    (kill-entire-line)))


(defun latex-frame-fix-folds-this-frame ()
  (interactive)
  (narrow-to-region
   (progn (latex-frame-beginning) (point))
   (progn (latex-frame-end) (point)))
  (latex-frame-make-folds t)
  (widen))


(defun latex-frame-looking-at-backward (regexp)
  (let* ((begin (point))
	 (found (re-search-backward regexp nil t)))
    (goto-char begin)
    (and found (= begin (match-end 0)))))

(defun exercises-to-solutions ()
  (interactive)
  (let* ((case-fold-search t)
	 (html-file (concat
		     (file-name-sans-extension
		      (buffer-file-name (current-buffer))) ".html"))
	 (solution-file (replace-regexp-in-string "Exercises" "Solutions" html-file)))
    (goto-char (point-min))
    (while (re-search-forward "COMMENT Solutions" nil t)
      (replace-match "Solutions"))
    (org-html-export-to-html)
    (copy-file html-file solution-file t)
    (goto-char (point-min))
    (while (re-search-forward "*[ ]+Solutions" nil t)
      (replace-match "* COMMENT Solutions"))
    (org-html-export-to-html)))

(defun beamer-handout ()
  (interactive)
  (let* ((pdf-file (concat
		    (file-name-sans-extension
		     (buffer-file-name (current-buffer))) ".pdf"))
	 (handout-file (concat
			(file-name-sans-extension
			 (buffer-file-name (current-buffer))) "-handout.pdf")))
    (shell-toggle-cd)
    (comint-send-input)
    (insert (concat "cp " pdf-file " " handout-file ";" 
		    "pdfnup --nup 2x2 --suffix '2x2' " handout-file))))
  
(defun beamer-to-org ()
  (interactive)
  (save-excursion
  (goto-char (point-min))
  (while (re-search-forward "\\\\begin{frame}\\\\frametitle{" nil t)
    (replace-match "** ")
    (end-of-line)
    (if (latex-frame-looking-at-backward "}")
    (replace-match "")))
  (goto-char (point-min))
  (while (re-search-forward "\\\\end{frame}" nil t)
    (replace-match "\n"))
  (goto-char (point-min))
  (while (re-search-forward "\\\\begin{itemize}" nil t)
    (replace-match "\n"))
  (goto-char (point-min))
  (while (re-search-forward "\\\\end{itemize}" nil t)
    (replace-match "\n"))
  (goto-char (point-min))
  (while (re-search-forward "\\\\item" nil t)
    (replace-match " - "))
  (goto-char (point-min))
  (while (re-search-forward "{\\\\bf" nil t)
    (replace-match "*")
    (re-search-forward "}" nil t)
    (replace-match "*") )))



(provide 'latex-frame-mode)


;;; latex-frame-mode.el ends here

