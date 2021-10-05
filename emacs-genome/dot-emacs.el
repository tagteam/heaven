;; Init file for 
;; and at DST needs to be set in the ~/.emacs file to the right place
;; (setq emacs-genome "v:/data/emacs/emacs-genome/")
(load-file (expand-file-name "init.el" (expand-file-name emacs-genome)))

;; avoid problems on DST windows machines (use magit for backup!)
(setq make-backup-files nil)
;; a personal habit
(global-set-key [f4] 'delete-other-windows)
;; useful for killing R
(global-set-key [f5] 'eg/kill-this-buffer)
;; for danish keyboards
(global-set-key [(meta æ)] 'comment-or-uncomment-line-or-region)
;;
;; (ignore-errors
  ;; (let* ((candidates (reverse (directory-files "c:/Program Files/R/" 'full "^R" nil)))
	 ;; (candidate (concat (car candidates) "/bin/x64/R.exe")))
    ;; (if (file-exists-p candidate)
	;; (progn 
	  ;; (message (concat "Set R-program: " candidate))
	  ;; (setq inferior-R-program-name candidate))
      ;; (message (concat "R program file name does not exist: " candidate)))))
