;;; popup-complete-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "popup-complete" "popup-complete.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from popup-complete.el

(autoload 'popup-complete--in-region "popup-complete" "\


\(fn NEXT-FUNC START END COLLECTION &optional PREDICATE)" nil nil)

(add-hook 'completion-in-region-functions 'popup-complete--in-region)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "popup-complete" '("popup-complete-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; popup-complete-autoloads.el ends here
