;;; browse-url-snps.el --- google search from emacs   -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Thomas Alexander Gerds

;; Author: Thomas Alexander Gerds <grb615@ku.dk>
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This is a crude hack to send google requests from emacs to browser.
;; Main function is M-x google-search-prompt 
;;; Code:



;; (require 'thingatpt+)
(defun region-or-word-at-point ()
  "Return non-empty active region or word at point."
  (if (and transient-mark-mode mark-active
           (not (eq (region-beginning) (region-end))))
      (buffer-substring-no-properties (region-beginning) (region-end))
    (current-word)))

(setq apropos-url-alist
      '(("^G?:? +\\(.*\\)" . ;; Google  
         "http://www.google.com/search?q=\\1")

	("^gw?:? +\\(.*\\)" . ;; Google Web 
         "http://www.google.com/search?q=\\1")

        ("^g!:? +\\(.*\\)" . ;; Google Lucky
         "http://www.google.com/search?btnI=I%27m+Feeling+Lucky&q=\\1")
        
        ("^gl:? +\\(.*\\)" .  ;; Google Linux 
         "http://www.google.com/linux?q=\\1")
        
        ("^gi:? +\\(.*\\)" . ;; Google Images
         "http://images.google.com/images?sa=N&tab=wi&q=\\1")

        ("^gg:? +\\(.*\\)" . ;; Google Groups
         "http://groups.google.com/groups?q=\\1")

        ("^gd:? +\\(.*\\)" . ;; Google Directory
         "http://www.google.com/search?&sa=N&cat=gwd/Top&tab=gd&q=\\1")

        ("^gn:? +\\(.*\\)" . ;; Google News
         "http://news.google.com/news?sa=N&tab=dn&q=\\1")

        ("^gt:? +\\(\\w+\\)|? *\\(\\w+\\) +\\(\\w+://.*\\)" . ;; Google Translate URL
         "http://translate.google.com/translate?langpair=\\1|\\2&u=\\3")
        
        ("^gt:? +\\(\\w+\\)|? *\\(\\w+\\) +\\(.*\\)" . ;; Google Translate Text
         "http://translate.google.com/translate_t?langpair=\\1|\\2&text=\\3")

        ("^/\\.$" . ;; Slashdot 
         "http://www.slashdot.org")

        ("^/\\.:? +\\(.*\\)" . ;; Slashdot search
         "http://www.osdn.com/osdnsearch.pl?site=Slashdot&query=\\1")        
        
        ("^fm$" . ;; Freshmeat
         "http://www.freshmeat.net")

        ("^ewiki:? +\\(.*\\)" . ;; Emacs Wiki Search
         "http://www.emacswiki.org/cgi-bin/wiki?search=\\1")
 
        ("^ewiki$" . ;; Emacs Wiki 
         "http://www.emacswiki.org")

        ("^arda$" . ;; The Encyclopedia of Arda 
         "http://www.glyphweb.com/arda/")
	
         ))
(add-to-list 'apropos-url-alist '("^googledict:? +\\(\\w+\\)|? *\\(\\w+\\) +\\(.*\\)" . "http://www.google.com/dictionary?aq=f&langpair=\\1|\\2&q=\\3&hl=\\1"))
(add-to-list 'apropos-url-alist '("^ewiki2:? +\\(.*\\)" .  "http://www.google.com/cse?cx=004774160799092323420%3A6-ff2s0o6yi&q=\\1&sa=Search"))

;; Don't know if it's the best way , but it seemed to work. (Requires emacs >= 20)
(defun browse-apropos-url (text)
  (interactive (browse-url-interactive-arg "Location: "))
  (let ((text (replace-regexp-in-string 
               "^ *\\| *$" "" 
               (replace-regexp-in-string "[ \t\n]+" " " text))))
    (let ((url (assoc-default 
                text apropos-url-alist 
                '(lambda (a b) (let () (setq __braplast a) (string-match a b)))
                text)))
      (tag-browse-url t (replace-regexp-in-string __braplast url text)))))

(defun tag-browse-url (arg &optional url)
  "Browse the URL passed. Use a prefix arg for external default browser else use default browser which is probably W3m"
  (interactive "P")
  (setq url (or url (w3m-url-valid (w3m-anchor)) (browse-url-url-at-point) (region-or-word-at-point)))
  (if arg
      (when url (browse-url-default-browser url))
    (if  url (browse-url url) (call-interactively 'browse-url))
    ))

(defun browse-url-firefox (url &optional new-window)
  "Ask the Firefox WWW browser to load URL.
Default to the URL around or before point.  The strings in
variable `browse-url-firefox-arguments' are also passed to
Firefox.

When called interactively, if variable
`browse-url-new-window-flag' is non-nil, load the document in a
new Firefox window, otherwise use a random existing one.  A
non-nil interactive prefix argument reverses the effect of
`browse-url-new-window-flag'.

If `browse-url-firefox-new-window-is-tab' is non-nil, then
whenever a document would otherwise be loaded in a new window, it
is loaded in a new tab in an existing window instead.

When called non-interactively, optional second argument
NEW-WINDOW is used instead of `browse-url-new-window-flag'.

On MS-Windows systems the optional `new-window' parameter is
ignored.  Firefox for Windows does not support the \"-remote\"
command line parameter.  Therefore, the
`browse-url-new-window-flag' and `browse-url-firefox-new-window-is-tab'
are ignored as well.  Firefox on Windows will always open the requested
URL in a new window."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment))
	 (use-remote nil) ;; fix for firefox version 36.0
	  ;; (not (memq system-type '(windows-nt ms-dos))))
	 (process
	  (apply 'start-process
		 (concat "firefox " url) nil
		 browse-url-firefox-program
		 (append
		  browse-url-firefox-arguments
		  (if use-remote
		      (list "-remote"
			    (concat
			     "openURL("
			     url
			     (if (browse-url-maybe-new-window new-window)
				 (if browse-url-firefox-new-window-is-tab
				     ",new-tab"
				   ",new-window"))
			     ")"))
		    (list url))))))
    ;; If we use -remote, the process exits with status code 2 if
    ;; Firefox is not already running.  The sentinel runs firefox
    ;; directly if that happens.
    (when use-remote
      (set-process-sentinel process
			    `(lambda (process change)
			       (browse-url-firefox-sentinel process ,url))))))

(defun google (&optional string)
  (interactive "sString: ")
  "Call google search for the specified term. Do not call if string is zero length."
  (let ((url (if (zerop (length string)) "http://www.google.com " (concat "gw: '" string "'"))))
    (browse-apropos-url url)))

(defun google-scholar (&optional string)
  (interactive "sString: ")
  "Call google-scholar search for the specified term. Do not call if string is zero length."
  (let ((url (if (zerop (length string)) "http://www.google.com " (concat "gw: " string))))
    (browse-apropos-url url)))

; use f4 for direct URLs. C-u f4 for external default browser.
;; (global-set-key (kbd "<f4>") 'google-search-prompt)


(defun google-search-prompt()
  (interactive)
  (google (region-or-word-at-point)))

(defun call-google-translate (langpair prompt)
  (interactive)
  (let* ((thing (region-or-word-at-point)))
    (setq thing (read-string (format prompt thing) nil nil thing))
    (browse-apropos-url  (concat (if (string-match " " thing) (quote "gt")(quote "googledict")) " " langpair " " thing))))
  
; google keys and url keys
;; (define-key mode-specific-map [?B] 'browse-apropos-url)

(provide 'browse-url-snps)
;;; browse-url-snps.el ends here
