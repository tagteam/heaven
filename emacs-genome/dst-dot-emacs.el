;;; dst-dot-emacs.el --- emacs init file for Denmark Statistics  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2021  Thomas Alexander Gerds

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

;; the emacs-genome is set by install-dst-emacs.el during setup
;; and at DST needs to be set in the ~/.emacs file to the right place
;; (setq emacs-genome "v:/data/emacs/emacs-genome/")

(add-to-list 'load-path (expand-file-name "snps" emacs-genome))
(setq installing (eq system-type 'gnu/linux))
(if (not installing)
    ;; inside DST or on mac/windows
    (if (file-exists-p (expand-file-name "snps/dst-package-path.el" emacs-genome))
	(load-file (expand-file-name "snps/dst-package-path.el" emacs-genome))
      (message "Cannot see file snps/dst-package-path.el"))
  ;; installing packages (outside DST)
  ;; check if emacs-genome is bound 
  (if (not (and (boundp 'emacs-genome) 
		(file-directory-p emacs-genome)))
      (let ((mess (concat "Cannot load emacs-genome: Variable emacs-genome does not specify a directory file."
			  "\nTo investigate the problem you could start an interactive lisp session via M-x ielm RET,"
			  "\nand then evaluate the variable\n\nELISP> emacs-genome\n\n and the test\n\nELISP> (file-directory-p emacs-genome)\n\nat the prompt."
			  "\n\nIf you have downloaded the emacs-genome in the folder\n\n" 
			  (expand-file-name "~" nil)
			  "\n\nThen, the value of the variable emacs-genome should be\n\n" (expand-file-name "~/emacs-genome/" nil)
			  "\n\nThat is, you should have a line\n\n(setq emacs-genome '~/emacs-genome/')\n\n in your init file (e.g., ~/.emacs or ~/.emacs.d/init.el).")))
	(pop-to-buffer "*EG load error*")
	(erase-buffer)
	(insert mess) 
	(error mess))
    (message (concat "Reading genes and snps from: " emacs-genome))))
;; locate emacs packages to emacs-genome
(require 'package)
(when installing
  (setq eg-elpa-sources '(("elpa" . "https://tromey.com/elpa/")
			  ("gnu" . "https://elpa.gnu.org/packages/")
			  ("org" . "https://orgmode.org/elpa/")
			  ("melpa" . "https://melpa.org/packages/")
			  ;;("melpa-stable" . "https://stable.melpa.org/packages/")
			  ;;("marmalade" . "https://marmalade-repo.org/packages/")
			  ))
  (dolist (source eg-elpa-sources) (add-to-list 'package-archives source t)))
(add-to-list 'package-directory-list (expand-file-name "genes/" emacs-genome))
(add-to-list 'package-directory-list package-user-dir)
(setq orig-package-user-dir package-user-dir)
(setq package-user-dir (expand-file-name "genes/" emacs-genome))
(setq package-enable-at-startup nil)   ; To prevent initialising twice
(package-initialize)
(when installing
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(require 'use-package)
(setq use-package-verbose t)

;; look, feel and behaviour
(use-package appearance-snps)
;; keybindings
(use-package global-key-snps)
;; org mode
(use-package org
  :ensure org-plus-contrib ;; ensure org's devel
  :pin org
  :config
  (use-package org-snps))

;; folding
(use-package folding)
(use-package fold-dwim)
(use-package folding-snps)

;; completion/expansion
(use-package company
  :ensure t :config)
;; completion in R 
;; (use-package ac-R :ensure t)
(use-package hippie-exp
  :commands hippie-expand)
(use-package auto-complete
  :ensure t)
(use-package popup-complete
  :ensure t)
(use-package yasnippet
  :ensure t)
(setq yas-snippet-dirs `(,(concat emacs-genome "/snps/yasnippets")))
(yas-global-mode 1)
(use-package auto-yasnippet
  :ensure t)
;; auto header for R-files
(use-package header2 
  :config
  (use-package header2-snps))

;; header buttons
(use-package header-button)

;; iedit mode 
(use-package iedit
  :ensure t
  :commands iedit-mode)

;; buffer and window cycling
(use-package ido
  :config
  (ido-mode 'buffers)
  ;;flexibly match names via fuzzy matching
  (setq ido-enable-flex-matching t)
  ;; use ido-mode everywhere, in buffers and for finding files
  (setq ido-everywhere nil)
  ;; sort-order, gives preferences to org 
  (setq ido-file-extensions-order '("org" "R" "pdf" "tex" "el"))
  (setq ido-default-buffer-method 'selected-window)
  ;; Last visited files appear in ido-switch-buffer C-x b
  (setq ido-use-virtual-buffers t))
(use-package ido-completing-read+ :ensure t)
;; buffer cycling
(use-package cycle-buffer-snps)
;; window cycling
(use-package winner
  :config
  (winner-mode))

;; browse url
(use-package browse-url-snps
  :commands (browse-url google-search-prompt))

;; deft
(use-package deft :ensure t)

;; anything/helm
(use-package helm
  :ensure t
  :config
  (use-package helm-config))

;; git
(use-package magit :ensure t)

;; shell and ssh within emacs
(use-package shell-snps)
(use-package ssh :ensure t)

;; pandoc: converting code and documents
(use-package pandoc-mode
  :ensure t)

;; markdown
(use-package  markdown-mode :ensure t)

;; Emacs speaks statistics: mostly R
(use-package ess-site
 :ensure ess)
(use-package ess-edit)
(use-package ess-R-snps)
;; (setq ess-use-auto-complete 'script-only)

;; LaTeX
(use-package tex-site
  :ensure auctex)
(use-package latex-snps)

;; superman
(add-to-list 'load-path (expand-file-name "genes/SuperMan/lisp" emacs-genome))

(use-package superman-manager
  :config
  ;; project profile
  (unless (file-exists-p superman-profile)
    (setq superman-profile "~/.SuperMan.org"))
  (superman-parse-projects)
  ;; header buttons
  (use-package header-button)
  (add-hook 'org-mode-hook #'(lambda ()
			       (when (buffer-file-name)
				 (superman-org-headline-mode)))))

(file-list-default-keybindings)

(global-set-key [(f2)] 'superman-switch-to-project)
(add-to-list 'file-list-directory-filter "^\\\.[a-zA-Z]+/")
(add-to-list 'file-list-file-name-filter "~$")

;; start-up behaviour
(setq inhibit-startup-screen 'yes-please)

(use-package emacs-genes)

(eg)

;; backtransform to original package location
(setq package-user-dir orig-package-user-dir)

(provide 'dst-dot-emacs)
;;; dst-dot-emacs.el ends here
