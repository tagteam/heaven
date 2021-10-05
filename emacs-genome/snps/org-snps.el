;;; org-snps.el --- 

;; Copyright (C) 2012-2018  Thomas Alexander Gerds

;; Author: Thomas Alexander Gerds <tag@linuxifsv007>
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

;;; Code:

;;{{{ loading org features
(require 'org nil t)
(require 'org-capture nil t)
(require 'org-agenda nil t)
(require 'ox-bibtex nil t)
(require 'org-clock nil t)
;; (require 'ob-R)
(require 'ox-latex nil t)
(require 'ox-beamer nil t)
(require 'ox-odt nil t)
;;}}}
;;{{{ babel settings
(setq org-babel-hash-show-time t)
;; it is most transparent and convenient 
;; if export actions *never* evaluate code:
(setq org-export-use-babel t)
;; (setq org-babel-default-header-args '((:tangle . "yes") (:eval . "never-export") (:session . "none") (:results . "replace") (:exports . "code") (:cache . "no") (:noweb . "no") (:hlines . "no")))

;; sanity is to set org-export-babel-evaluate to nil (Ista Zahn)
(add-to-list 'org-babel-default-header-args '(:eval . "never-export") 'append)
(add-to-list 'org-babel-default-header-args '(:tangle . "yes") 'append)
(add-to-list 'org-babel-tangle-lang-exts '("R" . "R") 'append)
;; org-src-lang-modes

;;}}}
;;{{{ disable hypersetup
(setq org-latex-with-hyperref nil)
;;}}}
(defun ox-export-to-docx-and-open ()
 "Export the current org file as a docx via markdown."
 (interactive)
 (let* ((bibfile (expand-file-name (car (org-ref-find-bibliography))))
        ;; this is probably a full path
        (current-file (buffer-file-name))
        (basename (file-name-sans-extension current-file))
        (docx-file (concat basename ".docx")))
   (save-buffer)
   (when (file-exists-p docx-file) (delete-file docx-file))
   (shell-command (format
                   "pandoc -s -S --bibliography=%s %s -o %s"
                   bibfile current-file docx-file))
   (org-open-file docx-file '(16))))

;;{{{ babel 
(defun eg/indent-elisp-org-buffer ()
  (interactive)
  (emacs-lisp-mode)
  (save-excursion (goto-char (point-min))
		  (while (re-search-forward "\\(^[ \t]+\\)\\((defun\\|(defvar\\)" nil t)
		    (replace-match (match-string-no-properties 2))))
  (mark-whole-buffer)
  (indent-region)
  (org-mode))

;;}}}
;;{{{adding special project agenda view

;; (setq org-time-stamp-custom-formats '("<%m/%d/%y %a>" . "<%m/%d/%y %a %H:%M>")
(setq org-time-stamp-custom-formats '("<%d/%b/%Y %a>" . "<%m/%d/%y %a %H:%M>"))

;;}}}
;;{{{ org mode

(defun org-mode-p () (eq major-mode 'org-mode))
(setq org-return-follows-link t)
;; (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-log-done t)

(defun org-babel-execute-above ()
  "Execute all code chunks above."
  (interactive)
  (save-restriction
    (save-excursion
      (let ((info (org-babel-get-src-block-info)))
	;; move out of current block
	(when info (goto-char (- (nth 5 info) 1)))
	(narrow-to-region (point-min) (point))
	(org-babel-execute-buffer)
	(widen)))))
    

(defun org-babel-execute-src-block-by-name (&optional name)
  (interactive)
  (save-excursion
    (goto-char
     (org-babel-find-named-block
      (or name
	  (completing-read "Code Block: " (org-babel-src-block-names)))))
    (org-babel-execute-src-block-maybe)))

(add-hook 'org-mode-hook
	  #'(lambda nil
	      ;; complete on ess
	      (add-to-list 'hippie-expand-try-functions-list (lambda (old) (ess-complete-object-name)))
	      (define-key org-mode-map (kbd "C-x c") 'superman-view-mode)
	      (define-key org-mode-map [(control tab)] 'hide-subtree)
	      (define-key org-mode-map [(meta e)] 'hippie-expand)
	      (define-key org-mode-map [(f12)] 'visible-mode)
	      (define-key org-mode-map [(control e)] 'end-of-line)
	      (define-key org-mode-map [(control z)] 'org-shifttab)
	      (define-key org-mode-map [(control meta j)] 'org-babel-execute-src-block-by-name)
	      (define-key org-mode-map "\C-xpd" 'superman-view-documents)
	      (define-key org-mode-map "\C-c\C-v" 'superman-browse-this-file)
	      (define-key org-mode-map [(meta up)] 'backward-paragraph)
	      (define-key org-mode-map [(meta down)] 'forward-paragraph)))

;;}}}

;;{{{ org-metareturn-hook

(add-hook 'org-metareturn-hook
	  #'(lambda ()
	      (when (save-excursion (beginning-of-line 0) (looking-at "\\#\\+CAPTION:"))
		  (end-of-line 1)
		(insert "\n#+CAPTION: ") t)))


;;}}}

;; Setting variable org-latex-prefer-user-labels
;; will make the exporter export
;;  #+NAME: fig:1 and #+LABEL: fig:1
;; to \label{fig:1} instead of \label{orgparagraph1}
(setq org-latex-prefer-user-labels t)

;;{{{ html export
;; see templates in ~/emacs-genome/snps/org-templates
;; which can be invoked by specifying
;; #+SETUPFILE: ~/.emacs.d/org-templates/level-N.org
;; #+TITLE: My Title
;; see also 
;; https://orgmode.org/worg/org-tutorials/org-publish-html-tutorial.org.html
(setq org-export-allow-bind-keywords t
      org-export-allow-BIND t)
(setq org-html-R-tutorial-home/up-format 
      "<ul class=\"navigation\"><li class=\"site-name\">TagTeam: R-tutorials</li><li><a accesskey=\"a\" href=\"%s\">Articles</a></li><li><a accesskey=\"c\" href=\"https://github.com/tagteam\">Code</a></li><li><a accesskey=\"i\" href=\"index.html\">Index</a></li></ul>")

(setq org-html-preamble-format 
      '(("en" "%d<br>%a")))
;;}}}

;;{{{ eg/org latex/export debug minor mode
;; see http://orgmode.org/manual/Export-options.html
;; C-c C-e t     (org-insert-export-options-template)

;; see superman-export.el

;;}}}
;;{{{ latex export
;; There are at least 3 different places/ways to control
;;  the latex header when exporting an org-document:
;; (1) GLOBAL
;;     the variables org-latex-default-packages-alist
;;     and org-latex-packages-alist control 
;;     \usepackage statements
;;     Really the former should not be changed and
;;     in case of an option clash one should rather
;;     put [NO-DEFAULT-PACKAGES] into the corresponding
;;     latex class, see (2).
;; (2) CLASS LOCAL
;;     the entries in org-latex-classes control
;;     packages, default packages and allow other
;;     additions to the latex header 
;; (3) FILE LOCAL
;;     the org-structure-templates (see org-structure-snps.el).
;;     Note: If there are many statements it is convenient
;;     to use a setupfile, e.g.
;;     #+SETUPFILE: ~/emacs-genome/genes/org-templates/bio.org

(setq org-latex-listing-options
      '(("numers" "left")))

(setq eg/org-latex-listing-options-string
      (concat "\\lstset{\n"
	      "keywordstyle=\\color{blue},\n"
	      "commentstyle=\\color{red},"
	      "stringstyle=\\color[rgb]{0,.5,0},\n"
               "literate={~}{$\\sim$}{1},\n"
	      "basicstyle=\\ttfamily\\small,\n"
	      "columns=fullflexible,\n"
	      "breaklines=true,\n"        
	      "breakatwhitespace=false,\n"
	      "numbers=left,\n"
	      "numberstyle=\\ttfamily\\tiny\\color{gray},\n"
	      "stepnumber=1,\n"
	      "numbersep=10pt,\n"
	      "backgroundcolor=\\color{white},\n"
	      "tabsize=4,\n"
              "keepspaces=true,\n"
	      "showspaces=false,\n"
	      "showstringspaces=false,\n"
	      "xleftmargin=.23in,\n"
	      "frame=single,\n"
	      "basewidth={0.5em,0.4em},\n"
	      "}\n"))
 ;; "literate={<-}{{\\,$\\leftarrow$\\,}}1 {~}{{\\,$\\sim$\\,}}1"

(setq eg/org-latex-common-header-string
      (concat
       eg/org-latex-listing-options-string))
;; (setq eg/org-latex-special-footnotes
;; "\newcommand{\sfootnote}[1]{\renewcommand{\thefootnote}{\fnsymbol{footnote}}\footnote{#1}\setcounter{footnote}{0}\renewcommand{\thefootnote}{\arabic{foot note}}}
;; \makeatletter\def\blfootnote{\xdef\@thefnmark{}\@footnotetext}\makeatother")
(eval-after-load 'ox '(require 'ox-koma-letter))

;; \\[DEFAULT-PACKAGES]
;; \\[PACKAGES]
;; \\[EXTRA]

(eval-after-load 'ox-koma-letter
  '(progn
     (add-to-list 'org-latex-classes
                  '("eg-letter"
                    "\\documentclass\{scrlttr2\}
     \\usepackage[english]{babel}
     \\setkomavar{frombank}{(1234)\\,567\\,890}
"
		    ))
     (setq org-koma-letter-default-class "eg-letter")))

(setq org-latex-packages-alist
      '(("" "listings")
	("" "color")
	("" "amsmath")
	("" "array")
	;; ("" "attachfile")
	("T1" "fontenc")
	;; ("table,usenames,dvipsnames" "xcolor")
	("" "natbib")
	;; ("citestyle=authoryear,bibstyle=authoryear" "biblatex")
	;; ("" "biblatex")
	))

(setq org-export-allow-BIND t)
(setq org-latex-listings t)
;; we put listing options into the latex-header-string

;; (setq org-latex-listings-options
      ;; '(("basicstyle" "\\small\\tt")
	;; ("numbers" "left")
	;; ("keywordstyle" "\\color{blue}")
	;; ("commentstyle" "\\color{red}")
	;; ("stringstyle" "\\color[rgb]{0,.5,0}")
	;; ("basicstyle" "\\ttfamily\\small")
	;; ("columns" "fullflexible")
	;; ("breaklines" "true") ;; sets automatic line breaking
	;; ("breakatwhitespace" "false") ;; sets if automatic breaks should only happen at whitespace
	;; ("numbers" "left")
	;; ("numberstyle" "\\ttfamily\\tiny\\color{gray}")
	;; ("stepnumber" "1")
	;; ("numbersep" "10pt")
	;; ("backgroundcolor" "\\color{white}")
	;; ("tabsize" "4")
	;; ("showspaces" "false")
	;; ("showstringspaces" "false")
	;; ("xleftmargin" ".23in")
	;; ("frame" "single")
	;; ("basewidth" "{0.5em,0.4em}")))

;; [NO-DEFAULT-PACKAGES]
;;(setq org-latex-classes nil)
(add-to-list 'org-latex-classes
	     `("org-article"
	       ,(concat "\\documentclass{article}
               [PACKAGES]
               [EXTRA]"
			eg/org-latex-common-header-string
			 ;;"\\renewcommand*\\familydefault{\\sfdefault}\n\\itemsep2pt"
			 )
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
	     `("els-article"
	       ,(concat "\\documentclass{elsarticle}
               [PACKAGES]
               [EXTRA]"
			eg/org-latex-common-header-string
			 )
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
	     `("Rnews-article"
	       ,(concat "\\documentclass{report}
               [NO-PACKAGES]
               [NO-DEFAULT-PACKAGES]
               [EXTRA]"
			eg/org-latex-common-header-string
			"\\renewcommand*\\familydefault{\\sfdefault}\n\\itemsep2pt")
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(add-to-list 'org-latex-classes
	     `("biostatistics"
	       ,(concat "\\documentclass[oupdraft]{bio}
                %% \\documentclass[oupdraft]{bio}
	       [NO-PACKAGES]
               [NO-DEFAULT-PACKAGES]
               [EXTRA]"
			eg/org-latex-common-header-string)
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; download: http://www.biometrics.tibs.org/latexdocumentclass.htm
;; wget http://www.biometrics.tibs.org/biomlatex.zip
(add-to-list 'org-latex-classes
	     `("biometrics-article"
	       ,(concat "\\documentclass[useAMS,usenatbib,referee]{biom}
                %% \\documentclass[useAMS,usenatbib]{biom}
               [PACKAGES]
               [EXTRA]"
			eg/org-latex-common-header-string)
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(add-to-list 'org-latex-classes
	     `("biometrical-journal"
	       ,(concat "\\documentclass[bimj,fleqn]{w-art}
               [NO-DEFAULT-PACKAGES]
               \\usepackage{listings}
                "
			eg/org-latex-common-header-string)
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
	     `("tas-article"
	       ,(concat "\\documentclass[12pt]{article}
               [PACKAGES]
               [EXTRA]"
			eg/org-latex-common-header-string)
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
	     `("jrss-article"
	       ,(concat "\\documentclass[12pt]{article}
               [PACKAGES]
               [EXTRA]"
			eg/org-latex-common-header-string)
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
	     `("ims-article"
	       ,(concat "\\documentclass[sts]{imsart}
                %% \\documentclass[useAMS,usenatbib]{biom}
               [PACKAGES]
               [EXTRA]"
			eg/org-latex-common-header-string)
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(add-to-list 'org-latex-classes
	     `("ims-article"
	       ,(concat "\\documentclass[sts]{imsart}
                %% \\documentclass[useAMS,usenatbib]{biom}
               [PACKAGES]
               [EXTRA]"
			eg/org-latex-common-header-string)
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; [NO-DEFAULT-PACKAGES]

(add-to-list 'org-latex-classes
	     `("beamer"
	       ,(concat "\\documentclass{beamer}"
               "[PACKAGES]"
	       eg/org-latex-common-header-string 
               "[EXTRA]"
			"\\renewcommand*\\familydefault{\\sfdefault}\n\\itemsep2pt")	     
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
	     `("dresdenbeamer"
	       ,(concat "\\documentclass{beamer}
               [PACKAGES]
               [EXTRA]"
			eg/org-latex-common-header-string
			"\\usetheme[numbers]{Dresden}
\\setbeamercolor{structure}{fg=white}
\\setbeamercolor*{palette primary}{fg=black,bg=white}
\\setbeamercolor*{palette secondary}{use=structure,fg=white,bg=white}
\\setbeamercolor*{palette tertiary}{use=structure,fg=white,bg=structure.fg!50!black}
\\setbeamercolor*{palette quaternary}{fg=white,bg=black}
\\setbeamercolor{item}{fg=red}
\\setbeamercolor{subitem}{fg=orange}
\\setbeamercolor*{sidebar}{use=structure,bg=structure.fg}
\\setbeamercolor*{palette sidebar primary}{use=structure,fg=structure.fg!10}
\\setbeamercolor*{palette sidebar secondary}{fg=black}
\\setbeamercolor*{palette sidebar tertiary}{use=structure,fg=structure.fg!50}
\\setbeamercolor*{palette sidebar quaternary}{fg=black}
\\setbeamercolor*{titlelike}{parent=palette primary}
\\setbeamercolor*{separation line}{}
\\setbeamercolor*{fine separation line}{}
\\setbeamertemplate{footline}[frame number]
\\setbeamertemplate{navigation symbols}{}
\\setbeamertemplate{subitem}[circle]"
			"\\renewcommand*\\familydefault{\\sfdefault}\n\\itemsep2pt")	     
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; "\\documentclass[useAMS,article,shortnames]{jss}
;; (setq org-latex-classes nil)
(add-to-list 'org-latex-classes
             `("jss"
	       ,(concat
		 "\\documentclass[article]{jss}
\\usepackage{amsmath,amsfonts}
\\usepackage{float}
\\usepackage{listings}"
		 eg/org-latex-listing-options-string
		 "\n[NO-DEFAULT-PACKAGES]"
		 "\n[NO-PACKAGES]"
		 "\n[EXTRA]")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             `("monograph"
	       ,(concat
		 "\\documentclass{book}"
		 "\n\\usepackage{amsmath,amsfonts}"
		 "\n\\usepackage{float}"
		 "\n\\usepackage{listings}"
		 "\n\\usepackage{array}"
		 "\n\\usepackage{graphicx}"
		 "\n\\usepackage{hyperref}"
		 "\n\\usepackage{grffile}"
		 "\n\\usepackage{color}"
		 "\n\\renewcommand*\\familydefault{\\sfdefault}"
		 "\n[NO-DEFAULT-PACKAGES]"
		 "\n[NO-PACKAGES]"
		 "\n[EXTRA]")
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
(add-to-list 'org-latex-classes
             `("subfiles"
	       ,(concat
		 "\\documentclass[./book.tex]{subfiles}"
		 ;; "\\usepackage{listings}"
		 ;; eg/org-latex-listing-options-string
		 "\n[NO-DEFAULT-PACKAGES]"
		 "\n[NO-PACKAGES]"
		 "\n[EXTRA]")
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(add-to-list 'org-latex-classes
             `("book"
               "\\documentclass{book}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))


(add-to-list 'org-latex-classes
	     '("biom"
               "\\documentclass[useAMS,usenatbib]{biom}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
	     '("biomref"
               "\\documentclass[useAMS,usenatbib,referee]{biom}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
	     '("simauth"
	       "[NO-DEFAULT-PACKAGES]
                [NO-PACKAGES]
                \\documentclass{simauth}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(add-to-list 'org-latex-classes
	     `("amsart"
	       ,(concat "\\documentclass{amsart}
               [PACKAGES]
               [EXTRA]"
			eg/org-latex-common-header-string
			"\\renewcommand*\\familydefault{\\sfdefault}\n\\itemsep2pt")
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(add-to-list 'org-latex-classes
	     '("scrartcl"
               "\\documentclass{scrartcl}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;;}}}
;;{{{ org-babel-defaults

;; (add-to-list 'org-latex-to-pdf-process '("latexmk -f -pdf %s"))
;; (setq org-latex-to-pdf-process '("latexmk -f -pdf %s"))
(setf org-babel-default-inline-header-args
      '((:session . "none")
	(:results . "silent")
	(:exports . "results")))

(defun org-babel-clear-all-results ()
  "clear all results from babel-org-mode"
  (interactive)
  (org-babel-map-src-blocks nil (org-babel-remove-result)))

;; (add-hook 'org-latex-after-save-hook 'latex-save-and-run)
;; (remove-hook 'org-latex-after-save-hook 'latex-save-and-run)

(defun eg/org-indent ()
  "context dependent indent"
  (interactive)
  (if (string= (car (org-babel-get-src-block-info)) "R")
      ;; (save-excursion
      (ess-edit-indent-call-sophisticatedly)
    (eg/org-tab)
    ;; )
    (pcomplete)))

(defun eg/org-mark-block-or-element ()
  "Mark block or element."
  (interactive)
  (if (not (org-babel-get-src-block-info))
    (org-mark-element)
	(goto-char (org-babel-where-is-src-block-head))
	(looking-at org-babel-src-block-regexp)
	(push-mark (match-end 5) nil t)
	(goto-char (match-beginning 5))))
    

(defun eg/org-ess-eval-function ()
  "If in R_SRC block, evaluate function at point."
  (interactive)
  (if (string= (car (org-babel-get-src-block-info)) "R")
        (ess-eval-function-and-go)))

;; see also eg/ess-smart-underscore in ess-R-snps.el 
(setq ess-assign-list '(" <- " "_" ":="))
(defun eg/org-smart-underscore ()
  "Smart \"_\" key: insert <- if in SRC R block unless in string/comment."
  (interactive)
  (if (and
       (string= (car (org-babel-get-src-block-info)) "R")
       (not (save-restriction (org-narrow-to-block) (ess-inside-string-or-comment-p (point)))))
      (if (not (eq last-command 'eg/org-smart-underscore))
	  (insert " <- ")
	(undo)
	(insert "_"))
    ;;      (ess-cycle-assign)
    ;; (ess-insert-assign)
    (insert "_")))

;;}}}
;;{{{ bibtex

;; I configured reftex to handle my biblio. It inserts a citation in this way
;; [[bib:myKey]]
;; where
;; #+LINK: bib file:~/mydocs/mybib.bib::%s

;; To customize this citation I have defined in my .emacs
(defun eg/org-mode-setup ()
  (interactive)
  (load-library "reftex")
  (reftex-parse-all) ; to make reftex aware of the biblio
                     ; # \bibliography{biblio}
   (reftex-set-cite-format
     '((?b . "[[bib::%l]]")
       (?n . "[[note::%l]]")
       (?c . "\\cite{%l}")))
    (define-key org-mode-map "\C-c\C-g" 'reftex-citation)
)

;;}}}
;;{{{ babel R

;; for block options see: org-babel-common-header-args-w-values
;; org-babel-R-evaluate-session
;; org-babel-R-write-object-command
;; org-babel-comint-eval-invisibly-and-wait-for-file
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (perl . t)
   (latex . t)
   (ditaa . t)
   (org . t)
   (dot . t)
   ;;   (shell . t)
   (R . t)))

;; Do not confirm source block evaluation
(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(add-to-list 'org-src-lang-modes '("emacs-lisp" . emacs-lisp))

;;}}}
;;{{{ babel emacs-lisp

(defun eg/org-lazy-load (&optional only-this-block)
  "Tangle, load and show emacs-lisp code. "
  (interactive "P")
  (when (string= (car (org-babel-get-src-block-info)) "emacs-lisp")
  (save-buffer)
  (let ((tfile (car (org-babel-tangle only-this-block nil 'emacs-lisp))))
    (switch-to-buffer-other-window (find-file-noselect tfile t))
    (revert-buffer t t)
    ;; (find-file-other-window tfile)
  (eval-buffer))))

;;}}}
;;{{{ org mode hook
(defun sh-send-line-or-region (&optional step)
  (interactive ())
  (let ((proc (get-process "shell"))
        pbuf min max command)
    (unless proc
      (let ((currbuff (current-buffer)))
        (shell)
        (switch-to-buffer currbuff)
        (setq proc (get-process "shell"))
        ))
    (setq pbuff (process-buffer proc))
    (if (use-region-p)
        (setq min (region-beginning)
              max (region-end))
      (setq min (point-at-bol)
            max (point-at-eol)))
    (setq command (concat (buffer-substring min max) "\n"))
    (with-current-buffer pbuff
      (goto-char (process-mark proc))
      (insert command)
      (move-marker (process-mark proc) (point))
      ) ;;pop-to-buffer does not work with save-current-buffer -- bug?
    (process-send-string  proc command)
    (display-buffer (process-buffer proc) t)
    (when step 
      (goto-char max)
      (next-line))
    ))
(defun sh-send-line-or-region-and-step ()
  (interactive)
  (sh-send-line-or-region t))
(defun sh-switch-to-process-buffer ()
  (interactive)
  (pop-to-buffer (process-buffer (get-process "shell")) t))

;; see also superman-control-export in superman-export.el
(defun superman-run-R-or-export-as (&optional debug)
  (interactive "P")
  (cond ((and (string= (car (org-babel-get-src-block-info)) "R")
	      (not (and (looking-at "[ \t]*$")
			(save-excursion (forward-line -1)
					(beginning-of-line)
					(looking-at "#\\+END_SRC")))))
	 (superman-ess-eval-and-go nil))
	((string= (car (org-babel-get-src-block-info)) "sh")
	 (progn (sh-send-line-or-region-and-step)
		(sh-switch-to-process-buffer)))
	(t (superman-org-export-as nil))))

;; never want tab to do a tab 
(setq org-cycle-emulate-tab nil)
(defun eg/org-tab ()
  (interactive)
  (if (string= (car (org-babel-get-src-block-info)) "R")
      (eg/indent-paragraph)
    (org-cycle)))

(add-hook 'org-mode-hook
	  #'(lambda nil
	      (require 'superman-export)
	      (define-key org-mode-map [(f12)] 'org-shifttab)
	      (define-key org-mode-map [(tab)] 'eg/org-tab)	      
	      (define-key org-mode-map [(meta k)] 'superman-control-export)
	      (define-key org-mode-map [(meta control k)] #'(lambda () (interactive)
							      (if (string= (car (org-babel-get-src-block-info)) "R")
								  (eg-switch-to-R 't))))
	      (define-key org-mode-map [(meta J)] 'superman-org-export-change-target)
	      ;; (define-key org-mode-map [(control c control b)] 'org-babel-execute-src-block-by-name)
	      ;; (define-key org-mode-map [(control c control D)] '(lambda () (org-babel-execute-src-block-by-name "data")))
	      (define-key org-mode-map [(meta j)] 'superman-run-R-or-export-as)
	      (define-key org-mode-map [(control shift e)] 'eg/org-lazy-load)
	      (define-key org-mode-map [(control xx)] 'eg/org-lazy-load)
	      (define-key org-mode-map "\M-F" 'ess-eval-function-and-go)
	      (define-key org-mode-map [(meta control i)] 'eg/org-indent)
	      (define-key org-mode-map "_" 'eg/org-smart-underscore)))

;;}}}
;;{{{ list documents

 ;; Example:
 ;; #+BEGIN_SRC emacs-lisp :results list
;; (org-directory-files '.../tex/' '\.tex$')
;; #+END_SRC"

(defun org-directory-files (dir &optional file-regexp path-regexp path-inverse-match sort-by reverse)
  "List files as org-links that match FILE-REGEXP
 in a directory which is related to the directory DIR. "
  (interactive)
  (let* ((file-regexp (or file-regexp "."))
	 (files (file-list-select-internal nil file-regexp nil nil dir nil t)))
    (setq files (file-list-sort-internal files (or sort-by "time") reverse 'dont))
    (if path-regexp
	(setq files (file-list-select-internal files path-regexp "path" path-inverse-match)))
    (when files 
      (mapcar #'(lambda (x)
		 (concat "[["(replace-in-string (file-list-make-file-name x) (getenv "HOME") "~") "]["
			 (car x) "]]")) files))))



(defun org-heading-directory-files-maybe (&optional limit)
  (interactive)
  (let ((dir (org-entry-get nil "DIR" nil))
	(beg (save-excursion (org-back-to-heading t) (point)))
	(end (save-excursion (outline-next-heading) (point)))
	(limit (or limit 10))
	(count 0) 
	flist)
    (when dir
      (save-window-excursion
      (setq flist (org-directory-files "\." dir nil  dir "time" nil)))
      (org-back-to-heading)
      (when flist
	(unless (re-search-forward org-property-end-re end t)
	  (end-of-line))
	(if (re-search-forward "BEGIN_SRC FILE-LIST" end t)
	    (progn 
	      (org-babel-mark-block)
	      (delete-region (region-beginning) (region-end)))
	  (insert "\n\n#+BEGIN_SRC FILE-LIST\n#+END_SRC\n")
	  (previous-line 1))
      (while (and flist (< count limit))
	(insert (car flist) "\n")
	(setq flist (cdr flist)
	      count (+ count 1)))))))



;;}}}
;;{{{ org-run-script
(defun eg/org-run-script (&optional server R)
  (interactive)
  (let* ((buf (buffer-file-name (current-buffer)))
	 (code (concat (file-name-sans-extension buf) ".R"))
	 (log (concat code "out"))
	 (R  (or R "/usr/local/bin/R"))
	 (cmd (concat "ssh " server " 'nohup nice -19 " R " --no-restore CMD BATCH " code " " log "'")))
    (save-buffer)
    (org-babel-tangle nil code "R")
    (save-excursion
      (when (get-buffer "*Async Shell Command*")
	(with-current-buffer "*Async Shell Command*"
	  (rename-uniquely)))
      (async-shell-command cmd))
    (concat "[[" log "]]")))



(defun org-project-files (file-regexp &optional path-regexp path-inverse-match subdir sort-by reverse)
  "List files as org-links that match FILE-REGEXP
 in a directory which is related to the directory DIR. "
  (interactive)
  (let* ((dir (concat (file-name-directory
		       (buffer-file-name (current-buffer)))
		      (if subdir (concat "../" subdir) "..")))
	 (files (file-list-select-internal nil file-regexp nil nil dir nil t)))
    (setq files (file-list-sort-internal files (or sort-by "time") reverse 'dont))
    (if path-regexp
    (setq files (file-list-select-internal files path-regexp "path" path-inverse-match)))
    (when files 
      (mapcar #'(lambda (x)
		 (concat "[["(replace-in-string (file-list-make-file-name x) (getenv "HOME") "~") "]["
			 (car x) "]]")) files))))
;;}}}
;;{{{ fix latex $$

(defun fix-latex-$$ ()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\\$\\$" nil t)
    (replace-match "\n\\begin{equation*}\n" nil t)
    (when (re-search-forward "\\$\\$" nil t)
      (replace-match "\n\\end{equation*}\n" nil t)))
    (goto-char (point-min))
    ;; (string-to-char " ") 160
   (while (re-search-forward " " nil t)
     (replace-match " ")))

;;}}}

;;{{{ babel execute blocks above
;; http://kitchingroup.cheme.cmu.edu/blog/2015/03/19/Restarting-org-babel-sessions-in-org-mode-more-effectively/
(defun src-block-in-session-p (&optional name)
  "Return if src-block is in a session of NAME.
NAME may be nil for unnamed sessions."
  (let* ((info (org-babel-get-src-block-info))
         (lang (nth 0 info))
         (body (nth 1 info))
         (params (nth 2 info))
         (session (cdr (assoc :session params))))

    (cond
     ;; unnamed session, both name and session are nil
     ((and (null session)
           (null name))
      t)
     ;; Matching name and session
     ((and
       (stringp name)
       (stringp session)
       (string= name session))
      t)
     ;; no match
     (t nil))))

(defun org-babel-restart-session-to-point (&optional arg)
  "Restart session up to the src-block in the current point.
Goes to beginning of buffer and executes each code block with
`org-babel-execute-src-block' that has the same language and
session as the current block. ARG has same meaning as in
`org-babel-execute-src-block'."
  (interactive "P")
  (unless (org-in-src-block-p)
    (error "You must be in a src-block to run this command"))
  (let* ((current-point (point-marker))
         (info (org-babel-get-src-block-info))
         (lang (nth 0 info))
         (params (nth 2 info))
         (session (cdr (assoc :session params))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-babel-src-block-regexp nil t)
        ;; goto start of block
        (goto-char (match-beginning 0))
        (let* ((this-info (org-babel-get-src-block-info))
               (this-lang (nth 0 this-info))
               (this-params (nth 2 this-info))
               (this-session (cdr (assoc :session this-params))))
          (when
              (and
               (< (point) (marker-position current-point))
               (string= lang this-lang)
               (src-block-in-session-p session))
            (org-babel-execute-src-block arg)))
        ;; move forward so we can find the next block
        (forward-line)))))
(defun org-babel-run-blocks-above nil
    (interactive)
  (org-babel-restart-session-to-point))

;;}}}

;;{{{  org 2 rmd
(defun org2rmd ()
  (interactive)
  ;; headers
  (goto-char (point-min))
  (while (outline-next-heading)
    (let* ((lev (org-current-level))
	   len)
      (beginning-of-line)
      (looking-at "^[\\*]+ ")
      (if (> lev 2)
	  (replace-match (concat
			  (make-string lev (string-to-char "#"))
			  " "))
	(replace-match "")
	(end-of-line)
	(setq len (- (point) (point-at-bol)))
	(insert "\n" (make-string len (string-to-char
				       (if (= lev 1)
					   "=" "-")))))))
  ;; R blocks
  (goto-char (point-min))
  (while (re-search-forward org-block-regexp nil t)
    (goto-char (match-beginning 0))
    (let ((info (org-babel-get-src-block-info)))
      (if (string= (car info) "R")
	  (let ((echo
		 (cdr
		  (assoc ':exports (caddr info)))))
	    (save-excursion
	      (re-search-forward "#\\+END_?SRC" nil t)
	      (replace-match "```"))
	    (kill-whole-line)
	    (if (member echo (list "none" "results"))
		(insert "```{r, echo=FALSE}\n")
	      (insert "```{r, echo=TRUE}\n")))
	(re-search-forward "#\\+END_?SRC" nil t)))))

;;}}}

(provide 'org-snps)
;;; org-snps.el ends here
