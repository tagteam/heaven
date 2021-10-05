;;; superman-faces.el --- faces for superman views

;; Copyright (C) 2014-2016  Thomas Gerds

;; Author: Thomas Gerds <tag@biostat.ku.dk>
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

;;{{{ todo keywords

 (unless org-todo-keyword-faces
 (setq org-todo-keyword-faces
 (quote (("TODO" :foreground "red" :weight bold)
	  ("URGENT" :foreground "goldenrod1" :weight bold)
	   ("IN PROGRESS" :foreground "blue" :weight bold)
	    ("ACTIVE" :foreground "red" :weight bold)
	     ("WAITING" :foreground "purple" :weight bold)
	     ("PERMANENT" :foreground "SkyBlue3" :weight bold)
	      ("DONE" :foreground "forest green" :weight bold)
	       ("CANCELED" :foreground "slate grey" :weight bold)))))

;;}}}
;;{{{
(defface superman-face
  '((((class color) (background dark))
     :inherit superman-default-button-face
     ;; :background "Yellow"
     ;; :foreground "red"
     ;; :box (:line-width 6 :color "CornflowerBlue" :style released-button)
     :foreground "black"
     :background "gray93"
     ;; :box (:line-width 0 :color "gray55" :style released-button)
     :inverse-video nil
     :height 1.3)
    (t
     :inherit superman-default-button-face
     :foreground "black"
     :background "gray93"
     ;; :box (:line-width 0 :color "gray66" :style released-button)
     ;; :foreground "red"
     ;; :background "Yellow"
     ;; :box (:line-width 6 :color "CornflowerBlue" :style released-button)
     :inverse-video nil
     :height 1.3))
  "Face for superman button."
  :group 'superman)

(defface superman-high-face
  '((t (:height 2.3
		:foreground "gray22"
		:background "gray90"
		:box (:line-width 1 :color "gray88" :style released-button))))
  "Default face used for superman capture item buttons."
  :group 'superman)

(defface superman-free-text-face
  '((t (:height 1.0
		:foreground "white"
		:background "aquamarine"
		:box (:line-width 1 :color "gray88" :style released-button))))
  "Default face used to mark free text regions."
  :group 'superman)

;;}}}
;;{{{ Save and quit button
(defface superman-edit-button-face
  '((((class color) (background dark))
     :inherit superman-default-button-face
     :background "gray93"
     :foreground "darkgreen"
     :height 1.0
     )
    (t
     :inherit superman-default-button-face
     :background "gray93"
     :foreground "darkgreen"
     :height 1.0))
  "Face for superman save button."
  :group 'superman)
(defface superman-save-button-face
  '((((class color) (background dark))
     :inherit superman-default-button-face
     :background "gray93"
     :foreground "red"
     :height 1.0
     )
    (t
     :inherit superman-default-button-face
     :background "gray93"
     :foreground "red"
     :height 1.0))
  "Face for superman save button."
  :group 'superman)
(defface superman-quit-button-face
  '((((class color) (background dark))
     :inherit superman-default-button-face
     :background "gray93"
     :foreground "blue"
     )
    (t
     :inherit superman-default-button-face
     :background "gray93"
     :foreground "blue"))
  "Face for superman quit button."
  :group 'superman)
;;}}}
;;{{{ file-list faces
(defface superman-file-name-face
  '((t (:inherit font-lock-variable-name-face :underline nil)))
  "Face used for file names."
  :group 'superman)

(defface superman-directory-name-face
  '((t (:inherit font-lock-keyword-face :underline nil)))
  "Face used for directory names."
  :group 'superman)

(defface superman-column-name-face
  '((t (:inherit font-lock-comment-face :underline nil)))
  "Face used for directory names."
  :group 'superman)


(defface file-list-clear-button-face
  '((((class color) (background dark))
     :inherit superman-default-button-face
     :background "red"
     :foreground "white"
     :inverse-video nil
     :height 1.0)
    (t
     :inherit superman-default-button-face
     :foreground "red"
     :background "gray90"
     :box (:line-width 1 :color "black" :style released-button)
     :inverse-video nil
     :height 1.0))
  "Face for next file-list-filter-buttons."
  :group 'superman)

(defface file-list-action-button-face
  '((((class color) (background dark))
     :inherit superman-default-button-face
     :background "darkblue"
     :inverse-video nil
     :forground "gray90"
     :height 1.0)
    (t
     :inherit superman-default-button-face
     :foreground "darkblue"
     :background "gray90"
     :box (:line-width 1 :color "black" :style released-button)
     :inverse-video nil
     :height 1.0))
  "Face for next file-list-filter-buttons."
  :group 'superman)

(defface file-list-info-button-face
  '((((class color) (background dark))
     :inherit superman-default-button-face
     :box (:line-width 1 :color "black" :style released-button)
     :foreground "DarkOrchid"
     :background "gray90"
     :inverse-video t
     :height 1.0)
    (t
     :inherit superman-default-button-face
     :foreground "DarkOrchid"
     :background "gray90"
     :box (:line-width 1 :color "black" :style released-button)
     :inverse-video nil
     :height 1.0))
  "Face for next file-list-filter-buttons."
  :group 'superman)

(defface file-list-active-filter-button-face
  '((((class color) (background dark))
     :inherit superman-default-button-face
     :foreground "white"
     :background "darkblue"
     :inverse-video nil
     :box (:line-width 1 :color "black" :style pressed-button)
     :height 1.0)
    (t
     :inherit superman-default-button-face
     :foreground "darkblue"
     :background "gray90"
     :box (:line-width 1 :color "black" :style pressed-button)
     :inverse-video nil
     :height 1.0))
  "Face for next file-list-filter-buttons."
  :group 'superman)

(defface file-list-filter-button-face
  '((((class color) (background dark))
     :inherit superman-default-button-face
     :background "LimeGreen"
     :foreground "black"
     :inverse-video nil
     :box (:line-width 1 :color "black" :style released-button)
     :height 1.0)
    (t
     :inherit superman-default-button-face
     :foreground "LimeGreen"
     :background "black"
     :box (:line-width 1 :color "black" :style released-button)
     :inverse-video nil
     :height 1.0))
  "Face for next file-list-filter-buttons."
  :group 'superman)

(defface file-list-inverse-filter-button-face
  '((((class color) (background dark))
     :inherit superman-default-button-face
     :background "OrangeRed1"
     :foreground "gray90"
     :box (:line-width 1 :color "black" :style pressed-button)
     :inverse-video nil
     :height 1.0)
    (t
     :inherit superman-default-button-face
     :foreground "OrangeRed1"
     :background "gray90"
     :box (:line-width 1 :color "black" :style pressed-button)
     :inverse-video nil
     :height 1.0))
  "Face for next file-list-filter-buttons."
  :group 'superman)



;;}}}
;;{{{ warning & subheader faces
(defface superman-warning-face
  '((t (:inherit font-lock-warning-face :underline nil)))
    "Face used for warnings."
    :group 'superman)

(defface superman-subheader-face
  '((t (:inherit font-lock-string-face)))
    "Face used for the selected tab."
    :group 'superman)
;;}}}
;;{{{ button faces
(defface superman-default-button-face
  '((t (:box (:line-width 1 :color "gray88" :style released-button))))
  "Default face used for superman-buttons."
  :group 'superman)

(defface superman-capture-button-face
  '((((class color) (min-colors 88) (background light))
     :inherit superman-default-button-face
     :height 1.0
     :foreground "gray11"
     :background "gray93")
    (((class color) (min-colors 88) (background dark))
     :inherit superman-default-button-face
     :height 1.1
     :foreground "gray93"
     :background "gray11")
    (((class color) (min-colors 8) (background light))
     :inherit superman-default-button-face
     :height 1.0
     :foreground "white"
     :background "gray55")
    (((class color) (min-colors 8) (background dark))
     :inherit superman-default-button-face
     :height 1.0
     :foreground "gray55"
     :background "white")
    (t (:inherit superman-default-button-face
		 :height 1.0
		 :inverse-video t
		 :bold t)))
  "Face for superman capture buttons."
  :group 'superman)

(defface superman-project-button-face
  '((((class color) (min-colors 88) (background light))
     :inherit superman-default-button-face
     :height 1.3
     :foreground "yellow"
     :background "red")
    (((class color) (min-colors 88) (background dark))
     :inherit superman-default-button-face
     :height 1.3
     :foreground "red"
     :background "yellow")
    (((class color) (min-colors 8) (background light))
     :inherit superman-default-button-face
     :height 1.3
     :foreground "black"
     :background "gray88")
    (((class color) (min-colors 8) (background dark))
     :inherit superman-default-button-face
     :height 1.3
     :foreground "gray88"
     :background "black")
    (t (:inherit superman-default-button-face
		 :height 1.3
		 :inverse-video t
		 :bold t)))
  "Face for superman project buttons."
  :group 'superman)

(defface superman-next-project-button-face
  '((((class color) (background dark))
     :inherit superman-default-button-face
     :background "LightYellow1"
     :inverse-video t
     :foreground "red"
     :height 1.0)
    (t
     :inherit superman-default-button-face
     :foreground "LightYellow1"
     :background "red"
     :inverse-video t
     :height 1.0))
  "Face for next superman project buttons."
  :group 'superman)

(defface superman-capture-button-face
  '((t (:inherit superman-default-button-face
		 :box (:line-width 1 :color "gray88" :style released-button)
		 :foreground "seagreen4")))
  "Face used for the selected tab."
  :group 'superman)

(defface superman-header-button-face
  '((((class color) (background dark))
     :inherit superman-default-button-face
     :background "black"
     :foreground "gray93"
     :inverse-video t
     :height 1.0)
    (t (:inherit superman-default-button-face
		 :box (:line-width 1 :color "gray88" :style released-button)
		 :foreground "black" 
		 :background "gray93")))
  "Face used for the selected tab."
  :group 'superman)

;;}}}
;;{{{ superman-git-keyboard

(defface superman-git-keyboard-face-d
  '((t (:inherit superman-default-button-face
		 :foreground "black"
		 :height 0.8
		 :background "orange"
		 )))
  "Face used for git-diff."
  :group 'superman)
(defface superman-git-keyboard-face-D
  '((t (:inherit superman-git-keyboard-face-d :height 1.0 :box (:line-width 1 :color "gray88" :style released-button))))
  "Face used for git-diff (larger box)."
  :group 'superman)

(defface superman-git-keyboard-face-a
  '((t (:inherit superman-default-button-face
		 :foreground "black"
		 :height 0.8
		 :background "yellow")))
  "Face used for git-add."
  :group 'superman)
(defface superman-git-keyboard-face-A
  '((t (:inherit superman-git-keyboard-face-a :height 1.0 :box (:line-width 1 :color "gray88" :style released-button))))
  "Face used for git-add (larger box)."
  :group 'superman)

(defface superman-git-keyboard-face-l
  '((t (:inherit superman-default-button-face
		 :foreground "white"
		 :height 0.8
		 :background "blue")))
  "Face used for git-log."
  :group 'superman)
(defface superman-git-keyboard-face-L
  '((t (:inherit superman-git-keyboard-face-l :height 1.0 :box (:line-width 1 :color "gray93" :style released-button))))
  "Face used for git-log (larger box)."
  :group 'superman)

(defface superman-git-keyboard-face-c
  '((t (:inherit superman-default-button-face
		 :foreground "black"
		 :height 0.8
		 :background "green")))
  "Face used for git-commit."
  :group 'superman)

(defface superman-git-keyboard-face-i
  '((t (:inherit superman-default-button-face
		 :foreground "black"
		 :height 0.8
		 :background "white")))
  "Face used for git-commit."
  :group 'superman)

(defface superman-git-keyboard-face-p
  '((t (:inherit superman-default-button-face
		 :foreground "black"
		 :height 0.8
		 :background "#00FFFF")))
  "Face used for git-commit."
  :group 'superman)

(defface superman-git-keyboard-face-C
  '((t (:inherit superman-git-keyboard-face-c :height 1.0 :box (:line-width 1 :color "gray93" :style released-button))))
  "Face used for git-commit (larger box)."
  :group 'superman)

(defface superman-git-keyboard-face-P
  '((t (:inherit superman-git-keyboard-face-p :height 1.0 :box (:line-width 1 :color "gray93" :style released-button))))
  "Face used for git-commit (larger box)."
  :group 'superman)

(defface superman-git-keyboard-face-x
  '((t (:inherit superman-default-button-face
		 :foreground "white"
		 :height 0.8
		 :background "black")))
  "Face used for git-rm."
  :group 'superman)
(defface superman-git-keyboard-face-X
  '((t (:inherit superman-git-keyboard-face-x :height 1.0 :box (:line-width 1 :color "gray93" :style released-button))))
  "Face used for git-rm (larger box)."
  :group 'superman)

(defface superman-git-keyboard-face-r
  '((t (:inherit superman-default-button-face
		 :foreground "white"
		 :height 0.8
		 :background "violet")))
  "Face used for git-stash."
  :group 'superman)
(defface superman-git-keyboard-face-R
  '((t (:inherit superman-git-keyboard-face-l :height 1.0 :box (:line-width 1 :color "gray88" :style released-button))))
  "Face used for git-stash (larger box)."
  :group 'superman)

(defface superman-git-keyboard-face-s
  '((t (:inherit superman-default-button-face
		 :foreground "black"
		 :height 0.8
		 :background "red")))
  "Face used for git-status."
  :group 'superman)
(defface superman-git-keyboard-face-S
  '((t (:inherit superman-git-keyboard-face-s :height 1.0 :box (:line-width 1 :color "gray88" :style released-button))))
  "Face used for git-status (larger box)."
  :group 'superman)
;;}}}

(provide 'superman-faces)
;;; superman-faces.el ends here
