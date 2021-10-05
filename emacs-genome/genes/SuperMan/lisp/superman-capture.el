;;; superman-capture.el --- superman captures stuff in superman style

;; Copyright (C) 2013-2016  Klaus Kähler Holst, Thomas Alexander Gerds

;; Authors: Klaus Kähler Holst <kkho@biostat.ku.dk>
;;          Thomas Alexander Gerds <tag@biostat.ku.dk>
;; 
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

;;; Code:

;;{{{ variables and hooks

(defvar superman-setup-scene-hook nil
  "Hook run by `superman-capture-whatever'
just before the capture buffer is given to the user.")

(defvar superman-capture-mode-map (make-sparse-keymap)
  "Keymap used for `superman-view-mode' commands.")
(define-key superman-capture-mode-map  "\C-c\C-c" 'superman-clean-scene)
(define-key superman-capture-mode-map [(tab)] 'superman-complete-property)
;;(define-key superman-capture-mode-map  "q" 'superman-quit-scene)
(define-key superman-capture-mode-map  "\C-x\C-s" 'superman-clean-scene)
(define-key superman-capture-mode-map  "\C-c\C-q" 'superman-quit-scene)

(defvar superman-unison-switches "-ignore \"Regex .*(~|te?mp|rda)$\" -ignore \"Regex ^(\\.|#).*\"")
      ;; "-ignore 'Regex .*' -ignorenot 'Regexp *.(org|R|tex|Rd)$'")

(defvar superman-unison-cmd "unison")
(setq superman-unison-cmd "unison")

;;}}}


;;{{{ superman goto project

(defun superman-goto-project (&optional project heading create end-of 
					leave-narrowed jabber property-string)
  "Goto project index file call `widen' and then search for HEADING
and narrow the buffer to this subtree. 

If HEADING is not found and CREATE is non-nil create the HEADING.

In both cases it returns the `point-marker' at the beginning of the heading.

If END-OF is non-nil, leave point at the end of the section,
otherwise at the beginning.

If JABBER is non-nil (and CREATE is nil) be talkative about non-existing headings.
"
  (interactive)
  (let* ((pro (superman-get-project project))
	 (index (superman-get-index pro))
	 (head (or heading (read-string "Goto heading: ")))
	 value)
    (cond ((bufferp index)
	   (switch-to-buffer index))
	  (index
	   (progn
	     (unless (file-exists-p (file-name-directory index))
	       (make-directory (file-name-directory index) 'with-parents))
	     (find-file index)))
	  (t (error (concat "Project " (car pro) " does not have an index"))))
    (widen)
    (show-all)
    (goto-char (point-min))
    (unless (eq major-mode 'org-mode)
      (org-mode))
    (setq value
	  (cond ((re-search-forward (format org-complex-heading-regexp-format
					    (regexp-quote head)) nil t)
		 (point-marker))
		(create
		 (let (marker)
		   (goto-char (point-max))
		   (insert "\n")
		   (setq marker (point-marker))
		   (insert "* " head "\n")
		   (when property-string  
		     (insert ":PROPERTIES:\n")
		     (insert property-string)
		     (insert "\n:END:\n\n")
		     (forward-line -1))
		   (save-buffer)
		   (point-marker)))
		(t (when jabber (message (concat "Heading " head " not found in index file of " (car pro))))
		   nil)))
    (when value 
      (org-narrow-to-subtree)
      (if end-of (goto-char (point-max))
	;; leave point at the first entry or at the end of this section
	(end-of-line)
	(if (outline-next-heading)
	    (beginning-of-line)
	  (goto-char (point-max)))))
    (unless leave-narrowed
      (widen)
      (show-all))
    value))

;;}}}
;;{{{ superman capture

(defun superman-capture (project heading-or-marker welcome-text 
				 &optional body plist level scene
				 quit-scene clean-hook quit-hook)
  "Superman captures entries, i.e., the contents of an outline-headings,
to be added to the index file of a PROJECT at a given HEADING-OR-MARKER.

HEADING-OR-MARKER can be the name of the outline heading-or-marker which is found
in the project view buffer or a marker pointing to the project index file.

WELCOME-TEXT is a string which should help the user fill the form.

If LEVEL is given it is the level of the new heading (default is `superman-item-level').
LEVEL can be 0 in which case no heading is created.

As a special case, this function is used to capture a new project
for the superman(ager).

If SCENE is a function it is applied by `superman-clean-scene' and
`superman-quit-scene', i.e., at the end of the capture.
If SCENE is nil then the current window configuration is restored.
If QUIT-SCENE is a function or window configuration then it is handled
by `superman-quit-scene' instead of SCENE.

CLEAN-HOOK and QUIT-HOOK are functions that are stored in the capture buffer
in form of text-properties at the point-min and called by `superman-clean-scene'
just before `superman-capture-before-clean-scene-hook'
and by `superman-quit-scene' just before killing the buffer.

See also `superman-capture-whatever' for the other arguments."
  (interactive)
  (let* ((scene (or scene (current-window-configuration)))
	 (welcome-text
	  (or welcome-text
	      (concat "\nType a title (max one line) in line starting with stars *** "
		      "\nPress [TAB] on a property for help and completions.")))
	 (destination (if heading-or-marker
			  (cond ((stringp heading-or-marker)
				 (superman-goto-project
				  project
				  heading-or-marker 'create nil nil nil))
				((markerp heading-or-marker)
				 (progn (switch-to-buffer
					 (marker-buffer heading-or-marker))
					(goto-char heading-or-marker)
					(if (ignore-errors (org-narrow-to-subtree))
					    (progn
					      (end-of-line)
					      (if (outline-next-heading)
						  (beginning-of-line)
						(goto-char (point-max)))
					      (widen)
					      (point-marker))
					  (point-marker)))))
			;; append item to the end of index file
			(find-file (superman-get-index project))
			(widen)
			(show-all)
			(goto-char (point-max))
			(point-marker))))
    (superman-capture-whatever
     destination welcome-text level body plist nil scene (if (stringp project) project (car project))
     nil quit-scene clean-hook quit-hook)))

(defun superman-capture-whatever (destination 
				  welcome-text level body fields
				  edit scene nick read-only
				  &optional quit-scene clean-hook quit-hook)
  " This function is the work-horse of `superman-capture' and `superman-view-edit-item'.

 DESTINATION is a marker which indicates where to place the result.

 TITLE is a string which is shown in the first line of the capture buffer.

 FIELDS is an alist which contains elements are transformed into property fields. The elements
 have the form (key . plist) where key can be:

 - a string: the name of a property for the new entry in which case value is a inserted as the default value.
 - equal 'body: then value is inserted after the properties. 
 - equal 'fun: then value is called via `funcall'. 
 - equal 'hdr: then value is used as heading for this capture
  
 and plist is a property list which can have the following elements:

 :value an initial input value, e.g., set :value ,(format-time-string \"<%Y-%m-%d %a>\") to initialize a date field 
 :hidden if non-nil capture will not show this field. Makes most sense when there also is a value.
 :complete decide about what should happen when `superman-complete-property' is involved (tab key).
 :test a function is evaluated without arguments, but the current value of the field can be obtained by calling `superman-get-property'. 
       if the function returns nil nothing happens, otherwise an error is produced. If the value of the function is a string 
       this is the error message otherwise the value of :message.
 :if-empty decide what to do when field is empty. Possible values are 'delete (whole line is delete) and 'complain (produce an error)
 :message message shown when an error occurs.

 Note: plist is ignored when key is not a string.

 If EDIT is non-nil then saving the capture will erase the subtree at
 destination before saving the capture.

 If READ-ONLY is non-nil then the capture will neither be editable nor savable.

 Runs `superman-setup-scene-hook' just before the capture
 buffer is handed over. The capture buffer can be left in two
 differnent ways:

 - save calls `superman-clean-scene' 
 - quit calls `superman-quit-scene'

 See `superman-capture' for the other arguments."
  (let* ((level (or level superman-item-level))
	 body-start
	 (body (or body ""))
	 (S-buf (generate-new-buffer-name "*Capture of SuperMan*"))
	 dest-heading)
    (switch-to-buffer S-buf)
    (delete-other-windows)
    (org-mode)
    (font-lock-mode -1) 
    (show-all)
    (when (> level 0) (progn
			(insert "\n"
				(make-string level (string-to-char "*"))
				" NIX \n")
			(forward-line -1))
	  (org-narrow-to-subtree)
	  (unless (= level 0) (progn 
				(skip-chars-forward "[* ]")
				(delete-region (point) (point-at-eol)))))
    ;; stuff point-min with text properties
    (goto-char (point-min))
    (insert "Superman captured: ")
    (put-text-property (point-min) (1+ (point-min)) 'destination destination)
    (when scene  
      (put-text-property (point-min) (1+ (point-min)) 'scene scene))
    (when nick  
      (put-text-property (point-min) (1+ (point-min)) 'nickname nick))
    (when quit-scene
      (put-text-property (point-min) (1+ (point-min)) 'quit-scene quit-scene))
    (when edit
      (put-text-property (point-min) (1+ (point-min)) 'edit t))
    (put-text-property (point-min) (1+ (point-min)) 'type 'capture)
    ;; destination
    (org-with-point-at destination
      (ignore-errors (setq dest-heading (org-get-heading t t))))
    (set-text-properties 0 (length dest-heading) nil dest-heading)
    (if (> (length dest-heading) 25)
	(setq dest-heading (concat (substring dest-heading 0 22) "...")))
    (superman-insert-destination-line destination dest-heading nil)
    (insert "\n\n" (superman-make-button
		    "Show context"
		    '(:fun superman-capture-show-context
			   :face superman-header-button-face
			   :help "Show the context\naround destination" 
			   :width 21)))
    (insert " " (superman-make-button
		 "Change destination"
		 '(:fun superman-change-destination
			:face superman-header-button-face
			:help "Change position and file\nwhere text will be saved" :width 21)))
    ;; welcome text
    (insert "\n\n")
    (insert welcome-text)
    (insert "\n\n -------------------- Exit by using one of two commands:\n\n")
    (unless read-only
      (insert (superman-make-button
	       "Save (C-c C-c)"
	       '(:fun  superman-clean-scene 
		       :face 'superman-save-button-face
		       :help "Save the current text at the destination" 
		       nil 21))
	      " "))
    (insert (superman-make-button
	     (if read-only "back (q)" "Cancel (C-c C-q)")
	     `(:fun superman-quit-scene
		    :face superman-quit-button-face
		    :help ,(if read-only "Back without saving" "Cancel the capture") 
		    :width 21)))
    (insert "\n")
    ;; end of non-editable header
    (insert "\n -------------------- Editable text to be saved below this line:")
    (insert "\n\n")
    (forward-line -1)    
    (put-text-property (1+ (point-min)) (point-at-eol) 'read-only t)
    (forward-line 1)
    (end-of-line)
    (insert "\n")
    (if (= level 0) (goto-char (point-max)) (end-of-line))
    ;; insert property drawer
    (when (and (> level 0) plist)
      (insert ":PROPERTIES:")
      (while plist
	(let* ((el (car plist))
	       (key (car el))
	       (value (plist-get (cdr el) :value))
	       (test (plist-get  (cdr el) :test))
	       (complete (plist-get  (cdr el) :complete))
	       (hidden (plist-get  (cdr el) :hidden))
	       (message (plist-get (cdr el) :message))
	       (if-empty (plist-get (cdr el) :if-empty)))
	  (cond ((stringp key)
		 (if hidden 
		     (put-text-property (point-min) (1+ (point-min)) 'hidden-properties 
					(append (get-text-property (point-min) 'hidden-properties)
						`((,key ,value))))
		   (ignore-errors
		     (insert "\n:" key ": ")
		     (put-text-property (point-at-bol) (1+ (point-at-bol)) 'prop-marker (point-at-bol))
		     (put-text-property  (point-at-bol) (- (point) 1) 'property key)
		     (when complete (put-text-property  (point-at-bol) (- (point) 1) 'complete complete))
		     (when test (put-text-property  (point-at-bol) (- (point) 1) 'test test))
		     (when hidden (put-text-property  (point-at-bol) (- (point) 1) 'hidden hidden))
		     (when message (put-text-property  (point-at-bol) (- (point) 1) 'message message))
		     (when if-empty (put-text-property  (point-at-bol) (- (point) 1) 'if-empty if-empty))
		     (when value (insert (superman-make-value value))))))
		((eq key 'fun) (ignore-errors (funcall (cadr el))))
		((eq key 'hdr)
		 (ignore-errors
		   (save-excursion
		     (org-back-to-heading)
		     (end-of-line)
		     (when test (put-text-property  (point-at-bol) (- (point) 1) 'test test))		     
		     (insert (superman-make-value value)))))
		((eq key 'body) (setq body (concat body (superman-make-value value)))))
	  (setq plist (cdr plist))))
      (insert "\n:END:\n")
      (org-remove-empty-drawer-at (point)))
    (when (stringp body) (insert body))
    (unless (= level 0) (insert "\n"))
    ;; leave cursor at beginning of entry
    (goto-char (previous-single-property-change (point-max) 'read-only))
    (forward-line 1)
    (end-of-line)
    (insert " ")
    (superman-capture-mode)
    (when read-only
      (setq buffer-read-only t))
    (run-hooks 'superman-setup-scene-hook)
    (let ((inhibit-read-only t))
      (when clean-hook
	(put-text-property (point-min) (1+ (point-min)) 'clean-hook clean-hook))
      (when quit-hook
	(put-text-property (point-min) (1+ (point-min)) 'quit-hook quit-hook)))))

(defun superman-insert-destination-line (marker heading &optional replace)
  "Insert first line of superman capture buffer."
  (goto-char (point-min))
  (when (re-search-forward "Superman captured: " nil t)
    (when replace (delete-region (point) (point-at-eol)))
    (insert "The captured text will be saved in " 
	    (buffer-name (marker-buffer marker))
	    " (line " (number-to-string (org-with-point-at marker (count-lines 1 (point))))
	    (if (stringp heading) (concat " just below " heading) "")
	    ")")))

(defun superman-capture-show-context ()
  "Show the destination buffer and position
in other window for this superman-capture."
  (interactive)
  (let ((capture-buffer (buffer-name (current-buffer)))
	(context-marker (get-text-property (point-min) 'destination)))
    (switch-to-buffer (marker-buffer context-marker))
    (goto-char (marker-position context-marker))
    (superman-set-config (concat capture-buffer
				 " / " 
				 (buffer-name
				  (marker-buffer context-marker))
				 ))))

(defun superman-change-destination ()
  (interactive)
  (let* (dest-heading
	 (inhibit-read-only t)
	 (buffer-read-only nil)
	 (old-destination (get-text-property (point-min) 'destination)))
    (if (not old-destination)
	(error "Cannot find current destination")
      (let* ((old-file (buffer-file-name (marker-buffer old-destination)))
	     (new-file (read-file-name "New destination in file: "
				       (file-name-directory old-file)
				       nil t nil nil))
	     (new-pos
	      (save-window-excursion
		(let* ((cats (progn (find-file new-file)
				    (when (eq major-mode 'org-mode)
				      (append (superman-parse-cats (current-buffer) 1)
					      (superman-parse-cats (current-buffer) 2)))))
		       (heading (when cats (completing-read (concat
							     "Select target heading in "
							     new-file " (tab to complete): ")
							    cats)))
		       (pos (when heading (car (cdaadr (assoc heading cats))))))
		  (if pos 
		      (goto-char pos)
		    (goto-char (point-max)))
		  (point-marker)))))
	(org-with-point-at new-pos
	  (ignore-errors (setq dest-heading (org-get-heading t t))))
	(if (> (length dest-heading) 25)
	    (setq dest-heading (concat (substring dest-heading 0 22) "...")))
	(superman-insert-destination-line new-pos dest-heading t)
	(put-text-property (point-min) (1+ (point-min)) 'destination new-pos)))))
      

(define-minor-mode superman-capture-mode
  "Toggle superman capture mode.
With argument ARG turn superman-capture-mode on if ARG is positive, otherwise
turn it off."
  :lighter " *S*-Capture"
  :group 'org
  :keymap 'superman-capture-mode-map)

(defun superman-capture-mode-on ()
  (interactive)
  (when superman-hl-line (hl-line-mode 1))
  (superman-capture-mode t))

(defun superman-make-value (val)
  (cond ((stringp val) val)
	((functionp val) (funcall val))
	((listp val)
	 (let ((thing (car val)))
	   (cond ((stringp thing)
		  (add-text-properties 0 (length thing) (cdr val) thing))
		 ((functionp thing)
		  (funcall thing (cdr val))))))))

;;}}}
;;{{{ Clean or quit the scene
(defun superman-clean-scene ()
  "Cleaning the capture scene. First save the capture
at the requested destination and then reset the window configuration."
  (interactive)
  (if buffer-read-only
      (error "Cannot save in read-only mode")
    (let* ((scene (get-text-property (point-min) 'scene))
	   (nick (get-text-property (point-min) 'nickname))
	   (edit (get-text-property (point-min) 'edit))
	   req
	   (n-error 0)
	   next
	   catch
	   hidden-hook
	   (dest (get-text-property (point-min) 'destination)))
      ;; remove previous warnings
      (goto-char (point-min))
      (while (setq next (next-single-property-change (point) 'superman-error))
	(goto-char next)
	(delete-region (point-at-bol) (1+ (point-at-eol))))
      ;; see if hdr is filled correctly
      (goto-char (point-min))
      (ignore-errors (outline-next-heading))
      (when (org-at-heading-p)
	(let ((test (or (get-text-property (point-at-bol) 'test)))
	      (message (or (get-text-property (point-at-bol) 'message) "Error"))
	      test-message)
	  (when (and test (setq test-message (funcall test)))
	    (beginning-of-line) (insert "<" (if (stringp test-message) test-message message) ">\n")
	    (forward-line -1)
	    (put-text-property (point-at-bol) (point-at-eol) 'superman-error 'yes)
	    (put-text-property (point-at-bol) (point-at-eol) 'face 'superman-warning-face)
	    (forward-line 1)
	    (setq n-error (1+ n-error)))))
      ;; move through form and test if filled correctly
      (goto-char (point-min))
      (while (setq next (next-single-property-change (point-at-eol) 'prop-marker))
	(goto-char next)
	(re-search-forward ":[a-zA-Z0-9]+:" nil t)
	;; make sure that there is a space between : and the value
	(unless (looking-at " ") (insert " "))
	;;
	(let* ((if-empty (get-text-property (point-at-bol) 'if-empty))
	       (test (or (get-text-property (point-at-bol) 'test)))
	       test-message
	       (message (or (get-text-property (point-at-bol) 'message) "Error")))
	  (cond ((and test (setq test-message (funcall test)))
		 (beginning-of-line) (insert "<" (if (stringp test-message) test-message message) ">\n")
		 (forward-line -1)
		 (put-text-property (point-at-bol) (point-at-eol) 'superman-error 'yes)
		 (put-text-property (point-at-bol) (point-at-eol) 'face 'superman-warning-face)
		 (forward-line 1)
		 (insert "\n")
		 (re-search-forward ":[a-zA-Z0-9]+:" nil t)
		 (setq n-error (1+ n-error)))
		;; empty field
		((looking-at "[ \t]*\n")
		 (cl-case if-empty 
		   ('delete (delete-region (point-at-bol) (1+ (point-at-eol))))
		   ('complain 
		    (beginning-of-line) (insert " <" (or message "Cannot be empty") ">\n")
		    (forward-line -1)
		    (put-text-property (point-at-bol) (point-at-eol) 'superman-error 'yes)
		    (put-text-property (point-at-bol) (point-at-eol) 'face 'superman-warning-face)
		    (forward-line 1)
		    (setq n-error (1+ n-error)))
		   (t (goto-char (point-at-eol))))))))
      (if (> n-error 0)
	  (progn (message (concat (number-to-string n-error) " fields are not filled correctly."))
		 (goto-char (next-single-property-change (point-min) 'superman-error))
		 (forward-line 1)
		 (end-of-line))
	;; google calendar hook (could be replaced by a button)
	(let ((calendar (superman-get-property (point) "GoogleCalendar" t nil)))
	  (when (and calendar (not (string= calendar "")))
	    (save-restriction
	      (superman-google-export-appointment))))
	;; run-hooks hidden hook 
	(when (setq hidden-hook (get-text-property (point-min) 'clean-hook))
	  (funcall hidden-hook))
	(goto-char (previous-single-property-change (point-max) 'read-only))
	(skip-chars-forward "\n\t ")
	;; put hidden properties
	(save-excursion
	  (when (org-at-heading-p)
	    (let ((hidden-props (get-text-property (point-min) 'hidden-properties)))
	      (when hidden-props
		(while hidden-props
		  (org-set-property (caar hidden-props) (cadar hidden-props))
		  (setq hidden-props (cdr hidden-props)))))))
	;; catch the thing
	(setq catch (buffer-substring (point-at-bol) (point-max)))
	;; say by by to capture buffer
	(kill-buffer (current-buffer))
	(set-buffer (marker-buffer dest))
	(goto-char (marker-position dest))
	(if edit
	    (progn (org-narrow-to-subtree)
		   ;; if this is an edit replace the heading at destination
		   (delete-region (point-min) (point-max)))
	  (ignore-errors (org-narrow-to-subtree))
	  (goto-char (point-max))
	  (insert "\n"))
	(insert catch)
	(save-buffer)
	(widen)
	;; update buffers
	(when nick
	  (let ((pro-buffers (superman-get-project-buffers nick)))
	    (while pro-buffers 
	      (when (get-buffer (car pro-buffers))
		(switch-to-buffer (car pro-buffers))
		(superman-redo))
	      (setq pro-buffers (cdr pro-buffers)))))
	;; perform action according to scene
	(cond ((window-configuration-p scene)
	       ;; set window-configuration
	       (set-window-configuration scene))
	      ;; call function
	      ((functionp scene) (funcall scene))
	      (t nil))
	(when (or superman-view-mode superman-mode) (superman-redo))))))

(defun superman-quit-scene ()
  "Cancel `superman-capture'
or `superman-view-edit-item' and restore the
window-configuration saved in text-property scene at
point-min.

If a file is associated with the current-buffer save it.
"
  (interactive)
  (let ((scene (or (get-text-property (point-min) 'quit-scene)
		   (get-text-property (point-min) 'scene))))
    (when (setq hidden-hook (get-text-property (point-min) 'quit-hook))
      (funcall hidden-hook))
    (kill-buffer (current-buffer))
    (when (window-configuration-p scene)
      (set-window-configuration scene))))
;;}}}
;;{{{ completion and test- functions

(defun superman-read-category ()
  (completing-read "Choose a category: " 
		   superman-project-categories 
		   nil nil nil))
(defun superman-read-index-file ()
  (concat "[[" (read-file-name "Choose index file: ") "]]"))

(defun superman-read-file-name ()
  (concat "[[" (read-file-name "Choose file: ") "]]"))

(defun superman-read-directory-name ()
  (concat "[[" (read-directory-name "Choose a location: ") "]]"))

(defvar superman-capture-completion-plist
  '(:filename superman-read-file-name
	      :link "link to url"
	      :appointmentdate superman-read-date
	      :meetingdate superman-read-date
	      :location superman-read-directory-name)
  "P-list to associate a function with a property name. The function is used 
 by `superman-complete-property' to complete property fields during capture.
 Note: by convention the keys of the list (property names with trailing colon) are in lower-case.")

(defun superman-complete-property ()
  "Read text properties at beginning of line to help finding a value for this property."
  (interactive)
  (save-excursion
    (if (org-at-heading-p)
	(message "Type a title after ***, activate todo: C-c C-t, change priority Shift-up")
      (let* ((prop (cond ((get-text-property (point-at-bol) 'property))
			 ((save-excursion (beginning-of-line)
					  (when (looking-at org-property-re)
					    (match-string 2))))
			 (t (error "superman-complete-property: Cannot see a property at point."))))
	     (text-props (text-properties-at (point-at-bol)))
	     (complete (or (get-text-property (point-at-bol) 'complete)
			   (plist-get superman-capture-completion-plist
				      (intern (concat ":" (downcase prop))))))
	     value)
	(when (and (symbolp complete) (not (symbol-function complete))) 
	  (setq complete (eval complete)))
	(cond ((functionp complete) (setq value (funcall complete)))
	      ((stringp complete) (message complete))
	      (t (message "Don't know what to do here")))
	(when value
	  (delete-region (point-at-bol) (point-at-eol))
	  (insert ":" prop ": " value)
	  (set-text-properties (point-at-bol) (1+ (point-at-bol)) text-props))))))

(defun superman-capture-change-priority ()
  (interactive) 
  (save-excursion
    (goto-char (point-max))
    (outline-previous-heading)
    (org-shiftup)))

(defun superman-capture-add-timeline ()
  (interactive) 
  (save-excursion
    (goto-char (point-max))
    (outline-previous-heading)
    (org-deadline)))

(defun superman-capture-change-todo ()
  (interactive) 
  (save-excursion
    (goto-char (point-max))
    (outline-previous-heading)
    (org-todo)))

(defun superman-test-nickname ()
  ;; check if nickname exists 
  (if
      (save-excursion (goto-char (point-min))
		      (re-search-forward "Superman edits item" nil t))
      nil
    (when (assoc  (superman-get-property (point) "nickname") superman-project-alist)
      "Name already in use")))

(defun superman-test-hdr (&optional message)
  (let ((hdr (ignore-errors (org-get-heading t t))))
    (unless hdr 
      (or message "Please type a title for this capture"))))
;;}}}

;;{{{ capture documents, notes, etc.
    
(defun superman-capture-note (&optional project marker ask)
  (interactive)
  (let ((pro (superman-get-project project ask))
	(marker (or marker (get-text-property (point-at-bol) 'org-hd-marker))))
    (superman-capture
     pro
     (or marker "Notes")
     "Note"
     nil
     `((hdr :test superman-test-hdr)
       ("CaptureDate" :hidden yes :value ,(format-time-string "[%Y-%m-%d %a]"))))))

(defun superman-capture-text (&optional project marker ask)
  (interactive)
  (let ((pro (superman-get-project project ask))
	(marker (or marker (get-text-property (point-at-bol) 'org-hd-marker))))
    (save-excursion
      (superman-goto-project project "Text" 'create nil nil nil ":FreeText: t"))
    (superman-capture
     pro
     (or marker "Text")
     "free text"
     nil nil 0)))

(defun superman-capture-bookmark (&optional project marker ask bookmark)
  (interactive)
  (let ((pro (superman-get-project project ask))
	(bookmark (or bookmark ""))
	(marker (or marker (get-text-property (point-at-bol) 'org-hd-marker))))
    (superman-capture
     pro
     (or marker "Bookmarks")
     "A bookmark will be created to the link specified by property \"Link\" below.\nTo change the file put cursor in this line and press TAB."
     nil
     `(("CaptureDate" :hidden yes :value ,(format-time-string "[%Y-%m-%d %a]"))
       (hdr :test superman-test-hdr)
       ("Link" :value ,bookmark :if-empty complain :message "Need a link to url or something similar")))))

(fset 'superman-capture-todo 'superman-capture-task)
(defun superman-capture-task (&optional project marker ask)
  "Capture a task for todo-list of PROJECT. MARKER is a marker which 
defaults to the value of text-property org-hd-marker at bol. If ASK 
is non-nil prompt for project."
  (interactive)
  (let ((pro (superman-get-project project ask))
	(marker (or marker (get-text-property (point-at-bol) 'org-hd-marker))))
    (superman-capture
     pro
     (or marker "Tasks")
     (concat "A task will be created.\n\n"
	     (superman-make-button "Change priority" '(:fun superman-capture-change-priority
							    :face superman-capture-button-face
							    :help "Change the priority."
							    :width 21))
	     " "
	     (superman-make-button "Change todo" '(:fun superman-capture-change-todo
							:face superman-capture-button-face
							:help "Change the todo status."
							:width 21))
	     " "
	     (superman-make-button "Add timeline" '(:fun superman-capture-add-timeline
							 :face superman-capture-button-face
							 :help "Add a time at which the task should be done."
							 :width 21)))

     ;; - to change the priority put cursor into header line (*** ...) and press Shift-up.
     ;; - to change the todo status put cursor into  header lin (*** ...) and press Ctrl-c Ctrl-t
     nil
     `(("CaptureDate" :hidden yes :value ,(format-time-string "[%Y-%m-%d %a]"))
       (hdr :test superman-test-hdr)
       (fun
	(lambda ()
	  (save-excursion
	    (org-todo)
	    (org-back-to-heading)
	    (org-shiftup))))))))

(defun superman-capture-thing (&optional project)
  "Associate a thing (file, note, task, link, meeting, etc.) with a project.
The default place for the new item is at the cursor in superman-view-mode
and in the first cat otherwise."
  (interactive)
  (let ((pro (superman-get-project project nil))
	(scene (current-window-configuration))
	(cat (superman-current-cat))
	(marker (get-text-property (point-at-bol) 'org-hd-marker))
	(defaults `((hdr " TODO [A] New item")
		    ("Link" :complete "link to url")
		    ("FileName" :complete superman-read-file-name)
		    ("AppointmentDate" :complete superman-read-date)
		    ("MeetingDate" :complete superman-read-date)
		    ;; ("ProjectStart" :complete superman-read-date)
		    ("Location" :complete "Where to meet")))
	props keys)
    (if superman-view-mode
	(when (and cat (superman-get-property (superman-cat-point) "freetext"))
	  (error "Cannot add items in freetext area"))
      ;; activate project view
      (superman-switch-config pro nil "PROJECT"))
    ;; now we are in project view
    (cond ((or (not cat) (not (stringp cat)))
	   (superman-capture-item pro))
	  ((string-match "calendar" cat)
	   (superman-capture-meeting pro cat nil))
	  ((string-match "document\\|file" cat)
	   (superman-capture-document pro cat nil))
	  ((string-match "bookmark" cat)
	   (superman-capture-bookmark pro cat nil))
	  ((string-match "task" cat)
	   (superman-capture-task pro cat nil))
	  ((string-match "note" cat)
	   (superman-capture-note pro cat nil))
	  (t
	   ;; supplement list of existing properties
	   ;; with default properties
	   (setq keys
		 (mapcar 'list
			 (superman-view-property-keys)))
	   (if (assoc "CaptureDate" keys)
	       (setq keys
		     (append `(("CaptureDate" :hidden yes :value ,(format-time-string "[%Y-%m-%d %a %R]")))
			     (delete (assoc "CaptureDate" keys) keys)))
	     (setq keys `(("CaptureDate" :hidden yes :value ,(format-time-string "[%Y-%m-%d %a %R]")))))
	   ;; add defaults (when needed)
	   (while defaults
	     (unless (assoc (caar defaults) keys)
	       (setq props (append (list (car defaults)) keys)))
	     (setq defaults (cdr defaults)))
	   (superman-capture pro
			     (or marker cat)
			     "item"
			     nil
			     props nil scene)))))


;; FIXME: this should become an edit of the project entry 
(defun superman-capture-others (&optional project)
  "Set the names of the OTHERS, i.e. the collaborators, for project PROJECT."
  (interactive)
  (let ((nick (if (stringp project) project (car project))))
    (save-window-excursion
      (find-file superman-profile)
      (goto-char (point-min))
      (re-search-forward (concat ":nickname:[ \t]*" nick) nil nil)
      (superman-set-others (assoc nick superman-project-alist))
      (save-buffer))
    (if superman-view-mode
	(superman-redo))))
;;}}}
;;{{{ capture files

(defun superman-capture-file-at-point ()
  (interactive)
  (if (not (eq major-mode 'file-list-mode))
      (error "Works only in file-list-mode")
    (let ((nick (get-text-property (point-min) 'nickname))
	  (file-list (list (file-list-make-entry (file-list-file-at-point)))))
      (superman-capture-file-list nick file-list nil 'ask))))
       
(defun superman-capture-file-list (&optional project file-list level ask)
  "Capture a FILE-LIST of files in PROJECT. Add them all to the project
index file as LEVEL headings. Then show the updated project view buffer."
  (interactive)
  (let* ((pro (superman-get-project project ask))
	 (gitp (superman-git-toplevel (superman-get-location pro)))
	 (marker (get-text-property (point-at-bol) 'org-hd-marker))
	 (heading (if (and marker (superman-current-cat))
		      marker
		    "Documents"))
	 (pro-file-list-buffer (concat "*File-list-" (car pro) "*"))
	 (level (or level superman-item-level))
	 (file-list (cond
		     (file-list)
		     ((or major-mode 'file-list-mode)
		      (eq major-mode 'file-list-completion-mode)
		      file-list-current-file-list)
		     ((and (buffer-live-p pro-file-list-buffer)
			   (progn (switch-to-buffer pro-file-list-buffer)
				  file-list-current-file-list))
		      (superman-file-list pro)))))
    ;; goto index file
    (if heading
	(cond ((stringp heading)
	       (superman-goto-project pro heading 'create nil nil nil))
	      ((markerp heading)
	       (progn (switch-to-buffer (marker-buffer heading))
		      (goto-char heading))))
      (find-file (superman-get-index pro))
      (widen)
      (show-all)
      (goto-char (point-max)))
    (while file-list
      (let* ((el (car file-list))
	     (fname (file-list-make-file-name~ el)))
	(message (concat "adding " fname))
	(insert (make-string level (string-to-char "*"))
		" "
		(car el)
		"\n:PROPERTIES:"
		"\n:FileName: [["  (abbreviate-file-name fname) "]]"
		"\n:END:\n\n"))
      (setq file-list (cdr file-list)))
    (superman-view-project pro)
    (superman-redo)))

(fset 'superman-capture-file 'superman-capture-document)
(defun superman-capture-document (&optional project marker ask)
  "Register a file in your project-manager. At this point
file does not need to exist."
  (interactive)
  (let* ((pro (superman-get-project project ask))
	 (marker (or marker (get-text-property (point-at-bol) 'org-hd-marker)))
	 (cat-name (get-text-property (point-at-bol) 'cat))
	 (heading (cond (cat-name)
			((and marker (superman-current-cat))
			 marker)
			(t "Documents")))
	 (dir (expand-file-name (superman-get-location pro)))
	 ;; FIXME: to circumvent a bug in ido-read-file-name
	 (read-file-name-function 'read-file-name-default)
	 (file (read-file-name (concat "Add document to " (car pro) ": ") (file-name-as-directory dir))))
    (superman-capture
     pro
     heading
     "A link will be created to the file specified by property \"FileName\" below.\nTo change the file put cursor in this line and press TAB."
     nil
     `(("FileName" :value ,(concat "[["  (abbreviate-file-name file) "]]"))
       ("CaptureDate" :hidden yes :value ,(format-time-string "[%Y-%m-%d %a]"))
       (hdr :value ,(file-name-nondirectory file)))
     nil nil)))

(defun superman-capture-superman ()
  "Set up superman profile."
  (unless superman-profile ;; user set the name but file does not exist yet.
    (stop "You need to set the variable superman-profile to a file-name in your .emacs file."))
  (find-file superman-profile)
  (goto-char (point-min))
  (let* ((profile-buffer (buffer-name (current-buffer)))
	 (marker (point-marker))
	 (quit-scene (current-window-configuration))
	 (welcome
	  (concat
	   "Initialize the SuperMan(ager)" 
	   "\n\nCheck and adjust the setup below, then press the Save button or press C-c C-c.\nThe profile will be saved in the file:\n\n"
	   superman-profile))
	 (clean-hook
	  `(lambda ()
	     (goto-char (point-min))
	     (re-search-forward "*[ ]+SupermanSetup" nil t)
	     ;; read-off value of superman-home
	     ;; (superman-parse-setup (point) (superman-defaults) nil)
	     ;; create directory if necessary
	     (unless (file-exists-p (file-name-directory superman-profile))
	       (make-directory (file-name-directory superman-profile)  't))
	     ;; save profile
	     (save-excursion
	       (find-file superman-profile)
	       (save-buffer)))))
    (superman-capture-whatever
     marker
     welcome
     1
     (concat "\n" (superman-set-up-defaults))
     `(("InitialVisit" :value,(format-time-string "<%Y-%m-%d %a>"))
       (hdr :value "Superman(ager)" :test superman-test-hdr))
     nil ;; this is not an edit
     'superman ;; scene
     nil ;; no nick name 
     nil ;; read-only
     quit-scene ;; quit-scene
     clean-hook ;; clean-hook
     nil ;; quit-hook
     )))

(fset 'superman-new-project 'superman-capture-project)
(defun superman-capture-project (&optional nickname category loc)
  "Create a new project. If CATEGORY is nil prompt for project category
  with completion in existing categories. If NICKNAME is nil prompt for nickname.
  If LOC is given it is the mother directory of the directory which
  defines the project. 

  The following steps are performed:

  Step (1) a new entry is added to the file `superman-profile' and the file saved.
  Step (2) The project directory is created (unless it exists).
  Step (3) The index file is initialized (unless it exists).
  Step (4) The new project is visited.

  Note that saving the file `superman-profile' also updates the `superman-project-alist'.

  To undo all this, enter the supermanager (shortcut: M-x `superman'), navigate to
  the new project and call `superman-delete-project' (shortcut: x)
  "
  (interactive)
  ;; (superman-refresh)
  (let* ((marker (get-text-property (point-at-bol) 'org-hd-marker))
	 (scene (current-window-configuration)))
    (superman-capture
     `("*S*" (("index" . ,superman-profile)))
     marker
     (concat "A project will be created.\n - Press [TAB] on the properties below for help and completions.
 - \"Nickname\" is used to quickly identify project. The Header will appear in the project manager overview.
 - \"Location\" is a folder on your computer associated with the project
 - If \"Location\" is not an existing folder it will be created along with its parents.
 - \"Index\" is a file which contains project information. 
 - If \"Index\" is not specified it will be \"Nickname\".org
 - \"Others\" are your collaborators on the project.")
     nil
     `(("Nickname" :if-empty complain :message "Choose a nickname" :test superman-test-nickname :complete "Type a name without blanks to identify the project.")
       ("Category" :if-empty complain :message "Choose a category" :complete superman-read-category)
       ("Others")
       ("Location" :value ,loc :if-empty complain :message "Choose a location" :complete superman-read-directory-name)
       (hdr :value "ACTIVE" :test superman-test-hdr)
       ("Index" :complete superman-read-index-file)
       ("InitialVisit" :value ,(format-time-string "[%Y-%m-%d %a]") :complete "This should not be changed")
       ("LastVisit" :value ,(format-time-string "<%Y-%m-%d %a>") :complete superman-read-date))
     superman-project-level
     'superman-view-new-project-hook ;; sets the scene to PROJECT 
     scene ;; when capture is canceled
     )))

(defun superman-view-new-project-hook ()
  "Hook to be run when a new project is captured.
Creates the project directory and index file."
  (let* ((case-fold-search t)
	 (nick
	  (progn
	    ;; (outline-next-heading)
	    (org-back-to-heading)
	    (superman-get-property (point) "nickname")))
	 (pro (progn
		(save-buffer)
		(assoc nick superman-project-alist))))
    (superman-create-project pro)
    (superman-update-project-overview)
    (superman-switch-to-project pro)
    (superman-switch-config pro nil "PROJECT")))

;;}}}
;;{{{ Capture meetings
;; Note: inactive time stamp for CaptureDate

(defun superman-read-date ()
  (with-temp-buffer (org-time-stamp t) (buffer-string)))

(defun superman-capture-calendar (&optional project marker ask)
  (interactive)  
  (superman-capture-meeting "Calendar" marker ask))


(defun superman-capture-meeting (&optional project marker ask)
  "Capture a meeting for PROJECT. MARKER is a marker or the name of a section 
which defaults to 'Calendar'. If ASK 
is non-nil prompt for project."
  (interactive)
  (let ((pro (superman-get-project project ask))
	(marker (or marker (get-text-property (point-at-bol) 'org-hd-marker)))
	(date (superman-read-date)))
    (superman-capture
     pro
     (or marker "Calendar")
     "Meeting"
     nil
     `(("MeetingDate" :value ,date :complete superman-read-date)
       ("Participants" :complete "Who will be attending?")
       ("Location" :complete "Place of meeting.")
       (when superman-google-default-calendar
	 ("GoogleReminderMinutes")
	 )
       ,(when superman-google-default-calendar
	  `("GoogleCalendar" :value ,superman-google-default-calendar 
	    :complete superman-google-calendars)
	  )
       ("CaptureDate" :hidden yes :value ,(format-time-string "[%Y-%m-%d %a]")
	(hdr :test superman-test-hdr))))))

;;}}}
;;{{{ capture unison 

(defun superman-capture-unison (&optional project ask)
  (interactive)
  (let ((pro (superman-get-project project ask))
	(root-1 (read-directory-name "Unison root directory 1: "))
	(root-2 (read-directory-name "Unison root directory 2: ")))
    (superman-capture
     pro
     "Configuration"
     "Unison"
     nil
     `(("UNISON" :value ,superman-unison-cmd)
       (hdr :value "Synchonise")
       ("SWITCHES" :value ,superman-unison-switches)
       ;; "-ignore 'Path .git' -ignore 'Regex ^(\\.|#).*' -ignore 'Regex .*~$' -perms 0")
       ;; :SWITCHES: -ignore 'Regex .*(~|te?mp|rda)$' -ignore 'Regex ^(\\.|#).*' -perms 0
       ("ROOT-1" :value ,(shell-quote-argument (expand-file-name root-1)) :complete superman-read-directory-name)
       ("ROOT-2" :value ,(shell-quote-argument (expand-file-name root-2)) :complete superman-read-directory-name)
       ("CaptureDate" :hidden yes :value ,(format-time-string "[%Y-%m-%d %a]"))))))

(defun superman-unison-insert-switches ()
  (interactive)
  (insert superman-unison-switches))

(defun superman-run-unison (&optional project)
  (interactive)
  (save-excursion
    (save-restriction
      (let* ((pro (or project
		      superman-current-project
		      (superman-switch-to-project nil t)))
	     (unison-list (superman-view-read-unison pro))
	     (ulen (length unison-list)))
	;; prevent synchronizing unsaved buffers
	(superman-save-some-buffers nil t)
	(cond ((= ulen 1)
	       (async-shell-command (cdar unison-list)))
	      ((> ulen 1)
	       (let ((u (completing-read
			 "Choose unison command: " unison-list)))
		 (when u (async-shell-command
			  (cdr (assoc u unison-list)))))))))))

;;}}}
;;{{{ capture mails

;; Support for saving Gnus messages by Message-ID
;; See ~/emacs-genome/genes/worg/org-hacks.org

;; (article (gnus-summary-article-number))
;; (header (gnus-summary-article-header article))
;; (message-id
;; (save-match-data
;; (let ((mid (mail-header-id header)))
;; (if (string-match "<\\(.*\\)>" mid)
;; (match-string 1 mid)
;; (error "Malformed message ID header %s" mid)))))
;; (org-store-link-functions 'superman-store-link-to-gnus-message)


(defun superman-capture-mail (&optional project)
  ;; (defun superman-gnus-goto-project-mailbox (project &optional arg)
  (interactive)
  (unless (or (eq major-mode 'gnus-article-mode)
	      (eq major-mode 'gnus-summary-mode))
    (error "Can only capture mails from either gnus-article or gnus-summary buffers"))
  ;; activate the connection with summary
  (when (eq major-mode 'gnus-article-mode)
    (gnus-article-show-summary))
  (let* ((buf (current-buffer))
	 (link (org-store-link 1))
	 (entry (superman-get-project project 'ask))
	 (pro (car entry))
	 (loc (expand-file-name (superman-get-location entry)))
	 (index (superman-get-index entry))
	 (region (progn (gnus-summary-select-article-buffer)
			(if (use-region-p)
			    ;; (mark-whole-buffer))
			    (concat "----\n#+BEGIN_EXAMPLE\n" 
				    (buffer-substring (region-beginning) (region-end))
				    "#+END_EXAMPLE\n----\n")
			  "")))
	 (mailbox (file-name-as-directory
		   (concat (file-name-as-directory loc) "Mailbox")))
	 (from (message-fetch-field "from"))
	 (subject (message-fetch-field "subject"))
	 (date (message-fetch-field "date"))
	 (attachments (superman-save-attachments pro mailbox (current-buffer) date)))
    (superman-capture
     entry
     "Mail"
     "Mail"
     nil
     `(("CaptureDate"  :hidden yes :value ,(format-time-string "<%Y-%m-%d %a>"))
       (body :value region) 
       (body :value ,attachments)
       ("EmailDate" :value ,date)
       (hdr :value ,(concat "Mail from " from " " subject ) :test superman-test-hdr)
       ("Link" :value ,link)))))

(defun superman-save-attachments (project dir buf date)
  "Interactively save mail contents in project org file
and MIME parts in sub-directory 'mailAttachments' of the project."
  (interactive)
  (gnus-article-check-buffer)
  (let* ((mime-line ""))
    (unless (file-exists-p dir)
      (if (featurep 'xemacs)
          (make-directory-path dir)
        (make-directory dir t)))
    (save-excursion
      (switch-to-buffer buf)
      (gnus-summary-display-buttonized 't))
    ;; (goto-char (point-min))
    (message-goto-body)
    (while (re-search-forward "\\[[0-9]+\\." nil t)
      ;; modified code from `mm-save-data'
      (save-excursion
        (let* ((data (get-text-property (point) 'gnus-data))
               (filename (or (mail-content-type-get
                              (mm-handle-disposition data) 'filename)
                             (mail-content-type-get
                              (mm-handle-type data) 'name)))
               file)
          (when filename
            (setq filename (gnus-map-function
                            mm-file-name-rewrite-functions
                            (file-name-nondirectory filename))))
          (when (and data filename)
            (setq file (read-file-name
                        "Save attached file to: "
                        dir nil nil (replace-regexp-in-string "[ ]+" "" filename)))
            (if (file-directory-p file)
                (message "file not saved")
              (when (or (not (file-exists-p file))
                        (y-or-n-p (concat "File " file " exists, overwrite?" )))
		(when (not (file-exists-p (file-name-directory file)))
		  (if (y-or-n-p (concat "Create directory "  (file-name-directory file) "? "))
		      (make-directory  (file-name-directory file))))
		(mm-save-part-to-file data file))
	      (setq mime-line (concat "\n**** " (file-name-nondirectory file)
				      "\n:PROPERTIES:\n:CaptureDate: " (format-time-string (car org-time-stamp-formats) (org-capture-get :default-time))
				      "\n:EmailDate: " date 
				      ;; (format-time-string (car org-time-stamp-formats) (org-capture-get :default-time))
				      "\n:Link:" " [[file:"
				      (replace-regexp-in-string
				       (expand-file-name "~")
				       "~"
				       file)
				      "][" (file-name-nondirectory file) "]]"
				      "\n:END:\n"
				      mime-line)))))))
    mime-line))

;;}}}
;;{{{ capture cats
(defun superman-capture-cat (&optional project marker ask)
  (interactive)
  (let ((pro (superman-get-project project ask))
	(marker (or marker (get-text-property (point-at-bol) 'org-hd-marker))))
    (superman-capture
     pro
     marker 
     "Heading"
     nil
     `(("Ball1" :value "todo :face superman-get-todo-face")
       ("Ball2" :value "hdr :name Title :width 13 :face font-lock-function-name-face")
       ("Ball3" :value ""))
     1)))

;;}}}
;;{{{ capture git section

(defun superman-capture-git-section (&optional project git-dir level Ask)
  "Capture files under version control. Delete and recreate section 'GitFiles' "
  (interactive)
  (let* ((pro (superman-get-project project ask))
	 (gitp (superman-git-toplevel (superman-get-location pro)))
	 (gitdir (or git-dir
		     (read-directory-name (concat "Directory : "))))
	 (gittop (superman-git-toplevel gitdir))
	 ;; (headingfound (superman-goto-project pro "GitFiles"))
	 (level (or level superman-item-level))
	 (file-list (delete ""
			    (split-string
			     (shell-command-to-string
			      (concat "cd " gitdir ";"
				      superman-cmd-git
				      " ls-files --full-name")) "\n")))
	 (file-hash (make-hash-table :test 'equal))
	 (probuf (get-buffer-create (concat
				     "*"
				     (superman-get-index pro)
				     (car pro)
				     "-git-profile*")))
	 (profile (concat (file-name-directory
			   (superman-get-index pro))
			  (car pro)
			  "-git-profile.org")))
    (set-buffer probuf)
    (erase-buffer)
    ;; goto index file
    (while file-list
      (let* ((el (car file-list))
	     (elsplit (split-string el "/"))
	     (filename (car (last elsplit)))
	     (direl (reverse (cdr (reverse elsplit))))
	     (dir (if (> (length elsplit) 1)
		      (mapconcat 'identity direl "/" )
		    "/")))
	(puthash dir (push filename (gethash dir file-hash)) file-hash)
	(setq file-list (cdr file-list))))
    (find-file profile)
    (erase-buffer)
    (goto-char (point-max))  
    (maphash (lambda (keys vv) 	       
	       (insert (concat "** " keys "\n\n"))
	       (while vv
		 (let ((filename (abbreviate-file-name
				  (expand-file-name
				   (concat (if (string= keys "/") "." keys) "/" (car vv)) gittop))))
		   (insert (make-string level (string-to-char "*"))
			   " "
			   (car vv)
			   "\n:PROPERTIES:"
			   "\n:FileName: [[" filename "]]"
			   "\n:GitStatus: " (if (file-exists-p filename) "Unknown" "Removed")
			   ;; (when gitp (concat "\n:GitStatus: " (nth 1 (superman-git-get-status fname nil))))
			   "\n:END:\n\n"))
		 (setq vv (cdr vv)))) file-hash)
    (goto-char (point-min))
    (superman-format-section superman-document-balls probuf)
    (switch-to-buffer probuf)
    (goto-char (point-min))
    (insert (superman-make-button
	     (concat "Git repository for project: " (car pro))
	     `(:fun (lambda () (interactive) (superman-capture-git-section ,(car pro) ,gitdir))
		    :face superman-project-button-face
		    :help "Refresh")))
    (insert "\n\n")
    (goto-char (point-min))
    (put-text-property (point-at-bol) (point-at-eol) 'git-dir gitdir)
    (put-text-property (point-at-bol) (point-at-eol) 'nickname (car pro))
    (put-text-property (point-at-bol) (point-at-eol) 'index profile)
    (put-text-property (point-at-bol) (point-at-eol) 'redo-cmd `(superman-capture-git-section ,(car pro) ,gitdir))
    (superman-view-mode)))


;;}}}

;;{{{ yank

(defun superman-yank (arg)
  "Yank content directly into superman buffer. 
   With prefix [C-u C-y] org-links can be captured."
  (interactive "P")
  (if arg (progn ;; Insert org-link into kill-ring
	    (with-temp-buffer
	      (org-insert-link)
	      (kill-ring-save (point-min) (point-max))
	      )))
    (let* ((yank (replace-regexp-in-string "^[ \t]*\\|[ \t]*$" "" (current-kill 0)))
	 (yank-text (cadr (split-string yank "//")))
	 (pro (superman-get-project (get-text-property (point-min) 'nickname)))
	 ;; (marker ))
	 )
      (cond ((or (string-match (regexp-opt thing-at-point-uri-schemes) yank)
		 (string-match org-bracket-link-regexp yank))
	     (superman-capture-bookmark pro nil nil yank)
	     (insert yank-text)
	     (superman-clean-scene))
	  (t (message "Superman: SoSorry, don't know how to yank this.")))))
;;}}}

(provide 'superman-capture)
;;; superman-capture.el ends here
