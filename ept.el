;;; ept.el -- Elpoint -- Yet Another Presentation Tool for Emacsen.

;; Copyright (C) 2002 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: Presentation

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;;; Commentary:
;; 

;;; History:
;; 

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'ept-align)
(require 'product)

(product-provide 'ept
  (product-define "Elpoint" nil
		  '(0 2 0)))

(defun elpoint-version ()
  "Return Elpoint version.
If it is called interactively, version string is appeared on minibuffer."
  (interactive)
  (let ((product-info (product-string 'ept)))
    (if (interactive-p)
	(message "%s" product-info)
      product-info)))

(autoload 'ept-dump-page "ept-dump" nil)
(autoload 'ept-dump-convert "ept-dump" nil)
(autoload 'ept-make-dump-page "ept-dump" nil)

(defvar ept-separator-string "-" "A charecter for separator")
(defvar ept-item-point-strings '("*") "A list of a string for item point.")

(defvar ept-play-frame-parameters nil)

(defvar ept-play-mode-map (make-sparse-keymap))

(define-key ept-play-mode-map " " 	  'ept-play-forward)
(define-key ept-play-mode-map [right] 	  'ept-play-forward)
(define-key ept-play-mode-map [backspace] 'ept-play-backward)
(define-key ept-play-mode-map [delete]	  'ept-play-backward)
(define-key ept-play-mode-map [left] 	  'ept-play-backward)
(define-key ept-play-mode-map "q" 	  'ept-play-stop)

(defvar ept-current-presentation nil
  "Presentation structure of the current buffer.")
(make-variable-buffer-local 'ept-current-position)

(defvar ept-current-position nil
  "Presentation point of the current buffer")
(make-variable-buffer-local 'ept-current-position)

(defvar ept-dump-page nil
  "Dump each presentation point if non-nil.")
(make-variable-buffer-local 'ept-dump-page)

(defvar ept-item-level 0
  "Item level of the page.")
(make-variable-buffer-local 'ept-item-level)

(defmacro ept-page (&rest body)
  `(progn 
     (if (string-match "^eval-" (symbol-name this-command))
	 (error "Don't eval this."))
     (erase-buffer)
     (setq ept-item-level 0)
     ,@ body
     (goto-char (point-min))))

(defmacro ept-itemize (&rest body)
  `(let ((ept-item-level (+ 1 ept-item-level))
	 pt)
     (setq pt (point))
     ,@ body
     (goto-char (point-min))
     (recenter)
     (goto-char (point-max))))

(defmacro ept-pause (&rest body)
  `(progn
     (goto-char (point-max))
     ,@ body))

(defvar ept-title-face-font (face-font 'default))
(defvar ept-body-face-font (face-font 'default))
(defvar ept-highlight-face-font (face-font 'default))
(defvar ept-separator-face-font (face-font 'default))
(defvar ept-item-face-font (face-font 'default))
(defvar ept-item-point-face-font (face-font 'default))
(defvar ept-basic-face-font (face-font 'default))

(defgroup ept nil
  "Elpoint -- Yet Another Presentation Tool for Emacsen"
  :group 'application)

(defface ept-title-face
  `((t (:foreground "Cyan" ,@(if window-system
				 (list :font ept-title-face-font)))))
  nil)

(defface ept-body-face
  `((t (:foreground "White" ,@(if window-system
				  (list :font ept-body-face-font)))))
  nil)

(defface ept-highlight-face
  `((t (:foreground "Yellow" ,@(if window-system
				   (list :font ept-highlight-face-font)))))
  nil)

(defface ept-separator-face
  `((t (:foreground "Gray" ,@(if window-system
				 (list :font ept-separator-face-font)))))
  nil)

(defface ept-item-face
  `((t (:foreground "Yellow" ,@(if window-system
				   (list :font ept-item-face-font)))))
  nil)

(defface ept-item-point-face
  `((t (:foreground "Orange" ,@(if window-system
				   (list :font ept-item-point-face-font)))))
  nil)

(defface ept-basic-face
  `((t (:foreground "Gray" ,@(if window-system
				 (list :font ept-basic-face-font)))))
  nil)

(defcustom ept-imagick-convert-program "convert"
  "*Program name of ImageMagick's `convert'."
  :type 'string)

(defcustom ept-content-type-alist
  '(("text/plain" "\\.\\(txt\\|tex\\|el\\)")
    ("text/html" "\\.s?html?$")
    ("image/jpeg" "\\.jpe?g$")
    ("image/png" "\\.png$")
    ("image/gif" "\\.gif$")
    ("image/tiff" "\\.tif?f$")
    ("image/x-xwd" "\\.xwd$")
    ("image/x-xbm" "\\.xbm$")
    ("image/x-xpm" "\\.xpm$")
    ("image/x-bmp" "\\.bmp$")
    ("video/mpeg" "\\.mpe?g$")
    ("video/quicktime" "\\.mov$")
    ("application/postscript" "\\.\\(ps\\|eps\\)$")
    ("application/pdf" "\\.pdf$"))
  "*Alist of file suffixes vs. content type."
  :group 'ept
  :type '(repeat
	  (list
	   (string :tag "Type")
	   (string :tag "Regexp"))))

(defcustom ept-url-regexp "\\<\\(https?://\\|ftp://\\|gopher://\\|telnet://\\|wais://\\|file:/\\|s?news:\\|mailto:\\)[^]	\n \"'()<>[^`{}]*[^]	\n \"'()<>[^`{}.,;]+"
  "A regular expression probably matching a complete URL."
  :group 'ept
  :type 'regexp)

(defcustom ept-play-frame-width nil
  "Presentation frame width. If nil, full-screen width."
  :group 'ept
  :type 'integer)

(defcustom ept-play-frame-height nil
  "Presentation frame height. If nil, full-screen height."
  :group 'ept
  :type 'integer)

(defcustom ept-play-frame-background-color "MidnightBlue"
  "Background color of the presentation frame."
  :group 'ept
  :type 'string)

(defcustom ept-play-frame-foreground-color "white"
  "Foreground color of the presentation frame."
  :group 'ept
  :type 'string)

(defcustom ept-retrieve-url-function nil
  "A function to retrieve specified URL. It should return content-type string."
  :group 'ept
  :type 'function)

(defcustom ept-play-mode-hook nil
  "A hook called after elpoint play mode."
  :group 'ept
  :type 'hook)

(defcustom ept-play-vertical-centering-font-regexp
  "iso8859\\|gb2312\\|jisx0208\\|jisx0212\\|ksc5601\\|cns11643\\|big5"
  "A regexp for `vertical-centering-font-regexp'."
  :group 'ept
  :type 'regexp)

(defconst ept-image-type-alist
  '(("image/jpeg" . jpeg)
    ("image/gif" . gif)
    ("image/png" . png)
    ("image/x-xbm" . xbm)
    ("image/x-xpm" . xpm))
  "An alist of CONTENT-TYPE and IMAGE-TYPE.")

(defun ept-content-type (url)
  (catch 'type-detected
    (dolist (elem ept-content-type-alist "unknown")
      (if (string-match (nth 1 elem) url)
	  (throw 'type-detected (car elem))))))

(defun ept-imagick-convert-buffer (from-type to-type &rest args)
  (let ((coding-system-for-read 'binary)
	(coding-system-for-write 'binary)
	(default-process-coding-system (cons 'binary 'binary)))
    (zerop (apply 'call-process-region
		  (point-min) (point-max)
		  ept-imagick-convert-program
		  t t nil (append args (list (concat from-type ":-")
					     (concat to-type ":-")))))))

(defun ept-imagick-convert-data (data from-type to-type &rest args)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert data)
    (and (apply 'ept-imagick-convert-buffer from-type to-type args)
	 (buffer-string))))

(defun ept-title (string)
  (insert "\n   " (propertize string 'face 'ept-title-face) "\n\n")
  (ept-insert-separator)
  (insert "\n"))

(defun ept-flatten (list-of-list)
  "Flatten LIST-OF-LIST."
  (unless (null list-of-list)
    (append (if (and (car list-of-list)
		     (listp (car list-of-list)))
		(car list-of-list)
	      (list (car list-of-list)))
	    (ept-flatten (cdr list-of-list)))))

(defun ept-split-lines (string)
  (let ((start 0) parts)
    (while (string-match "\n" string start)
      (setq parts (cons (substring string start (match-beginning 0)) parts)
	    start (match-end 0)))
    (nreverse (cons (substring string start) parts))))

(defun ept-text (&rest strings)
  (apply
   'insert 
   (ept-flatten
    (mapcar
     (lambda (string)
       (let (face strs)
	 (when (consp string)
	   (setq face (cdr string)
		 string (car string)))
	 (setq strs (ept-split-lines string))
	 (if (eq (length strs) 1)
	     (list (propertize (car strs) 'face
			       (or face 'ept-body-face)))
	   (mapconcat
	    (lambda (str)
	      (propertize str 'face (or face 'ept-body-face)))
	    strs "\n\n"))))
     strings))))

(defun ept-insert-separator ()
  (let ((pos (point)))
    (insert "  " (make-string (- (/ (frame-width)
				    (string-width
				     ept-separator-string)) 3)
			      (aref ept-separator-string 0)) "\n")
    (put-text-property pos (point) 'face 'ept-separator-face)))

(defun ept-item (string)
  (let ((pos (point))
	(ept-item-level (if (eq ept-item-level 0) 1 ept-item-level)))
    (insert (make-string (* ept-item-level 3) ? )
	    (propertize (nth (% (- ept-item-level 1)
				(length ept-item-point-strings))
			     ept-item-point-strings)
			'face
			'ept-item-point-face)
	    (propertize string
			'face
			'ept-item-face)
	    "\n\n")
    (save-excursion
      (goto-char pos)
      (ept-fill-line))))

(defun ept-slide-item (string)
  (ept-slide-line  
   (concat (make-string (* ept-item-level 3) ? )
	   (propertize (nth (% (- ept-item-level 1)
			       (length ept-item-point-strings))
			    ept-item-point-strings)
		       'face
		       'ept-item-point-face)
	   (propertize string
		       'face
		       'ept-item-face)
	   "\n")))

(defun ept-slide-text (string)
  (ept-slide-line
   (let (face)
     (when (consp string)
       (setq face (cdr string)
	     string (car string)))
     (propertize string 'face
		 (or face 'ept-body-face)))))

(defun ept-slide-line (string)
  (let ((slot 10)
	beg end)
    (dotimes (i slot)
      (setq beg (point))
      (insert (concat (make-string (/ (* (frame-width) (- 10 i)) 10)
				   ? ) string))
      (setq end (point))
      (beginning-of-line)
      (sit-for 0.05)
      (delete-region beg end))
    (insert string)))

(defun ept-inline (file)
  "Expand content of the FILENAME."
  (let* ((ct (ept-content-type file)))
    (cond
     ((string-match "^image/" ct)
      (ept-insert-image file))
     (t
      (ept-text
       (with-temp-buffer
	 (insert-file-contents file)
	 (buffer-string)))))))

(defun ept-insert-image (filename &optional from-type to-type size)
  (if size
      (insert-image
       (let ((coding-system-for-read 'binary)
	     (coding-system-for-write 'binary)
	     (default-process-coding-system (cons 'binary 'binary)))
	 (with-temp-buffer
	   (set-buffer-multibyte nil)
	   (insert-file-contents (expand-file-name filename
						   default-directory))
	   (ept-imagick-convert-buffer from-type to-type "-geometry" size)
	   (create-image (buffer-string) nil t)))
       "x")
    (if (and (string-match ept-url-regexp filename)
	     ept-retrieve-url-function)
	(let (type)
	  (insert-image
	   (create-image
	    (with-temp-buffer
	      (setq type (funcall ept-retrieve-url-function filename))
	      (buffer-string))
	    (cdr (assoc type ept-image-type-alist))
	    'data)
	   "x"))
      (insert-image (create-image (expand-file-name filename
						 default-directory))
		    "x"))))

(defun ept-play-mode ()
  "Major mode for Elpoint presentation."
  (use-local-map ept-play-mode-map)
  (setq mode-line-format "")
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (make-local-variable 'vertical-centering-font-regexp)
  (setq vertical-centering-font-regexp
	ept-play-vertical-centering-font-regexp)
  (setq major-mode 'ept-play-mode
	mode-name "Elpoint-play")
  (run-hooks 'ept-play-mode-hook))

(defun ept-play-stop ()
  "Stop playing presentation."
  (interactive)
  (kill-buffer (current-buffer))
  (delete-frame))

(defun ept-funcall (function &rest args)
  (let* ((mname (when (string-match "-mode" (symbol-name major-mode))
		  (substring (symbol-name major-mode) 0 (match-beginning 0))))
	 (sym (intern (concat "ept-" mname "-"
			      (symbol-name function)))))
    (unless (eq major-mode 'ept-mode)
      (require (intern (concat "ept-" mname))))
    (if (fboundp sym)
	(apply sym args)
      (error "%s is not implemented" sym))))

(defun ept-number ()
  (ept-funcall 'number))

(defun ept-play-buffer (&optional from-current)
  "Play current buffer content as an Elpoint presentation.
If optional FROM-CURRENT is non-nil, start from the page under the cursor."
  (interactive "P")
  (ept-funcall 'play-buffer (if from-current (ept-number) 0)))

(defun ept-dump-buffer (&optional dir)
  "Play current buffer content as an Elpoint presentation and dump it to DIR."
  (interactive "DOutput Directory: ")
  (ept-funcall 'play-buffer 0 dir))

(defun ept-play-file (&optional file)
  (interactive "fFile: ")
  (find-file file)
  (let ((buffer (current-buffer)))
    (unwind-protect
	(ept-play-buffer)
      (kill-buffer buffer))))

(defalias 'ept-exec-file 'ept-play-file)
(defalias 'ept-exec-buffer 'ept-play-buffer)

(defun ept-dump-file (&optional file)
  (interactive "fFile: ")
  (find-file file)
  (let ((buffer (current-buffer)))
    (unwind-protect
	(ept-dump-buffer)
      (kill-buffer buffer))))

(defun ept-play (presentation start-page &optional dump)
  "Play the PRESANTATION from START-PAGE.
If optional DUMP is non-nil, play as DUMP mode."
  (unless (font-info (face-font 'ept-basic-face))
    (error "You must setup at least `ept-basic-face' font"))
  (let ((dir (when (buffer-file-name (current-buffer))
	       (file-name-directory (buffer-file-name (current-buffer)))))
	(buffer (get-buffer-create "*elpoint*"))
	(frame (make-frame
		`((name . "elpoint")
		  (width . ,(or ept-play-frame-width
				(/ (display-pixel-width)
				   (aref (font-info
					  (face-font 
					   'ept-basic-face
					   (selected-frame)))
					 2))))
		  (height . ,(or ept-play-frame-height
				 (/ (display-pixel-height)
				    (aref (font-info
					   (face-font
					    'ept-basic-face
					    (selected-frame)))
					  3))))
		  (font . ,(face-font 'ept-basic-face)))))
	bgcolor)
    (let ((ept-play-frame-parameters
	   (append (list (cons 'foreground-color
 			       ept-play-frame-foreground-color)
 			 (cons 'background-color
 			       ept-play-frame-background-color))
 		   ept-play-frame-parameters)))
      (modify-frame-parameters frame ept-play-frame-parameters))
    (select-frame frame)
    (with-current-buffer buffer
      (ept-play-mode)
      (setq ept-current-presentation presentation
	    default-directory dir)
      (setq ept-dump-page dump))
    (setq bgcolor (cdr (assq 'background-color (frame-parameters))))
    (set-face-attribute 'fringe (selected-frame) :background bgcolor)
    (set-face-attribute 'modeline (selected-frame) :background bgcolor)
    (set-face-attribute 'modeline (selected-frame) :box nil)
    (switch-to-buffer buffer)
    (with-current-buffer buffer
      (let (buffer-read-only)
	(setq ept-current-position
	      (if start-page
		  (let ((page-count 0)
			(point-count 0)
			(presen ept-current-presentation))
		    (while (and presen
				(< page-count start-page))
		      (when (eq (car (car presen)) 'ept-page)
			(incf page-count))
		      (incf point-count)
		      (setq presen (cdr presen)))
		    (max (- point-count 1) 0))
		0))
	(eval (nth ept-current-position ept-current-presentation))))))

(defun ept-play-forward (&optional no-dump)
  "Play forward.
If optional NO-DUMP is non-nil and the presantation is DUMP mode,
don't dump current page."
  (interactive "P")
  (let (buffer-read-only end)
    (when ept-dump-page
       (let ((ept-dump-directory ept-dump-page))
	(unless no-dump
	  (sit-for 1)
	  (ept-dump-page))
	(when (eq ept-current-position (max (- 
					     (length ept-current-presentation)
					     1)
					    0))
	  (ept-dump-convert)
	  (ept-make-dump-page)
	  (ept-play-stop)
	  (setq end t))))
    (when (and (not end)
	       (< (+ 1 ept-current-position)
		  (length ept-current-presentation)))
      (incf ept-current-position)
      (while (not (string-match "^ept-"
				(symbol-name
				 (car (nth ept-current-position
					   ept-current-presentation)))))
	(eval (nth ept-current-position ept-current-presentation))
	(incf ept-current-position))
      (eval (nth ept-current-position ept-current-presentation)))))
    

(defun ept-play-backward ()
  "Play backward."
  (interactive)
  (let (buffer-read-only)
    (when (>= (- ept-current-position 1) 0)
      (setq ept-current-position (- ept-current-position 1))
      (while (eq (car (nth ept-current-position ept-current-presentation))
		 'ept-pause)
	(setq ept-current-position (- ept-current-position 1)))
      (eval (nth ept-current-position ept-current-presentation)))))

(provide 'ept)

;;; ept.el ends here
