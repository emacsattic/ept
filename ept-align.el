;;; ept-align.el --- Alignment utilities for Elpoint

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

;;; Code:
(defun ept-char-pixel-width ()
  "Return the pixel width of the character just after the current point."
  (let ((cw (char-width (char-after (point)))))
    ;; XXX FIXME: Assume ascii string width is half of the height.
    (* cw (/ (aref (font-info (face-font (or (get-text-property
					      (point)
					      'face)
					     'ept-basic-face)
					 (selected-frame)))
		   3)
	     2))))

(defun ept-string-image-p (string)
  (get-text-property 0 'display string))

(defun ept-string-pixel-width (string)
  "Return the pixel width of the character just after the current point."
  ;; XXX FIXME: We should calculate each character width.
  (cond
   ((ept-string-image-p string)
    (car (image-size (get-text-property 0 'display string) 'pixel)))
   (t
    (let ((sw (string-width string)))
      ;; XXX FIXME: Assume ascii string width is half of the height.
      (* sw (/ (aref (font-info (face-font (or (get-text-property
						0
						'face
						string)
					       'ept-basic-face)
					   (selected-frame)))
		     3) 2))))))

(defun ept-string-pixel-height (string)
  "Return the pixel height of the character just after the current point."
  ;; XXX FIXME: We should examine max height of the each character.
  (cond
   ((ept-string-image-p string)
    (cdr (image-size (get-text-property 0 'display string) 'pixel)))
   (t
    (aref (font-info (face-font (or (get-text-property
				     0
				     'face
				     string)
				    'ept-basic-face)
				(selected-frame)))
	  3))))

(defun ept-default-font-width ()
  (aref (font-info (face-font 'ept-basic-face) (selected-frame)) 2))

(defmacro ept-align-center (&rest body)
  `(let ((start (point)))
     ,@body
     (ept-align-center-region start (point))))

(defmacro ept-align-right (&rest body)
  `(let ((start (point)))
     ,@body
     (ept-align-right-region start (point))))
  
(defun ept-apply-region (function beg end)
  (save-restriction
    (narrow-to-region (save-excursion (goto-char beg)
				      (point-at-bol))
		      (save-excursion (goto-char end)
				      (point-at-eol)))
    (goto-char (point-min))
    (while (not (eobp))
      (funcall function)
      (forward-line 1))))

(defun ept-align-center-region (beg end)
  (interactive "r")
  (ept-apply-region 'ept-align-center-line beg end))

(defun ept-align-right-region (beg end)
  (interactive "r")
  (ept-apply-region 'ept-align-right-line beg end))

(defun ept-align-center-line ()
  (beginning-of-line)
  (when (looking-at "^ *")
    (delete-region (point) (match-end 0)))
  (let ((string (buffer-substring (point-at-bol)
				  (point-at-eol))))
    (insert (propertize " " 'display
			(list 'space ':align-to
			      (max 0 (round (- (window-width)
					       (/ (ept-string-pixel-width
						   string)
						  (ept-default-font-width)))
					    2)))))))

(defun ept-align-right-line ()
  (beginning-of-line)
  (when (looking-at "^ *")
    (delete-region (point) (match-end 0)))
  (let ((string (buffer-substring (point-at-bol)
				  (point-at-eol))))
    (insert (propertize " " 'display
			(list 'space ':align-to
			      (max 0 (- (window-width)
					(/ (ept-string-pixel-width
					    string)
					   (ept-default-font-width))
					1)))))))

(defun ept-string-pixel-width-2 (string)
  (let ((total 0)
	eol)
    (with-temp-buffer
      (insert string)
      (setq eol (point))
      (goto-char (point-min))
      (while (< (point) eol)
	(setq total (+ total (ept-char-pixel-width)))
	(forward-char 1))
      total)))

(defun ept-window-pixel-width ()
  (* (- (window-width) 1) (ept-default-font-width)))

(defun ept-fill-line ()
  (beginning-of-line)
  (let ((total 0)
	(window-pixel (ept-window-pixel-width))
	(prefix-pixel 0)
	eol)
    (when (looking-at (concat " *\\("
			      (regexp-opt
			       (symbol-value 'ept-item-point-strings)) "\\)?"))
      (setq prefix-pixel (ept-string-pixel-width-2
			  (buffer-substring (point)
					    (match-end 0)))))
    (setq eol (point-at-eol))
    (if (catch 'newline
	  (while (< (point) eol)
	    (setq total (+ total (ept-char-pixel-width)))
	    (when (>= total window-pixel)
	      (insert "\n" (make-string (/ prefix-pixel
					   (ept-default-font-width))
					? ))
	      (throw 'newline t))
	    (forward-char 1))
	  nil)
	(ept-fill-line))))

(provide 'ept-align)

;;; ept-align.el ends here
