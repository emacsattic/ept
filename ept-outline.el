;;; ept-outline.el -- outline processor for Elpoint.

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
(require 'ept)
(require 'outline)

(defcustom ept-outline-process-item-alist
  '(((string-match "^ *<\\([^>]+\\)> *$" string)
     .
     (list 'ept-align-center
	   (list 'ept-inline
		 (substring string (match-beginning 1)
			    (match-end 1)))
	   (list 'ept-text "\n")))
    ((string-match "^ *(" string)
     .
     (read string)))
  "An alist of process outline item string.
Each cons cell is a form like:
\(CONDITION . ACTION)
A symbol `string' is bound while evaluation."
  :group 'ept
  :type '(repeat (cons (sexp :tag "Condition")
		       (sexp :tag "Action"))))

(defun ept-outline-heading-string (string)
  (save-match-data
    (if (string-match (concat outline-regexp "[ \t]*") string)
	(substring string (match-end 0))
      string)))

(defun ept-outline-text-string (string)
  (if (string-match "^\n*" string)
      (substring string (match-end 0))
    string))

(defun ept-outline-process-item (string level)
  (or (catch 'done
	(dolist (pair ept-outline-process-item-alist)
	  (when (eval (car pair))
	    (throw 'done (eval (cdr pair))))))
      (let ((item string))
	(setq item (list 'ept-item item))
	(dotimes (i (1- level))
	  (setq item (list 'ept-itemize item)))
	item)))

(defun ept-outline-number ()
  0)

(defun ept-outline-play-buffer (start-page &optional dump)
  (let (text level start pend end presentation page item inline)
    (goto-char (point-min))
    ;; Cover page.
    (outline-next-heading)
    (setq presentation
	  `((ept-page
	     (ept-align-center
	      (ept-text "\n\n\n\n\n" ; FIXME
			'(,(buffer-substring (point-min) (point)) .
			  ept-title-face)
			"\n\n\n" ; FIXME
			,user-full-name
			" "
			,user-mail-address)))))
    (while (not (eobp))
      (setq start (point))
      (setq level (funcall outline-level))
      (save-excursion (outline-end-of-heading)
		      (setq end (point)))
      (setq text
	    (ept-outline-heading-string
	     (buffer-substring start end)))
      (save-excursion
	(outline-forward-same-level 1)
	(setq pend (point)))
      (if (string= text "")
	  (goto-char pend)
	(setq page (list (list 'ept-title text)))
	(setq page
	      (nconc page
		     (list
		      (list 'ept-text
			    (ept-outline-text-string
			     (buffer-substring end
					       (save-excursion 
						 (outline-next-heading)
						 (point))))))))
	(outline-next-heading)
	(while (/= (point) pend)
	  (save-excursion
	    (outline-next-heading)
	    (setq end (point)))
	  (setq start (point))
	  (setq level (funcall outline-level))
	  (outline-end-of-heading)
	  (setq item (ept-outline-heading-string
		      (buffer-substring start (point))))
	  (setq item (ept-outline-process-item item level))
	  (setq page (nconc page (list item)))
	  (setq page
		(nconc page
		       (list
			(list 'ept-text
			      (ept-outline-text-string
			       (buffer-substring (point)
						 (save-excursion
						   (outline-next-heading)
						   (point))))))))
	  (goto-char end))
	(setq presentation (cons
			    (nconc (list 'ept-page) page) presentation))))
    (ept-play (nreverse presentation) start-page dump)))

(provide 'ept-outline)

;;; ept-outline.el ends here
