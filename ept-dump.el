;;; ept-dump.el --- Dump utils for elpoint presentation.

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

(defcustom ept-dump-page-size "320x240"
  "Argument for convert's geometry."
  :type 'string
  :group 'ept)

(defvar ept-xwd-program "xwd")
(defvar ept-dump-directory "~")

(defun ept-xwd-dump-page ()
  (apply 'call-process ept-xwd-program nil nil nil
	 (list "-root" "-out"
	       (expand-file-name
		(format "ept%d.xwd" (symbol-value 'ept-current-position))
		ept-dump-directory))))

(defun ept-imagick-convert-dumped-page (number)
  (apply 'call-process (symbol-value 'ept-imagick-convert-program) nil nil nil
	 (list "-geometry" ept-dump-page-size
	       (expand-file-name
		(format "ept%d.xwd" number)
		ept-dump-directory)
	       (expand-file-name
		(format "ept%d.png" number)
		ept-dump-directory))))

(defun ept-dump-page ()
  (ept-xwd-dump-page))

(defun ept-dump-convert ()
  (interactive)
  (let ((number 0))
    (while (< number (length (symbol-value 'ept-current-presentation)))
      (message "Converting...%d" number)
      (ept-imagick-convert-dumped-page number)
      (incf number))))

(defun ept-make-dump-page ()
  (interactive)
  (let ((p 0) file)
    (with-temp-buffer
      (insert "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">
<html>
  <head>
    <title>Presentation dump by Elpoint</title>
    <LINK REV=\"MADE\" HREF=\"mailto:" user-mail-address
    "\">
    <META HTTP-EQUIV=\"CONTENT-TYPE\" CONTENT=\"text/html; CHARSET=iso-2022-jp\">
  </head>
 <body BGCOLOR=\"#D9FFD9\">
 <table>")
      (dotimes (i (length (symbol-value 'ept-current-presentation)))
	(setq file (format "ept%d.png" i))
	(when (file-exists-p (expand-file-name file ept-dump-directory))
	  (insert (format
		   (if (zerop (% p 2))
		       "<tr><td><img src=\"%s\"></td>"
		     "<td><img src=\"%s\"></td></tr>\n")
		   file)))
	(incf p))
      (insert " </table></body>\n<hr>\n\
Dump created by <i>Elpoint -- Yet Another Presentation Tool for Emacsen</i>.
</html>")
      (write-region (point-min) (point-max)
		    (expand-file-name "index.html" ept-dump-directory)))))

(provide 'ept-dump)

;;; ept-dump.el ends here
