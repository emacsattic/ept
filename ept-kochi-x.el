;;; ept-kochi-x.el --- kochi font setup for X.

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

;; This is a sample setting of kochi fonts for X with true type feature.
;; kochi font is available from http://www.on.cs.keio.ac.jp/~yasu/jp_fonts.html

;;; Commentary:
;; 

;;; History:
;; 

;;; Code:
(dolist (spec
 '("-kochi-gothic-medium-r-normal-*-100-*-*-*-*-*-fontset-gothic100,
japanese-jisx0208:-kochi-gothic-medium-r-normal-*-100-*-*-*-*-*-jisx0208.1983-0,
ascii:-kochi-gothic-medium-r-normal-*-100-*-*-*-*-*-iso8859-1"
   "-kochi-gothic-medium-r-normal-*-64-965-75-75-*-*-fontset-gothic64,
japanese-jisx0208:-kochi-gothic-medium-r-normal-*-64-*-*-*-*-*-jisx0208.1983-0,
ascii:-kochi-gothic-medium-r-normal-*-64-*-*-*-*-*-iso8859-1"
    "-kochi-gothic-bold-r-*-*-*-*-*-*-*-*-fontset-gothic54,
japanese-jisx0208:-kochi-gothic-medium-r-normal-*-54-*-*-*-*-*-jisx0208.1983-0,
ascii:-kochi-gothic-medium-r-normal-*-54-*-*-*-*-*-iso8859-1"
    "-kochi-gothic-medium-r-*-*-*-*-*-*-*-*-fontset-gothic48,
japanese-jisx0208:-kochi-gothic-medium-r-normal-*-48-*-*-*-*-*-jisx0208.1983-0,
ascii:-kochi-gothic-medium-r-normal-*-48-*-*-*-*-*-iso8859-1"
    "-kochi-gothic-medium-r-*-*-*-*-*-*-*-*-fontset-gothic32,
japanese-jisx0208:-kochi-gothic-medium-r-normal-*-32-*-*-*-*-*-jisx0208.1983-0,
ascii:-kochi-gothic-medium-r-normal-*-32-*-*-*-*-*-iso8859-1"
    "-kochi-gothic-medium-r-*-*-*-*-*-*-*-*-fontset-gothic36,
japanese-jisx0208:-kochi-gothic-medium-r-normal-*-36-*-*-*-*-*-jisx0208.1983-0,
ascii:-kochi-gothic-medium-r-normal-*-36-*-*-*-*-*-iso8859-1"
    "-kochi-gothic-medium-r-*-*-*-*-*-*-*-*-fontset-gothic24,
japanese-jisx0208:-kochi-gothic-medium-r-normal-*-24-*-*-*-*-*-jisx0208.1983-0,
ascii:-kochi-gothic-medium-r-normal-*-24-*-*-*-*-*-iso8859-1"
    "-kochi-gothic-medium-r-*-*-*-*-*-*-*-*-fontset-gothic16,
japanese-jisx0208:-kochi-gothic-medium-r-normal-*-16-*-*-*-*-*-jisx0208.1983-0,
ascii:-kochi-gothic-medium-r-normal-*-16-*-*-*-*-*-iso8859-1"    
;; add some fonts here.
  ))
  (create-fontset-from-fontset-spec spec nil t))

(provide 'ept-kochi-x)

;;; ept-kochi-x.el ends here
