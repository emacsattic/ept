;;; ept-setup.el --- Sample setup for Elpoint.

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

;; font setup.
(defvar ept-use-kochi t
  "Use kochi font as a true type font.")

(cond
 ((and ept-use-kochi
       (eq window-system 'x))
  (require 'ept-kochi-x))
 ;; Another setting is not implemented yet.
 )

(setq ept-basic-face-font (if (query-fontset "fontset-gothic16")
			      "fontset-gothic16"
			    (face-font 'default)))

;; URL retrieval setup.
(cond
 ((condition-case nil
      (require 'w3m)
    (error))
  ;; w3m is installed.
  (require 'ept-w3m)
  (setq ept-retrieve-url-function 'ept-retrieve-url-with-w3m))
 ;; Another setting is not implemented yet.
 )

;; special character setup.
(setq ept-separator-string "━")
(setq ept-item-point-strings '("・" " - " " = "))

;; play-frame setup.
(setq ept-play-frame-parameters
      `((tool-bar-lines . 0)
	(menu-bar-lines . 0)
	(left-fringe . 0)  ; For Emacs 21.3
	(right-fringe . 0)
	(line-space . 0)
	(top . 0)
	(left . 0)
	(vertical-scroll-bars . nil)
	(cursor-type . nil)
	(minibuffer . nil)))

;; font-lock setup.
(eval-after-load "font-lock"
  '(font-lock-add-keywords
    'ept-mode
    '(("(\\(ept-page\\)"
       (1 font-lock-function-name-face))
      ("(\\(ept-title\\)"
       (1 font-lock-type-face))
      ("(\\(ept-[a-z-]*\\)"
       (1 font-lock-keyword-face)))))

;; autoload setup.
(autoload 'ept-play-file   "ept" nil t)
(autoload 'ept-play-buffer "ept" nil t)
(autoload 'ept-dump-file   "ept" nil t)
(autoload 'ept-dump-buffer "ept" nil t)
(autoload 'ept-mode        "ept-mode" nil t)

;; auto-mode setup.
(setq auto-mode-alist
      (append '(("\\.ept$" . ept-mode))
	      auto-mode-alist))

(provide 'ept-setup)

;;; ept-setup.el ends here
