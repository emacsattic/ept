;;; ept-mode.el --- Major mode for Elpoint editing.

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

(require 'ept)
(require 'derived)

(defcustom ept-mode-hook nil
  "A hook called after elpoint editing mode."
  :group 'ept
  :type 'hook)

(define-derived-mode ept-mode emacs-lisp-mode "Elpoint"
  "Elpoint editor mode derived from emacs-lisp mode."
  (run-hooks 'ept-mode-hook))

(define-key ept-mode-map "\C-c\C-e" 'ept-play-buffer)
(define-key ept-mode-map "\C-c\C-d" 'ept-dump-buffer)
(define-key ept-mode-map "\C-c\C-n" 'ept-number)

(defun ept-ept-number ()
  (interactive)
  (let ((pages 0)
	(pos (point)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^ *(ept-page" pos t)
	(incf pages)))
    (when (interactive-p)
      (message "Page %d" (setq pages (max 0 (- pages 1)))))
    pages))

(defun ept-ept-play-buffer (start-page &optional dump)
  (let ((source (current-buffer))
	work presentation)
    (with-temp-buffer
      (setq work (current-buffer))
      (with-current-buffer source
	(copy-to-buffer work (point-min) (point-max)))
      (goto-char (point-min))
      (insert "(")
      (goto-char (point-max))
      (insert ")")
      (goto-char (point-min))
      (setq presentation (read (current-buffer))))
    (ept-play presentation start-page dump)))

(provide 'ept-mode)

;;; ept-mode.el ends here
