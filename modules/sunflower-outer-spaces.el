;;; sunflower-outer-spaces.el --- Emacs Sunflower: Outer space. Highlights redundant spaces
;;
;; Copyright © 2017 Dmytro Koval
;;
;; Version: 1.0.0
;; Keywords: convenience space highlight

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Install:

;; Put this file on your Emacs-Lisp load path, add following into your
;; ~/.emacs startup file
;;
;;     (require 'sunflower-outer-spaces)
;;
;; Enable outer-spaces-mode in any buffer to highlight spaces in that buffer.
;; Disabling minor-mode will unhighlight the highlighted spaces.
;;

;;; Commentary:

;;     You can use this package to highlight annoying empty spaces at the end of
;;     a line or the ones that appear on an empty line.
;;
;;  Overview of features:
;;
;;     o   Highlight redundant spaces in a buffer
;;

;;; Code:

;;;###autoload
(define-minor-mode outer-spaces-mode
  "Toggle outer-spaces-mode"
  :init-value nil
  :lighter " outer-spaces"
  (cond (outer-spaces-mode (progn (highlight-regexp " +$"
                                                    'outer-spaces-line-end-face)
                                  (highlight-regexp "^ +$"
                                                    'outer-spaces-line-start-face)))
        (t (progn (unhighlight-regexp " +$")
                  (unhighlight-regexp "^ +$")))))

(defface outer-spaces-line-start-face
  '((t (:background "white")))
  "Used in outer-spaces for spaces at the start of an empty line.")

(defface outer-spaces-line-end-face
  '((t (:background "red")))
  "Used in outer-spaces for spaces at the end of a line.")

(provide 'sunflower-outer-spaces)

(add-hook 'prog-mode-hook 'outer-spaces-mode)

;;; sunflower-outer-spaces.el ends here
