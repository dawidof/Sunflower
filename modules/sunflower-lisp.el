;;; sunflower-lisp.el --- Emacs Sunflower: Configuration common to all lisp modes.
;;
;; Copyright @ 2017 Dmytro Koval
;;
;; Author: Dmytro Koval <dawidof@mail.ru>
;; URL: https://github.com/dawidof/sunflower
;; Version: 0.0.1
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configuration shared between all modes related to lisp-like languages.

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

;;; Code:

(require 'sunflower-programming)
(sunflower-require-packages '(rainbow-delimiters))

;; Lisp configuration
(define-key read-expression-map (kbd "TAB") 'completion-at-point)

;; wrap keybindings
(define-key lisp-mode-shared-map (kbd "M-(") (sunflower-wrap-with "("))
;; FIXME: Pick terminal-friendly binding.
;;(define-key lisp-mode-shared-map (kbd "M-[") (sunflower-wrap-with "["))
(define-key lisp-mode-shared-map (kbd "M-\"") (sunflower-wrap-with "\""))

;; a great lisp coding hook
(defun sunflower-lisp-coding-defaults ()
  ;(smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1))

(setq sunflower-lisp-coding-hook 'sunflower-lisp-coding-defaults)

;; interactive modes don't need whitespace checks
(defun sunflower-interactive-lisp-coding-defaults ()
  ;(smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1)
  (whitespace-mode -1))

(setq sunflower-interactive-lisp-coding-hook 'sunflower-interactive-lisp-coding-defaults)

(provide 'sunflower-lisp)

;;; sunflower-lisp.el ends here
