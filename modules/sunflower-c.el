;;; sunflower-c.el --- Emacs Sunflower: cc-mode configuration.
;;
;; Copyright @ 2017 Dmytro Koval
;;
;; Author: Dmytro Koval <dawidof@mail.ru>
;; URL: https://github.com/dawidof/sunflower
;; Version: 0.0.1
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for cc-mode and the modes derived from it.

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

(defun sunflower-c-mode-common-defaults ()
  (setq c-default-style "k&r"
        c-basic-offset 4)
  (c-set-offset 'substatement-open 0))

(setq sunflower-c-mode-common-hook 'sunflower-c-mode-common-defaults)

;; this will affect all modes derived from cc-mode, like
;; java-mode, php-mode, etc
(add-hook 'c-mode-common-hook (lambda ()
                                (run-hooks 'sunflower-c-mode-common-hook)))

(defun sunflower-makefile-mode-defaults ()
  (whitespace-toggle-options '(tabs))
  (setq indent-tabs-mode t ))

(setq sunflower-makefile-mode-hook 'sunflower-makefile-mode-defaults)

(add-hook 'makefile-mode-hook (lambda ()
                                (run-hooks 'sunflower-makefile-mode-hook)))
(provide 'sunflower-c)

;;; sunflower-c.el ends here
