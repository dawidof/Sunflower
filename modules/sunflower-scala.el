;;; sunflower-scala.el --- Emacs Sunflower: scala-mode configuration.
;;
;; Copyright @ 2017 Dmytro Koval
;;
;; Author: Dmytro Koval <dawidof@mail.ru>
;; URL: https://github.com/dawidof/sunflower
;; Version: 0.0.1
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic support for the Scala programming language

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
(sunflower-require-packages '(scala-mode ensime))

(defun sunflower-scala-mode-defaults ()
  (subword-mode +1)
  (ensime-mode +1))

(setq sunflower-scala-mode-hook 'sunflower-scala-mode-defaults)

(add-hook 'scala-mode-hook (lambda ()
                             (run-hooks 'sunflower-scala-mode-hook)))
(provide 'sunflower-scala)

;;; sunflower-scala.el ends here
