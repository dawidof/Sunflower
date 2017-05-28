;;; sunflower-clojure.el --- Emacs Sunflower: Clojure programming configuration.
;;
;; Copyright @ 2017 Dmytro Koval
;;
;; Author: Dmytro Koval <dawidof@mail.ru>
;; URL: https://github.com/dawidof/sunflower
;; Version: 0.0.1
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for clojure-mode.

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

(require 'sunflower-lisp)
(sunflower-require-packages '(clojure-mode cider))

(eval-after-load 'clojure-mode
  '(progn
     (defun sunflower-clojure-mode-defaults ()
       (subword-mode +1)
       (run-hooks 'sunflower-lisp-coding-hook))

     (setq sunflower-clojure-mode-hook 'sunflower-clojure-mode-defaults)

     (add-hook 'clojure-mode-hook (lambda ()
                                    (run-hooks 'sunflower-clojure-mode-hook)))))

(eval-after-load 'cider
  '(progn
     (setq nrepl-log-messages t)

     (add-hook 'cider-mode-hook 'eldoc-mode)

     (defun sunflower-cider-repl-mode-defaults ()
       (subword-mode +1)
       (run-hooks 'sunflower-interactive-lisp-coding-hook))

     (setq sunflower-cider-repl-mode-hook 'sunflower-cider-repl-mode-defaults)

     (add-hook 'cider-repl-mode-hook (lambda ()
                                       (run-hooks 'sunflower-cider-repl-mode-hook)))))

(provide 'sunflower-clojure)

;;; sunflower-clojure.el ends here
