;;; sunflower-erlang.el --- Emacs Sunflower: Erlang programming support.
;;
;; Copyright @ 2017 Dmytro Koval
;;
;; Author: Dmytro Koval <dawidof@mail.ru>
;; URL: https://github.com/dawidof/sunflower
;; Version: 0.0.1
;; Keywords: convenience erlang

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Erlang is a concurrent functional language.

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
(sunflower-require-packages '(erlang))

(defcustom wrangler-path nil
  "The location of wrangler elisp directory."
  :group 'sunflower-erlang
  :type 'string
  :safe 'stringp)

(require 'projectile)

(when (require 'erlang-start nil t)

  (eval-after-load 'erlang-mode
    '(progn
       (flymake-mode)))

  (when (not (null wrangler-path))
    (add-to-list 'load-path wrangler-path)
    (require 'wrangler)))

(add-hook 'erlang-mode-hook (lambda ()
                              (setq erlang-compile-function 'projectile-compile-project)))

(provide 'sunflower-erlang)

;;; sunflower-erlang.el ends here
