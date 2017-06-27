;;; sunflower-packages.el --- Emacs Sunflower: default package selection.
;;
;; Copyright © 2017 Dmytro Koval
;;
;; Author: Dmytro Koval <dawidof@mail.ru>
;; URL: https://github.com/dawidof/sunflower
;; Version: 0.0.1
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Takes care of the automatic installation of all the packages required by
;; Emacs Sunflower.

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
(require 'cl)
(require 'package)

;; accessing a package repo over https on Windows is a no go, so we
;; fallback to http there
(if (eq system-type 'windows-nt)
    (add-to-list 'package-archives
                 '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t))

;; load the pinned packages
(let ((sunflower-pinned-packages-file (expand-file-name "sunflower-pinned-packages.el" sunflower-dir)))
  (if (file-exists-p sunflower-pinned-packages-file)
      (load sunflower-pinned-packages-file)))

;; set package-user-dir to be relative to Sunflower install path
(setq package-user-dir (expand-file-name "elpa" sunflower-dir))

(package-initialize)

(defvar sunflower-packages
  '(ace-window
    avy
    anzu
    beacon
    browse-kill-ring
    dash
    diff-hl
    diminish
    easy-kill
    editorconfig
    epl
    expand-region
    flycheck
    gist
    git-timemachine
    gitconfig-mode
    gitignore-mode
    god-mode
    grizzl
    hlinum
    hydra
    imenu-anywhere
    ov
    projectile
    magit
    ;monokai-alt-theme
    move-text
    operate-on-number
    rainbow-delimiters
    rainbow-mode
    server
    smart-mode-line
    smartparens
    smartrep
    try
    undo-tree
    use-package
    volatile-highlights
    which-key
    zop-to-char)
  "A list of packages to ensure are installed at launch.")

(defun sunflower-packages-installed-p ()
  "Check if all packages in `sunflower-packages' are installed."
  (every #'package-installed-p sunflower-packages))

(defun sunflower-require-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package sunflower-packages)
    (add-to-list 'sunflower-packages package))
  (unless (package-installed-p package)
    (package-install package)))

(defun sunflower-require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'sunflower-require-package packages))

(define-obsolete-function-alias 'sunflower-ensure-module-deps 'sunflower-require-packages)

(defun sunflower-install-packages ()
  "Install all packages listed in `sunflower-packages'."
  (unless (sunflower-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs Sunflower is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (sunflower-require-packages sunflower-packages)))

;; run package installation
(sunflower-install-packages)

(defun sunflower-list-foreign-packages ()
  "Browse third-party packages not bundled with Sunflower.
Behaves similarly to `package-list-packages', but shows only the packages that
are installed and are not in `sunflower-packages'.  Useful for
removing unwanted packages."
  (interactive)
  (package-show-package-list
   (set-difference package-activated-list sunflower-packages)))

(defmacro sunflower-auto-install (extension package mode)
  "When file with EXTENSION is opened triggers auto-install of PACKAGE.
PACKAGE is installed only if not already present.  The file is opened in MODE."
  `(add-to-list 'auto-mode-alist
                `(,extension . (lambda ()
                                 (unless (package-installed-p ',package)
                                   (package-install ',package))
                                 (,mode)))))

(defvar sunflower-auto-install-alist
  '(("\\.clj\\'" clojure-mode clojure-mode)
    ("\\.coffee\\'" coffee-mode coffee-mode)
    ("\\.css\\'" css-mode css-mode)
    ("\\.csv\\'" csv-mode csv-mode)
    ("Cask" cask-mode cask-mode)
    ("\\.elm\\'" elm-mode elm-mode)
    ("\\.ex\\'" elixir-mode elixir-mode)
    ("\\.exs\\'" elixir-mode elixir-mode)
    ("\\.elixir\\'" elixir-mode elixir-mode)
    ("\\.erl\\'" erlang erlang-mode)
    ("\\.feature\\'" feature-mode feature-mode)
    ("\\.go\\'" go-mode go-mode)
    ("\\.groovy\\'" groovy-mode groovy-mode)
    ("\\.haml\\'" haml-mode haml-mode)
    ("\\.hs\\'" haskell-mode haskell-mode)
    ("\\.json\\'" json-mode json-mode)
    ("\\.latex\\'" auctex LaTeX-mode)
    ("\\.less\\'" less-css-mode less-css-mode)
    ("\\.lua\\'" lua-mode lua-mode)
    ("\\.markdown\\'" markdown-mode markdown-mode)
    ("\\.md\\'" markdown-mode markdown-mode)
    ("\\.pp\\'" puppet-mode puppet-mode)
    ("\\.php\\'" php-mode php-mode)
    ("\\.proto\\'" protobuf-mode protobuf-mode)
    ("\\.rs\\'" rust-mode rust-mode)
    ("\\.sass\\'" sass-mode sass-mode)
    ("\\.scala\\'" scala-mode scala-mode)
    ("\\.scss\\'" scss-mode scss-mode)
    ("\\.slim\\'" slim-mode slim-mode)
    ("\\.styl\\'" stylus-mode stylus-mode)
    ("\\.textile\\'" textile-mode textile-mode)
    ("\\.yml\\'" yaml-mode yaml-mode)
    ("\\.yaml\\'" yaml-mode yaml-mode)
    ("Dockerfile\\'" dockerfile-mode dockerfile-mode)))

;; markdown-mode doesn't have autoloads for the auto-mode-alist
;; so we add them manually if it's already installed
(when (package-installed-p 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)))

;; build auto-install mappings
(mapc
 (lambda (entry)
   (let ((extension (car entry))
         (package (cadr entry))
         (mode (cadr (cdr entry))))
     (unless (package-installed-p package)
       (sunflower-auto-install extension package mode))))
 sunflower-auto-install-alist)


(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(provide 'sunflower-packages)
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; sunflower-packages.el ends here
