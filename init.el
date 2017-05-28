;;; init.el --- Sunflower Emacs configuration
;;
;; Copyright (c) 2017 Dmytro Koval
;;
;; Author: Dmytro Koval <dawidof@mail.ru>
;; URL: https://github.com/dawidof/sunflower
;; Version: 0.0.1
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Easy, quick Emacs configuration.

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


(defvar current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))


(message "Sunflower loading...")

(when (version< emacs-version "24.4")
  (error "Sunflower requires at least GNU Emacs 24.4, but you're running %s" emacs-version))

;; Always load newest byte code
(setq load-prefer-newer t)


(defvar sunflower-dir (file-name-directory load-file-name)
  "The root dir of the Emacs Sunflower distribution.")
(defvar sunflower-core-dir (expand-file-name "core" sunflower-dir)
  "The home of Sunflower's core functionality.")
(defvar sunflower-modules-dir (expand-file-name  "modules" sunflower-dir)
  "This directory houses all of the built-in Sunflower modules.")
(defvar sunflower-personal-dir (expand-file-name "personal" sunflower-dir)
  "This directory is for your personal configuration.")

(defvar sunflower-savefile-dir (expand-file-name "savefile" sunflower-dir)
  "This folder stores all the automatically generated save/history-files.")
(defvar sunflower-modules-file (expand-file-name "sunflower-modules.el" sunflower-dir)
  "This files contains a list of modules that will be loaded by Sunflower.")

(unless (file-exists-p sunflower-savefile-dir)
  (make-directory sunflower-savefile-dir))

;; add Sunflower's directories to Emacs's `load-path'
(add-to-list 'load-path sunflower-core-dir)
(add-to-list 'load-path sunflower-modules-dir)


;; Garbage collector - decrease threshold to 50 MB
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold (* 50 1024 1024))

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold (* 100 1024 1024))

(message "Loading Sunflower's core...")

;; the core stuff
(require 'sunflower-packages)
(require 'sunflower-custom)  ;; Needs to be loaded before core, editor and ui
(require 'sunflower-ui)
(require 'sunflower-core)
;(require 'sunflower-mode)
(require 'sunflower-editor)
;(require 'sunflower-global-keybindings)

;; OSX specific settings
(when (eq system-type 'darwin)
  (require 'sunflower-osx))

(message "Loading Sunflower's modules...")

;; the modules
(if (file-exists-p sunflower-modules-file)
    (load sunflower-modules-file)
  (message "Missing modules file %s" sunflower-modules-file)
  (message "You can get started by copying the bundled example file"))

;; config changes made through the customize UI will be store here
(setq custom-file (expand-file-name "custom.el" sunflower-personal-dir))

;; load the personal settings (this includes `custom-file')
(when (file-exists-p sunflower-personal-dir)
  (message "Loading personal configuration files in %s..." sunflower-personal-dir)
  (mapc 'load (directory-files sunflower-personal-dir 't "^[^#\.].*el$")))

(message "Sunflower is ready. %s!" current-user)

;;; init.el ends here
