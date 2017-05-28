;;; sunflower-custom.el --- Emacs Sunflower: Custom variables
;;
;; Copyright © 2017 Dmytro Koval;
;;
;; Author: Dmytro Koval <dawidof@mail.ru>
;; URL: https://github.com/dawidof/sunflower
;; Version: 0.0.1
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Here are the definitions of most of the functions added by Sunflower.

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


;; customize
(defgroup sunflower nil
  "Emacs Sunflower configuration."
  :prefix "sunflower-"
  :group 'convenience)


(defcustom sunflower-theme 'monokai-alt
  "The default color theme, change this in your /personal config."
  :type 'symbol
  :group 'sunflower)


(provide 'sunflower-custom)
;;; sunflower-custom.el ends here
