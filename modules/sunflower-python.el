;;; sunflower-python.el --- Emacs Sunflower: python.el configuration.
;;
;; Copyright @ 2017 Dmytro Koval
;;
;; Author: Dmytro Koval <dawidof@mail.ru>
;; URL: https://github.com/dawidof/sunflower
;; Version: 0.0.1
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for python.el (the latest and greatest
;; Python mode Emacs has to offer).

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

(sunflower-require-package 'anaconda-mode)

(when (boundp 'company-backends)
  (sunflower-require-package 'company-anaconda)
  (add-to-list 'company-backends 'company-anaconda))

(require 'electric)
(require 'sunflower-programming)

;; Copy pasted from ruby-mode.el
(defun sunflower-python--encoding-comment-required-p ()
  (re-search-forward "[^\0-\177]" nil t))

(defun sunflower-python--detect-encoding ()
  (let ((coding-system
         (or save-buffer-coding-system
             buffer-file-coding-system)))
    (if coding-system
        (symbol-name
         (or (coding-system-get coding-system 'mime-charset)
             (coding-system-change-eol-conversion coding-system nil)))
      "ascii-8bit")))

(defun sunflower-python--insert-coding-comment (encoding)
  (let ((newlines (if (looking-at "^\\s *$") "\n" "\n\n")))
    (insert (format "# coding: %s" encoding) newlines)))

(defun sunflower-python-mode-set-encoding ()
  "Insert a magic comment header with the proper encoding if necessary."
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (sunflower-python--encoding-comment-required-p)
      (goto-char (point-min))
      (let ((coding-system (sunflower-python--detect-encoding)))
        (when coding-system
          (if (looking-at "^#!") (beginning-of-line 2))
          (cond ((looking-at "\\s *#\\s *.*\\(en\\)?coding\\s *:\\s *\\([-a-z0-9_]*\\)")
                 ;; update existing encoding comment if necessary
                 (unless (string= (match-string 2) coding-system)
                   (goto-char (match-beginning 2))
                   (delete-region (point) (match-end 2))
                   (insert coding-system)))
                ((looking-at "\\s *#.*coding\\s *[:=]"))
                (t (sunflower-python--insert-coding-comment coding-system)))
          (when (buffer-modified-p)
            (basic-save-buffer-1)))))))

(when (fboundp 'exec-path-from-shell-copy-env)
  (exec-path-from-shell-copy-env "PYTHONPATH"))

(defun sunflower-python-mode-defaults ()
  "Defaults for Python programming."
  (subword-mode +1)
  (anaconda-mode 1)
  (eldoc-mode 1)
  (setq-local electric-layout-rules
              '((?: . (lambda ()
                        (and (zerop (first (syntax-ppss)))
                             (python-info-statement-starts-block-p)
                             'after)))))
  (when (fboundp #'python-imenu-create-flat-index)
    (setq-local imenu-create-index-function
                #'python-imenu-create-flat-index))
  (add-hook 'post-self-insert-hook
            #'electric-layout-post-self-insert-function nil 'local)
  (add-hook 'after-save-hook 'sunflower-python-mode-set-encoding nil 'local))

(setq sunflower-python-mode-hook 'sunflower-python-mode-defaults)

(add-hook 'python-mode-hook (lambda ()
                              (run-hooks 'sunflower-python-mode-hook)))

(provide 'sunflower-python)

;;; sunflower-python.el ends here
