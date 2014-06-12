;;; company-ghc.el --- company-mode ghc-mod backend -*- lexical-binding: t -*-

;; Copyright (C) 2014 by Iku Iwasa

;; Author:    Iku Iwasa <iku.iwasa@gmail.com>
;; URL:       http://github.com/iquiw/company-ghc
;; Version:   0.0.1
;; Package-Requires: ((cl-lib "0.5") (company "0.8.0") (ghc "4.1.1") (emacs "24"))
;; Keywords:  haskell, completion
;; Stability: unstable

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'company)
(require 'ghc)

(defgroup company-ghc nil
  "company-mode back-end for haskell-mode."
  :group 'company)

(defcustom company-ghc-show-info 'nomodule
  "Whether to show type info in minibuffer."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "Show raw output" t)
                 (const :tag "Show in oneline" oneline)
                 (const :tag "Show without module" nomodule)))

(defconst company-ghc-pragma-regexp "{-#[[:space:]]+\\([[:upper:]]+\\>\\|\\)")

(defconst company-ghc-langopt-regexp
  (concat "{-#[[:space:]\n]+\\(LANGUAGE\\|OPTIONS_GHC\\)[[:space:]\n]+"
          "\\(?:[^[:space:]]+,[[:space:]\n]*\\)*"
          "\\([^[:space:]]+\\_>\\|\\)"))

(defconst company-ghc-import-regexp
  (concat "import[[:space:]\n]+"
          "\\(?:safe[[:space:]\n]+\\)?"
          "\\(?:qualified[[:space:]\n]+\\)?"
          "\\(?:\"[^\"]+\"[[:space:]\n]+\\)?"
          "\\([[:word:].]+\\_>\\|\\)"))

(defconst company-ghc-impspec-regexp
  (concat company-ghc-import-regexp
          "\\(?:[[:space:]\n]+as[[:space:]\n]+\\w+\\)?"
          "[[:space:]\n]*\\(?:hiding[[:space:]\n]\\)*("
          "\\(?:[[:space:]\n]*[[:word:]]+[[:space:]\n]*,\\)*"
          "[[:space:]\n]*\\([[:word:]]+\\_>\\|\\)"))

(defvar company-ghc-propertized-modules '())

(defun company-ghc-prefix ()
  (let ((ppss (syntax-ppss)))
    (cond
     ((nth 3 ppss) 'stop)
     ((nth 4 ppss) (or
                    (company-grab company-ghc-pragma-regexp 1)
                    (company-grab company-ghc-langopt-regexp 2)))
     (t (or (company-grab company-ghc-import-regexp 1)
            (company-grab company-ghc-impspec-regexp 2)
            (company-grab-symbol))))))

(defun company-ghc-candidates (prefix)
  (cond
   ((company-grab company-ghc-impspec-regexp)
    (let ((mod (match-string-no-properties 1)))
      (unless (boundp (ghc-module-symbol mod))
        (message "load %s" mod)
        (ghc-load-module-buffer))
      (all-completions prefix (company-ghc-get-module-keywords mod))))
   ((company-grab company-ghc-import-regexp)
    (all-completions prefix ghc-module-names))
   ((company-grab company-ghc-pragma-regexp)
    (all-completions prefix ghc-pragma-names))
   ((company-grab company-ghc-langopt-regexp)
    (if (string-equal (match-string-no-properties 1) "LANGUAGE")
        (all-completions (match-string-no-properties 2)
                         ghc-language-extensions)
      (all-completions (match-string-no-properties 2)
                       ghc-option-flags)))
   (t (all-completions prefix
                       (if (member "dummy" company-ghc-propertized-modules)
                           ghc-merged-keyword
                         (push "dummy" company-ghc-propertized-modules)
                         (mapcar (lambda (k) (company-ghc-set-module k "dummy"))
                                 ghc-merged-keyword))))))

(defun company-ghc-meta (candidate)
  (when (company-ghc-get-module candidate)
    (let ((info (ghc-get-info candidate)))
      (pcase company-ghc-show-info
        (`t info)
        (`oneline (replace-regexp-in-string "\n" "" info))
        (`nomodule (replace-regexp-in-string
                    "\t.*" "" (replace-regexp-in-string "\n" "" info)))))))

(defun company-ghc-get-module-keywords (mod)
  (if (and (boundp (ghc-module-symbol mod))
           (not (member mod company-ghc-propertized-modules)))
      (progn
        (push mod company-ghc-propertized-modules)
        (mapcar (lambda (k) (company-ghc-set-module k mod))
                (ghc-module-keyword mod)))
    (ghc-module-keyword mod)))

(defun company-ghc-get-module (s)
  (get-text-property 0 'company-ghc-module s))

(defun company-ghc-set-module (s mod)
  (put-text-property 0 (length s) 'company-ghc-module mod s)
  s)

;;;###autoload
(defun company-ghc (command &optional arg &rest ignored)
  "`company-mode' completion back-end for `haskell-mode' via ghc-mod."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-ghc))
    (prefix (and (derived-mode-p 'haskell-mode)
                 (company-ghc-prefix)))
    (candidates (company-ghc-candidates arg))
    (meta (company-ghc-meta arg))
    (sorted t)))

(provide 'company-ghc)
;;; company-ghc.el ends here
