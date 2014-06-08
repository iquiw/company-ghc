;;; company-ghc.el --- company-mode ghc-mod backend -*- lexical-binding: t -*-

;; Copyright (C) 2014 by Iku Iwasa

;; Author:    Iku Iwasa <iku.iwasa@gmail.com>
;; URL:       http://github.com/iquiw/company-ghc
;; Version:   0.0.0
;; Package-Requires: ((cl-lib "0.5") (company "0.8.0") (ghc "4.1.1") (emacs "24.4"))
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

(defun company-ghc-candidates (arg)
  (cond
   ((company-grab company-ghc-impspec-regexp)
    (let ((mod (match-string-no-properties 1)))
      (unless (boundp (ghc-module-symbol mod))
        (ghc-load-module-buffer))
      (all-completions arg (ghc-module-keyword mod))))
   ((company-grab company-ghc-import-regexp)
    (all-completions arg ghc-module-names))
   ((company-grab company-ghc-pragma-regexp)
    (all-completions arg ghc-pragma-names))
   ((company-grab company-ghc-langopt-regexp)
    (if (string-equal (match-string-no-properties 1) "LANGUAGE")
        (all-completions (match-string-no-properties 2)
                         ghc-language-extensions)
      (all-completions (match-string-no-properties 2)
                       ghc-option-flags)))
   (t (all-completions arg ghc-merged-keyword))))

;;;###autoload
(defun company-ghc (command &optional arg &rest ignored)
  "`company-mode' completion back-end for `haskell-mode' via ghc-mod."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-ghc))
    (prefix (and (derived-mode-p 'haskell-mode)
                 (company-ghc-prefix)))
    (candidates (company-ghc-candidates arg))
    (sorted t)))

(provide 'company-ghc)
;;; company-ghc.el ends here
