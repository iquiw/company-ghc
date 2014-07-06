;;; company-ghc.el --- company-mode ghc-mod backend -*- lexical-binding: t -*-

;; Copyright (C) 2014 by Iku Iwasa

;; Author:    Iku Iwasa <iku.iwasa@gmail.com>
;; URL:       https://github.com/iquiw/company-ghc
;; Version:   0.0.4
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

;; `company-mode' back-end for `haskell-mode' via `ghc-mod'.
;;
;; Provide context sensitive completion by using information from `ghc-mod'.
;; Add `company-ghc' to `company-mode' back-ends list.
;;
;;     (add-to-list 'company-backends 'company-ghc)
;;
;; or grouped with other back-ends.
;;
;;     (add-to-list 'company-backends '(company-ghc :with company-dabbrev))

;;; Code:

(require 'cl-lib)
(require 'company)
(require 'ghc)

(defgroup company-ghc nil
  "company-mode back-end for haskell-mode."
  :group 'company)

(defcustom company-ghc-show-info 'nomodule
  "Specify how to show type info in minibuffer."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "Show raw output" t)
                 (const :tag "Show in oneline" oneline)
                 (const :tag "Show without module" nomodule)))

(defcustom company-ghc-show-module t
  "Non-nil to show module name as annotation."
  :type 'boolean)

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

(defconst company-ghc-module-regexp
  "module[[:space:]]*\\([[:word:].]+\\_>\\|\\)")

(defvar company-ghc-propertized-modules '())
(defvar company-ghc-imported-modules '())
(make-variable-buffer-local 'company-ghc-imported-modules)

(defun company-ghc-prefix ()
  (let ((ppss (syntax-ppss)))
    (cond
     ((nth 3 ppss) 'stop)
     ((nth 4 ppss) (or
                    (company-grab company-ghc-pragma-regexp 1)
                    (company-grab company-ghc-langopt-regexp 2)))
     ((looking-back "^[^[:space:]]*") nil)
     (t (company-grab-symbol)))))

(defun company-ghc-candidates (prefix)
  (cond
   ((company-grab company-ghc-impspec-regexp)
    (let ((mod (match-string-no-properties 1)))
      (all-completions prefix (company-ghc-get-module-keywords mod))))

   ((company-grab company-ghc-import-regexp)
    (all-completions (match-string-no-properties 1) ghc-module-names))

   ((company-grab company-ghc-module-regexp)
    (all-completions (match-string-no-properties 1) ghc-module-names))

   ((company-grab company-ghc-pragma-regexp)
    (all-completions prefix ghc-pragma-names))

   ((company-grab company-ghc-langopt-regexp)
    (if (string-equal (match-string-no-properties 1) "LANGUAGE")
        (all-completions (match-string-no-properties 2)
                         ghc-language-extensions)
      (all-completions (match-string-no-properties 2)
                       ghc-option-flags)))

   (t (sort (apply 'append
                   (mapcar
                    (lambda (mod)
                      (all-completions prefix (company-ghc-get-module-keywords mod)))
                    company-ghc-imported-modules))
            'string<))))

(defun company-ghc-meta (candidate)
  (let ((mod (company-ghc-get-module candidate)))
    (when mod
      (let ((info (ghc-get-info (concat mod "." candidate))))
        (pcase company-ghc-show-info
          (`t info)
          (`oneline (replace-regexp-in-string "\n" "" info))
          (`nomodule
           (when (string-match "\\(?:[^[:space:]]+\\.\\)?\\([^\t]+\\)\t" info)
             (replace-regexp-in-string
              "\n" "" (match-string-no-properties 1 info)))))))))

(defun company-ghc-annotation (candidate)
  (when company-ghc-show-module
    (concat " " (company-ghc-get-module candidate))))

(defun company-ghc-get-module-keywords (mod)
  (let ((sym (ghc-module-symbol mod)))
    (unless (boundp sym)
      (ghc-load-merge-modules (list mod)))
    (when (boundp sym)
      (if (member mod company-ghc-propertized-modules)
          (ghc-module-keyword mod)
        (push mod company-ghc-propertized-modules)
        (mapcar (lambda (k) (company-ghc-set-module k mod))
                (ghc-module-keyword mod))))))

(defun company-ghc-get-module (s)
  (get-text-property 0 'company-ghc-module s))

(defun company-ghc-set-module (s mod)
  (put-text-property 0 (length s) 'company-ghc-module mod s)
  s)

(defun company-ghc-scan-modules ()
  (when (derived-mode-p 'haskell-mode)
    ;; TODO: write own module parser
    (setq company-ghc-imported-modules
          (cons "Prelude" (ghc-gather-import-modules-buffer)))))

(add-hook 'after-save-hook 'company-ghc-scan-modules)


;;;###autoload
(defun company-ghc (command &optional arg &rest ignored)
  "`company-mode' completion back-end for `haskell-mode' via ghc-mod."
  (interactive (list 'interactive))
  (cl-case command
    (init (company-ghc-scan-modules))
    (interactive (company-begin-backend 'company-ghc))
    (prefix (and (derived-mode-p 'haskell-mode)
                 (company-ghc-prefix)))
    (candidates (company-ghc-candidates arg))
    (meta (company-ghc-meta arg))
    (annotation (company-ghc-annotation arg))
    (sorted t)))

(provide 'company-ghc)
;;; company-ghc.el ends here
