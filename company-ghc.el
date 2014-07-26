;;; company-ghc.el --- company-mode ghc-mod backend -*- lexical-binding: t -*-

;; Copyright (C) 2014 by Iku Iwasa

;; Author:    Iku Iwasa <iku.iwasa@gmail.com>
;; URL:       https://github.com/iquiw/company-ghc
;; Version:   0.0.7
;; Package-Requires: ((cl-lib "0.5") (company "0.8.0") (ghc "4.1.1") (emacs "24"))
;; Keywords:  haskell, completion
;; Stability: experimental

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
  "Provide completion prefix at the current point."
  (let ((ppss (syntax-ppss)))
    (cond
     ((nth 3 ppss) 'stop)
     ((nth 4 ppss) (or
                    (company-grab company-ghc-pragma-regexp 1)
                    (company-grab company-ghc-langopt-regexp 2)))
     ((looking-back "^[^[:space:]]*") nil)
     (t (company-grab-symbol)))))

(defun company-ghc-candidates (prefix)
  "Provide completion candidates for the given PREFIX."
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
                      (all-completions
                       prefix (company-ghc-get-module-keywords mod)))
                    (mapcar 'car company-ghc-imported-modules)))
            'string<))))

(defun company-ghc-meta (candidate)
  "Show type info for the given CANDIDATE."
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

(defun company-ghc-doc-buffer (candidate)
  "Display documentation in the docbuffer for the given CANDIDATE."
  (with-temp-buffer
    (erase-buffer)
    (let ((hoogle (if (boundp 'haskell-hoogle-command)
                      haskell-hoogle-command
                    "hoogle"))
          (mod (company-ghc-get-module candidate)))
      (call-process hoogle nil t nil "search" "--info"
                    (if mod (concat mod "." candidate) candidate)))
    (company-doc-buffer
     (buffer-substring-no-properties (point-min) (point-max)))))

(defun company-ghc-annotation (candidate)
  "Show module name as annotation where the given CANDIDATE is defined."
  (when company-ghc-show-module
    (concat " " (company-ghc-get-module candidate))))

(defun company-ghc-get-module-keywords (mod)
  "Get defined keywords in the specified module MOD."
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
  "Get module name from the keyword S."
  (get-text-property 0 'company-ghc-module s))

(defun company-ghc-set-module (s mod)
  "Set module name of the keywork S to the module MOD."
  (put-text-property 0 (length s) 'company-ghc-module mod s)
  s)

(defun company-ghc-scan-modules ()
  "Scan imported modules in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (mod (mod-alist '(("Prelude"))))
      (while (setq mod (company-ghc--scan-impspec))
        (when (consp mod)
          (setq mod-alist
                (cons
                 mod
                 (if (and (assoc-string (car mod) mod-alist) (cdr mod))
                     (delete (assoc-string (car mod) mod-alist) mod-alist)
                   mod-alist)))))
      (setq company-ghc-imported-modules mod-alist))))

(defun company-ghc--scan-impspec ()
  "Scan one import spec and return module alias cons.
If proper import spec is not found, return boolean value whether import spec
continues or not."
  (let* ((beg (company-ghc--search-import-start))
         (end (and beg (company-ghc--search-import-end (cdr beg)))))
    (when end
      (save-restriction
        (narrow-to-region (car beg) (car end))
        (goto-char (point-min))
        (let (chunk prev-chunk attrs mod)
          (while (setq chunk (company-ghc--next-import-chunk))
            (cond
             ((string= chunk "qualified") (push 'qualified attrs))
             ((string= chunk "safe") (push 'safe attrs))
             ((let ((case-fold-search nil)) (string-match-p "^[A-Z]" chunk))
              (cond
               ((not mod) (setq mod (if (memq 'qualified attrs)
                                        (cons chunk chunk)
                                      (cons chunk nil))))
               ((string= prev-chunk "as") (setcdr mod chunk)))))
            (setq prev-chunk chunk))
          (or mod
              (string= (cdr end) "import")))))))

(defun company-ghc--search-import-start ()
  "Search start of import spec and return the point after import and offset."
  (catch 'result
    (while (re-search-forward "^\\([[:space:]]*\\)import\\>" nil t)
      (unless (nth 4 (syntax-ppss))
        (throw 'result
               (cons (match-end 0)
                     (string-width (match-string-no-properties 1))))))))

(defun company-ghc--search-import-end (offset)
  "Search end of import spec and return the end point and next token."
  (forward-line)
  (catch 'result
    (let ((p (point)))
      (while (not (eobp))
        (cond
         ((company-ghc--in-comment-p) nil)
         ((looking-at "^[[:space:]]*$") nil)
         ((looking-at "^#") nil)
         ((not (and (looking-at "^\\([[:space:]]*\\)\\([^[:space:]\n]*\\)")
                    (< offset (string-width (match-string-no-properties 1)))))
          (throw 'result (cons p (match-string-no-properties 2)))))
        (forward-line)
        (setq p (point))))))

(defun company-ghc--next-import-chunk ()
  "Return next chunk in the current import spec."
  (catch 'result
    (while (and (skip-chars-forward " \t\n") (not (eobp)))
      (cond
       ((or (looking-at-p "{-") (looking-at-p "--"))
        (forward-comment 1))
       ((looking-at-p "(")
        (throw 'result (buffer-substring-no-properties
                        (point) (progn (forward-sexp) (point)))))
       ((looking-at-p "\"")
        (re-search-forward "\"\\([^\"]\\|\\\\\"\\)*\"")
        (throw 'result (match-string-no-properties 0)))
       (t
        (when (re-search-forward "\\=.[[:alnum:].]*\\>" nil t)
          (throw 'result (match-string-no-properties 0))))))))

(defun company-ghc--in-comment-p ()
  "Return whether the point is in comment or not."
  (let ((ppss (syntax-ppss))) (nth 4 ppss)))


;;;###autoload
(defun company-ghc (command &optional arg &rest ignored)
  "`company-mode' completion back-end for `haskell-mode' via ghc-mod.
Provide completion info according to COMMAND and ARG.  IGNORED, not used."
  (interactive (list 'interactive))
  (cl-case command
    (init (when (derived-mode-p 'haskell-mode)
            (company-ghc-scan-modules)
            (add-hook 'after-save-hook 'company-ghc-scan-modules nil t)))
    (interactive (company-begin-backend 'company-ghc))
    (prefix (and (derived-mode-p 'haskell-mode)
                 (company-ghc-prefix)))
    (candidates (company-ghc-candidates arg))
    (meta (company-ghc-meta arg))
    (doc-buffer (company-ghc-doc-buffer arg))
    (annotation (company-ghc-annotation arg))
    (sorted t)))

(provide 'company-ghc)
;;; company-ghc.el ends here
