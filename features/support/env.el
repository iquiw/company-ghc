(add-to-list 'load-path ".")

(require 'ert)
(require 'espuds)
(require 'haskell-mode-autoloads)
(require 'company-ghc)

(defvar company-ghc-test-prefix-output)
(defvar company-ghc-test-candidates-output)
(defvar company-ghc-test-imported-modules-output)

(Before
 (setq company-ghc-test-prefix-output nil)
 (setq company-ghc-test-candidates-output nil)
 (setq company-ghc-test-imported-modules-output nil)
 (switch-to-buffer
  (get-buffer-create "*company-ghc*"))
 (haskell-mode))

(After)
