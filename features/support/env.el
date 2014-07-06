(add-to-list 'load-path ".")

(require 'ert)
(require 'espuds)
(require 'haskell-mode-autoloads)
(require 'company-ghc)

(defvar company-ghc-test-prefix-output)
(defvar company-ghc-test-candidates-output)

(Before
 (setq company-ghc-test-prefix-output nil)
 (setq company-ghc-test-candidates-output nil)
 (switch-to-buffer
  (get-buffer-create "*company-ghc*"))
 (haskell-mode))

(After)
