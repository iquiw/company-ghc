(add-to-list 'load-path ".")

(require 'ert)
(require 'espuds)
(require 'haskell-mode)
(require 'company-ghc)

(defvar company-ghc-test-prefix-output)

(Before
 (setq company-ghc-test-prefix-output nil)
 (switch-to-buffer
  (get-buffer-create "*company-ghc*"))
 (haskell-mode))

(After)
