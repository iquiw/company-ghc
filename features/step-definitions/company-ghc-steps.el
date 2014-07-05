(When "^I execute company-ghc-prefix at current point"
      (lambda ()
        (setq company-ghc-test-prefix-output (company-ghc-prefix))))

(Then "^company-ghc prefix is\\(?: \"\\(.*\\)\"\\|:\\)$"
      (lambda (expected)
        (should (equal company-ghc-test-prefix-output expected))))

(Then "^company-ghc prefix stopped$"
      (lambda ()
        (should (eq company-ghc-test-prefix-output 'stop))))

(Then "^company-ghc prefix none$"
      (lambda ()
        (should (not company-ghc-test-prefix-output))))
