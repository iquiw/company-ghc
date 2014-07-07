Feature: company-ghc scan modules

  Scenario: Scan simple import
    Given the haskell buffer template
    When I replace template "IMPORT" by:
    """
    import System.IO
    """
    And I execute company-ghc-scan-modules
    # Prelude always added
    Then scanned modules are "Prelude System.IO"

    Given the haskell buffer template
    When I replace template "IMPORT" by:
    """
    import           Data.Text
    import           System.IO
    """
    And I execute company-ghc-scan-modules
    # Prelude always added
    Then scanned modules are "Prelude System.IO Data.Text"

    Given the haskell buffer template
    When I replace template "IMPORT" by ""
    And I execute company-ghc-scan-modules
    # Prelude always added
    Then scanned modules are "Prelude"

  Scenario: Scan qualified import
    Given the haskell buffer template
    When I replace template "IMPORT" by:
    """
    import qualified Data.Text
    """
    And I execute company-ghc-scan-modules
    # Prelude always added
    Then scanned modules are "Prelude Data.Text"

    Given the haskell buffer template
    When I replace template "IMPORT" by:
    """
    import qualified Data.Text as T
    import qualified Data.Text.Lazy as TL
    """
    And I execute company-ghc-scan-modules
    # Prelude always added
    Then scanned modules are "Prelude Data.Text.Lazy Data.Text"
