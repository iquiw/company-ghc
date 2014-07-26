Feature: company-ghc scan modules

  @ghc-mod
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

  @ghc-mod
  Scenario: Scan qualified import
    Given the haskell buffer template
    When I replace template "IMPORT" by:
    """
    import qualified Data.Text
    """
    And I execute company-ghc-scan-modules
    Then scanned modules are "Prelude Data.Text"

    Given the haskell buffer template
    When I replace template "IMPORT" by:
    """
    import qualified Data.Text as T
    import qualified Data.Text.Lazy as TL
    """
    And I execute company-ghc-scan-modules
    Then scanned modules are "Prelude Data.Text.Lazy Data.Text"

  @ghc-mod
  Scenario: Scan selective import
    Given the haskell buffer template
    When I replace template "IMPORT" by:
    """
    import Control.Monad ()
    import Data.Text (Text, strip)
    import Data.ByteString hiding (splitAt)
    """
    And I execute company-ghc-scan-modules
    Then scanned modules are "Prelude Data.ByteString Data.Text Control.Monad"


  Scenario: Scan safe import
    Given the haskell buffer template
    When I replace template "IMPORT" by:
    """
    import safe Control.Applicative
    import safe qualified Data.Monoid
    """
    And I execute company-ghc-scan-modules
    Then scanned modules are "Prelude Data.Monoid Control.Applicative"

  Scenario: Scan package import
    Given the haskell buffer template
    When I replace template "IMPORT" by:
    """
    import "mtl" Control.Monad.Trans
    import safe qualified "mtl" Control.Monad.State as State
    """
    And I execute company-ghc-scan-modules
    Then scanned modules are "Prelude Control.Monad.State Control.Monad.Trans"

  Scenario: Scan import with newline
    Given the haskell buffer template
    When I replace template "IMPORT" by:
    """
    import
        Data.Text
    import
        safe
        qualified
        "bytestring"
        Data.ByteString
        as
        B
        (
          readFile,
          writeFile
        )
    """
    And I execute company-ghc-scan-modules
    Then scanned modules are "Prelude Data.ByteString Data.Text"

  @ghc-mod
  Scenario: Scan incomplete import
    Given the haskell buffer template
    When I replace template "IMPORT" by:
    """
    import
    import Control.Applicative
    import
    import Control.Monad
    """
    And I execute company-ghc-scan-modules
    Then scanned modules are "Prelude Control.Monad Control.Applicative"

  Scenario: Not scan import in string,comment
    Given the haskell buffer template
    When I replace template "IMPORT" by:
    """
    {-
    import Data.Text
    -}
    "import Data.ByteString"
    """
    And I execute company-ghc-scan-modules
    Then scanned modules are "Prelude"
