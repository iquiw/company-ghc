#! /bin/sh

/bin/cat <<EOF
Prelude abs :: Num a => a -> a
Prelude acosh :: Floating a => a -> a
Prelude all :: (a -> Bool) -> [a] -> Bool
Data.List all :: (a -> Bool) -> [a] -> Bool
Prelude and :: [Bool] -> Bool
package accelerate
System.IO appendFile :: FilePath -> String -> IO ()
Data.Monoid newtype All
Network.Socket accept :: Socket -> IO (Socket, SockAddr)
Control.Applicative class Applicative f => Alternative f
Control.Exception data ArithException
Control.Applicative module Control.Applicative
Test.QuickCheck.Exception type AnException = SomeException
Network.Socket AF_ARP :: Family
keyword case
Data.Array accumArray :: Ix i => (e -> a -> e) -> e -> (i, i) -> [(i, a)] -> Array i e
EOF
