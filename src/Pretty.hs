module Pretty where

import Text.PrettyPrint ( Doc )

class PrettyPrint a where
    prettyPrint :: a -> Doc
