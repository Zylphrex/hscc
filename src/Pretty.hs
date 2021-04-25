module Pretty where

import qualified Text.PrettyPrint

class PrettyPrint a where
    render :: a -> String
    render = Text.PrettyPrint.render . prettyPrint

    prettyPrint :: a -> Text.PrettyPrint.Doc
