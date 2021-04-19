module Assembly where

import Data.Default

data OsOption = Darwin | Other

newtype Option = Option { osOption :: OsOption }

instance Default Option where
    def = Option { osOption = Other }

class Assembly a where
    toAssembly :: Option -> a -> String

joinAssembly :: [String] -> String
joinAssembly = unlines . map (reverse . dropWhile (== '\n') . reverse)
