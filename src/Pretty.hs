module Pretty where

import Text.PrettyPrint ( Doc, char, colon, empty, int, nest, parens, space, text, ($$) )

import Ast ( Program(Program)
           , Function(Function)
           , Type(Int)
           , Statement(Return)
           , Expression(Int32)
           )

class PrettyPrint a where
    prettyPrint :: a -> Doc

instance PrettyPrint Program where
    prettyPrint (Program function) = prettyPrint function

instance PrettyPrint Function where
    prettyPrint (Function returnType identifier arguments body) =
      text "FUN" <> space <> prettyPrint returnType <> space <> text identifier <> colon $$
      nest 4 (
          (text "params" <> colon <> space <> parens empty) $$
          (text "body" <> colon $$ nest 4 (prettyPrint body))
      )

instance PrettyPrint Type where
    prettyPrint Int = text "INT"

instance PrettyPrint Statement where
    prettyPrint (Return expression) = text "RETURN" <> space <> prettyPrint expression

instance PrettyPrint Expression where
    prettyPrint (Int32 num) = text "INT32" <> char '<' <> text (show num) <> char '>'
