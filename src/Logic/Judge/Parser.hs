-- Copyright © 2017 ns@slak.ws; see LICENSE file.
{-|
Module      : Logic.Judge.Parser
Description : Class instance and functions for parsable structures.
License     : GPL-3
Stability   : experimental
-}

{-# LANGUAGE PackageImports #-}
module Logic.Judge.Parser where

import "text" Data.Text (Text)
import qualified "attoparsec" Data.Attoparsec.Text as P


class Parseable a where

    -- | A parser for type @a@.
    parser :: P.Parser a


    -- | In some cases, the parser for a type must be embellished with some
    -- other symbols when it occurs as part of a parser of a different type,
    -- but not when it occurs on its own. This parser allows us to specify this
    -- alternative.
    parserEmbedded :: P.Parser a
    parserEmbedded = parser



-- | Apply parser to text.
parse :: (Monad m) => P.Parser a -> Text -> m a
parse p = either fail return . P.parseOnly (p <* P.endOfInput)


