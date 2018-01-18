{-|
Module      : Logic.Judge.Formula.Parser
Description : Parser for formulas.
Copyright   : (c) 2017 ns@slak.ws
License     : GPL-3
Maintainer  : ns@slak.ws
Stability   : experimental

Attoparsec-based parser for various logical (sub)structures.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
module Logic.Judge.Formula.Parser
    (
    -- * Parser typeclass
      Parseable(..)
    , parse
    -- * Formula parsers
    , formula
    , modality
    , justification
    , quantifier
    -- * Auxiliary parsers
    , named
    , marked
    , identifier
    , boolean
    , comments
    -- * Generic parser building
    , Operator
    , expression
    , ambiguity
    ) where

import Prelude hiding (length)
import "base" Data.List (tails, sortBy, groupBy)
import "base" Data.Function (on)
import "base" Data.Maybe (catMaybes, listToMaybe)
import "base" Data.Char (isAlphaNum, isUpper)
import "base" Control.Applicative ((<|>), (<*), (*>), liftA2)
import "text" Data.Text (Text, pack, unpack, empty, length)
import "attoparsec" Data.Attoparsec.Combinator ((<?>))
import qualified "attoparsec" Data.Attoparsec.Combinator as P (lookAhead)
import qualified "attoparsec" Data.Attoparsec.Text as P

import qualified Logic.Judge.Formula.Datastructure as F

--------------------------------------------------------------------------------
-- * Parser typeclass

-- | A Parseable is something with an associated Attoparsec 'P.Parser'.
class Parseable a where

    -- | A parser for type @a@.
    parser :: P.Parser a

    -- | In some cases, the parser for a type must be embellished with some
    -- other symbols when it occurs as part of a parser of a different type,
    -- but not when it occurs on its own. This parser allows us to specify this
    -- alternative.
    parserEmbedded :: P.Parser a
    parserEmbedded = parser


instance Parseable f => Parseable [f] where
    parser = comments *> P.many1 (parser <* comments)


instance Parseable F.Classical where
    parser = fail "proposition has no extension"


instance Parseable F.Modality where
    parser = modality


instance Parseable F.Quantifier where
    parser = quantifier


instance Parseable F.Justification where
    parser = justification
    parserEmbedded = parser <* (spaced $ P.char ':')


instance Parseable e => Parseable (F.Formula e) where
    parser = formula parserEmbedded


instance Parseable f => Parseable (F.Marked f) where
    parser = marked parser


instance Parseable e => Parseable (F.Ambiguous (F.Term e)) where
    -- Ambiguities encountered when parsing a 'F.Term' can only be resolved
    -- when the context is known, that is, when we know where the term will be
    -- used. Therefore, the ambiguity will have to be retained during parsing.
    parser = F.Ambiguous <$> ambiguity 
        [ F.Formula <$> parser
        , F.Extension <$> parser 
        , F.MarkedFormula <$> parser ]


-- | Read a text into a parseable structure.
parse :: (Monad m, Parseable a) => Text -> m a
parse = either fail return . P.parseOnly (parser <* P.endOfInput)


-------------------------------------------------------------------------------
-- * Generic parser builders

-- | Auxiliary: Compose a parser of a list of functions into a single function.
compose, composeReverse :: P.Parser [a -> a] -> P.Parser (a -> a)
compose        = fmap $ foldr       (.)  id
composeReverse = fmap $ foldl (flip (.)) id


-- | Operators wrap a parser for a function in additional information. Note
-- that the function they wrap must take arguments of the same type.
data Operator a = Infix Associativity (P.Parser (a -> a -> a))
                | Prefix (P.Parser (a -> a))
                | Postfix (P.Parser (a -> a))
data Associativity = L | R

-- | Build a parser for a recursive expression with prefix-, infix- and postfix
-- operators. Note: To avoid ambiguous left/right associative operators, don't 
-- put multiple operators of different associative direction into one
-- precedence bucket.
expression :: [[ Operator a ]] ->
              P.Parser a         
           -> P.Parser a
expression buckets base = spaced $ foldl buildUpon (spaced base) buckets' where

    -- Associate all looser, 'pending' operators with each bucket 
    buckets' = zip buckets (map concat . tail . tails $ buckets)

    -- Build operators of looser precedence upon existing parser
    buildUpon tighterParser (current, pending) =
        let infixL         = spaced $ P.choice [ p | Infix L p <- current ]
            infixR         = spaced $ P.choice [ p | Infix R p <- current ]
            postfix        = spaced $ P.choice [ p | Postfix p <- current ]
            prefix         = spaced $ P.choice [ p | Prefix  p <- current ]
            prefixLooser   = spaced $ P.choice [ p | Prefix  p <- pending ]

            -- Looser prefix operators are considered in @loosePrefix@ at this 
            -- level already, because we want to allow situations in which such
            -- an operator occurs immediately after a tighter one. After all,
            -- such occurrences are unambiguous whether they are in a sequence
            -- of prefixes or occurring in an infix expression. To illustrate,
            -- consider consider infix operators ⊙ₚ and prefix operators ⊡ₚ
            -- with precedence p (lower is tighter). The following readings 
            -- are the only reasonable ones:
            --  'a ⊙₂ ⊡₃ b ⊙₁ c'  →  'a ⊙₂ (⊡₃ (b ⊙₁ c))'
            --  'a ⊙₁ ⊡₂ b ⊙₃ c'  →  '(a ⊙₁ (⊡₂ b)) ⊙₃ c'
            
            postfixes      = composeReverse $ P.many' postfix
            prefixesLooser = compose $ P.many' prefixLooser
            prefixes       = compose $ (:) 
                                       <$> prefix
                                       <*> P.many' (prefixLooser <|> prefix)
                                       <|> return []

            continueL x = do f <- infixL
                             g <- prefixesLooser
                             y <- tighterParser
                             continueL (x `f` g y) <|> return (x `f` g y)
                                  
            continueR x = do f  <- infixR
                             g  <- prefixesLooser
                             y' <- tighterParser
                             y  <- continueR y' <|> return y'
                             return $ x `f` g y
           
        in do f <- prefixes
              x <- tighterParser
              g <- postfixes
              let y = g (f x) in continueL y <|> continueR y <|> return y



-- | Given a number of parsers that introduce an ambiguity (e.g. parsers that
-- may succeed on precisely the same text), collects the results of all
-- successful parses, provided that at least one succceeds.
ambiguity :: [P.Parser a] -> P.Parser [a]
ambiguity options = do
    (n, results) <- longestParses =<< consider options
    P.take n
    return results
    where 
    
    -- | Try to apply a parser without actually consuming input. This will
    -- tell us whether the parse would succeed, what text it would consume
    -- and what its result would be.
    whatIf :: P.Parser a -> P.Parser (Maybe (Text, a))
    whatIf p = P.option Nothing (Just <$> P.lookAhead (P.match p))

    -- | Consider what would happen if we ran the given parsers. Collect the
    -- ones that would succeed, remembering what they would consume.
    consider :: [P.Parser a] -> P.Parser [(Text, a)]
    consider = fmap catMaybes . sequence . map whatIf

    -- | There may be multiple successful parses. The longest parse is the 
    -- 'real' one; ambiguity only exists if there are multiple such parses,
    -- since it would otherwise already have been resolved by whatever suffix 
    -- the shorter parses were unable to process. This function finds the
    -- longest parses among a list of parse results produced by 'consider', and
    -- also tells us how many characters would be consumed.
    longestParses :: [(Text, a)] -> P.Parser (Int, [a])
    longestParses =
        maybe (fail "failed at ambiguity") return . 
        fmap (\xs -> (fst $ head xs, map snd xs)) .  
        listToMaybe .
        groupBy ((==) `on` fst) . 
        reverse .
        sortBy (compare `on` fst) .
        map (\(x, y) -> (length x, y))



-------------------------------------------------------------------------------
-- * Formula parsers


-- | Builds a parser for formulas of classical propositional logic extended
-- with some type @e@.
formula :: P.Parser ext -> P.Parser (F.Formula ext)
formula extension = expression operators base 

    where
    operators =  
        [   [ Prefix  (oneOf  ['~','¬']    >> return F.Negation) ]
        ,   [ Infix L (oneOf  ['&','∧']    >> return F.Conjunction) ]
        ,   [ Infix L (oneOf  ['|','∨']    >> return F.Disjunction) ]
        ,   [ Infix L (oneOf  ['^','⊻']    >> return F.XDisjunction) ]
        ,   [ Infix R (oneOf' ["->", "→"]  >> return F.Implication) ]
        ,   [ Infix R (oneOf' ["<-", "←"]  >> return (flip F.Implication)) ]
        ,   [ Infix R (oneOf' ["<->", "↔"] >> return F.BiImplication) ]
        ]
    
    base =  (F.Extend <$> extension <*> (optPrefixed <*> base) )
        <|> (F.Constant <$> boolean)
        <|> (F.Variable <$> identifier)
        <|> (P.char '(' *> formula extension <* P.char ')')
        <?> "formula term"

    -- Parser that parses and applies zero or more prefixes. This exists
    -- because prefixes need not be wrapped in parentheses when they occur
    -- directly after the extension operator.
    optPrefixed = 
        compose . P.many' . P.choice $ 
        [ p | bucket <- operators, Prefix p <- bucket ]


-- | Parser for modal operators of modal logic.
modality :: P.Parser F.Modality
modality =  (oneOf' ["[]","□"] >> return F.Necessary) 
        <|> (oneOf' ["<>","◇"] >> return F.Possible)


-- | Parser for quantifiers of first-order predicate logic.
quantifier :: P.Parser F.Quantifier
quantifier = quantor <*> identifier <* P.char '.' 
    where
    quantor =  (oneOf' ["\\A","∀"] >> return F.Universal) 
           <|> (oneOf' ["\\E","∃"] >> return F.Existential)


-- | Parser for justification terms of justification logic.
justification :: P.Parser F.Justification
justification = expression operators base 

    where
    operators = 
        [   [ Prefix  (oneOf  ['!']         >> return (F.ProofChecker)) ]
        ,   [ Infix L (oneOf  ['+']         >> return (F.Sum)) ]
        ,   [ Infix L (oneOf  ['*','⋅','·'] >> return (F.Application)) ]
        ]

    base =  (toAtom <$> identifier)
        <|> (P.char '(' *> justification <* P.char ')')
        <?> "justification term"

    toAtom s | isUpper (head s) = F.ProofVariable s
             | otherwise        = F.ProofConstant s



-------------------------------------------------------------------------------
-- * Auxiliary parsers


-- | Auxiliary: Parser that accepts and returns any @Char@ in a given list of @Char@s.
oneOf :: [Char] -> P.Parser Char
oneOf c = P.satisfy (`elem` c)


-- | Auxiliary: Parser that accepts and returns any string in a given list of strings.
oneOf' :: [String] -> P.Parser String
oneOf' s = fmap unpack $ P.choice (map (P.string . pack) s)


-- | Auxiliary: Skip surrounding spaces.
spaced :: P.Parser a -> P.Parser a
spaced p = P.skipSpace *> p <* P.skipSpace


-- | Parser that accepts and returns any string that starts with a letter.
identifier :: P.Parser String
identifier = liftA2 (:) P.letter (unpack <$> P.takeWhile (\x -> isAlphaNum x || x == '\''))
    <?> "identifier"


-- | Parser that accepts a boolean (as binary number or unicode ⊥, ⊤).
boolean :: P.Parser Bool
boolean = (oneOf "⊥0" >> return False) <|> (oneOf "⊤1" >> return True)
    <?> "boolean"


-- | Make a parser for something that is named by prepending it with an 
-- identifier and a = sign.
named :: P.Parser x -> P.Parser (String, x)
named p = (,) 
    <$> (identifier <* (spaced $ P.char '=')) 
    <*> p


-- | Parser for a marked formula.
marked :: P.Parser formula -> P.Parser (F.Marked formula)
marked p = F.Marked <$> P.option [] marks <*> p where 
    marks = (spaced $ P.char '[') 
            *> P.sepBy' identifier (spaced $ P.char ',') <* 
            (spaced $ P.char ']')


-- | Parser for comments.
comments :: P.Parser ()
comments = 
    P.skipSpace *> P.skipMany (
        P.char '#' *> P.manyTill P.anyChar P.endOfLine <* P.skipSpace
    )
