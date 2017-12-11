-- Copyright © 2017 ns@slak.ws; see LICENSE file.
{-|
Module      : Logic.Judge.Tableau.Yaml
Description : YAML- and JSON-parsing class instances for tableau systems.
License     : GPL-3
Stability   : experimental
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PackageImports #-}
module Logic.Judge.Tableau.Yaml where

import "base" Data.List (delete)
import "base" Data.Maybe (fromMaybe)
import "base" Control.Monad (foldM)
import "base" Control.Applicative ((<|>), liftA2)
import "text" Data.Text (Text, empty, pack, unpack)
import "yaml" Data.Yaml ((.:),(.:?),(.!=))
import qualified "yaml" Data.Yaml as Y
import qualified "aeson" Data.Aeson.Types as Y (typeMismatch, withText, withObject)
import qualified "vector" Data.Vector as V
import qualified "containers" Data.Map as M1
import qualified "unordered-containers" Data.HashMap.Strict as M2


import Logic.Judge.Tableau.Specification (Ref((:=)), Guard((:|)), BaseRule((:>)))
import Logic.Judge.Parser (Parseable, parser, parse)
import qualified Logic.Judge.Formula as F
import qualified Logic.Judge.Tableau.Specification as T


instance (F.Extension ext) => Y.FromJSON (T.TableauSystem ext) where
    parseJSON = Y.withObject "tableau system" $ \o ->
        T.TableauSystem
            <$> o .:? "name" .!= "untitled"
            <*> (map namer . M2.toList <$> o .: "rules")
            <*> o .:? "assumptions" .!= mempty

        where

        -- | If the rule does not have a name, use the dictionary key as a
        -- name.
        namer :: (String, Ref String a) -> Ref String a
        namer (key, (name := value)) =
            if name == mempty
                then key := value
                else name := value


instance (Monoid a, Y.FromJSON a, Y.FromJSON b) => Y.FromJSON (Ref a b) where
    parseJSON = Y.withObject "named object" $ \o -> 
        (:=) 
            <$> o .:? "name" .!= mempty
            <*> Y.parseJSON (Y.Object o)



instance (F.Extension ext) => Y.FromJSON (T.BaseRule ext) where
    parseJSON = Y.withObject "tableau rule" $ \o ->
        (:>)
            <$> o .: "if"
            <*> o .: "then"


instance (Y.FromJSON a, F.Extension ext) => Y.FromJSON (T.Guard a (T.Constraint ext)) where
    parseJSON = Y.withObject "constrained tableau rule" $ \o ->
        (:|)
            <$> Y.parseJSON (Y.Object o) 
            <*> (
            (,)
                <$> o .:? "combine" .!= T.Nondeterministic
                <*> o .:? "where" .!= T.None
            )



instance Y.FromJSON T.ConstraintHandler where
    parseJSON = Y.withText expected $ \s -> case s of
        "nondeterministic" -> return T.Nondeterministic
        "greedy"           -> return T.Greedy
        invalid            -> Y.typeMismatch expected (Y.String invalid)

        where expected = "instance combinator"



instance Y.FromJSON T.TermsPrimitive where
    parseJSON = Y.withText expected $ \s -> case s of
        "root"        -> return T.Root
        "assumptions" -> return T.Assumption
        "processed"   -> return T.Processed
        "unprocessed" -> return T.Unprocessed
        invalid       -> Y.typeMismatch expected (Y.String invalid)

        where expected = "formula source"


instance (F.Extension ext) => Y.FromJSON (T.TermsSpecification ext) where
    parseJSON (Y.Object o) 
         =  T.Union        <$> o .: "union" 
        <|> T.Intersection <$> o .: "intersection" 
        <|> T.Transform    <$> (o .: "with" >>= stringify) <*> o .: "with" <*> o .: "in"
        <|> fail "expected term specification"

        where
        stringify :: Y.Value -> Y.Parser String
        stringify (Y.String string) = return (unpack string)
        stringify (Y.Array vector) = return "<combined>" -- TODO
        stringify _ = fail "could not stringify transformation function"

    parseJSON (Y.String s) = T.Primitive <$> Y.parseJSON (Y.String s)
    parseJSON other = Y.typeMismatch "term specification" other
    

instance (F.Extension ext) => Y.FromJSON ([F.Term ext] -> [F.Term ext]) where
    parseJSON (Y.Array vector) = foldM fold id vector

        where 
        fold :: (F.Extension ext) 
             => ([F.Term ext] -> [F.Term ext]) 
             -> Y.Value 
             -> Y.Parser ([F.Term ext] -> [F.Term ext])  
        fold fs f = (. fs) <$> Y.parseJSON f
   

    parseJSON (Y.String s) = case s of
        "all"            -> return id
        "subterms"       -> return (>>= F.subterms)
        "formulas"       -> return (filter F.isFormula)
        "marked"         -> return (filter F.isMarkedFormula)
        "extensions"     -> return (filter F.isExtension)
        "modalities"     -> return (filter F.isExtension)
        "justifications" -> return (filter F.isExtension)
        "atomary"        -> return (filter F.isAtomary)
        "complex"        -> return (filter $ not . F.isAtomary)
        "constants"      -> return (filter F.isConstant)
        "variables"      -> return (filter F.isVariable)
        invalid          -> fail ("unknown: " ++ unpack invalid)

    parseJSON invalid = Y.typeMismatch "transformation function" invalid


instance (F.Extension ext) => Y.FromJSON (T.Constraint ext) where
    parseJSON = Y.withObject "constraint" $ \o ->
            T.Choose <$> o .: "or"
        <|> T.Merge  <$> o .: "and"
        <|> T.Occurs <$> o .: "occurs" <*> Y.parseJSON (Y.Object o)
        <|> T.Bind   <$> o .: "bind" <*> Y.parseJSON (Y.Object o)
        <|> fail "expected constraint"


instance Parseable ext => Y.FromJSON (F.Ambiguous (F.Term ext)) where
    parseJSON = Y.withText "term" $ parse parser


instance Parseable ext => Y.FromJSON (F.Formula ext) where
    parseJSON = Y.withText "formula" $ parse parser


instance Parseable term => Y.FromJSON (F.Marked term) where
    parseJSON = Y.withText "marked formula" $ parse parser


