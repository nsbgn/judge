{-|
Module      : Logic.Judge.Tableau.Yaml
Description : YAML- and JSON-parsing.
Copyright   : (c) 2017, 2018 N Steenbergen
License     : GPL-3
Maintainer  : ns@slak.ws
Stability   : experimental

This module provides instances for parsing tableau systems in YAML- and 
JSON-representation, via 'Y.FromJSON'.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
module Logic.Judge.Prover.Yaml 
    () where

import "base" Data.List (delete)
import "base" Data.Maybe (fromMaybe)
import "base" Control.Monad (foldM)
import "base" Control.Applicative ((<|>), liftA2)
import "text" Data.Text (Text, empty, pack, unpack)
import "yaml" Data.Yaml ((.:),(.:?),(.!=))
import qualified "yaml" Data.Yaml as Y
import qualified "aeson" Data.Aeson.Types as Y (typeMismatch, withText, withObject)

import Logic.Judge.Prover.Tableau (Ref((:=)))
import qualified Logic.Judge.Formula as F
import qualified Logic.Judge.Prover.Tableau as T


instance (F.Extension ext) => Y.FromJSON (T.TableauSystem ext) where
    parseJSON = Y.withObject "tableau system" $ \o ->
        T.TableauSystem
            <$> o .:? "name" .!= "untitled"
            <*> o .:  "rules"
            <*> o .:? "assumptions" .!= mempty



instance {-# OVERLAPPABLE #-} (Monoid a, Y.FromJSON a, Y.FromJSON b) => Y.FromJSON (T.Ref a b) where
    parseJSON = Y.withObject "named object" $ \o -> 
        (:=) 
            <$> o .:? "id" .!= mempty
            <*> Y.parseJSON (Y.Object o)



instance (F.Extension ext, Y.FromJSON primitive) => Y.FromJSON (T.Rule (T.Constraint primitive ext) ext) where
    parseJSON = Y.withObject "tableau rule" $ \o ->
        T.Rule
            <$> o .:  "name"
            <*> o .:  "consume"
            <*> o .:  "produce"
            <*> o .:? "generate" .!= T.None
            <*> o .:? "restrict" .!= T.None
            <*> o .:? "compose"  .!= T.Nondeterministic



instance Y.FromJSON T.Compositor where
    parseJSON = Y.withText expected $ \s -> case s of
        "nondeterministic" -> return T.Nondeterministic
        "greedy"           -> return T.Greedy
        invalid            -> Y.typeMismatch expected (Y.String invalid)

        where expected = "compositor"



instance Y.FromJSON T.PrimitiveStaticTerms where
    parseJSON = Y.withText expected $ \s -> case s of
        "root"        -> return T.Root
        "assumptions" -> return T.Assumption
        invalid       -> Y.typeMismatch expected (Y.String invalid)

        where expected = "term"


instance Y.FromJSON T.PrimitiveDynamicTerms where
    parseJSON = Y.withText "term" $ \s -> case s of
        "processed"   -> return T.Processed
        "unprocessed" -> return T.Unprocessed
        other         -> T.Static <$> Y.parseJSON (Y.String s)



instance (F.Extension ext, Y.FromJSON primitive) => Y.FromJSON (T.Terms primitive ext) where
    parseJSON (Y.Object o) 
         =  T.Union        <$>  o .: "union" 
        <|> T.Intersection <$>  o .: "intersection" 
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


instance (F.Extension ext, Y.FromJSON primitive) => Y.FromJSON (T.Constraint primitive ext) where
    parseJSON = Y.withObject "constraint or generator" $ \o ->
            T.Choose <$> o .: "or"
        <|> T.Merge  <$> o .: "and"
        <|> T.Bind   <$> o .: "match" <*> Y.parseJSON (Y.Object o)
        <|> fail "expected constraint or generator"


instance F.Parseable ext => Y.FromJSON (F.Ambiguous (F.Term ext)) where
    parseJSON = Y.withText "term" F.parse


instance F.Parseable ext => Y.FromJSON (F.Formula ext) where
    parseJSON = Y.withText "formula" F.parse


instance F.Parseable term => Y.FromJSON (F.Marked term) where
    parseJSON = Y.withText "marked formula" F.parse


