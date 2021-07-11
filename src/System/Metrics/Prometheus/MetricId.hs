{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Metrics.Prometheus.MetricId where

import           Data.Char (isDigit)
import           Data.Bifunctor (first)
import           Data.Map       (Map)
import qualified Data.Map       as Map
import           Data.Monoid    (Monoid)
import           Data.Semigroup (Semigroup)
import           Data.String    (IsString(..))
import           Data.Text      (Text)
import qualified Data.Text as Text
import           Prelude        hiding (null)


newtype Name = Name { unName :: Text } deriving (Show, Eq, Ord, Monoid, Semigroup)

instance IsString Name where
  fromString = makeName . Text.pack

newtype Labels = Labels { unLabels :: Map Text Text } deriving (Show, Eq, Ord, Monoid, Semigroup)


data MetricId =
    MetricId
    { name   :: Name
    , labels :: Labels
    } deriving (Eq, Ord, Show)


addLabel :: Text -> Text -> Labels -> Labels
addLabel key val = Labels . Map.insert (makeValid key) val . unLabels


fromList :: [(Text, Text)] -> Labels
fromList = Labels . Map.fromList . map (first makeValid)


toList :: Labels -> [(Text, Text)]
toList = Map.toList . unLabels


null :: Labels -> Bool
null = Map.null . unLabels


allowedChar :: Char -> Bool
allowedChar c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || isDigit c || c == '_'


makeValid :: Text -> Text
makeValid "" = "_"
makeValid txt = prefix_ <> Text.map (\c -> if allowedChar c then c else '_' ) txt
  where prefix_ = if isDigit (Text.head txt) then "_" else ""


makeName :: Text -> Name
makeName = Name . makeValid
