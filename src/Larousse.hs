module Larousse (lookup) where

import           Control.Applicative ((<|>))
import           Data.Aeson          (ToJSON)
import           Data.Maybe          (listToMaybe)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           GHC.Generics        (Generic)
import           Prelude             hiding (lookup)
import           Text.HTML.Scalpel


data Definition = MkDefinition
  { defText    :: !Text
  , defExample :: !(Maybe Text)
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

lookup :: String -> IO [Definition]
lookup word = do
  let url = "https://www.larousse.fr/dictionnaires/francais/" ++ word
  definitions <- scrapeURL url defs
  case definitions of
    Nothing -> return []
    Just ds -> return ds

  where
    defs = chroots ("li" @: [hasClass "DivisionDefinition"]) $ do
      txt <- avecIndicateur <|> multiDefs <|> simpleDef
      MkDefinition (T.strip txt) <$> fmap (fmap T.strip) example

    simpleDef = text textSelector

    multiDefs = inSerial $ do
      _ <- seekNext $ text ("span" @: [hasClass "numDef"])
      stepNext $ text textSelector

    avecIndicateur = inSerial $ do
      _ <- seekNext $ text ("span" @: [hasClass "indicateurDefinition"])
      stepNext $ text textSelector

    example = listToMaybe <$> texts ("span" @: [hasClass "ExempleDefinition"])



