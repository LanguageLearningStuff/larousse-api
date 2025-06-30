module Main where

import qualified Larousse

import           Control.Monad.IO.Class   (liftIO)
import           Data.Proxy               (Proxy (..))
import           Data.Text                (Text)
import           Network.Wai.Handler.Warp
import           Servant

type API = "api" :> "v1" :> "words" :> "search" :> QueryParam "q" Text :> Get '[JSON] [Larousse.Definition]

handleSearch :: Maybe Text -> Handler [Larousse.Definition]
handleSearch (Just word) = do
  defs <- liftIO $ Larousse.lookup word
  return defs

srv :: Server API
srv = handleSearch

api :: Proxy API
api = Proxy

main :: IO ()
main = run 3000 . serve api $ srv
