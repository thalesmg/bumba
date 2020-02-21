{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import qualified GHCJS.DOM.Types        as JST
import           Reflex
import           Reflex.Dom

import           Control.Monad          (void)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson             ((.:))
import qualified Data.Aeson             as A (FromJSON (..), Object, withObject)
import           Data.Aeson.Types       (Parser)
import           Data.List              (sortOn)
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text, pack)
import           Data.Time              (DiffTime, TimeOfDay, getCurrentTime)
import           Data.Time.Format       (defaultTimeLocale, parseTimeM)
import           Data.Time.LocalTime    (timeOfDayToTime)
import           System.Environment     (lookupEnv)

data Bumba = Bumba Text Previsao deriving Show

newtype Bumbas = Bumbas { getBumbas :: [Bumba] } deriving Show

newtype Previsao = Previsao { getPrevisao :: DiffTime } deriving (Eq, Ord)

instance Show Previsao where
  show prev =
    let mins = show @Int . truncate . (/ 60) . getPrevisao $ prev
    in mins <> " min"

parseBumba :: A.Object -> Parser [(Text, TimeOfDay)]
parseBumba b = do
  nomeLinha <- b .: "c"
  vs <- b .: "vs"
  traverse (\v -> v .: "t" >>= \previsaoChegada -> pure (nomeLinha, previsaoChegada)) vs

instance A.FromJSON Bumbas where
  parseJSON = A.withObject "bumba" $ \v -> do
    agora <- timeOfDayToTime <$> (v .: "hr" >>= parseTimeM False defaultTimeLocale "%H:%M")
    p <- v .: "p"
    ls :: [A.Object] <- p .: "l"
    proximos :: [(Text, TimeOfDay)] <- concat <$> traverse parseBumba ls
    pure
      . Bumbas
      . sortOn (\(Bumba _ prev) -> prev)
      . map (\(nome, previsao) -> Bumba nome (Previsao $ timeOfDayToTime previsao - agora)) $ proximos

buscarBumbas ::
  ( Reflex t
  , JST.MonadJSM (Performable m)
  , PerformEvent t m
  , HasJSContext (Performable m)
  , TriggerEvent t m
  , MonadIO m
  )
  => Text -> Event t e -> m (Event t (Maybe Bumbas))
buscarBumbas baseUrl e = getAndDecode (baseUrl <> "/.netlify/functions/api?cod=630012906" <$ e)

main :: IO ()
main = do
  now <- getCurrentTime
  baseUrl <- pack . fromMaybe "" <$> lookupEnv "BUMBA_URL"
  mainWidget $ el "div" $ do
    postbuild <- getPostBuild
    eTick <- tickLossy 15 now
    bumbas <- buscarBumbas baseUrl $ leftmost [() <$ eTick, postbuild]
    texto <- holdDyn [] (fmap (maybe [] getBumbas) bumbas)
    let texto'' = fmap (map (pack . show)) texto
    el "div" $ do
      text "Próximos bumbas:"
      void $ el "ul" $ simpleList texto'' (el "li" . dynText)
