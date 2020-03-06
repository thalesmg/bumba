{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
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
import Text.Regex.TDFA ((=~))

data Filtro = MkFiltro { _filtroRegex :: Text
                       , _filtroCodigoParada :: Text
                       }

data Bumba = Bumba Text Previsao deriving Show

newtype Bumbas = Bumbas { getBumbas :: [Bumba] } deriving Show

newtype Previsao = Previsao { getPrevisao :: DiffTime } deriving (Eq, Ord)

instance Show Previsao where
  show prev =
    let mins = show @Int . truncate . (/ 60) . getPrevisao $ prev
    in mins <> " min"

bumbaName :: Bumba -> Text
bumbaName (Bumba name _) = name

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
  => Text -> Dynamic t Filtro -> Event t e -> m (Event t (Maybe Bumbas))
buscarBumbas baseUrl dFiltro eCheck =
  let
    eFiltro = attachPromptlyDyn dFiltro eCheck
    eUrl = (\(filtro, _) -> baseUrl <> "/.netlify/functions/api?cod=" <> _filtroCodigoParada filtro) <$> eFiltro
  in getAndDecode eUrl

mconcatMapM :: (Monad m, Monoid b) => (a -> m b) -> [a] -> m b
mconcatMapM f = fmap mconcat . mapM f

filtroDefault :: Filtro
filtroDefault = MkFiltro {_filtroRegex = "^847P", _filtroCodigoParada = "630012906"}

filtros :: [(Text, Filtro)]
filtros =
  [ ("VM -> Casa", filtroDefault)
  ]

seletorDeParadas ::
  ( Reflex t
  , DomBuilder t m
  , Monad m
  ) =>  m (SelectElement EventResult (DomBuilderSpace m) t, ())
seletorDeParadas =
  let
     opts = mconcatMapM (\(k, filtro) -> elAttr "option" ("value" =: k <> "id" =: _filtroCodigoParada filtro) $ text k) filtros
  in selectElement (def {_selectElementConfig_initialValue = "VM -> Casa"}) opts

main :: IO ()
main = do
  now <- getCurrentTime
  baseUrl <- pack . fromMaybe "" <$> lookupEnv "BUMBA_URL"
  mainWidget $ el "div" $ mdo
    postbuild <- getPostBuild
    eTick <- tickLossy 1 now
    bumbas <- buscarBumbas baseUrl dFiltro $ leftmost [ postbuild
                                                      , () <$ eRefresh
                                                      , () <$ eCheck
                                                      ]
    nomesBumbas' <- holdDyn [] (fmap (maybe [] getBumbas) bumbas)
    let texto = do
          filtro <- dFiltro
          filter ((=~ _filtroRegex filtro) . bumbaName) <$> nomesBumbas'
    lastUpdate <- foldDyn ($) (0 :: Int)
                  . mergeWith (.) $ [ (\t -> if t == 15 then 0 else t + 1) <$ eTick
                                    , const 0 <$ eRefresh
                                    ]
    let eCheck = ffilter (== 15) (updated lastUpdate)
    let texto'' = fmap (map (pack . show)) texto
    (eRefresh, dFiltro) <- el "div" $ do
      (seletor, ()) <- seletorDeParadas
      let dFiltro' = fromMaybe filtroDefault . flip lookup filtros <$> _selectElement_value seletor
      el "p" $ text "Próximos bumbas:"
      void $ el "ul" $ simpleList texto'' (el "li" . dynText)
      el "p" $ do
        text "Última atualização há "
        display lastUpdate
        dynText $ fmap (\n -> if n > 1 then " segundos" else " segundo") lastUpdate
      (e, ()) <- elAttr' "p" ("style" =: "font-size: 4rem;") $ text "↺"
      pure (domEvent Click e, dFiltro')
    pure ()
