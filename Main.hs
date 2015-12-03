import Control.Monad (forever)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (
    evalStateT
  , get
  , StateT
  , put
  )
import Graphics.Vty (
    defAttr
  , Event ( EvKey )
  , Key ( KChar )
  , mkVty
  , Modifier ( MCtrl )
  , nextEvent
  , picForImage
  , Picture
  , shutdown
  , standardIOConfig
  , string
  , update
  )
import System.Exit (exitSuccess)

numberOfLines :: Int
numberOfLines = 100

type Offset = Int

scroll :: Int -> StateT Offset IO ()
scroll n = (+ n) <$> get >>= put . min (numberOfLines - 1) . max 0

picForCurrentOffset :: StateT Offset IO Picture
picForCurrentOffset = do
    offset <- get
    return . picForImage . mconcat . map imageForInt . drop offset $ [0..(numberOfLines - 1)]
    where imageForInt = string defAttr . show

main :: IO ()
main = do
    vty <- standardIOConfig >>= mkVty

    flip evalStateT 0 . forever $ do
        picForCurrentOffset >>= lift . update vty

        e <- lift $ nextEvent vty
        case e of
            EvKey (KChar 'c') [MCtrl] -> lift $ shutdown vty >> exitSuccess
            EvKey (KChar 'j') []      -> scroll 1
            EvKey (KChar 'k') []      -> scroll (-1)
            _                         -> return ()
