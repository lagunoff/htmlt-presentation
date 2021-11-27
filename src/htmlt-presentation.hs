import Control.Lens
import Control.Monad
import Data.Foldable
import Data.Text as T
import HtmlT
import Text.Read

import "this" Utils
import "this" Slides

main :: IO ()
main = void $ attachToBody do
  win <- getCurrentWindow
  hashRef <- mkRouteRef
  el "style" . text $ fold [globalStyles, slidesStyles]
  let slideNumberRef = lensMap (iso parseNum printNum) hashRef
  div_ [class_ "Slide"] do
    div_ [class_ "Clearfix"] blank
    dyn $ slidesWidget <$> fromRef slideNumberRef
    div_ [class_ "Progress"] do
      div_ [class_ "ProgressBar"] do
        dynStyles $ progressBarStyle <$> fromRef slideNumberRef
  onGlobalEvent defaultListenerOpts {lo_sync_callback = True} (DOMNode win) "keydown" $
    withDecoder keyboardEventDecoder (handleGlobalKeydown slideNumberRef)
  where
    parseNum t = case T.splitOn "-" t of
      [num, step] -> (readInt num, readInt step)
      _ -> (0, 0)
    printNum (n, s) = T.pack $ show (n + 1) <> "-" <> show (s + 1)
    readInt = maybe 0 pred . readMaybe . T.unpack
    progressBarStyle (n, s)  = "width: " <> percentage <> "%" where
      percentage = T.pack . show @Double $ (100 * currentNum) / slidesNum
      slidesNum = fromIntegral $ Prelude.foldr (+) 0 slideStepLengths
      currentNum = fromIntegral $ s + pastSlides n slideStepLengths + 1
      pastSlides 0 _ = 0
      pastSlides _ [] = 0
      pastSlides n (x:xs) = x + pastSlides (n - 1) xs

handleGlobalKeydown :: MonadReactive m => DynRef (Int, Int) -> KeyboardEvent -> m ()
handleGlobalKeydown slideNumberRef KeyboardEvent{..}
  | ke_key_code `elem` [32, 39] = modifyRef slideNumberRef (stepForward (kmod_ctrl_key ke_modifiers))
  | ke_key_code `elem` [8, 37] = modifyRef slideNumberRef (stepBackward (kmod_ctrl_key ke_modifiers))
  | ke_key_code `elem` [38, 36] = writeRef slideNumberRef slideMax
  | ke_key_code `elem` [40, 35] = writeRef slideNumberRef slideMin
  | otherwise = return ()
  where
    stepForward ctrl (n, s)
      | not ctrl && s < (slideStepLengths !! n) - 1 = (n, s + 1)
      | n < Prelude.length slideStepLengths - 1 = (n + 1, 0)
      | otherwise = (n, s)
    stepBackward ctrl (n, s)
      | not ctrl && s > 0 = (n, s - 1)
      | n > 0 = (n - 1, (slideStepLengths !! (n - 1)) - 1)
      | otherwise = (n, s)
    slideLen = Prelude.length slideStepLengths
    slideMax = (slideLen - 1, (slideStepLengths !! (slideLen - 1)) - 1)
    slideMin = (0, 0)

globalStyles :: Text
globalStyles = "\
  \html, body, body * {\
  \  font-family: Arial, sans-serif;\
  \}\
  \body {\
  \  transform: scale(1.3) ;\
  \  padding: 0;\
  \  position: absolute;\
  \  top: 50%;\
  \  left: 50%;\
  \  width: 1024px;\
  \  height: 640px;\
  \  margin: -320px 0 0 -512px;\
  \  background: black;\
  \  color: rgba(0,0,0,0.87);\
  \}\
  \.Clearfix {\
  \  width: 100%;\
  \  height: 1px;\
  \  clear: both;\
  \}\
  \.Slide {\
  \  background: white;\
  \  width: 100%;\
  \  height: 100%;\
  \  padding: 24px;\
  \  box-sizing: border-box;\
  \}\
  \.Progress {\
  \  width: 100%;\
  \  height: 4px;\
  \  position: absolute;\
  \  bottom: 0;\
  \  left: 0;\
  \}\
  \.ProgressBar {\
  \  height: 100%;\
  \  background: red;\
  \}"
