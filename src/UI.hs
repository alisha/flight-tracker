{-# LANGUAGE BlockArguments #-}
module UI
  ( ui,
  )
where

import qualified Graphics.Vty as V


import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center)
import Brick.Forms
import Brick.Widgets.Core
import Lens.Micro (Lens', lens)
import Data.Time.Clock (nominalDay)
import Data.Time.Clock.POSIX
import Brick.Main
import Brick.Types
import Brick.Focus

import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick
import Data.Time.Clock
import Requests (makeApiRequest)
import Types

data ArrivalsFields = AirportField | BeginField | EndField
  deriving(Eq, Ord, Show)

type BeginTime = Integer
type EndTime = Integer

data ArrivalsInput = ArrivalsInput {
  _airport :: AirportCode,
  _begin :: BeginTime,
  _end :: EndTime
} deriving Show

airport :: Lens' ArrivalsInput AirportCode
airport = lens _airport (\input newCode -> input { _airport = newCode})
begin :: Lens' ArrivalsInput BeginTime
begin = lens _begin (\input newBegin -> input { _begin = newBegin})
end :: Lens' ArrivalsInput EndTime
end = lens _end (\input newEnd -> input { _end = newEnd})

arrivalsForm :: ArrivalsInput -> Form ArrivalsInput e ArrivalsFields
arrivalsForm = let label s w = padBottom (Pad 1) $ (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
  in newForm [
    label "Airport" @@= editShowableField airport AirportField,
    label "begin" @@= editShowableField begin BeginField,
    label "end" @@= editShowableField end EndField
  ]

draw :: Form ArrivalsInput e ArrivalsFields -> [Widget ArrivalsFields]
draw f = [C.vCenter $ C.hCenter form <=> C.hCenter help]
  where
    form = B.border $ padTop (Pad 1) $ hLimit 50 $ renderForm f
    help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
    body = str "ICAO airport code and begin/end epoch times in seconds"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  ]

app :: App (Form ArrivalsInput e ArrivalsFields) e ArrivalsFields
app =
    App { appDraw = draw
        , appHandleEvent = \s ev ->
            case ev of
                VtyEvent V.EvResize {}     -> continue s
                VtyEvent (V.EvKey V.KEsc [])   -> halt s
                VtyEvent (V.EvKey (V.KChar 'q') [])   -> halt s
                -- Enter quits only when we aren't in the multi-line editor.
                VtyEvent (V.EvKey V.KEnter [])
                    | focusGetCurrent (formFocus s) /= Just AirportField -> halt s
                _ -> do
                    s' <- handleFormEvent ev s
                    continue s'

                    -- Example of external validation:
                    -- Require age field to contain a value that is at least 18.
                    -- continue $ setFieldValid ((formState s')^.age >= 18) AgeField s'

        , appChooseCursor = focusRingCursor formFocus
        , appStartEvent = return
        , appAttrMap = const theMap
        }
ui :: IO ()
ui = do
  now <- round `fmap` getPOSIXTime :: IO Integer
  let daySeconds = round nominalDay :: Integer
  let beginTime = now - daySeconds
  let defaultArrivals = ArrivalsInput { _airport = KSAN, _begin = beginTime, _end = now  }
  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v

      f = arrivalsForm defaultArrivals

  initialVty <- buildVty
  f' <- customMain initialVty buildVty Nothing app f

  putStrLn "The starting form state was:"
  print defaultArrivals

  putStrLn "The final form state was:"
  let params = formState f'
  print $ params
  x <- makeApiRequest (_airport params) (_begin params) (_end params)
  print $ x


  -- if allFieldsValid f'
  --     then putStrLn "The final form inputs were valid."
  --     else putStrLn $ "The final form had invalid inputs: " <> show (invalidFields f')
  -- putStrLn $ "The final form had invalid inputs: " <> show f'




