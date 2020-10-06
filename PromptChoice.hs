-- |
--
-- This dialog allows the user to choose from a list of options and
-- returns that option to the caller.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module PromptChoice (spawnPromptChoice) where

import           Brick (EventM, Widget, BrickEvent(VtyEvent), padLeftRight, str, withDefAttr, vBox)
import           Data.Function ((&))
import           Lens.Micro ((^.), (.~), (%~))
import           Lens.Micro.TH (makeLenses)
import           Brick.Widgets.Center (center)
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Border as B
import           Brick.Forms (focusedFormInputAttr, invalidFormInputAttr)
import qualified Brick.Widgets.List as L
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Border as B
import qualified Brick.Types as T
import qualified Brick.Widgets.Center as C
import qualified Data.Vector as Vec
import Brick.Widgets.Core
  ( (<+>)
  , str
  , vLimit
  , hLimit
  , vBox
  , withAttr
  )

import Types (Name(BuildkiteChoice), DialogReply(DialogReplyContinue), Name, Dialog(dRender, dHandleEvent, Dialog), CustomEvent, AppState)

data PromptChoiceState a = PromptChoiceState
  { _pcItemDescription :: String
  -- ^ What is each item?
  , _pcCallback        :: Maybe a -> EventM Name DialogReply
  -- ^ Return selected item to caller
  , _pcList            :: L.List Name a
  -- ^ List of items
  }

makeLenses ''PromptChoiceState

spawnPromptChoice
  :: Show a
  => String
  -- ^ What is each item?
  -> [a]
  -- ^ Items to choose from
  -> (Maybe a -> EventM Name DialogReply)
  -- ^ Return selected item to caller
  -> Dialog
spawnPromptChoice itemDescrip items callback =
  let
    itemRowHeight = 1
    initialList = L.list BuildkiteChoice (Vec.fromList items) itemRowHeight
    initialState = PromptChoiceState itemDescrip callback initialList
  in
    mkPromptChoice initialState

mkPromptChoice :: Show a => PromptChoiceState a -> Dialog
mkPromptChoice state = Dialog { dRender = renderUI state, dHandleEvent = handleEvents state }

renderUI :: Show a => PromptChoiceState a -> AppState -> [ Widget Name ]
renderUI pState _ = [ui]
    where
      l = pState ^. pcList
      label = str "Currently selected: " <+> cur
      cur = case L.listSelectedElement $ pState ^. pcList of
              Nothing     -> str "None"
              Just (_, x) -> str (show x)
      box = B.borderWithLabel label $
            hLimit 30 $
            vLimit 15 $
            L.renderList (listDrawElement (pState ^. pcItemDescription)) True l
      ui = C.vCenter $ vBox [ C.hCenter box
                            , str " "
                            , C.hCenter $ str $ "Press Enter to select an option."
                            , C.hCenter $ str "Press Esc to exit."
                            ]


handleEvents :: Show a => PromptChoiceState a -> AppState -> BrickEvent Name CustomEvent -> EventM Name DialogReply
handleEvents pState _  (T.VtyEvent e) =
    case e of
        V.EvKey (V.KEnter) [] ->
            case L.listSelectedElement $ pState ^. pcList of
              Nothing     -> pure $ DialogReplyContinue $ mkPromptChoice pState
              Just (_, x) -> pState ^. pcCallback $ Just x

        V.EvKey V.KEsc [] ->
          pState ^. pcCallback $ Nothing

        -- From Brick doco:
        --   Up (up arrow key)
        --   Down (down arrow key)
        --   Page Up (PgUp)
        --   Page Down (PgDown)
        --   Go to first element (Home)
        --   Go to last element (End)
        ev -> do
          let l = pState ^. pcList
          l' <- L.handleListEvent ev l
          let newState = pState & pcList .~ l'
          pure $ DialogReplyContinue $ mkPromptChoice newState
handleEvents pState _ _ = pure $ DialogReplyContinue $ mkPromptChoice pState

listDrawElement :: (Show a) => String -> Bool -> a -> Widget Name
listDrawElement itemDescrip sel a =
    let selStr s = if sel
                   then (str $ "<" <> s <> ">")
                   else str s
    in C.hCenter $ str (itemDescrip <> " ") <+> (selStr $ show a)

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"
