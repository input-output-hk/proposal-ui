{-# LANGUAGE ExistentialQuantification #-}

module Types (Name(FileChoice, BuildkiteChoice, MainMenu, Menu1), Dialog(Dialog, dRender, dHandleEvent), AppState(AppState, asLastMsg, asDialogStack, asReplyChan), CustomEvent, DialogReply(DialogReplyHalt, DialogReplyContinue, DialogReplyLiftIO)) where

import           Brick.BChan (BChan)
import           Brick (Widget, EventM, BrickEvent)

data Name = MainMenu | FileChoice | BuildkiteChoice | Menu1 | None deriving (Eq, Ord, Show)

data AppState = AppState
  { asReplyChan :: BChan ()
  , asLastMsg :: String
  , asDialogStack :: Dialog
  }

data DialogReply = DialogReplyHalt AppState | DialogReplyContinue Dialog | DialogReplyLiftIO (IO Dialog)

data Dialog = Dialog
  { dRender :: AppState -> [ Widget Name ]
  , dHandleEvent :: AppState -> BrickEvent Name CustomEvent -> EventM Name DialogReply
  }

data CustomEvent = CustomEvent  deriving Show
