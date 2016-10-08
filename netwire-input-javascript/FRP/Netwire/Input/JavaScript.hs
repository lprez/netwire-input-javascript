{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module FRP.Netwire.Input.JavaScript (
        JSInputState,
        JSInputControl,
        JSInput,
        JSInputT,
        Key(..),
        MouseButton(..),
        mkInputControl,
        initialInputState,
        pollJavaScript,
        cursorLocked,
        lockCursor,
        unlockCursor
) where

import Control.Applicative
import Control.Wire
import FRP.Netwire.Input hiding (Key, MouseButton)
import qualified FRP.Netwire.Input

import Control.Monad.Trans.State
import JavaScript.Input

instance FRP.Netwire.Input.Key Key
instance FRP.Netwire.Input.MouseButton MouseButton

instance Monad m => MonadMouse MouseButton (JSInputT m) where
        setCursorMode cm = setCursorModeM $
                case cm of
                     CursorMode'Enabled -> (Just False, Just False)
                     CursorMode'Hidden -> (Just False, Just True)
                     _ -> (Just True, Nothing)
        mbIsPressed = mbIsPressedM
        releaseButton = releaseButtonM
        cursor = cursorM
        scroll = scrollM

instance Monad m => MonadKeyboard Key (JSInputT m) where
        keyIsPressed = keyIsPressedM
        releaseKey = releaseKeyM

-- | In JavaScript, you can lock the pointer only after the user releases a
-- mouse button or a key. This means that 'cursorMode' (with 'CursorMode'Reset')
-- and 'mouseMickies' will not actually lock the pointer, but will schedule
-- the pointer lock request for the next interaction from the user.
-- In particular, 'mouseMickies' will behave like 'mouseCursor' if the pointer
-- is not locked.
--
-- This wire, which inhibits if the pointer is not locked, is
-- useful if you want to know if you're still waiting for the user to lock the
-- pointer, and if the user manually unlocked it.
cursorLocked :: (Monoid e, Monad m) => Wire s e (JSInputT m) a a
cursorLocked = mkGen_ $ \x -> boolToEither x <$> lockedCursorM
        where boolToEither _ False = Left mempty
              boolToEither x True = Right x
