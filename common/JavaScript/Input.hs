{-# LANGUAGE OverloadedStrings #-}

module JavaScript.Input (
        JSInputState,
        JSInputControl,
        JSInput,
        JSInputT,
        Key(..),
        MouseButton(..),
        mkInputControl,
        initialInputState,
        pollJavaScript,
        lockCursor,
        unlockCursor,
        setCursorModeM,
        mbIsPressedM,
        releaseButtonM,
        cursorM,
        lockedCursorM,
        scrollM,
        keyIsPressedM,
        releaseKeyM
) where

import qualified Data.IntSet as S
import Control.Applicative
import Control.Monad (when)
import Control.Monad.Trans.State
import Data.IORef

import GHCJS.Foreign hiding (Object)
import GHCJS.Foreign.Callback
import GHCJS.Marshal
import GHCJS.Types
import JavaScript.Object.Internal

import JavaScript.Input.Key

data JSInputState = JSInputState {
        keyPressedSet :: S.IntSet,
        keyReleasedSet :: S.IntSet,
        mbPressedSet :: S.IntSet,
        mbReleasedSet :: S.IntSet,
        cursorPos :: (Float, Float),
        cursorMovement :: (Float, Float),
        scrollAmount :: (Double, Double),
        hiddenCursor :: Bool,
        lockedCursor :: Bool,
        reqHiddenCursor :: Maybe Bool,
        reqLockedCursor :: Maybe Bool
}

data Event = KeyDown Int | KeyUp Int | MouseDown Int | MouseUp Int
           | MouseMove (Float, Float) (Float, Float) | Wheel (Int, Int)
           | PointerLockChange Bool

data JSInputControl = JSInputControl (IORef [Event]) (IORef Bool) JSVal

type JSInput = State JSInputState
type JSInputT m = StateT JSInputState m

-- | Create a 'JSInputControl' from a DOM element.
mkInputControl :: JSVal -> IO (Maybe JSInputControl)
mkInputControl element | isNull element = return Nothing
mkInputControl element = do eventsVar <- newIORef []
                            ptrLockVar <- newIORef False
                            doc <- document
                            event element eventsVar "keydown" eventKeyDown
                            event element eventsVar "keyup" eventKeyUp
                            event element eventsVar "mousedown" eventMouseDown
                            event element eventsVar "mouseup" eventMouseUp
                            event element eventsVar "mousemove" $
                                    eventMouseMove element
                            event element eventsVar "wheel" eventWheel
                            event doc eventsVar "pointerlockchange" $
                                    const (eventPointerLockChange element)
                            event doc eventsVar "mozpointerlockchange" $
                                    const (eventPointerLockChange element)
                            ptrLockPerformer element ptrLockVar "mouseup"
                            ptrLockPerformer element ptrLockVar "keyup"
                            return . Just $
                                    JSInputControl eventsVar ptrLockVar element

        where event element eventsVar name getEvent =
                do callback <- asyncCallback1 $ \rawEvent ->
                                do event <- getEvent $ Object rawEvent
                                   modifyIORef' eventsVar (event :)
                   addEventListener element name callback

              ptrLockPerformer element ptrLockVar name =
                do callback <- asyncCallback1 $ \_ -> do
                                lock <- atomicModifyIORef' ptrLockVar
                                                           (\x -> (False, x))
                                when lock $ lockCursorRaw element
                   addEventListener element name callback

-- | Use this with 'pollJavaScript' the first time.
initialInputState :: JSInputState
initialInputState = JSInputState {
        keyPressedSet = S.empty,
        keyReleasedSet = S.empty,
        mbPressedSet = S.empty,
        mbReleasedSet = S.empty,
        cursorPos = (0, 0),
        cursorMovement = (0, 0),
        scrollAmount = (0, 0),
        hiddenCursor = False,
        lockedCursor = False,
        reqHiddenCursor = Nothing,
        reqLockedCursor = Nothing
        }

-- | Update the 'JSInputState' with the new events.
pollJavaScript :: JSInputState
               -> JSInputControl
               -> IO JSInputState
pollJavaScript is (JSInputControl eventsVar ptrLockVar element) =
        do events <- atomicModifyIORef eventsVar $ \e -> ([], e)

           let is' = foldr compEvent
                           is { scrollAmount = (0, 0)
                              , cursorMovement = (0, 0) }
                           events

           hidden <- case (hiddenCursor is, reqHiddenCursor is') of
                          (True, Just False) -> showCursor element >>
                                                return False
                          (False, Just True) -> hideCursor element >>
                                                return False
                          _ -> return $ hiddenCursor is


           case (lockedCursor is', reqLockedCursor is') of
                (True, Just False) -> writeIORef ptrLockVar False >>
                                      unlockCursorRaw
                (False, Just True) -> writeIORef ptrLockVar True
                _ -> return ()

           return is' { hiddenCursor = hidden
                      , reqHiddenCursor = Nothing
                      , reqLockedCursor = Nothing }

-- | Manually schedule cursor lock.
lockCursor :: JSInputControl -> IO ()
lockCursor (JSInputControl _ ptrLockVar _) = writeIORef ptrLockVar True

-- | Manually unlock the cursor.
unlockCursor :: JSInputControl -> IO ()
unlockCursor (JSInputControl _ ptrLockVar _) = writeIORef ptrLockVar False
                                               >> unlockCursorRaw

setCursorModeM :: Monad m => (Maybe Bool, Maybe Bool) -> JSInputT m ()
setCursorModeM (lock, hide) = modify $ \is -> is { reqLockedCursor = lock
                                                 , reqHiddenCursor = hide }

mbIsPressedM :: Monad m => MouseButton -> JSInputT m Bool
mbIsPressedM mb = S.member (fromMouseButton mb) . mbPressedSet <$> get

releaseButtonM :: Monad m => MouseButton -> JSInputT m ()
releaseButtonM mb = modify $
        \is -> is { mbReleasedSet = S.insert (fromMouseButton mb) $
                                        mbReleasedSet is
                  , mbPressedSet = S.delete (fromMouseButton mb) $
                                        mbPressedSet is }

cursorM :: Monad m => JSInputT m (Float, Float)
cursorM = (<$> get) $ \is -> if lockedCursor is
                             then cursorMovement is
                             else cursorPos is

lockedCursorM :: Monad m => JSInputT m Bool
lockedCursorM = lockedCursor <$> get

scrollM :: Monad m => JSInputT m (Double, Double)
scrollM = scrollAmount <$> get

keyIsPressedM :: Monad m => Key -> JSInputT m Bool
keyIsPressedM k = do kp <- keyPressedSet <$> get
                     return $ any (flip S.member kp) (fromKey k)

releaseKeyM :: Monad m => Key -> JSInputT m ()
releaseKeyM k = modify $
        \is -> is { keyReleasedSet = foldr S.insert
                                           (keyReleasedSet is)
                                           (fromKey k)
                  , keyPressedSet = foldr S.delete
                                          (keyPressedSet is)
                                          (fromKey k)
                  }

compEvent :: Event -> JSInputState -> JSInputState
compEvent (KeyDown k) is | S.member k $ keyReleasedSet is = is
                         | otherwise = is { keyPressedSet =
                                                S.insert k $ keyPressedSet is }
compEvent (KeyUp k) is =
        is { keyPressedSet = S.delete k $ keyPressedSet is
           , keyReleasedSet = S.delete k $ keyReleasedSet is }
compEvent (MouseDown k) is | S.member k $ mbReleasedSet is = is
                           | otherwise = is { mbPressedSet =
                                                S.insert k $ mbPressedSet is }
compEvent (MouseUp k) is =
        is { mbPressedSet = S.delete k $ mbPressedSet is
           , mbReleasedSet = S.delete k $ mbReleasedSet is }
compEvent (MouseMove (cx, cy) (dmx, dmy)) is =
        let (mx0, my0) = cursorMovement is
        in is { cursorPos = (cx, cy)
              , cursorMovement = (mx0 + dmx, my0 + dmy) }
compEvent (Wheel (x, y)) is =
        let (dx, dy) = (fromIntegral x, fromIntegral y)
            (x0, y0) = scrollAmount is
        in is { scrollAmount = (x0 + dx, y0 + dy) }
compEvent (PointerLockChange locked) is = is { lockedCursor = locked }

eventKeyDown :: Object -> IO Event
eventKeyDown ev = KeyDown <$> prop "keyCode" ev

eventKeyUp :: Object -> IO Event
eventKeyUp ev = KeyUp <$> prop "keyCode" ev

eventMouseDown :: Object -> IO Event
eventMouseDown ev = MouseDown <$> prop "button" ev

eventMouseUp :: Object -> IO Event
eventMouseUp ev = MouseUp <$> prop "button" ev

eventMouseMove :: JSVal -> Object -> IO Event
eventMouseMove elem ev@(Object evVal) =
        do width <- fi <$> prop "clientWidth" (Object elem)
           height <- fi <$> prop "clientHeight" (Object elem)
           clientX <- fi <$> prop "clientX" ev
           clientY <- fi <$> prop "clientY" ev
           movementX <- fi <$> movementX evVal
           movementY <- fi <$> movementY evVal
           return $ MouseMove ( clientX * 2 / width - 1
                              , clientY * 2 / height - 1)
                              ( movementX * 2 / width
                              , movementY * 2 / height )
        where fi = fromIntegral :: Int -> Float

eventWheel :: Object -> IO Event
eventWheel ev = Wheel <$> ((,) <$> prop "deltaX" ev
                               <*> prop "deltaY" ev)

eventPointerLockChange :: JSVal -> IO Event
eventPointerLockChange elem = PointerLockChange <$> isPointerLockElement elem

prop :: FromJSVal a => JSString -> Object -> IO a
prop s o = unsafeGetProp s o >>= fromJSValUnchecked

foreign import javascript unsafe "$1.addEventListener($2, $3)"
        addEventListener :: JSVal -> JSString -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe " if ($1.requestPointerLock)\
                                 \     $1.requestPointerLock()\
                                 \ else\
                                 \     $1.mozRequestPointerLock()"
        lockCursorRaw :: JSVal -> IO ()

foreign import javascript unsafe " if (document.exitPointerLock)\
                                 \     document.exitPointerLock()\
                                 \ else\
                                 \     document.mozExitPointerLock()"
        unlockCursorRaw :: IO ()

foreign import javascript unsafe "$1.style.cursor = 'none'"
        hideCursor :: JSVal -> IO ()

foreign import javascript unsafe "$1.style.cursor = 'auto'"
        showCursor :: JSVal -> IO ()

foreign import javascript unsafe " document.pointerLockElement === $1 ||\
                                 \ document.mozPointerLockElement === $1"
        isPointerLockElement :: JSVal -> IO Bool

foreign import javascript unsafe "$r = document" document :: IO JSVal

foreign import javascript unsafe "$1.movementX || $1.mozMovementX || 0"
        movementX :: JSVal -> IO Int

foreign import javascript unsafe "$1.movementY || $1.mozMovementY || 0"
        movementY :: JSVal -> IO Int
