module JavaScript.Input.Key (
        Key(..),
        MouseButton(..),
        fromKey,
        fromMouseButton
) where

data Key = KeyA | KeyB | KeyC | KeyD | KeyE | KeyF | KeyG | KeyH | KeyI | KeyJ
         | KeyK | KeyL | KeyM | KeyN | KeyO | KeyP | KeyQ | KeyR | KeyS | KeyT
         | KeyU | KeyV | KeyW | KeyX | KeyY | KeyZ | Key0 | Key1 | Key2 | Key3
         | Key4 | Key5 | Key6 | Key7 | Key8 | Key9 | KeySpace | KeyEnter
         | KeyTab | KeyEsc | KeyBackspace | KeyShift | KeyControl | KeyAlt
         | KeyCapsLock | KeyNumLock | KeyArrowLeft | KeyArrowUp | KeyArrowRight
         | KeyArrowDown | KeyIns | KeyDel | KeyHome | KeyEnd | KeyPgUp
         | KeyPgDown | KeyF1 | KeyF2 | KeyF3 | KeyF4 | KeyF5 | KeyF6 | KeyF7
         | KeyF8 | KeyF9 | KeyF10 | KeyF11 | KeyF12 | KeyPadDel | KeyPadIns
         | KeyPadEnd | KeyPadDown | KeyPadPgDown | KeyPadLeft | KeyPadRight
         | KeyPadHome | KeyPadUp | KeyPadPgUp | KeyPadAdd | KeyPadSub
         | KeyPadMul | KeyPadDiv | KeyPadEnter | KeyPadDot | KeyPad0 | KeyPad1 
         | KeyPad2 | KeyPad3 | KeyPad4 | KeyPad5 | KeyPad6 | KeyPad7 | KeyPad8
         | KeyPad9
         deriving (Eq, Show)

data MouseButton = MouseLeft | MouseMiddle | MouseRight deriving (Eq, Show)

fromKey :: Key -> [Int]
fromKey KeyA = [65, 97]
fromKey KeyB = [66, 98]
fromKey KeyC = [67, 99]
fromKey KeyD = [100, 68]
fromKey KeyE = [101, 69]
fromKey KeyF = [102, 70]
fromKey KeyG = [103, 71]
fromKey KeyH = [104, 72]
fromKey KeyI = [105, 73]
fromKey KeyJ = [106, 74]
fromKey KeyK = [107, 75]
fromKey KeyL = [108, 76]
fromKey KeyM = [109, 77]
fromKey KeyN = [110, 78]
fromKey KeyO = [111, 79]
fromKey KeyP = [112, 80]
fromKey KeyQ = [113, 81]
fromKey KeyR = [114, 82]
fromKey KeyS = [115, 83]
fromKey KeyT = [116, 84]
fromKey KeyU = [117, 85]
fromKey KeyV = [118, 86]
fromKey KeyW = [119, 87]
fromKey KeyX = [120, 88]
fromKey KeyY = [121, 89]
fromKey KeyZ = [122, 90]
fromKey Key0 = [48]
fromKey Key1 = [49]
fromKey Key2 = [50]
fromKey Key3 = [51]
fromKey Key4 = [52]
fromKey Key5 = [53]
fromKey Key6 = [54]
fromKey Key7 = [55]
fromKey Key8 = [56]
fromKey Key9 = [57]
fromKey KeySpace = [32]
fromKey KeyEnter = [13]
fromKey KeyTab = [9]
fromKey KeyEsc = [27]
fromKey KeyBackspace = [8]
fromKey KeyShift = [16]
fromKey KeyControl = [17]
fromKey KeyAlt = [18]
fromKey KeyCapsLock = [20]
fromKey KeyNumLock = [144]
fromKey KeyArrowLeft = [37]
fromKey KeyArrowUp = [38]
fromKey KeyArrowRight = [39]
fromKey KeyArrowDown = [40]
fromKey KeyIns = [45]
fromKey KeyDel = [46]
fromKey KeyHome = [36]
fromKey KeyEnd = [35]
fromKey KeyPgUp = [33]
fromKey KeyPgDown = [34]
fromKey KeyF1 = [112]
fromKey KeyF2 = [113]
fromKey KeyF3 = [114]
fromKey KeyF4 = [115]
fromKey KeyF5 = [116]
fromKey KeyF6 = [117]
fromKey KeyF7 = [118]
fromKey KeyF8 = [119]
fromKey KeyF9 = [120]
fromKey KeyF10 = [121]
fromKey KeyF11 = [122]
fromKey KeyF12 = [123]
fromKey KeyPadDel = [46]
fromKey KeyPadIns = [45]
fromKey KeyPadEnd = [35]
fromKey KeyPadDown = [40]
fromKey KeyPadPgDown = [34]
fromKey KeyPadLeft = [37]
fromKey KeyPadRight = [39]
fromKey KeyPadHome = [36]
fromKey KeyPadUp = [38]
fromKey KeyPadPgUp = [33]
fromKey KeyPadAdd = [107]
fromKey KeyPadSub = [109]
fromKey KeyPadMul = [106]
fromKey KeyPadDiv = [111]
fromKey KeyPadEnter = [13]
fromKey KeyPadDot = [46]
fromKey KeyPad0 = [48]
fromKey KeyPad1 = [49]
fromKey KeyPad2 = [50]
fromKey KeyPad3 = [51]
fromKey KeyPad4 = [52]
fromKey KeyPad5 = [53]
fromKey KeyPad6 = [54]
fromKey KeyPad7 = [55]
fromKey KeyPad8 = [56]
fromKey KeyPad9 = [57]

fromMouseButton :: MouseButton -> Int
fromMouseButton MouseLeft = 0
fromMouseButton MouseMiddle = 1
fromMouseButton MouseRight = 2
