{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

import XMonad
import XMonad.Actions.UpdatePointer
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Simplest
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Decoration
import XMonad.Layout.NoBorders
import XMonad.Layout.Circle
import XMonad.Layout.MultiToggle
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Actions.Navigation2D
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.ManageHook
import XMonad.Util.EZConfig
import XMonad.Util.Cursor
import Data.Bits ((.|.))
import qualified XMonad.StackSet as W
import qualified Data.Map as M (keys)
import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (catch,SomeException(..))
import Data.List (partition,isInfixOf,any)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)

active   = "#888888"
inactive = "#222222"

main = do
  myModMask <- fromMaybe mod4Mask
               <$> catch (fmap read <$> lookupEnv "XmonadModMask")
                         (\(SomeException e) -> return Nothing)
  -- writeFile "/tmp/xmonadUserLog.txt" $ show (mod4Mask)
  -- mod4Mask (command key) == 64
  --
  -- exec "XmonadModMask=64 xmonad", then modMask is mod4Mask
  xmonad
    $ withNavigation2DConfig
      (Navigation2DConfig
        { defaultTiledNavigation
            = hybridOf lineNavigation sideNavigation
        , floatNavigation
            = hybridOf sideNavigation centerNavigation
        , screenNavigation   = lineNavigation
        , layoutNavigation   = [ ("Full",             centerNavigation)
                               , ("Spacing Full",     centerNavigation)
                               , ("Simplest",         centerNavigation)
                               , ("Spacing Simplest", centerNavigation)
                               ]
        , unmappedWindowRect = [ ("Full",             fullScreenRect)
                               , ("Spacing Full",     fullScreenRect)
                               , ("Simplest",         singleWindowRect)
                               , ("Spacing Simplest", singleWindowRect)
                               ]
        })
    $ additionalNav2DKeys
      (xK_k, xK_h, xK_j, xK_l)
      [ (myModMask, windowGo)
      , (myModMask .|. controlMask, windowSwap)
      -- , (myModMask .|. shiftMask, \_ _ -> switchLayer)
      ]
      False
    $ ewmh def
    { terminal           = "termonad"
    , manageHook         = myManageHookFloat
    , focusedBorderColor = active
    , normalBorderColor  = inactive
    , borderWidth        = 4
    , layoutHook         = myLayout
    , startupHook        = startup
    , focusFollowsMouse  = False
    , clickJustFocuses   = True
    , logHook            = updatePointer (0.998, 0.998) (0, 0)
    , modMask            = myModMask
    }

    `removeKeys`
    [ (myModMask, xK_j)
    , (myModMask, xK_k)
    , (myModMask, xK_h)
    , (myModMask, xK_l)
    , (myModMask, xK_p)
    -- , (myModMask .|. shiftMask, xK_space)
    ]
    `additionalKeys`
    [ ((myModMask, xK_d), spawn "dmenu_run -fn 'Iosevka Custom-11'")
    , ((myModMask, xK_p), spawn "dmenu_run -fn 'Iosevka Custom-11'")
    , ((myModMask .|. shiftMask, xK_j), sendMessage MirrorShrink)
    , ((myModMask .|. shiftMask, xK_k), sendMessage MirrorExpand)
    , ((myModMask .|. shiftMask, xK_h), sendMessage Shrink)
    , ((myModMask .|. shiftMask, xK_l), sendMessage Expand)
    , ((myModMask .|. shiftMask, xK_bracketright)
      , spawn $ "termonad --title " ++ floatingTerminal)

    , ((myModMask, xK_s), fixFullStack >> switchLayer)
    , ((myModMask, xK_f), sendMessage (Toggle FullScreen)
                         >> sendMessage (Toggle Tile))
    , ((myModMask .|. shiftMask, xK_f), sendMessage $ Toggle SIMPLEST)
    , ((myModMask, xK_e), test)
    ]


layoutDesc :: W.StackSet i (Layout Window) w s sd -> String
layoutDesc = description . W.layout . W.workspace . W.current


separateFloats :: (String -> Bool)
                  -- decide whether to separate by layout description
               -> W.StackSet i (Layout Window) Window s sd
               -> W.StackSet i (Layout Window) Window s sd
separateFloats p set =
  if null floats || not (p $ layoutDesc set) then set else
    flip W.modify' set $ \(W.Stack focus up down) ->
      let (upF,   upT)   = partition (flip elem floats) up
          (downF, downT) = partition (flip elem floats) down
      in  W.Stack focus (upF ++ downF) (downT ++ upT)
    where
      floats = M.keys $ W.floating set


fixFullStack :: X ()
fixFullStack = windows $ separateFloats $ \desc -> any ($ desc)
  [ isInfixOf "Full"
  , isInfixOf "Simplest"
  ]


tmpLog s = spawn $ "echo '" ++ s ++ "' > /tmp/xmonadUser.log"

logFocusedWindowInfo :: X ()
logFocusedWindowInfo = do
  let logFile = "/tmp/xmonadFocusedWindowInfo.log"
  XState { windowset = ws } <- get
  forM_ (W.peek ws) $ \id ->
    spawn ("xprop WM_CLASS WM_ICON_NAME -id " ++ show id ++ " >> " ++ logFile)

test :: X ()
test = do
  XState { windowset = old } <- get
  logFocusedWindowInfo
  let focused = W.peek old
      all     = W.index old
      ft      = partition (flip elem $ floats old) all
  liftIO $ writeFile "/tmp/xmonadUser.log" "test"
  -- tmpLog $ show focused ++ " : " ++ show all ++ " : " ++ show ft
  -- tmpLog $ layoutDesc old
    where
      floats :: W.StackSet i l Window s sd -> [Window]
      floats = M.keys . W.floating


myManageHookFloat = composeAll
  [ className =? "Gimp"                       --> doFloat
  , className =? "qjackctl"                   --> doFloat
  , className =? "Nitrogen"                   --> doFloat
  , className =? "Xsensors"                   --> doFloat
  , className =? "Simplescreenrecorder"       --> doFloat
  , title     =? "Stethoscope"                --> doFloat
  , title     =? "localhost Node Tree"        --> doFloat
  , title     =? "SynthDef browser"           --> doFloat
  , title     =? "another control panel"      --> doFloat
  , title     =? "Volume Control"             --> doFloat
  , title     =? "Input Method Configuration" --> doFloat
  , title     =? "vimiv"                      --> doFloat
  , title     =? "Select Font"                --> doFloat
  , stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog"
    --> doFloat
  , title =? floatingTerminal
    --> (doRectFloat $ W.RationalRect 0.6 0.7 0.4 0.3)
  , resource =? floatingTerminal
    --> (doRectFloat $ W.RationalRect 0.6 0.7 0.4 0.3)
  ]



myLayout
  = onWorkspace "1" (mkToggle (single Tile) fullLayouts)
  $ onWorkspace "3" (chooseSpace' $ mkToggle (single SIMPLEST) Full)
  $ onWorkspace "4" (mkToggle (single FullScreen) simplestFloat)
  $ mkToggle (single FullScreen) tileLayouts


chooseSpace  l = noBorders $ mySpacing l ||| l
chooseSpace' l = noBorders $ l ||| mySpacing l
fullLayouts    = chooseSpace $ mkToggle (single SIMPLEST) Full
tileLayouts    = noBorders
  $ myDecoration D tiled ||| myDecoration R (Mirror tiled)
                         ||| myDecoration D Circle


myDecoration d = decoration shrinkText myTheme (SideDecoration d)
               . mySpacing

myTheme = def { activeColor         = active
              , inactiveColor       = inactive
              , activeBorderColor   = active
              , inactiveBorderColor = inactive
              , activeTextColor     = active
              , inactiveTextColor   = inactive
              , decoWidth           = 40 -- (%)
              , decoHeight          = 4
              }


data SideDecoration a = SideDecoration Direction2D
  deriving (Show, Read)

instance Eq a => DecorationStyle SideDecoration a where
  describeDeco (SideDecoration d) = "SideDecoration " ++ show d

  shrink d (Rectangle _ _ dw dh) (Rectangle x y w h)
    | SideDecoration U <- d = Rectangle x (y + fi dh) w (h - dh)
    | SideDecoration R <- d = Rectangle x y (w - dw) h
    | SideDecoration D <- d = Rectangle x y w (h - dh)
    | SideDecoration L <- d = Rectangle (x + fi dw) y (w - dw) h

  pureDecoration d dw dh _ st _ (win, Rectangle x y w h)
    | win `elem` W.integrate st = Just $ case d of
        SideDecoration U -> Rectangle mx y wx h'
        SideDecoration R -> Rectangle (x + fi (w - h')) my h' wy
        SideDecoration D -> Rectangle mx (y + fi (h - h')) wx h'
        SideDecoration L -> Rectangle x my h' wy
    | otherwise = Nothing
    where
      fw w     = if win == W.focus st then w else 1
      f p l s  = if p >= 100 then (fw l, s) else
                 let l' = fw $ l * p `div` 100
                 in  (l', fi (l - l') `div` 2 + s)
      (wx, mx) = f dw w x
      (wy, my) = f dw h y
      h'       = if dh >= h then h else dh



data MyTransformers
  = FullScreen
  | Tile
  | SIMPLEST
  deriving (Read, Show, Eq, Typeable)

instance Transformer MyTransformers Window where
  transform FullScreen x k = k fullLayouts (const x)
  transform Tile       x k = k tileLayouts (const x)
  transform SIMPLEST   x k = k Simplest    (const x)


-- Retina
mySpacing = spacingRaw False (Border 7 7 4 4) True
                             (Border 7 7 8 8) True
-- not Retina
-- mySpacing = spacingRaw False (Border 9 9 3 3) True
--                              (Border 3 3 3 3) True

tiled = ResizableTall nmaster ratioInc ratio []
  where
    nmaster  = 1
    ratio    = 1/2
    ratioInc = 1/100

startup :: X ()
startup = do
  io $ putStrLn "Starting Xmonad"
  setDefaultCursor xC_left_ptr

floatingTerminal :: String
floatingTerminal = "floatingTerminal"



data BGFGLayout l1 l2 a = BGFG
  { background   :: (l1, Int)
  , foreground   :: (l2, Int)
  , inForeground :: Bool
  } deriving (Show)

instance (Default (l1 a), Default (l2 a))
         => Default (BGFGLayout (l1 a) (l2 a) a) where
  def = BGFG (def, 0) (def, 0) True

-- new
-- delete
-- send left
-- send right

instance (LayoutClass l1 a, LayoutClass l2 a)
         => LayoutClass (BGFGLayout (l1 a) (l2 a)) a where
  runLayout work@(W.Workspace _ _    Nothing)      _   = return ([], Nothing)
  runLayout work@(W.Workspace _ bgfg (Just stack)) rec = do
    let ws    = reverse (W.up stack) ++ W.focus stack : W.down stack
        diff  = length ws - snd (background bgfg) + snd (foreground bgfg)
        f b   = (\x -> if x < 0 then 0 else x) . if b then (+ diff) else id
        fgN   = f (inForeground bgfg)       $ snd $ foreground bgfg
        bgN   = f (not $ inForeground bgfg) $ snd $ background bgfg
    fg <- fst <$> runLayout (work { W.layout = fst $ foreground bgfg
                                  , W.stack  = undefined })
                            rec
    let ls = fg
    return (ls, Nothing)
