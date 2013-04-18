-----------------------------------------------------------------------------
--
-- Module      :  XChat.Example
-- Copyright   :
-- License     :  GPL-2
--
-- Maintainer  :  sedrikov@gmail.com
-- Stability   :  experimental
-- Portability :
--
-- | A sample module, to test the plugin compiler
--
-----------------------------------------------------------------------------

module Network.IRC.XChat.Examples.AutoOp ( -- $doc
                                           pluginInit
                                         , NBool
) where

import Network.IRC.XChat.Plugin
-- import Data.List

{- $doc

Sample plugin

This simple plugin autoOps anyone who joins a channel you're in.
It also adds a new command \/AUTOOPTOGGLE, which can be used to
 turn the feature ON or OFF.
Every XChat plugin must define an xChatPluginInit function,
 this is the normal entry point. xChatPluginDeinit is optional.

This plugin is a traduction from the sample of
 the official XChat documentation.

The plugin has a 1 bit memory which tells if it is or not enabled,
rather than a global variable.

Here is the code:

@
norm :: PriorityA -- the normal priority
norm = abstractPriority Norm

eatNone :: Eating
eatNone = Eating { eatPlugin = False, eatXChat = False }

joinCb :: XChatPlugin Bool -> [String] -> Bool -> IO (Eating, Bool)
joinCb ph args enable =
  (if enable
   then xChatCommand ph (\"OP \" ++ (args !! 0))
   else return ()) >>
  return (eatNone, enable) -- This commands still needs to be managed

eatAll :: Eating
eatAll = Eating { eatPlugin = True, eatXChat = True }

autoOptToggleCb :: XChatPlugin Bool -> String -> Bool -> IO (Eating, Bool)
autoOptToggleCb ph _ enable =
  if enable
  then (xChatPrint ph \"AutoOping now disabled!\\n\" >> return (eatAll, False))
  else (xChatPrint ph \"AutoOping now enabled!\\n\" >> return (eatAll, True))
  -- we can eat the command, as we do not want XChat to execute it

pluginInit :: (Bool -> IO (XChatPlugin Bool)) -> IO PluginDescriptor
pluginInit fph =
  let pd = PluginDescriptor \"AutoOp\" \"Auto Op anyone that joins\" \"0.1\"
      noMemoryMgmt b = return ((),b)
  in
  do ph <- fph True
     xChatHookCommand \"AutoOpToggle\" norm
                      (Just \"Usage: AUTOOPTTOGGLE, Turns OFF\/ON Auto Oping\")
                      ph (autoOptToggleCb ph) noMemoryMgmt
     xChatHookPrint \"Join\" norm ph (joinCb ph) noMemoryMgmt
     xChatPrint ph \"AutoOpPlugin loaded successfully!\\n\"
     return pd
@

To test it, run:

@
hsxchat ...
@

-}

type NBool = Bool

norm :: PriorityA -- the normal priority
norm = abstractPriority Norm

eatNone :: Eating
eatNone = Eating { eatPlugin = False, eatXChat = False }

joinCb :: XChatPlugin Bool -> [String] -> Bool -> IO (Eating, Bool)
joinCb ph args enable =
  (if enable
   then xChatCommand ph ("OP " ++ (args !! 0))
   else return ()) >>
  return (eatNone, enable) -- This commands still needs to be managed

eatAll :: Eating
eatAll = Eating { eatPlugin = True, eatXChat = True }

autoOptToggleCb :: XChatPlugin Bool -> String -> Bool -> IO (Eating, Bool)
autoOptToggleCb ph _ enable =
  if enable
  then (xChatPrint ph "AutoOping now disabled!\n" >> return (eatAll, False))
  else (xChatPrint ph "AutoOping now enabled!\n" >> return (eatAll, True))
  -- we can eat the command, as we do not want XChat to execute it

pluginInit :: (NBool -> IO (XChatPlugin NBool)) -> IO PluginDescriptor
pluginInit fph =
  let pd = PluginDescriptor "AutoOp" "Auto Op anyone that joins" "0.1"
      noMemoryMgmt b = return ((),b)
  in
  do ph <- fph False
     xChatHookCommand "AutoOpToggle" norm
                      (Just "Usage: AUTOOPTTOGGLE, Turns OFF/ON Auto Oping")
                      ph (autoOptToggleCb ph) noMemoryMgmt
     xChatHookPrint "Join" norm ph (joinCb ph) noMemoryMgmt
     xChatPrint ph "AutoOpPlugin loaded successfully!\n"
     return pd
