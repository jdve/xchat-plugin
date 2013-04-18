-----------------------------------------------------------------------------
--
-- Module      :  Network.IRC.XChat.Examples.CountDown
-- Copyright   :
-- License     :  GPL-2
--
-- Maintainer  :  sedrikov@gmail.com
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Network.IRC.XChat.Examples.CountDown (pluginInit, Memo) where

import Network.IRC.XChat.Plugin
import Data.List
import Data.Char

norm :: PriorityA -- the normal priority
norm = abstractPriority Norm

eatAll :: Eating
eatAll = Eating { eatPlugin = True, eatXChat = True }

eatNone :: Eating
eatNone = Eating { eatPlugin = False, eatXChat = False }

data Memo = Memo Int String (XChatHook Memo () ())
          | Wait

countCb :: XChatPlugin Memo -> () -> Memo -> IO (Eating, Memo)
countCb ph _ Wait =
  xChatPrint ph "Some error occured" >> return (eatAll, Wait)
countCb ph _ (Memo i ps hook) =
  let err = xChatPrint ph "Some error occured" >> return (eatAll, Wait)
  in
  if i < 0
  then err
  else xChatGetInfo ph "channel" >>= \ s ->
       case s of
         Nothing -> err
         Just s -> if i == 0
                   then xChatCommand ph ("MSG "++ s ++" "++ ps ++ ": end of countdown") >>
                        xChatUnhook ph hook (\ _ -> return ((), Wait)) >>
                        -- wow self unhooking!!
                        return (eatAll, Wait)
                   else xChatCommand ph ("MSG "++ s ++ " " ++ (show i)) >>
                        return (eatAll, Memo (i-1) ps hook)

parseInt :: Int -> String -> Int
parseInt acc [] = acc
parseInt acc (c:l) =
  let i = case c of
            '0' -> 0
            '1' -> 1
            '2' -> 2
            '3' -> 3
            '4' -> 4
            '5' -> 5
            '6' -> 6
            '7' -> 7
            '8' -> 8
            '9' -> 9
            _ -> (-9) * acc
  in parseInt (10*acc+i) l

startCb :: XChatPlugin Memo -> String -> String -> Memo -> IO (Eating, Memo)
startCb ph pseudo arg mem =
  let err = xChatPrint ph "Some error occured" >> return (eatAll, Wait)
  in
  case words arg of
    check : arg : _ ->
      if map toUpper check == "COUNTER"
      then xChatGetInfo ph "channel" >>= \ s ->
           case s of
             Nothing -> err
             Just s -> case mem of
                         Wait -> let i = parseInt 0 arg
                                 in
                                 if i <= 0
                                 then xChatCommand ph
                                       ("MSG "++ s ++" only positive numbers expected") >>
                                      return (eatAll, Wait)
                                 else xChatHookTimer 1000 ph (countCb ph)
                                       (\ _ -> return ((), Wait)) >>= \ hk ->
                                      return (eatAll, Memo i pseudo (snd hk))
                         Memo _ _ _ -> xChatCommand ph
                                        ("MSG "++ s ++" sorry, only one countdown at a time") >>
                                       return (eatAll, mem)
      else return (eatNone, mem)
    _ -> return (eatNone, mem)

pluginInit :: (Memo -> IO (XChatPlugin Memo)) -> IO PluginDescriptor
pluginInit fph =
  let pd = PluginDescriptor "Counter" "Ability to run countdowns" "0.1"
      noMemoryMgmt b = return ((),b)
  in
  do ph <- fph Wait
     xChatHookPrint "Channel Message"
      norm ph (\ l m -> case l of
                          pseudo : texte : _ ->
                            startCb ph pseudo texte m
                          _ -> return (eatNone, m)) noMemoryMgmt
     xChatHookCommand "Counter" norm (Just "Counter <time in seconds>") ph (startCb ph "")
                      noMemoryMgmt
     xChatPrint ph "Counter loaded successfully!\n"
     return pd
