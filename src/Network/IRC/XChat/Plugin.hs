{- Module      :  XChat.Plugin
   Copyright   :
   License     :  GNU/GPL

   Maintainer  :  sedrikov@gmail.com
   Stability   :  experimental
-}
{-# LANGUAGE ForeignFunctionInterface #-}

{-
TODO:

Check memory usage, ie.
newCstring should be freed and so on

Complete documentation
-}

module Network.IRC.XChat.Plugin ( -- $intro

                      -- * Functions
                      PluginDescriptor (..)
                    , XchatPlugin
                    , XChatPlugin
                    , xChatPluginInit
                      -- ** The Lists
                      -- $lists

                      -- *** Channels
                    , xChatGetChanList
                    , ChanFlgs (..)
                    , ChanType (..)
                    , Chan (..)
                      -- *** DCC
                    , xChatGetDccList
                    , DccStatus (..)
                    , DccType (..)
                    , Dcc (..)
                      -- *** Ignore
                    , xChatGetIgnList
                    , IgnFlgs (..)
                    , Ign (..)
                      -- *** Notify
                    , xChatGetNotifyList
                    , Notify (..)
                      -- *** Users
                    , xChatGetUserList
                    , User (..)
                      -- ** The hooks system
                      -- $hooks

                      -- *** Types
                    , XChatHook
                    , Eating (..)
                    , Hook
                    , PriorityC (..)
                    , PriorityA
                    , abstractPriority
                    , concretePriority
                    , Flags (..)
                      -- *** Hooks
                    , xChatHookCommand
                    , xChatHookServer
                    , xChatHookPrint
                    , xChatHookTimer
                    , xChatHookFd
                      -- *** Removing hooks
                    , xChatUnhook
                      -- ** Contexts
                      -- $contexts
                    , XChatContext
                    , xChatSetContext
                    , xChatFindContext
                    , xChatGetContext
                      -- ** Some other commands
                    , xChatPrint
                    , xChatCommand
                    , xChatNickcmp
                    , SettingResult (..)
                    , xChatGetPrefs
                    , xChatGetInfo
                    , xChatEmitPrint
                    , xChatSendModes
                    , StripRules (..)
                    , xChatStrip
                      -- ** Undocumented
                    , xChatPluginguiAdd
                    , xChatPluginguiRemove
                    , xChatGettext
) where
import Data.List          (reverse)
import Foreign            (peek, poke, shiftL, shiftR, (.|.), (.&.), testBit,
                           Int32, Int64, Ptr, FunPtr, Storable,
                           advancePtr, malloc, free, nullPtr, mallocArray)
import Foreign.C.Types    (CInt (..), CChar (..), CTime (..))
import Foreign.C.String   (CString (..), newCString, peekCString)
import Control.Monad      (unless)
import Data.Foldable      (foldlM)
import Data.IORef         (IORef (..), readIORef, writeIORef, newIORef)

{- $intro
   This is my first binding in Haskell (and in any other language btw),
   so I probably lack of good practice.

   I tried to do bindings for the XChat plugin system, but some
   functions were not documented (these are tagged [Undocumented]),
   those function are expect to have their signature changed once
   they become documented (and I understand what they truly are for).
   For these functions, I would gladly accept some enlightment of what
   they really do.

   For the other functions, I would also accept advice on how to improve
   the binding.

   There are also some lacking functions:

 - xChatPrintF   (special case of xChatPrint)

 - xChatCommandF (special case of xChatCommand)

 - xChatFree     (automatically called)

 - all functions on lists but the xChatGetList specialized in 5 versions

 To contact me: sed?rikov\@gma?il.com (remove the question marks)
 For more information, read the README.txt file
-}

cIntOfBools :: [a -> Bool] -> a -> CInt
cIntOfBools l a =
  let step i f = (shiftL (i :: Int32) 1) .|. (if f a then 1 else 0)
  in
  fromIntegral $! foldl step 0 (reverse l)

-- | The plugin descriptor
data PluginDescriptor = PluginDescriptor { pluginName :: String
                                         , pluginDescription :: String
                                         , pluginVersion :: String
                                         }

-- | The result of a setting request
data SettingResult = SetFailure
                   | SetString String
                   | SetBool Bool
                   | SetInt Int32

data XchatList = XchatList
data XchatContext = XchatContext
type XchatListHandle = Ptr XchatList
type XchatContextHandle = Ptr XchatContext


-- | the type of contexts
newtype XChatContext = XCC XchatContextHandle

{- It seems that the nullPtr is never used and that
 - the list is just empty string terminated
 - there is also a limit of 31 entries (+ a reserved one)
 - Since the stacked calls are limited to 31, I am not sure
 - being tail-recursive is very usefull here.
 -}
getArgs :: Ptr CString -> IO [String]
getArgs p =
  let getAllArgs 0 acc _ = return $! reverse acc
      getAllArgs n acc p = peek p >>= peekCString >>= \ s ->
                           if s == ""
                           then return $! reverse acc
                           else let m = n - 1
                                    q = advancePtr p 1
                                in
                                (seq m . seq q $
                                 getAllArgs m (s : acc) q)
  in
  getAllArgs 31 [] (advancePtr p 1)

putArgs :: [String] -> IO (Ptr CString, IO ())
putArgs l =
  let step (ptr, freer) str = newCString str >>= \ p ->
                              poke ptr p >>
                              return (advancePtr ptr 1, free p >> freer)
  in
  mallocArray (length l) >>= \ arr ->
  foldlM step (arr, return ()) l >>= \ res ->
  case res of
    (_, freer) -> return (arr, freer >> free arr)

getOptString :: Maybe String -> IO CString
getOptString Nothing  = return nullPtr
getOptString (Just s) = newCString s

freeOptString :: CString -> IO ()
freeOptString s = unless (s == nullPtr) $ free s

sideEffect :: IORef a -> (a -> IO (b, a)) -> IO b
sideEffect ptra trans = readIORef ptra >>= trans >>= \ p ->
                        writeIORef ptra (snd p) >> return (fst p)

-- -----------------------------------------------------------------------------

data XchatPlugin a = XchatPlugin
type XchatPluginHandle a = Ptr (XchatPlugin a)
{- | The type of plugins; it is associated to a memory
     which can be used and modified by the hooked callbacks.

     All the following functions needs a @'XChatPlugin'@ as
     argument. In fact it is a handle which needs to be initialized
     by the @init@ function, and will be freed by an optional @deinit@
     function.
-}
newtype XChatPlugin a = XCP (IORef a, XchatPluginHandle a)

{- | The plugin initializer. NEVER call it, it is only used by @hsxchat@.
-}
xChatPluginInit :: Ptr (XchatPlugin a) -> a -> IO (XChatPlugin a)
xChatPluginInit ph pa = newIORef pa >>= \ ra -> return $ XCP (ra, ph)

{- $hooks
   All @'XChatHook'@s returned can be passed to @'xChatUnhook'@.
   This is not mandatory, as they are automatically unhooked
   at unloading time. All hooks return a @'Hook' a b c@ type.

   The hook system is not exactly the one used in the original
   @C@\/@C++@ library. In the original library, some memory was
   attached to each hook, but that meant that shared memory
   had to be done through pointers. Now all functions of
   the original library are given the same plugin memory pointer,
   and this memory is attached to the plugin itself,
   so all hooking functions are only given a @'XChatPlugin'@
   (which internally contains a memory) and
   its memory is not explicitly given anymore at hooking time.
-}

data XchatHook a = XchatHook
type XchatHookHandle a = Ptr (XchatHook a)
{- | The type of hooks; it has three type arguments.

    - The first is the type of the plugin memory

    - The second one is the type returned at hook creation,
      it can be used to restore the memory at unhooking time,
      or to print some debugging information.

    - The third one is the type returned at unhooking time,
      it can be used to display debugging information.

-}
newtype XChatHook a b c = XCH (XchatHookHandle a)

{- | The way callbacks are managed; a callback function can be eat an event,
     that is make the event unprocessable, either to XChat or
     to the other plgins.
-}
data Eating = Eating { eatXChat :: Bool
                     , eatPlugin :: Bool
                     }

-- a boilerplate function for the callbacks
retHook :: IORef a -> (a -> IO (Eating, a)) -> IO CInt
retHook pa cb = sideEffect pa cb >>= \ i ->
                return (cIntOfBools [eatXChat, eatPlugin] i)

{- | The returned type of all hooking functions;
     if /f/ is a @'Hook' /a/ /b/ /c/ /d/@ hooking function, then:

    @
      /f/ /ph/ /cb/ /init/
    @

    means that a new @'XChatHook' /a/ /b/ /c/@ hook
    using the callback @/cb/@ function is
    created for the @/ph/@ plugin; the hooking modifies
    the @/a/@ plugin memory according to @/init/@,
    and returns the @/b/@ returned by @/init/@ as well as the
    created hook.

   The callback function itself is a function which takes some
   @/d/@ data, the @/a/@ plugin memory at the moment when the
   callback function is called and returns how the event which
   triggered the callback call is eaten as well as the new
   @/a/@ plugin memory.
-}
type Hook a b c d = XChatPlugin a -> (d -> a -> IO (Eating, a)) ->
                    (a -> IO (b, a)) -> IO (b, XChatHook a b c)

-- a boilerplate function for hooks introduction
hook :: (Ptr () -> IO (XchatHookHandle a)) -> IORef a -> (a -> IO (b, a)) ->
          IO (b, XChatHook a b c)
hook ioXCHH refa init =
  ioXCHH nullPtr >>= \ r -> sideEffect refa init >>= \ b -> return (b, XCH r)

-- | a concrete type to define the priority of a command
data PriorityC = Highest      -- ^ Highest priority (127)
               | High         -- ^ High priority (64)
               | Norm         -- ^ Normal priority (0)
               | Low          -- ^ Low priority (-64)
               | Lowest       -- ^ Lowest priority (-128)
               | Custom Int32 -- ^ Custom, is normalized when cast to
                              -- 'PriorityA'

-- | an abstract type to define the priority of a command
newtype PriorityA = P Int32 deriving (Eq, Ord)

-- | to get a (normalized) abstract priority from a concrete one
abstractPriority :: PriorityC -> PriorityA
abstractPriority Highest    = P  127
abstractPriority High       = P   64
abstractPriority Norm       = P    0
abstractPriority Low        = P (-64)
abstractPriority Lowest     = P (-128)
abstractPriority (Custom i)
                | i < -128  = P (-128)
                | i >  127  = P   127
                | otherwise = P i

-- | to get a concrete priority from an abstract one
concretePriority :: PriorityA -> PriorityC
concretePriority (P i)
  | i <  -128 = Lowest
  | i >   127 = Highest
  | i ==  -64 = Low
  | i ==    0 = Norm
  | i ==   64 = High
  | otherwise = Custom i

-- | flags for file descriptors
data Flags = Flags { fgRead :: Bool
                   , fgWrite :: Bool
                   , fgExn :: Bool
                   , fgNsock :: Bool
                   }

-- | flags on how to strip rules
data StripRules = StripRules { noColor :: Bool
                             , noAttribute :: Bool
                             }

cIntOfFlag :: Flags -> CInt
cIntOfFlag = cIntOfBools [fgRead, fgWrite, fgExn, fgNsock]

flagOfCInt :: CInt -> Flags
flagOfCInt c =
  Flags { fgRead  = testBit c 0
        , fgWrite = testBit c 1
        , fgExn   = testBit c 2
        , fgNsock = testBit c 3
        }

-- a bundle of functions used for wrapping
type Cb a = a -> IO (FunPtr a)
type Cb1 = Ptr CString -> Ptr CString -> Ptr () -> IO CInt
foreign import ccall "wrapper" wrap1 :: Cb Cb1
type Cb2 = Ptr CString -> Ptr () -> IO CInt
foreign import ccall "wrapper" wrap2 :: Cb Cb2
type Cb3 = Ptr () -> IO CInt
foreign import ccall "wrapper" wrap3 :: Cb Cb3
type Cb4 = CInt -> CInt -> Ptr () -> IO CInt
foreign import ccall "wrapper" wrap4 :: Cb Cb4

-- -----------------------------------------------------------------------------

foreign import ccall "xchat-plugin-hack.h xchat_hook_command"
 xChatHookCommandFFI
 :: XchatPluginHandle a -> CString -> CInt -> FunPtr Cb1 -> CString ->
     Ptr () -> IO (XchatHookHandle a)
{- |

@
'xChatHookCommand' /cmd/ /pri/ /help/
@

/Description:/

Hooking to the @\//cmd/@ input box command at priority @/pri/@ with an
optional @/help/@ message.

To capture text without a \'@\/@\' at the start (non-commands),
you may hook a special name of \"\" as in:

@
let eatAll           = 'Eating' { 'eatXChat' = 'True', 'eatPlugin' = 'True' }
    startWithYou s a = 'xChatPrint' p (\"you: \"'++'s) '>>' 'return' (eatAll, a)
in  'xChatHookCommand' \"\" 'Norm' 'Nothing' p startWithYou ()
@

which automatically adds \"@you: @\" at the beginning of each sentence you type
(and is undocumented as the help message argument is @'Nothing'@).

Commands hooked that begin with a period (\'.\') will be hidden in
@\/HELP@ and @\/HELP -l@.

/Arguments:/

 [cmd]  The command (without the forward slash) with some special
        treatment if it is the empty string or if it begins with a dot

 [pri]  Priority of the hook, you should probably use 'Norm'

 [help] Optionnal help message, displayed at @\/HELP /cmd/@ command

/Callback Function Main Argument:/

The callback function expects a string containing all the arguments passed
to the command.
-}
xChatHookCommand
 :: String -> PriorityA -> Maybe String -> Hook a b c String
xChatHookCommand com (P pri) help (XCP (pa, ph)) cb init =
  let w _ s _ = peek (advancePtr s 1) >>= peekCString >>= retHook pa . cb
      prio     = fromIntegral pri
  in
  newCString com >>= \ com ->
  getOptString help >>= \ help ->
  wrap1 w >>= \ cb ->
  hook (xChatHookCommandFFI ph com prio cb help) pa init >>= \ res ->
  freeOptString help >>
  free com >>
  return res

foreign import ccall "xchat-plugin-hack.h xchat_hook_server"
 xChatHookServerFFI
 :: XchatPluginHandle a -> CString -> CInt -> FunPtr Cb1 ->
     Ptr () -> IO (XchatHookHandle a)
{- |

/Description:/

@
'xChatHookServer' /ev/ /pri/
@

Hooking to the server event @/ev/@ at priority @/pri/@.

To capture all server events, use @\"RAW LINE\"@.

/Arguments:/

 [ev]   The server event to be captured

 [pri]  Priority of the hook, you should probably use 'Norm'

/Callback Function Main Argument:/

The callback function expects a string containing all the arguments of
the captured server event.
-}
xChatHookServer :: String -> PriorityA -> Hook a b c String
xChatHookServer ev (P pri) (XCP (pa, ph)) cb init =
  let w _ s _ = peek (advancePtr s 1) >>= peekCString >>= retHook pa . cb
      prio     = fromIntegral pri
  in
  newCString ev >>= \ ev ->
  wrap1 w >>= \ cb ->
  hook (xChatHookServerFFI ph ev prio cb) pa init >>= \ res ->
  free ev >>
  return res

foreign import ccall "xchat-plugin-hack.h xchat_hook_print"
 xChatHookPrintFFI
 :: XchatPluginHandle a -> CString -> CInt -> FunPtr Cb2 -> Ptr () ->
     IO (XchatHookHandle a)
{- |

/Description:/

@
'xChatHookPrint' /prev/ /pri/
@

Hooking to the print event @/prev/@ at priority @/pri/@.

Available events are those in \"Advanced > Text Events\" plus these ones:

 [@\"Open Context\"@] Called when a new 'XChatContext' is created.

 [@\"Close Context\"@] Called when a xchat_context pointer is closed.

 [@\"Focus Tab\"@] Called when a tab is brought to front.

 [@\"Focus Window\"@] Called a toplevel window is focused,
                      or the main tab-window is focused by the window manager.

 [@\"DCC Chat Text\"@] Called when some text from a DCC Chat arrives.
                       It provides these arguments for the callback function:
                       @[_, Address, Port, Nick, The Message]@

 [@\"Key Press\"@] Called when some keys are pressed in the input-box.
                   It provides these arguments for the callback function:
                   @[_, Key Value, State Bitfield (shift, capslock, alt),
                     String version of the key, Length of the string
                     (may be 0 for unprintable keys)]@

/Arguments:/

 [prev] The print event to be captured

 [pri]  Priority of the hook, you should probably use 'Norm'

/Callback Function Main Argument:/

The callback function expects a list of strings containing all the arguments of
the captured print event.
-}
xChatHookPrint :: String -> PriorityA -> Hook a b c [String]
xChatHookPrint ev (P pri) (XCP (pa, ph)) cb init =
  let prio = fromIntegral pri in
  newCString ev >>= \ ev ->
  wrap2 (\ s _ -> getArgs s >>= \ l -> retHook pa (cb l)) >>= \ cb ->
  hook (xChatHookPrintFFI ph ev prio cb) pa init >>= \ res ->
  free ev >>
  return res

foreign import ccall "xchat-plugin-hack.h xchat_hook_timer"
 xChatHookTimerFFI
 :: XchatPluginHandle a -> CInt -> FunPtr Cb3 -> Ptr () ->
     IO (XchatHookHandle a)
{- |

/Description:/

@
'xChatHookTimeout' /timeout/
@

Hooking to call a function every @/timeout/@ milliseconds.

/Arguments:/

 [timeout] The time(ms) to wait before the next triggering of the callback.

/Callback Function Main Argument:/

The callback function expects just a unit type.
-}
xChatHookTimer
 :: Int32 -> Hook a b c ()
xChatHookTimer to (XCP (pa, ph)) cb init =
  let tmo = fromIntegral to in
  wrap3 (\ _ -> retHook pa (cb ())) >>= \ cb ->
  hook (xChatHookTimerFFI ph tmo cb) pa init

foreign import ccall "xchat-plugin-hack.h xchat_hook_fd"
 xChatHookFdFFI
 :: XchatPluginHandle a -> CInt -> CInt -> FunPtr Cb4 -> Ptr () ->
     IO (XchatHookHandle a)
{- |

/Description:/

@
'xChatHookFd' /fd/ /flgs/
@

Hooking to the file descriptor @/fd/@ with flags @/flgs/@.
The callback function is called every time the file descriptor is available
to an action described by the flags.

/Arguments:/

 [fd] The file descriptor or socket

 [flgs] The flags of the file descriptor

/Callback Function Main Argument:/

The callback function expects a file descriptor and a flag (that may be
removed in a newer version).
-}
xChatHookFd :: CInt -> Flags -> Hook a b c (CInt, Flags)
xChatHookFd (CInt fd) f (XCP (pa, ph)) cb init =
  wrap4 (\ fd f _ -> retHook pa (cb (fd, flagOfCInt f))) >>= \ cb ->
  hook (xChatHookFdFFI ph (CInt fd) (cIntOfFlag f) cb) pa init

foreign import ccall "xchat-plugin-hack.h xchat_unhook"
 xChatUnhookFFI
 :: XchatPluginHandle a -> XchatHookHandle a -> IO (Ptr a)
{- |

/Description:/

@
'xChatUnhook' /ph/ /hook/ /restore/
@

Unhooking of the given @/hook/@.
According to the xchat plugin documentation,
hooks are automatically removed at deinit time.
But you may wish for some reason to hook or unhook
dynamically some function.
There is an argument that allows you to modify the
memory at unhook time. For example, if you have a counter,
your memory could be a @'Maybe' 'Int'@. When you hook, you may want
to put it @'Just' 0@ and when you unhook to put it back to @'Nothing'@.

/Arguments:/

 [ph] The plugin handle of which we want to unhook.

 [hook] The hook to remove from the plugin.

 [restore] The function to be called on the memory of the plugin.

-}
xChatUnhook
 :: XChatPlugin a -> XChatHook a b c -> (a -> IO (c, a)) -> IO c
xChatUnhook (XCP (pa, ph)) (XCH ha) deinit =
  xChatUnhookFFI ph ha >> sideEffect pa deinit

foreign import ccall "xchat-plugin-hack.h xchat_print"
 xChatPrintFFI :: XchatPluginHandle a -> CString -> IO ()
{- |

/Description:/

@
'xChatPrint' /ph/ /text/
@

Displays some text in the xchat window.

/Arguments:/

 [ph] The plugin handle which manages the printing.

 [text] The text to display. May contain mIRC color codes.

-}
xChatPrint :: XChatPlugin a -> String -> IO ()
xChatPrint (XCP (_, ph)) s =
  newCString s >>= \ p ->
  xChatPrintFFI ph p >>
  free p

foreign import ccall "xchat-plugin-hack.h xchat_command"
 xChatCommandFFI :: XchatPluginHandle a -> CString -> IO ()
{- |

/Description:/

@
'xChatCommand' /ph/ /cmd/
@

Executes a command as if it were typed in xchat's input box.

/Arguments:/

 [ph] The plugin handle which manages the command.

 [text] The command to execute without the heading \'\/\'.

-}
xChatCommand :: XChatPlugin a -> String -> IO ()
xChatCommand (XCP (_, ph)) s =
  newCString s >>= \ s ->
  xChatCommandFFI ph s >>
  free s

{- $contexts
   Contexts are mainly a tab+window pair.
   I do not know more as the original documentation is rather sparse on it.

   You have 3 functions on contexts (find, get and set).

   The @'xChatHookPrint'@ can detect opening and closing of contexts.
-}
foreign import ccall "xchat-plugin-hack.h xchat_set_context"
 xChatSetContextFFI
 :: XchatPluginHandle a -> XchatContextHandle -> IO CInt
{- |

/Description:/

@
'xChatSetContext' /ph/ /ctx/
@

Changes the current context.

/Arguments:/

 [ph] Plugin handle whose context is to be changed.

 [ctx] Context (given by @'xChatGetContext'@ or @'xChatFindContext'@).

/Returns:/

 @'True'@ if successful, @'False'@ else.

-}
xChatSetContext :: XChatPlugin a -> XChatContext -> IO Bool
xChatSetContext (XCP (_, ph)) (XCC ctx) =
  xChatSetContextFFI ph ctx >>= \ i ->
  return (i == 1) -- the documentation say that the result must be 0 or 1

foreign import ccall "xchat-plugin-hack.h xchat_find_context"
 xChatFindContextFFI
 :: XchatPluginHandle a -> CString -> CString -> IO XchatContextHandle
{- |

/Description:/

@
'xChatFindContext' /ph/ /servname/ /channel/
@

Finds a context based on a channel and servername.

If @/servname/@ is @'Nothing'@, it finds the channel (or query) by the
given name in the same server group as the current context. If that doesn't
exists then find any by the given name.

If channel is @'Nothing'@, it finds the front-most tab\/window of the given servname.

/Arguments:/

 [ph] Plugin handle.

 [servname] Servername.

 [channel] Channelname.

/Returns:/

 Context (for use with xChatSetContext).

-}
xChatFindContext :: XChatPlugin a -> Maybe String -> Maybe String ->
                     IO XChatContext
xChatFindContext (XCP (_, ph)) serv chan =
  getOptString serv >>= \ t1 ->
  getOptString chan >>= \ t2 ->
  fmap XCC $ xChatFindContextFFI ph t1 t2 >>= \ res ->
  freeOptString t1 >>
  freeOptString t2 >>
  return res

foreign import ccall "xchat-plugin-hack.h xchat_get_context"
 xChatGetContextFFI
 :: XchatPluginHandle a -> IO XchatContextHandle
{- |

/Description:/

@
'xChatGetContext' /ph/
@

Get the current context.

/Arguments:/

 [ph] Plugin handle whose context is to be taken.

/Returns:/

 The current context.

-}
xChatGetContext :: XChatPlugin a -> IO XChatContext
xChatGetContext (XCP (_, ph)) =
  fmap XCC $ xChatGetContextFFI ph

foreign import ccall "xchat-plugin-hack.h xchat_nickcmp"
 xChatNickcmpFFI :: XchatPluginHandle a -> CString -> CString -> IO CInt
{- |

/Description:/

@
'xChatNickcmp' /ph/ /s1/ /s2/
@

Performs a nick name comparision, based on the current server
connection. This might be a RFC1459 compliant string compare, or plain ascii
(in the case of DALNet). Use this to compare channels and nicknames. The
function works the same way as strcasecmp.

/Quote from RFC1459:/

    Because of IRC's scandanavian origin, the characters {}| are considered to
    be the lower case equivalents of the characters \[\]\\, respectively.
    This is a critical issue when determining the equivalence of two nicknames.

/Arguments:/

 [ph] Plugin handle whose context is to be taken.

 [s1] 1st string to compare

 [s2] 2nd string to compare

/Returns:/

 The comparison of the two strings.

-}
xChatNickcmp :: XChatPlugin a -> String -> String -> IO Ordering
xChatNickcmp (XCP (_, ph)) s1 s2 =
  newCString s1 >>= \ t1 ->
  newCString s2 >>= \ t2 ->
  xChatNickcmpFFI ph t1 t2 >>= \ i ->
  free t1 >>
  free t2 >>
  return (if i < 0 then LT else if i > 0 then GT else EQ)

foreign import ccall "xchat-plugin-hack.h xchat_get_info"
 xChatGetInfoFFI
 :: XchatPluginHandle a -> CString -> IO CString
{- |

/Description:/

@
'xChatGetInfo' /ph/ /info/
@

Returns information based on your current context.

/Arguments:/

 [ph] Plugin handle.

 [id] ID of the information you want. Currently supported IDs are
      (case sensitive):

-  [away]         away reason or @'Nothing'@ if you are not away.

-  [channel]      current channel name.

-  [charset]      character-set used in the current context.

-  [event_text]   text event format string for name.

-  [host]         real hostname of the server you connected to.

-  [inputbox]     the input-box contents, what the user has typed.

-  [libdirfs]     library directory. e.g. \/usr\/lib\/xchat. The same
                 directory used for auto-loading plugins.
                 This string isn't necessarily UTF-8, but local file
                 system encoding.

-  [modes]        channel modes, if known, or @'Nothing'@.

-  [network]      current network name or @'Nothing'@.

-  [nick]         your current nick name.

-  [nickserv]     nickserv password for this network or @'Nothing'@.

-  [server]       current server name (what the server claims to be).
                 @'Nothing'@ if you are not connected.

-  [topic]        current channel topic.

-  [version]      xchat version number.

-  [win_ptr]      native window pointer. Unix: (GtkWindow *) Win32: HWND.

-  [win_status]   window status: \"active\", \"hidden\" or \"normal\".

-  [xchatdir]     xchat config directory, e.g.: \/home\/user\/.xchat2 This
                 string is encoded in UTF-8, which means you _should_
                 convert it to \"locale\" encoding before using functions
                 like open() or OpenFile(). For best Unicode support on
                 Linux, convert this string using g_filename_from_utf8 and
                 on Windows convert this string to UTF-16LE (wide) and use
                 OpenFileW() etc.

-  [xchatdirfs]   xchat config directory, e.g.: \/home\/user\/.xchat2.
                 This string is encoded in local file system
                 encoding, making it ideal for direct use with functions
                 like open() or OpenFile(). For real Unicode support on
                 Windows, it's best not to use xchatdirfs, but xchatdir
                 instead.

/Returns:/

 A string of the requested information, or @'Nothing'@.
-}
xChatGetInfo :: XChatPlugin a -> String -> IO (Maybe String)
xChatGetInfo (XCP (_, ph)) s =
  newCString s >>= \ p ->
  xChatGetInfoFFI ph p >>= \ c ->
  free p >>
  if c == nullPtr
  then return Nothing
  else fmap Just $ peekCString c

foreign import ccall "xchat-plugin-hack.h xchat_get_prefs"
 xChatGetPrefsFFI
 :: XchatPluginHandle a -> CString -> Ptr CString -> Ptr CInt -> IO CInt
{- |

/Description:/

@
'xChatGetPrefs' /ph/ /pref/
@

Provides xchat's setting information
(that which is available through the \"\/set\" command).
A few extra bits of information are available that
don't appear in the \"\/set list\", currently they are:

 [state_cursor] Current input-box cursor position (characters, not bytes).

 [id] Unique server id.

/Arguments:/

 [ph] Plugin handle.

 [pref] Setting name required.

/Returns:/

 A failure, a @'String'@, a @'Bool'@ or an @'Int32'@ according to
 the @'SettingResult'@ case.

-}
xChatGetPrefs :: XChatPlugin a -> String -> IO SettingResult
xChatGetPrefs (XCP (_, ph)) s =
  newCString s >>= \ c ->
  malloc >>= \ sptr ->
  malloc >>= \ iptr ->
  xChatGetPrefsFFI ph c sptr iptr >>= \ i ->
  (case i of 1 -> peek sptr >>= fmap SetString . peekCString
             2 -> fmap (SetInt . fromIntegral) (peek iptr)
             3 -> peek iptr >>= \ i -> return . SetBool $ (i /= 0)
             otherwise -> return SetFailure) >>= \ res ->
  free c >>
  free sptr >>
  free iptr >>
  return res

foreign import ccall "xchat-plugin-hack.h xchat_emit_print0"
 xChatEmitPrintFFI0
 :: XchatPluginHandle a -> CString ->
     IO CInt
foreign import ccall "xchat-plugin-hack.h xchat_emit_print1"
 xChatEmitPrintFFI1
 :: XchatPluginHandle a -> CString ->
     CString -> IO CInt
foreign import ccall "xchat-plugin-hack.h xchat_emit_print2"
 xChatEmitPrintFFI2
 :: XchatPluginHandle a -> CString ->
     CString -> CString -> IO CInt
foreign import ccall "xchat-plugin-hack.h xchat_emit_print3"
 xChatEmitPrintFFI3
 :: XchatPluginHandle a -> CString ->
     CString -> CString -> CString -> IO CInt
foreign import ccall "xchat-plugin-hack.h xchat_emit_print4"
 xChatEmitPrintFFI4
 :: XchatPluginHandle a -> CString ->
     CString -> CString -> CString -> CString -> IO CInt
{- |

/Description:/

@
'xChatEmitPrint' /ph/ /ev/ /args/
@

Generates a print event.
This can be any event found in the Preferences > Advanced > Text Events window.
The @/args/@ are the arguments of the event.
Special care should be taken when calling this
function inside a print callback (from @'xChatHookPrint'@),
as not to cause endless recursion.

/Arguments:/

 [ph] Plugin handle.

 [ev] Text event to print.

 [args] Arguments of the event to print.

/Returns:/

 @'True'@ in case of success @'False'@ else.

/Example:/

@
'xChatEmitPrint' \"Channel Message\" [\"John\", \"Hi there\", \"\@\"]
@

-}
xChatEmitPrint :: XChatPlugin a -> String -> [String] -> IO Bool
xChatEmitPrint (XCP (_, ph)) ev l =
-- xxxx
  newCString ev >>= \ s ->
  mapM newCString l >>= \ l ->
  fmap (== 1) (case l of
                []                    -> xChatEmitPrintFFI0 ph s
                [a0]                  -> xChatEmitPrintFFI1 ph s a0
                [a0, a1]              -> xChatEmitPrintFFI2 ph s a0 a1
                [a0, a1, a2]          -> xChatEmitPrintFFI3 ph s a0 a1 a2
                a0 : a1 : a2 : a3 : _ -> xChatEmitPrintFFI4 ph s a0 a1 a2 a3
              ) >>= \ res ->
  free s >>
  mapM free l >> -- probably not the best thing to do...
  return res

foreign import ccall "xchat-plugin-hack.h xchat_send_modes"
 xChatSendModesFFI
 :: XchatPluginHandle a -> Ptr CString -> CInt -> CInt -> CChar -> CChar ->
     IO ()
{- |

/Description:/

@
'xChatSendModes' /ph/ /str_list/ /mpl/ /sgn/ /mode/
@
 Sends a number of channel mode changes to the current channel.
 For example, you can Op a whole group of people in one go.
 It may send multiple MODE lines if the request doesn't fit on one.
 Pass @'Nothing'@ for /mpl/ to use the current server's maximum possible.
 This function should only be called while in a channel context.

/Arguments:/

 [ph] The plugin handle

 [str_list] The targets

 [mpl] The number of modes per line

 [sgn] The sign (@True@ is \'+\', @False@ is \'-\')

 [mode] The mode char, e.g. \'o\' for Ops

/Example:/

@'xChatSendModes' \"Alice\":\"Bob\":[] 3 True \'o\'@
-}
xChatSendModes :: XChatPlugin a -> [String] -> Maybe Int32 -> Bool -> Char -> IO ()
xChatSendModes (XCP (_, ph)) l n b c =
  let mpp = case n of { Nothing -> 0 ; Just i -> fromIntegral i } in
  putArgs l >>= \ (arr, freer) ->
  let len = fromIntegral $ length l in
  let sign = toEnum $ fromEnum (if b then '+' else '-') in
  xChatSendModesFFI ph arr len mpp sign
                    (toEnum $ fromEnum c) >>
  freer

foreign import ccall "xchat-plugin-hack.h xchat_free"
 xChatFreeFFI
 :: XchatPluginHandle a -> CString -> IO ()
foreign import ccall "xchat-plugin-hack.h xchat_strip"
 xChatStripFFI
 :: XchatPluginHandle a -> CString -> CInt -> CInt -> IO CString
{- |

/Description:/

@
'xChatStrip' /str/ /rules/
@

Strips mIRC color codes and\/or text attributes (bold, underlined etc)
from the given string and returns a new string.
The original function had an unused plugin handle.

/Arguments:/

 [str] The string to strip

 [rules] The description of the plugin to add
-}
xChatStrip :: String -> StripRules -> IO String
xChatStrip s r =
  newCString s >>= \ ps ->
  xChatStripFFI nullPtr ps (fromIntegral $ length s)
                (cIntOfBools [noColor, noAttribute] r) >>= \ cs ->
  peekCString cs >>= \ s ->
  xChatFreeFFI nullPtr cs >>
  free ps >>
  return s

foreign import ccall "xchat-plugin-hack.h xchat_plugingui_add"
 xChatPluginguiAddFFI
 :: XchatPluginHandle a -> CString -> CString -> CString -> CString ->
    Ptr () -> IO (XchatPluginHandle a)
{- |

[Undocumented]

/Description:/

@
'xChatPluginguiAdd' /ph/ /filename/ /pdesc/
@

Add of a new GUI plugin to the list of the current plugins.
Due to lack of documentation, it is not further documented.
In the original source code, such added plugins are tagged \'fake\'.
It seems that beside their name, file name, version, description and position
in the list, there is no memory allocation. Furthermore, the original code
had an extra unused argument.

/Arguments:/

 [ph] The plugin handle to be used by default if the USE_PLUGIN directive
      was not given at compile time for XChat, if the USE_PLUGIN directive
      was provided, a new plugin handle is created, with the data of
      [ph] passed by

 [filename] The path name of the file containing the plugin to add

 [pdesc] The description of the plugin to add
-}
xChatPluginguiAdd :: XChatPlugin a -> String -> PluginDescriptor -> IO (XChatPlugin a)
xChatPluginguiAdd (XCP (a, ph)) flnm d =
  newCString flnm                  >>= \ fn ->
  newCString (pluginName        d) >>= \  n ->
  newCString (pluginDescription d) >>= \ ds ->
  newCString (pluginVersion     d) >>= \  v ->
  xChatPluginguiAddFFI ph fn n ds v nullPtr >>= \ pph ->
  free fn >>
  free n >>
  free ds >>
  free v >>
  return (XCP (a, pph))

foreign import ccall "xchat-plugin-hack.h xchat_plugingui_remove"
 xChatPluginguiRemoveFFI
 :: XchatPluginHandle a -> XchatPluginHandle b -> IO ()
{- |

[Undocumented]

/Description:/

@
'xChatPluginguiRemove' /ph/
@

The counterpart of @'xChatPluginguiAdd'@ function.
So it is used to remove \'fake\' plugins. Once again, one of the arguments
is unused in the original source code, so I removed it.

/Arguments:/

 [ph] The plugin handle to be removed
-}
xChatPluginguiRemove :: XChatPlugin a -> IO ()
xChatPluginguiRemove (XCP (_, ph)) = xChatPluginguiRemoveFFI nullPtr ph

foreign import ccall "xchat-plugin-hack.h xchat_gettext"
 xChatGettextFFI
 :: XchatPluginHandle a -> CString -> IO CString
{- |

[Undocumented]

/Description:/

@
'xChatGettext' /str/
@

Converts a string to its internal XChat representation.
I automatically free it to avoid memory leak, although as
I don't know what it really does, it may be a bad idea.
The original code had an unused plugin handle.

/Arguments:/

 [str] The string to convert
-}
xChatGettext :: String -> IO String
xChatGettext s =
  newCString s >>= \ cs ->
  xChatGettextFFI nullPtr cs >>= \ ds ->
  peekCString ds >>= \ t ->
  free ds >>
  free cs >>
  return t
-- -----------------------------------------------------
{- $lists
   All /XChat/ informations are stored in lists.
   There are 5 lists, each of them having its own section for
   a further description.

   To get access to such a list, you have to request it from /XChat/,
   using the provided function. There is no magic you have to get the list
   each time you have get informations, since it may have changed since the last
   time.
-}

data ChanFlgs = ChanFlgs { connected  :: Bool -- ^ Already connected
                         , connecting :: Bool -- ^ Connecting in progress
                         , away       :: Bool -- ^ You are away
                         , logged     :: Bool -- ^ Login complete
                         , whox       :: Bool -- ^ Has WHOX (ircu)
                         , idmsg      :: Bool -- ^ Has IDMSG (FreeNode)
                         , jPmsg      :: Bool -- ^ Hide Join\/Part messages
                         -- unused flag not figuring in the ChanFlgs
                         , beep       :: Bool -- ^ Beep on message
                         , blinkTray  :: Bool -- ^ Blink tray
                         , blinkTask  :: Bool -- ^ Blink task bar
                         }

chanFlgsOfInt :: Int32 -> ChanFlgs
chanFlgsOfInt i =
  ChanFlgs               { connected  = testBit i 0
                         , connecting = testBit i 1
                         , away       = testBit i 2
                         , logged     = testBit i 3
                         , whox       = testBit i 4
                         , idmsg      = testBit i 6
                         , jPmsg      = testBit i 6
                         -- unused flag not figuring in the ChanFlgs
                         , beep       = testBit i 8
                         , blinkTray  = testBit i 9
                         , blinkTask  = testBit i 10
                         }

data ChanType = ChanServer
              | ChanChannel
              | ChanDialog

chanTypeOfInt :: Int32 -> ChanType
chanTypeOfInt c = case c of
                    0 -> ChanServer
                    1 -> ChanChannel
                    _ -> ChanDialog

data Chan = Chan { cChannel      :: String   -- ^ Channel or query name
                 , cChantypes    :: String   -- ^ Channel type e.g. \"#!&\"
                 , cContext      :: XChatContext -- ^ Context of the channel
                 , cFlags        :: ChanFlgs -- ^ Server\/Channel bits
                 , cId           :: Int32    -- ^ Unique server ID
                 , cLag          :: Int32    -- ^ Lag in milliseconds
                 , cMaxmodes     :: Int32    -- ^ Maximum modes per line
                 , cNetwork      :: String   -- ^ Network name of the channel
                 , cNickprefixes :: String   -- ^ Nickname prefixes e.g. \"\@+\"
                 , cNickmodes    :: String   -- ^ Nickname mod chars e.g. \"ov\"
                 , cQueue        :: Int32    -- ^ Number of bytes in send-queue
                 , cServer       :: String   -- ^ Server name of the channel
                 , cType         :: ChanType -- ^ Type of context
                 , cUsers        :: Int32    -- ^ Number of users in the channel
                 }

data DccStatus = DccQueued
               | DccActive
               | DccFailed
               | DccDone
               | DccConnecting
               | DccAborted

dccStatusOfInt :: Int32 -> DccStatus
dccStatusOfInt c = case c of
                     0 -> DccQueued
                     1 -> DccActive
                     2 -> DccFailed
                     3 -> DccDone
                     4 -> DccConnecting
                     _ -> DccAborted

data DccType = DccSend
             | DccReceive
             | DccChatRecv
             | DccChatSend

dccTypeOfInt :: Int32 -> DccType
dccTypeOfInt c = case c of
                   0 -> DccSend
                   1 -> DccReceive
                   2 -> DccChatRecv
                   _ -> DccChatSend

data Dcc = Dcc { dAddress32 :: Int32
                 -- ^ ipv4 address of remote user (dunno how to have ipv6)
               , dCps       :: Int32
                 -- ^ Bytes per seconds
               , dDestfile  :: String
                 -- ^ Destination full pathname
               , dFile      :: String
                 -- ^ File name
               , dNick      :: String
                 -- ^ Nickname of the person who the file is from\/to (Receive\/Send mode)
               , dPort      :: Int32
                 -- ^ TCP port number
               , dPos       :: Int32
                 -- ^ Bytes send\/received up to now for the current transfert
               , dResume    :: Int32
                 -- ^ Offset of file from which it is resumed (0 if not resumed)
               , dSize      :: Int64
                 -- ^ File size in bytes
               , dStatus    :: DccStatus
                 -- ^ Status of the DCC transfert
               , dType      :: DccType
                 -- ^ Type of the DCC transfert
               }

data IgnFlgs = IgnFlgs { private  :: Bool
                       , notice   :: Bool
                       , channel  :: Bool
                       , ctcp     :: Bool
                       , invite   :: Bool
                       , unIgnore :: Bool
                       , noSave   :: Bool
                       , dcc      :: Bool
                       }
ignFlgsOfInt :: Int32 -> IgnFlgs
ignFlgsOfInt i =
               IgnFlgs { private  = testBit i 0
                       , notice   = testBit i 1
                       , channel  = testBit i 2
                       , ctcp     = testBit i 3
                       , invite   = testBit i 4
                       , unIgnore = testBit i 5
                       , noSave   = testBit i 6
                       , dcc      = testBit i 7
                       }

data Ign = Ign { iMask  :: String  -- ^ Ignore mask. .e.g: *!*\@*.aol.com
               , iFlags :: IgnFlgs -- ^ Flags
               }

data Notify = Notify { nNetworks :: [String]   -- ^ Networks to which this nick applies
                     , nNick     :: String     -- ^ Nickname
                     , nOnline   :: Bool       -- ^ Currently on-line
                     , nOn       :: Int32      -- ^ Time when nick came online
                     , nOff      :: Int32      -- ^ Time when nick went offline
                     , nSeen     :: Int32      -- ^ Time when nick was last seen
                     }

data User = User { uAway     :: Bool          -- ^ Away status
                 , uLasttalk :: Int32         -- ^ Last time when user talked
                 , uNick     :: String        -- ^ Nickname
                 , uHost     :: Maybe String  -- ^ Host name, /user/\@/host/
                 , uPrefix   :: String        -- ^ e.g. \@ or +
                 , uRealname :: Maybe String  -- ^ Real name
                 , uSelected :: Bool          -- ^ If user belongs to the focused tab
                 }

foreign import ccall "xchat-plugin-hack.h xchat_list_get"
 xChatListGetFFI
 :: XchatPluginHandle a -> CString -> IO XchatListHandle
foreign import ccall "xchat-plugin-hack.h xchat_list_free"
 xChatListFreeFFI
 :: XchatPluginHandle a -> XchatListHandle -> IO ()
foreign import ccall "xchat-plugin-hack.h xchat_list_next"
 xChatListNextFFI
 :: XchatPluginHandle a -> XchatListHandle -> IO CInt
foreign import ccall "xchat-plugin-hack.h xchat_list_str"
 xChatListStrFFI
 :: XchatPluginHandle a -> XchatListHandle -> CString -> IO CString
foreign import ccall "xchat-plugin-hack.h xchat_list_int"
 xChatListIntFFI
 :: XchatPluginHandle a -> XchatListHandle -> CString -> IO CInt
foreign import ccall "xchat-plugin-hack.h xchat_list_time"
 xChatListTimeFFI
 :: XchatPluginHandle a -> XchatListHandle -> CString -> IO CTime
foreign import ccall "xchat-plugin-hack.h xchat_list_context"
 xChatListContextFFI
 :: XchatPluginHandle a -> XchatListHandle -> CString -> IO XchatContextHandle

{- External strings -}
foreign import ccall "xchat-plugin-hack.h strListChan    " strListChan     :: CString
foreign import ccall "xchat-plugin-hack.h strListDcc     " strListDcc      :: CString
foreign import ccall "xchat-plugin-hack.h strListIgn     " strListIgn      :: CString
foreign import ccall "xchat-plugin-hack.h strListNotify  " strListNotify   :: CString
foreign import ccall "xchat-plugin-hack.h strListUser    " strListUser     :: CString
foreign import ccall "xchat-plugin-hack.h strAddress     " strAddress      :: CString
foreign import ccall "xchat-plugin-hack.h strCps         " strCps          :: CString
foreign import ccall "xchat-plugin-hack.h strDestfile    " strDestfile     :: CString
foreign import ccall "xchat-plugin-hack.h strFile        " strFile         :: CString
foreign import ccall "xchat-plugin-hack.h strNick        " strNick         :: CString
foreign import ccall "xchat-plugin-hack.h strPort        " strPort         :: CString
foreign import ccall "xchat-plugin-hack.h strPos         " strPos          :: CString
foreign import ccall "xchat-plugin-hack.h strResume      " strResume       :: CString
foreign import ccall "xchat-plugin-hack.h strSize        " strSize         :: CString
foreign import ccall "xchat-plugin-hack.h strSizehigh    " strSizehigh     :: CString
foreign import ccall "xchat-plugin-hack.h strStatus      " strStatus       :: CString
foreign import ccall "xchat-plugin-hack.h strChannel     " strChannel      :: CString
foreign import ccall "xchat-plugin-hack.h strChantypes   " strChantypes    :: CString
foreign import ccall "xchat-plugin-hack.h strContext     " strContext      :: CString
foreign import ccall "xchat-plugin-hack.h strFlags       " strFlags        :: CString
foreign import ccall "xchat-plugin-hack.h strId          " strId           :: CString
foreign import ccall "xchat-plugin-hack.h strLag         " strLag          :: CString
foreign import ccall "xchat-plugin-hack.h strMaxmodes    " strMaxmodes     :: CString
foreign import ccall "xchat-plugin-hack.h strNetwork     " strNetwork      :: CString
foreign import ccall "xchat-plugin-hack.h strNickprefixes" strNickprefixes :: CString
foreign import ccall "xchat-plugin-hack.h strNickmodes   " strNickmodes    :: CString
foreign import ccall "xchat-plugin-hack.h strQueue       " strQueue        :: CString
foreign import ccall "xchat-plugin-hack.h strServer      " strServer       :: CString
foreign import ccall "xchat-plugin-hack.h strType        " strType         :: CString
foreign import ccall "xchat-plugin-hack.h strUsers       " strUsers        :: CString
foreign import ccall "xchat-plugin-hack.h strMask        " strMask         :: CString
foreign import ccall "xchat-plugin-hack.h strAway        " strAway         :: CString
foreign import ccall "xchat-plugin-hack.h strLasttalk    " strLasttalk     :: CString
foreign import ccall "xchat-plugin-hack.h strHost        " strHost         :: CString
foreign import ccall "xchat-plugin-hack.h strPrefix      " strPrefix       :: CString
foreign import ccall "xchat-plugin-hack.h strRealname    " strRealname     :: CString
foreign import ccall "xchat-plugin-hack.h strSelected    " strSelected     :: CString
foreign import ccall "xchat-plugin-hack.h strNetworks    " strNetworks     :: CString
foreign import ccall "xchat-plugin-hack.h strOn          " strOn           :: CString
foreign import ccall "xchat-plugin-hack.h strOff         " strOff          :: CString
foreign import ccall "xchat-plugin-hack.h strSeen        " strSeen         :: CString

getString :: XchatPluginHandle a -> XchatListHandle -> CString -> IO String
getString h l s = xChatListStrFFI h l s >>= peekCString

getMString :: XchatPluginHandle a -> XchatListHandle -> CString ->
               IO (Maybe String)
getMString h l s = xChatListStrFFI h l s >>= \ ms ->
                     if ms == nullPtr
                     then return Nothing
                     else fmap return $ peekCString ms

getInt :: XchatPluginHandle a -> XchatListHandle -> CString -> IO Int32
getInt h l s = xChatListIntFFI h l s >>= \ k -> return $ fromIntegral k

getTime :: XchatPluginHandle a -> XchatListHandle -> CString -> IO Int32
getTime h l s = xChatListTimeFFI h l s >>= \ (CTime k) -> return k

xChatGetNotifyList :: XChatPlugin a -> IO [Notify]
xChatGetNotifyList (XCP (_, ph)) =
  xChatListGetFFI ph strListNotify >>= \ l ->
  let getNotify = let gStr = getString ph l
                      gInt = getInt ph l
                      gMSt = getMString ph l
                      gTim = getTime ph l
                      splitOn a1 a2 []        = (reverse a1) : a2
                      splitOn a1 a2 (',' : l) = splitOn [] ((reverse a1) : a2) l
                      splitOn a1 a2 (a : l)   = splitOn (a : a1) a2 l
                      net ns = case ns of
                                 Nothing -> []
                                 Just n -> splitOn [] [] n
                  in
                  gMSt strNetworks >>= \ networks  ->
                  gStr strNick     >>= \ nick ->
                  gInt strFlags    >>= \ online ->
                  gTim strOn       >>= \ on ->
                  gTim strOff      >>= \ off ->
                  gTim strSeen     >>= \ seen ->
                  return $ Notify { nNetworks = net networks
                                  , nNick     = nick
                                  , nOnline   = online /= 0
                                  , nOn       = on
                                  , nOff      = off
                                  , nSeen     = seen
                                  }
      getNotifies acc = xChatListNextFFI ph l >>= \ c ->
                        case fromIntegral c of
                          0 -> return acc
                          _ -> getNotify >>= \ not -> getNotifies (not : acc)
  in
  getNotifies [] >>= \ list ->
  xChatListFreeFFI ph l >>
  return list

xChatGetUserList :: XChatPlugin a -> IO [User]
xChatGetUserList (XCP (_, ph)) =
  xChatListGetFFI ph strListUser >>= \ l ->
  let getUser = let gStr = getString ph l
                    gInt = getInt ph l
                    gTim = getTime ph l
                    gMSt = getMString ph l
                in
                gInt strAway     >>= \ away     ->
                gTim strLasttalk >>= \ last     ->
                gStr strNick     >>= \ nick     ->
                gMSt strHost     >>= \ host     ->
                gStr strPrefix   >>= \ prefix   ->
                gMSt strRealname >>= \ realname ->
                gInt strSelected >>= \ selected ->
                return $ User { uAway     = away /= 0
                              , uLasttalk = last
                              , uNick     = nick
                              , uHost     = host
                              , uPrefix   = prefix
                              , uRealname = realname
                              , uSelected = selected /= 0
                              }
      getUsers acc = xChatListNextFFI ph l >>= \ c ->
                     case fromIntegral c of
                       0 -> return acc
                       _ -> getUser >>= \ usr -> getUsers (usr : acc)
  in
  getUsers [] >>= \ list ->
  xChatListFreeFFI ph l >>
  return list

xChatGetIgnList :: XChatPlugin a -> IO [Ign]
xChatGetIgnList (XCP (_, ph)) =
  xChatListGetFFI ph strListIgn >>= \ l ->
  let getIgn = let gStr = getString ph l
                   gInt = getInt ph l
               in
               gStr strMask  >>= \ mask  ->
               gInt strFlags >>= \ flags ->
               return $ Ign { iMask  = mask
                            , iFlags = ignFlgsOfInt flags
                            }
      getIgns acc = xChatListNextFFI ph l >>= \ c ->
                    case fromIntegral c of
                      0 -> return acc
                      _ -> getIgn >>= \ ign -> getIgns (ign : acc)
  in
  getIgns [] >>= \ list ->
  xChatListFreeFFI ph l >>
  return list

xChatGetDccList :: XChatPlugin a -> IO [Dcc]
xChatGetDccList (XCP (_, ph)) =
  xChatListGetFFI ph strListDcc >>= \ l ->
  let getDcc = let gStr = getString ph l
                   gInt = getInt ph l
               in
               gInt strAddress      >>= \ address  ->
               gInt strCps          >>= \ cps      ->
               gStr strDestfile     >>= \ destfile ->
               gStr strFile         >>= \ file     ->
               gStr strNick         >>= \ nick     ->
               gInt strPort         >>= \ port     ->
               gInt strPos          >>= \ pos      ->
               gInt strResume       >>= \ resume   ->
               gInt strSize         >>= \ sizeL    ->
               gInt strSizehigh     >>= \ sizeH    ->
               gInt strStatus       >>= \ status   ->
               gInt strType         >>= \ typ      ->
               let bsizeH :: Int64
                   bsizeH = shiftL (fromIntegral sizeH) 32
                   bsizeL :: Int64
                   bsizeL = (fromIntegral sizeL) .&. (shiftR 1 32 - 1)
                   bsize = bsizeH .|. bsizeL
               in
               return $ Dcc { dAddress32 = address
                            , dCps       = cps
                            , dDestfile  = destfile
                            , dFile      = file
                            , dNick      = nick
                            , dPort      = port
                            , dPos       = pos
                            , dResume    = resume
                            , dSize      = bsize
                            , dStatus    = dccStatusOfInt status
                            , dType      = dccTypeOfInt typ
                            }
      getDccs acc = xChatListNextFFI ph l >>= \ c ->
                    case fromIntegral c of
                      0 -> return acc
                      _ -> getDcc >>= \ dcc -> getDccs (dcc : acc)
  in
  getDccs [] >>= \ list ->
  xChatListFreeFFI ph l >>
  return list

xChatGetChanList :: XChatPlugin a -> IO [Chan]
xChatGetChanList (XCP (_, ph)) =
  xChatListGetFFI ph strListChan >>= \ l ->
  let getChan = let gStr = getString ph l
                    gInt = getInt ph l
                in
                gStr strChannel      >>= \ channel      ->
                gStr strChantypes    >>= \ chantypes    ->
                (xChatListContextFFI ph l strContext)
                                     >>= \ ctx          ->
                gInt strFlags        >>= \ flags        ->
                gInt strId           >>= \ id           ->
                gInt strLag          >>= \ lag          ->
                gInt strMaxmodes     >>= \ maxmodes     ->
                gStr strNetwork      >>= \ network      ->
                gStr strNickprefixes >>= \ nickprefixes ->
                gStr strNickmodes    >>= \ nickmodes    ->
                gInt strQueue        >>= \ queue        ->
                gStr strServer       >>= \ server       ->
                gInt strType         >>= \ typ          ->
                gInt strUsers        >>= \ users        ->
                return $  Chan { cChannel      = channel
                               , cChantypes    = chantypes
                               , cContext      = XCC ctx
                               , cFlags        = chanFlgsOfInt flags
                               , cId           = id
                               , cLag          = lag
                               , cMaxmodes     = maxmodes
                               , cNetwork      = network
                               , cNickprefixes = nickprefixes
                               , cNickmodes    = nickmodes
                               , cQueue        = queue
                               , cServer       = server
                               , cType         = chanTypeOfInt typ
                               , cUsers        = users
                               }
      getChans acc = xChatListNextFFI ph l >>= \ c ->
                     case fromIntegral c of
                       0 -> return acc
                       _ -> getChan >>= \ chan -> getChans (chan : acc)
  in
  getChans [] >>= \ list ->
  xChatListFreeFFI ph l >>
  return list
