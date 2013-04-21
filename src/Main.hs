-- ---------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  GPL-2
--
-- Maintainer  :  sedrikov@gmail.com
-- Stability   :  experimental
-- Portability :
--
-- | The Main module which provides the plugin compiler
--
-- ---------------------------------------------------------------------------

module Main (main
) where

import System.Cmd            (system)
import System.Info           (os)
import System.IO.Error       (catchIOError)
import System.Directory      (createDirectory, removeDirectory,
                              copyFile, removeFile,
                              getCurrentDirectory, setCurrentDirectory)
import System.FilePath       (pathSeparator)
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..),
                              getOpt, usageInfo)
import System.Environment    (getArgs, getProgName)
import Paths_xchat_plugin    (getDataFileName)

data Options = Options { pluginName  :: Maybe String
                       , installDir  :: Maybe String
                       , moduleName  :: Maybe String
                       , initFunc    :: Maybe String
                       , deinitFunc  :: Maybe String
                       , hType       :: Maybe String
                       , help        :: Bool
                       , wspace      :: Bool
                       , compile     :: Bool
                       , install     :: Bool
                       , cleanFiles  :: Bool
                       , verbose     :: Bool
                       , progName    :: String
                       }
 deriving Show

defOptions = Options { pluginName  = Nothing
                     , installDir  = Nothing
                     , moduleName  = Nothing
                     , initFunc    = Nothing
                     , deinitFunc  = Nothing
                     , hType       = Nothing
                     , help        = False
                     , wspace      = False
                     , compile     = False
                     , install     = False
                     , cleanFiles  = False
                     , verbose     = False
                     , progName    = ""
                     }


-- Options

argPluginName = ReqArg (\ s o -> o { pluginName = Just s }) "FileName"
argInstallDir = ReqArg (\ s o -> o { installDir = Just s }) "FilePath"
argModuleName = ReqArg (\ s o -> o { moduleName = Just s }) "ModName"
argInitFunc   = ReqArg (\ s o -> o { initFunc   = Just s }) "FunName"
argDeinitFunc = ReqArg (\ s o -> o { deinitFunc = Just s }) "FunName"
argHType      = ReqArg (\ s o -> o { hType      = Just s }) "TypeName"

defPluginName = "plugin"
defInstallDir = "/usr/lib/xchat/plugins/"
defModuleName = "*** Please provide a module name with the -M option ***"
defInitFunc   = "xChatPluginInit"
defHType      = "()"

def :: String -> String -> String
def a b = a ++ "\n(defaults to \"" ++ b ++ "\")"

optPluginName = Option ['o'] ["output"   ] argPluginName
                       $ def "name of plugin file without .so" defPluginName
optInstallDir = Option ['d'] ["directory"] argInstallDir
                       $ def "path to xchat plugins directory" defInstallDir
optModuleName = Option ['M'] ["module"   ] argModuleName
                       ("the already compiled and registered Haskell "++
                        "module that you want to turn into a xchat plugin")
optInitFunc   = Option ['I'] ["init"     ] argInitFunc
                       $ def ("name of the Haskell init function; "++
                              "must be of type:\n"++
                              "'(<T> -> IO (XChatPlugin <T>)) -> "++
                              "IO PluginDescriptor'\nwhere <T> is the type "++
                              "associated to the memory of the plugin.\n"++
                              "Plugin initialisation is not (yet?) allowed "++
                              "to fail, so be careful!")
                             defInitFunc
optDeinitFunc = Option ['D'] ["deinit"   ] argDeinitFunc
                       ("name of the Haskell deinit function (if any) "++
                        "its type must simply be 'IO ()'.\nDeinitialisation "++
                        "is not (yet?) allowed to fail, so be careful!")
optHType      = Option ['T'] ["type"     ] argHType
                       $ def "Haskell type of the plugin memory" defHType
optHelp       = Option ['h'] ["help"     ] (NoArg (\o -> o {help      = True}))
                       "prints this help, cancels all other options"
optWspace     = Option ['w'] ["workspace"] (NoArg (\o -> o {wspace    = True}))
                       "creates the workspace"
optCompile    = Option ['c'] ["compile"  ] (NoArg (\o -> o {compile   = True}))
                       "compile the plugin"
optInstall    = Option ['i'] ["install"  ] (NoArg (\o -> o {install   = True}))
                       "install the plugin (by moving the so file)"
optCleanFiles = Option ['C'] ["clean"    ] (NoArg (\o -> o {cleanFiles= True}))
                       "removes the generated workspace"
optVerbose    = Option ['v'] ["verbose"  ] (NoArg (\o -> o {verbose   = True}))
                       "activate verbose mode"

options = [ optPluginName
          , optInstallDir
          , optModuleName
          , optInitFunc
          , optDeinitFunc
          , optHelp
          , optCompile
          , optWspace
          , optInstall
          , optCleanFiles
          , optVerbose
          , optHType
          ]

helpMsg :: Options -> IO (Maybe String)
helpMsg o =
  let prn = progName o in
  return
   (Just
     (usageInfo (prn ++ " [options] : tool for generating xchat plugins"
                     ++ " from Haskell.\n\nFor a basic example, run:\n"
                     ++ prn ++ " -osample -MNetwork.IRC.XChat.Examples.AutoOp"
                     ++ " -TNBool -IpluginInit -w\n"
                     ++ prn ++ " -osample -c\n"
                     ++ "sudo " ++ prn ++ " -osample -i\n"
                     ++ prn ++ " -osample -C\n"
                     ++ "  or if you do not want to do it step by step, run:\n"
                     ++ prn ++ " -osample -MNetwork.IRC.XChat.Examples.AutoOp\n"
                     ++ " -TNBool -IpluginInit -w -c\n"
                     ++ "sudo " ++ prn ++ " -osample -i -C\n\nusage:"
                ) options))

getOption :: Options -> (Options -> Maybe String) -> String -> String
getOption o f s = case f o of { Nothing -> s ; Just s -> s }

printHeaderHaskellCode :: Options -> String
printHeaderHaskellCode o =
  let ty = getOption o hType defHType
      pn = getOption o pluginName defPluginName
      mod = getOption o moduleName defModuleName
      ini = getOption o initFunc defInitFunc
      deini = case deinitFunc o of { Nothing -> "" ; Just s -> ", "++s }
  in
  "{-# LANGUAGE ForeignFunctionInterface #-}\n"++
  "module M"++ pn ++" () where\n"++
  "import Foreign\n"++
  "import Foreign.C.Types\n"++
  "import Foreign.C.String\n"++
  "import Network.IRC.XChat.Plugin\n"++
  -- I should refine more precisely what to import
  "import "++mod++"("++ty++", "++ini++deini++")\n"

printInitHaskellCode :: Options -> String
printInitHaskellCode o =
  let ty = getOption o hType defHType
      ini = getOption o initFunc defInitFunc
  in
  "xChatPluginInitM :: Ptr (XchatPlugin "++ty++") ->\n"++
  "                      Ptr CString -> Ptr CString -> Ptr CString ->\n"++
  "                      CString -> IO CInt\n"++
  "xChatPluginInitM ph ps1 ps2 ps3 _ =\n"++
  "  do descr <- "++ini++" (xChatPluginInit ph)\n"++
  "     s1 <- newCString (pluginName descr)\n"++
  "     s2 <- newCString (pluginDescription descr)\n"++
  "     s3 <- newCString (pluginVersion descr)\n"++
  "     poke ps1 s1\n"++
  "     poke ps2 s2\n"++
  "     poke ps3 s3\n"++
  "     return 1\n"++
  "foreign export ccall \"xchat_plugin_init\" xChatPluginInitM\n"++
  "  :: Ptr (XchatPlugin "++ty++") ->\n"++
  "     Ptr CString -> Ptr CString -> Ptr CString -> CString -> IO CInt\n"++
  "foreign export ccall \"hexchat_plugin_init\" xChatPluginInitM\n"++
  "  :: Ptr (XchatPlugin "++ty++") ->\n"++
  "     Ptr CString -> Ptr CString -> Ptr CString -> CString -> IO CInt\n"

printDeinitHaskellCode :: Options -> String
printDeinitHaskellCode o =
  "xChatPluginDeinitM :: IO CInt\n"++
  "xChatPluginDeinitM =\n"++
  (case deinitFunc o of
     Nothing -> "  return 1\n"
     Just s -> "  "++s++" >> return 1\n")++
  "foreign export ccall \"xchat_plugin_deinit\" xChatPluginDeinitM\n"++
  "  :: IO CInt\n"++
  "foreign export ccall \"hexchat_plugin_deinit\" xChatPluginDeinitM\n"++
  "  :: IO CInt\n"

printHaskellCode :: Options -> String
printHaskellCode o =
  (printHeaderHaskellCode o)++"\n"++
  (printInitHaskellCode o)++"\n"++
  (printDeinitHaskellCode o)

createWorkspace :: Options -> IO (Maybe String)
createWorkspace o =
  getCurrentDirectory >>= \ cd ->
  let pn = getOption o pluginName defPluginName
      newdir = cd++(pathSeparator:pn)
      catchErrA _ = "Sorry, unable to create the " ++ newdir ++ " directory."
      csym s = do old <- getDataFileName s
                  copyFile old (newdir ++ (pathSeparator : s))
  in
  catchIOError
   (createDirectory newdir >> csym "module_init.c" >> csym "xchat-plugin.h" >>
    csym "xchat-plugin-hack.h" >> csym "xchat-plugin-hack.c" >>
    writeFile (newdir ++ (pathSeparator : (pn ++ ".hs")))
              (printHaskellCode o) >> return Nothing)
   (return . Just . catchErrA)

libraryExt :: String
libraryExt = case os of
  "linux" -> "so"
  "win" -> "dll"

commandLine :: Options -> String
commandLine o =
  let plug = getOption o pluginName defPluginName
  in
  "ghc -O2 --make -no-hs-main -optl \"-shared\" " ++
  "-fPIC -optc \"-DMODULE=M" ++ plug ++ "\" -optc \"-Wl,--export-dynamic\" " ++
  "-optc \"-g\" " ++
  "-o " ++ plug ++ "." ++ libraryExt ++ " " ++ plug ++ ".hs module_init.c xchat-plugin-hack.c"

compilePlugin :: Options -> IO (Maybe String)
compilePlugin o =
  getCurrentDirectory >>= \ cd ->
  let pn = getOption o pluginName defPluginName
      newdir = cd++(pathSeparator:pn)
  in
  setCurrentDirectory newdir >>
  system (commandLine o) >>
  setCurrentDirectory cd >>
  return Nothing

installPlugin :: Options -> IO (Maybe String)
installPlugin o =
  getCurrentDirectory >>= \ cd ->
  let pn = getOption o pluginName defPluginName
      sofile = pn++".so"
      newso = cd++(pathSeparator:pn++(pathSeparator:sofile))
      dest = (getOption o installDir defInstallDir)++(pathSeparator:sofile)
  in
  copyFile newso dest >>
  return Nothing

cleanWorkspace :: Options -> IO (Maybe String)
cleanWorkspace o =
  getCurrentDirectory >>= \ cd ->
  let pn = getOption o pluginName defPluginName
      newdir = cd++(pathSeparator:pn)
      dir = newdir ++ (pathSeparator:"")
  in
  catchIOError (removeFile (dir++"module_init.c") >>
                removeFile (dir++"xchat-plugin-hack.h") >>
                removeFile (dir++"xchat-plugin-hack.c") >>
                removeFile (dir++"xchat-plugin.h") >>
                removeFile (dir++pn++".hi") >>
                removeFile (dir++pn++".hs") >>
                removeFile (dir++pn++".o") >>
                removeFile (dir++pn++".so") >>
                removeFile (dir++pn++"_stub.h") >>
                removeFile (dir++"module_init.o") >>
                removeFile (dir++"xchat-plugin-hack.o") >>
                removeDirectory newdir >>
                return Nothing)
               (\ err ->
                    return (
                      Just (
                        "Some error occured, delete manually the " ++
                        newdir ++ "directory."
                      )
                    )
               )

runOption :: String ->
             (Options -> Bool) ->
             (Options -> IO (Maybe String)) ->
             (Options -> IO ()) -> Options -> IO ()
runOption s doit step next o =
  if doit o
  then (if verbose o then putStrLn s else return ()) >>
       step o >>= \ res ->
       case res of
         Nothing -> next o
         Just s -> putStrLn s
  else next o

main :: IO ()
main =
  getArgs >>= \ args ->
  getProgName >>= \ prog ->
  let (optmake, nonopts, errs) = getOpt Permute options args
      opt = foldl (flip ($)) (defOptions {progName = prog}) optmake
      optionHelp = if nonopts /= [] || errs /= []
                   then opt { help = True }
                   else opt
  in
  ( runOption "" help helpMsg
  $ runOption "Creating the workspace" wspace createWorkspace
  $ runOption ("Compiling with:\n"++
               (commandLine optionHelp)) compile compilePlugin
  $ runOption "Installing the plugin" install installPlugin
  $ runOption "Removing the workspace" cleanFiles cleanWorkspace
  $ \ o -> putStrLn "Work done!")
  optionHelp
