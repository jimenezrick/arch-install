{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}

module FsTree
    ( FsTree(..)
    , Content(..)
    , defAttrs
    , createFsTree
    , createFsTreeAt
    ) where

import RIO
import RIO.Directory
import RIO.FilePath

import Data.Default.Class
import System.Posix.Files
import System.Posix.Types
import System.Posix.User

import qualified Data.Text.IO as T

type Attrs = (Maybe FileMode, Maybe (String, String))

data Content where
    Copy :: FilePath -> Content
    Content :: Text -> Content

data FsTree
    = File FilePath
           Content
           Attrs
    | Mode FilePath
           Attrs
    | Dir FilePath
          Attrs
          [FsTree]
    | DirEmpty FilePath
               Attrs

defAttrs :: Attrs
defAttrs = def

createFsTree :: MonadIO m => FsTree -> m ()
createFsTree = createFsTreeAt "."

createFsTreeAt :: MonadIO m => FilePath -> FsTree -> m ()
createFsTreeAt base fstree =
    liftIO $ do
        createFile base fstree
        applyAttrs base fstree

createFile :: FilePath -> FsTree -> IO ()
createFile base (File name (Content txt) _) = T.writeFile (base </> name) txt
createFile base (File name (Copy src) _) = copyFile src (base </> name)
createFile _ (Mode _ _) = return ()
createFile base (DirEmpty name _) = mkdir (base </> name)
createFile base (Dir name _ subtree) = do
    mkdir (base </> name)
    mapM_ (createFile $ base </> name) subtree

applyAttrs :: FilePath -> FsTree -> IO ()
applyAttrs base (File name _ attrs) = useAttrs (base </> name) attrs
applyAttrs base (Mode name attrs) = useAttrs (base </> name) attrs
applyAttrs base (DirEmpty name attrs) = useAttrs (base </> name) attrs
applyAttrs base (Dir name attrs subtree) = do
    useAttrs (base </> name) attrs
    mapM_ (applyAttrs $ base </> name) subtree

useAttrs :: FilePath -> Attrs -> IO ()
useAttrs path (mode, perm) = do
    forM_ mode (chmod path)
    forM_ perm (uncurry $ chown path)

mkdir :: FilePath -> IO ()
mkdir = createDirectoryIfMissing True

chmod :: FilePath -> FileMode -> IO ()
chmod path mode
    | mode > 0o7777 = throwString "FsTree.chmod: invalid mode"
    | otherwise = setFileMode path mode

chown :: FilePath -> String -> String -> IO ()
chown path user group = do
    uentry <- getUserEntryForName user
    gentry <- getGroupEntryForName group
    setOwnerAndGroup path (userID uentry) (groupID gentry)
