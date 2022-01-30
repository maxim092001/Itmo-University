{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingVia #-}
module HW3.Action ( HIO (..)
                  , HiPermission (..)
                  , PermissionException (..)
                  ) where


import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.Sequence as SQ
import Data.Set
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import HW3.Base
import System.Directory
import System.Random
import System.Random.Stateful
import Control.Monad.Trans.Reader

data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime deriving (Show, Eq, Ord)

newtype PermissionException =
    PermissionRequired HiPermission deriving (Show, Eq, Ord)

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a } deriving (Functor, Applicative, Monad) via (ReaderT (Set HiPermission) IO)

instance MonadIO HIO where
  liftIO :: IO a -> HIO a
  liftIO i = HIO (const i)

instance HiMonad HIO where
  runAction :: HiAction -> HIO HiValue
  runAction a = case a of

                  HiActionRead f -> HIO (\perms ->
                    do
                      unless (member AllowRead perms) (throwIO $ PermissionRequired AllowRead)
                      isFile <- liftIO $ doesFileExist f
                      if isFile then do
                        x <- liftIO $ BS.readFile f
                        let decoded = decodeUtf8Bytes x
                        pure $ if decoded == HiValueNull then HiValueBytes x else decoded
                      else do
                        l <- listDirectory f
                        pure $ HiValueList $ SQ.fromList (HiValueString . T.pack <$> l))

                  HiActionCwd -> HIO (\perms ->
                    do
                      unless (member AllowRead perms) (throwIO $ PermissionRequired AllowRead)
                      HiValueString . T.pack <$> getCurrentDirectory)

                  HiActionWrite f s -> HIO (\perms ->
                    do
                      unless (member AllowWrite perms) (throwIO $ PermissionRequired AllowWrite)
                      liftIO $ BS.writeFile f s
                      pure HiValueNull)

                  HiActionChDir f -> HIO (\perms ->
                    do
                      unless (member AllowRead perms) (throwIO $ PermissionRequired AllowRead)
                      liftIO $ setCurrentDirectory f
                      pure HiValueNull)

                  HiActionMkDir f -> HIO (\perms ->
                    do
                      unless (member AllowWrite perms) (throwIO $ PermissionRequired AllowWrite)
                      liftIO $ createDirectory f
                      pure HiValueNull)

                  HiActionNow -> HIO (\perms ->
                    do
                      unless (member AllowTime perms) (throwIO $ PermissionRequired AllowTime)
                      HiValueTime <$> liftIO getCurrentTime)

                  HiActionRand x y -> HiValueNumber . toRational <$> getStdRandom (uniformR (x, y))

                  HiActionEcho t   -> HIO (\perms ->
                    do
                      unless (member AllowWrite perms) (throwIO $ PermissionRequired AllowWrite)
                      liftIO $ print t
                      pure HiValueNull)
