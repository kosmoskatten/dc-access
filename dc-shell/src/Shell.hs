{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Shell
    ( Shell
    , runShell
    ) where

import Control.Monad.State

import Access (Access, Config, runAccess)

type ShellState = Int

newtype ShellT m a = ShellT { unShell :: StateT ShellState m a }
    deriving ( Functor, Applicative, Monad
             , MonadState ShellState, MonadIO, MonadTrans
             )

type Shell = ShellT Access

runShell :: Shell a -> Config -> IO a
runShell action = runAccess (evalStateT (unShell action) 0)
