{-# LANGUAGE LambdaCase                 #-}

module Tokens.Android.IDIoT
  ( privateProvider
  ) where

import           Control.Monad.Except                (MonadError)
import           Control.Monad.IO.Class              (MonadIO)
import           Data.Text                           (Text)
import           IDIoT.Types                         (PrivateProvider(PrivateProvider))

import           Tokens.Android.Keystore             (sign, getPublicKey)

type AndroidContext m =
  ( MonadIO m
  , MonadError Text m
  )

privateProvider :: AndroidContext m => Text -> PrivateProvider m
privateProvider alias =
  PrivateProvider (getPublicKey alias) (sign alias)
