module Tokens.Time
  ( getTime
  , timer
  , fromUnits
  ) where

import           Control.Concurrent        (threadDelay)
import           Control.Concurrent.Loops  (Step(Step), onCancel, loop)
import           Control.DeepSeq           (NFData)
import           Data.Time.Units           (TimeUnit, toMicroseconds)
import           System.Clock              (TimeSpec, Clock(Monotonic), toNanoSecs)
import qualified System.Clock              as Clock (getTime)

instance NFData TimeSpec

getTime :: IO TimeSpec
getTime = Clock.getTime Monotonic

fromUnits :: TimeUnit a => a -> TimeSpec
fromUnits = fromInteger . (* 1000) . toMicroseconds

timer :: TimeUnit a => a -> IO (Step () TimeSpec)
timer period = do
  let go state = Step $ \() -> do
       let current0 = state
       let next = (+ current0) . fromUnits $ period
       now <- getTime
       current1 <- onCancel (pure current0) $ (threadDelay . fromInteger . (`div` 1000) . max 0 . toNanoSecs . subtract now $ next) *> pure next
       pure (current1, go current1)
  now <- getTime
  pure . loop now $ go now
