module Market.Trade where

import Market.Types
import Market.Util
import Market.Price

import Data.Time
import Data.Maybe (fromJust)
import Data.Monoid (mconcat)
import Data.Function (on)
import Data.List (intercalate)

import Control.Monad (when)
import Control.Monad.State
import Control.Lens

import qualified Data.Set as S

executeCrossingTrade :: MarketState ()
executeCrossingTrade = do
    myBestBid <- liftM fromJust bestBid
    myBestAsk <- liftM fromJust bestAsk
    let myBestBidVolume = volume myBestAsk
    let myBestAskVolume = volume myBestBid 
    let executedTradeVolume = min myBestAskVolume myBestBidVolume
    currentTime <- lift getCurrentTime
    let currentTrade = Trade (owner myBestBid) (owner myBestAsk) (getPrice myBestBid myBestAsk) executedTradeVolume currentTime
    executedOrders %= S.insert currentTrade
    removeVolumeFromBothSides executedTradeVolume

resolveCrosses :: MarketState ()
resolveCrosses = do
    marketCrosses <- marketIsCrossing
    when marketCrosses $ do
        executeCrossingTrade
        resolveCrosses

executeTrade :: Order -> Quote -> MarketState ()
executeTrade o q = do
    let myTradePrice = getPrice o q
    let myTradeVolume = min (volume o) (volume q)
    myCurrentTime <- lift getCurrentTime
    let (myBuyer, mySeller) = getBuyerSeller o q
    let currentTrade = Trade myBuyer mySeller myTradePrice myTradeVolume myCurrentTime
    executedOrders %= S.insert currentTrade
    removeVolumeFromOneSide (side q) myTradeVolume

orderCrossesMarket :: Order -> MarketState Bool
orderCrossesMarket o = do
    bestOnOpposingSide <- bestOnSide $ oppSide $ side o
    case bestOnOpposingSide of
        Nothing -> return False
        Just bestQuote -> do
            if (canTrade o bestQuote) 
            then do
                case compare (volume o) (volume bestQuote) of
                    LT -> return False
                    EQ -> return False
                    GT -> return False
            else return False

executeOrder :: Order -> MarketState ()
executeOrder o = case _orderType o of 
    Limit -> do
        myQuote <- orderToQuote o
        determineSide o %= S.insert myQuote
    Market -> return ()
    FOK -> return ()
    Cancel -> return ()
