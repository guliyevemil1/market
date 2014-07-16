module Market.Util where

import Market.Types

import Data.Time
import Data.Maybe (fromJust)
import Data.Monoid (mconcat)
import Data.Function (on)
import Data.List (intercalate)

import Control.Monad (when)
import Control.Monad.State
import Control.Lens

import qualified Data.Set as S

oppSide :: Side -> Side
oppSide Buy = Sell
oppSide Sell = Buy

bestOnSide :: Side -> MarketState (Maybe Quote)
bestOnSide s = do
    side <- use (getMarketSide s)
    return $ if S.null side then Nothing else Just $ S.findMax side

bestOnOppSide :: Side -> MarketState (Maybe Quote)
bestOnOppSide = bestOnSide . oppSide

bestBid :: MarketState (Maybe Quote)
bestBid = bestOnSide Buy

bestAsk :: MarketState (Maybe Quote)
bestAsk = bestOnSide Sell

getMarketSide :: (Functor f) => Side -> MarketSideLens f
getMarketSide Buy = buyOrders
getMarketSide Sell = sellOrders

determineSide :: (Functor f, Tradeable t) => t -> MarketSideLens f
determineSide = getMarketSide . side

marketIsCrossing :: MarketState Bool
marketIsCrossing = do
    bb <- liftM (maybe 0 price) bestBid
    ba <- liftM (maybe (bb+1) price) bestAsk
    return $ ba <= bb

removeVolumeFromSet :: Volume -> S.Set Quote -> S.Set Quote
removeVolumeFromSet v s = if S.null s then s else volumeRemovedS
    where
        (myMax, s') = S.deleteFindMax s
        volumeRemovedS = if volume myMax == v 
                then s'
                else S.insert (quoteVolume %~ (\x -> x-v) $ myMax) s'

removeVolumeFromOneSide :: Side -> Volume -> MarketState ()
removeVolumeFromOneSide s v = getMarketSide s %= removeVolumeFromSet v

removeVolumeFromBothSides :: Volume -> MarketState ()
removeVolumeFromBothSides v = do
    removeVolumeFromOneSide Buy v
    removeVolumeFromOneSide Sell v

orderToQuote :: Order -> MarketState Quote
orderToQuote myOrder = do
    myId <- use leastUnusedId
    leastUnusedId %= (+1)
    myTimestamp <- lift getCurrentTime
    return $ Quote 
        myId 
        myTimestamp 
        (owner myOrder)
        (side myOrder)
        (price myOrder)
        (volume myOrder)

canTrade :: (Tradeable a, Tradeable b) => a -> b -> Bool
canTrade x y = case (side x, side y) of
    (Buy, Sell) -> price x >= price y
    (Sell, Buy) -> price x <= price y
    _ -> False

getBuyerSeller :: (Tradeable a, Tradeable b) => a -> b -> (Owner, Owner)
getBuyerSeller x y = case (side x, side y) of
    (Buy, Sell) -> (owner x, owner y)
    (Sell, Buy) -> (owner y, owner x)
    _ -> error "Both agents on the same side."
