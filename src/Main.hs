{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (product)

import qualified Data.Text as T
import Web.Scotty
import Text.Blaze.Html5 hiding (html, map)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.Text
import qualified Text.Blaze.Html5.Attributes as A

import Data.Time
import Data.List
import System.IO.Unsafe

import Market.Trade
import Market.Types

import qualified Data.Set as S

{-
    data MarketDepth = MarketDepth { 
            _product :: Product,
            _leastUnusedId :: Id,
            _executedTrades :: S.Set Trade,
            _buyOrders :: OrderSide, 
            _sellOrders :: OrderSide
        } 

    data Order = Order {
            _quoteId :: Maybe Id,
            _orderOwner :: Owner,
            _orderType :: OrderType,
            _orderSide :: Side,
            _orderPrice :: Price, 
            _orderVolume :: Volume
        } deriving (Eq, Show)

    data Quote = Quote {
            _id :: Id,
            _timestamp :: UTCTime,
            _quoteOwner :: Owner,
            _quoteSide :: Side,
            _quotePrice :: Price, 
            _quoteVolume :: Volume
        } deriving (Eq)
-}

t = unsafePerformIO $ getCurrentTime

myBuyOrder = Quote 0 t "Emil" Buy 10.0 10
mySellOrder = Quote 0 t "Emil" Sell 11.0 10

myMarketDepth = MarketDepth "My Favorite Product"  0 S.empty (S.singleton myBuyOrder) (S.singleton mySellOrder)

ordersToList :: OrderSide -> [H.Html]
ordersToList = map (H.html . H.toHtml . show) . S.toList

marketToHtml :: MarketDepth -> H.Html
marketToHtml md = H.docTypeHtml $ do
    H.head $ do
        H.title $ H.toHtml $ _product md
    H.body $ do
        H.table $ do
            H.tr $ mapM_ H.td $ ordersToList $ _buyOrders md
            H.tr $ mapM_ H.td $ ordersToList $ _sellOrders md

main :: IO ()
main = scotty 3000 $ do
  get "/" $ do
    html $ renderHtml $ marketToHtml $ myMarketDepth
