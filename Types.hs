{-# LANGUAGE TemplateHaskell, OverloadedStrings, TypeSynonymInstances #-}

module Market.Types where

import Data.Time
import Data.Maybe (fromMaybe)
import Data.Monoid hiding (Product)
import Data.Function (on)
import Data.List (intercalate)
import Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as H

import Control.Monad (when)
import Control.Monad.State
import Control.Lens (makeLenses)

import qualified Data.Set as S

import Control.Applicative ((<$>), (<*>))
import qualified Data.Text as T
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS

data Side = Buy | Sell | None deriving (Eq, Ord, Show)

instance FromJSON Side where
    parseJSON (String str) = case T.toLower str of
        "buy" -> return Buy
        "sell" -> return Sell
        _ -> error "Side is specified incorrectly."
    parseJSON _ = error "Side is not specified."

instance ToJSON Side where
    toJSON = String . T.toLower . T.pack . show

oppSide :: Side -> Side
oppSide Buy = Sell
oppSIde Sell = Buy

instance Monoid Side where
    mempty = None
    mappend = undefined

sideToNum :: Num a => Side -> a
sideToNum Buy = 1
sideToNum Sell = -1

type Id = Int
type Product = T.Text
type Price = Double
type Volume = Int
type Owner = T.Text

instance Monoid Price where
    mempty = 0
    mappend = (+)

instance Monoid Volume where
    mempty = 0
    mappend = (+)

data OrderType = Market | Limit | Cancel | FOK deriving (Eq, Show)

instance FromJSON OrderType where
    parseJSON (String str) = case T.toLower str of
        "market" -> return Market
        "limit" -> return Limit
        "cancel" -> return Cancel
        "fok" -> return FOK
        _ -> error "Order type is specified incorrectly."
    parseJSON _ = error "Order type is not specified."

class Tradeable a where
    owner :: a -> Owner
    side :: a -> Side
    volume :: a -> Volume
    price :: a -> Price

data Order = Order {
        _quoteId :: Maybe Id,
        _orderOwner :: Owner,
        _orderType :: OrderType,
        _orderSide :: Side,
        _orderPrice :: Price, 
        _orderVolume :: Volume
    } deriving (Eq, Show)
    
$( makeLenses ''Order )

instance Tradeable Order where
    owner = _orderOwner
    side = _orderSide
    volume = _orderVolume
    price = _orderPrice

(.:!) v f = liftM (fromMaybe mempty) $ v .:? f

instance FromJSON Order where
    parseJSON (Object v) = Order <$>
            v .:? "id" <*>
            v .: "owner" <*>
            v .: "type"  <*>
            v .:! "side" <*>
            v .:! "price" <*>
            v .:! "volume"

data OrderResponse = OrderFullTrade |
    OrderPartialTrade |
    OrderAdded |
    OrderCancelled |
    OrderUnfulfilled |
    OrderFailed deriving Show

instance ToJSON OrderResponse where
    toJSON o = object ["response" .= T.pack (show o)]

data Quote = Quote {
        _id :: Id,
        _timestamp :: UTCTime,
        _quoteOwner :: Owner,
        _quoteSide :: Side,
        _quotePrice :: Price, 
        _quoteVolume :: Volume
    } deriving (Eq)

$( makeLenses ''Quote )

instance ToJSON Quote where
    toJSON q = object [
        "id" .= _id q, 
        "time" .= _timestamp q,
        "side" .= side q, 
        "price" .= price q, 
        "volume" .= volume q]

instance Tradeable Quote where
    owner = _quoteOwner
    side = _quoteSide
    volume = _quoteVolume
    price = _quotePrice

instance Ord Quote where
    compare o o' = mconcat $ map (\f -> f o o') [compare `on` side, compare `on` priceComparator, compare `on` _timestamp]

instance Show Quote where
    show o = intercalate " " $ map ($ o) [show . owner, show . price, show . volume]

data Trade = Trade {
        _buyer :: Owner,
        _seller :: Owner,
        _tradedPrice :: Price,
        _tradedVolume :: Volume,
        _tradedTimestamp :: UTCTime
    } deriving (Eq, Show)

instance Tradeable Trade where
    owner = undefined
    side = undefined
    volume = _tradedVolume
    price = _tradedPrice

instance Ord Trade where
    compare = compare `on` _tradedTimestamp

priceComparator :: Quote -> Price
priceComparator order = negate $ sideToNum (side order) * (price order)

type OrderSide = S.Set Quote

data MarketDepth = MarketDepth { 
        _product :: Product,
        _leastUnusedId :: Id,
        _executedOrders :: S.Set Trade,
        _buyOrders :: OrderSide, 
        _sellOrders :: OrderSide
    } 

newMarketDepth = MarketDepth "" 0 S.empty S.empty S.empty

$( makeLenses ''MarketDepth )

type MarketSideLens f = (OrderSide -> f OrderSide) -> (MarketDepth -> f MarketDepth)

instance Show MarketDepth where
    show ms = intercalate " --- " $ 
        map (\f -> intercalate "," . map show . f $ ms) [reverse . S.toList . _buyOrders, reverse . S.toList . _sellOrders]

type MarketState = StateT MarketDepth IO
