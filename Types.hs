{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Market.Types where

import Data.Time
import Data.Monoid (mconcat)
import Data.Function (on)
import Data.List (intercalate)

import Control.Monad (when)
import Control.Monad.State
import Control.Lens (makeLenses)

import qualified Data.Set as S

import Control.Applicative ((<$>), (<*>))
import Data.Aeson
import Data.Data
import Data.Typeable

data Side = Buy | Sell deriving (Eq, Ord, Show)

oppSide :: Side -> Side
oppSide Buy = Sell
oppSIde Sell = Buy

sideToNum :: Num a => Side -> a
sideToNum Buy = 1
sideToNum Sell = -1

type Id = Int
type Product = String
type Price = Double
type Volume = Int
type Owner = String

data OrderType = Market | Limit | Cancel | FOK deriving (Eq, Show)

class Tradeable a where
    owner :: a -> Owner
    side :: a -> Side
    volume :: a -> Volume
    price :: a -> Price

data Order = Order {
        _orderOwner :: Owner,
        _orderType :: OrderType,
        _orderSide :: Side,
        _orderPrice :: Price, 
        _orderVolume :: Volume
    } deriving (Eq)
    
$( makeLenses ''Order )

instance Tradeable Order where
    owner = _orderOwner
    side = _orderSide
    volume = _orderVolume
    price = _orderPrice

--instance FromJSON OrderType where
{-
instance FromJSON Order where
    parseJSON (Object v) = Order <$>
                            v .: "owner" <*>
                            v .: "type"  <*>
                            v .: "side" <*>
                            v .: "price" <*>
                            v .: "volume"

instance ToJSON Order where
    toJSON (Order o ot s p v) = object ["owner" .= _orderOwner, "side" .= _orderSide, "price" .= _orderPrice, "volume" .= _orderVolume]
-}

data Quote = Quote {
        _id :: Id,
        _timestamp :: UTCTime,
        _quoteOwner :: Owner,
        _quoteSide :: Side,
        _quotePrice :: Price, 
        _quoteVolume :: Volume
    } deriving Eq

$( makeLenses ''Quote )

instance Tradeable Quote where
    owner = _quoteOwner
    side = _quoteSide
    volume = _quoteVolume
    price = _quotePrice

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

instance Ord Quote where
    compare o o' = mconcat $ map (\f -> f o o') [compare `on` side, compare `on` priceComparator, compare `on` _timestamp]

data MarketDepth = MarketDepth { 
        _product :: Product,
        _leastUnusedId :: Id,
        _executedOrders :: S.Set Trade,
        _buyOrders :: S.Set Quote, 
        _sellOrders :: S.Set Quote
    } 

newMarketDepth = MarketDepth "" 0 S.empty S.empty S.empty

instance Show MarketDepth where
    show ms = intercalate " --- " $ map (\f -> intercalate "," . map show . f $ ms) [reverse . S.toList . _buyOrders, reverse . S.toList . _sellOrders]

$( makeLenses ''MarketDepth )

type MarketState = StateT MarketDepth IO
