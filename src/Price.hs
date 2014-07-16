module Market.Price where

import Market.Types

getAveragePrice :: (Tradeable a, Tradeable b) => a -> b -> Price
getAveragePrice x y = (price x + price y) / 2

getBuyerPrice :: (Tradeable a, Tradeable b) => a -> b -> Price
getBuyerPrice x y = price x

getSellerPrice :: (Tradeable a, Tradeable b) => a -> b -> Price
getSellerPrice x y = price y

getPrice :: (Tradeable a, Tradeable b) => a -> b -> Price
getPrice = getAveragePrice
