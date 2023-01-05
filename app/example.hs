module Main where

import Manifold
import Types

main :: IO ()
main = do
    manager <- createClient "YOUR_API_KEY"
    markets <- listMarkets manager Nothing Nothing
    print $ markets !! 0
    market <- getMarketById manager $ id $ markets !! 0
    print market
    market <- getMarketBySlug manager "manifold-markets-is-a-decentralized-prediction-market-platform"
    print market
    user <- getUser manager "manifold"
    print user
    groups <- listGroups manager Nothing
    print $ groups !! 0
    bets <- listBets manager Nothing Nothing Nothing Nothing
    print $ bets !! 0