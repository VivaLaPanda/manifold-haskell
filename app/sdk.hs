module Manifold where
-- API Code
import Network.HTTP.Client
import Network.HTTP.Types.Header
import Data.Aeson
import Data.Text
import Types

-- Define the base url for the API
baseUrl = "https://manifold.markets/api/v0"

-- Create a client that will attach an auth header of the form "Authorization: Key {key}" to every request
createClient :: Text -> IO Manager
createClient key = do
    manager <- newManager defaultManagerSettings
    let authHeader = mkHeader HdrAuthorization ("Key " <> key)
    return $ manager { managerModifyRequest = \req -> return $ req { requestHeaders = authHeader : requestHeaders req } }

-- Endpoints that query against the base url

-- List all markets
listMarkets :: Manager -> Maybe Int -> Maybe Text -> IO [LiteMarket]
listMarkets manager limit before = do
    let params = [("limit", show <$> limit), ("before", before)]
    response <- httpLbs (parseRequest_ $ baseUrl <> "/markets" <> queryString params) manager
    return $ fromJSON $ responseBody response

-- List all groups
listGroups :: Manager -> Maybe Text -> IO [Group]
listGroups manager availableToUserId = do
    let params = [("availableToUserId", availableToUserId)]
    response <- httpLbs (parseRequest_ $ baseUrl <> "/groups" <> queryString params) manager
    return $ fromJSON $ responseBody response

-- List all bets
listBets :: Manager -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Text -> IO [Bet]
listBets manager limit before username market = do
    let params = [("limit", show <$> limit), ("before", before), ("username", username), ("market", market)]
    response <- httpLbs (parseRequest_ $
        baseUrl <> "/bets" <> queryString params) manager
    return $ fromJSON $ responseBody response

-- Endpoints that query against the base url with a market id

-- Get a market by id
getMarketById :: Manager -> Text -> IO Market
getMarketById manager marketId = do
    response <- httpLbs (parseRequest_ $ baseUrl <> "/market/" <> marketId) manager
    return $ fromJSON $ responseBody response

-- Get a market by slug
getMarketBySlug :: Manager -> Text -> IO Market
getMarketBySlug manager slug = do
    response <- httpLbs (parseRequest_ $ baseUrl <> "/slug/" <> slug) manager
    return $ fromJSON $ responseBody response

-- Get a market by url
getMarketByUrl :: Manager -> Text -> IO Market
getMarketByUrl manager url = do
    let slug = last $ splitOn "/" url
    response <- httpLbs (parseRequest_ $ baseUrl <> "/slug/" <> slug) manager
    return $ fromJSON $ responseBody response

-- Get a user by handle
getUser :: Manager -> Text -> IO LiteUser
getUser manager handle = do
    response <- httpLbs (parseRequest_ $ baseUrl <> "/user/" <> handle) manager
    return $ fromJSON $ responseBody response

-- Endpoints that query against the base url with a market id and require an auth header

-- Cancel a market, resolving it N/A
cancelMarket :: Manager -> Text -> IO Response
cancelMarket manager marketId = do
    response <- httpLbs (parseRequest_ $ baseUrl <> "/market/" <> marketId <> "/resolve") manager
    return $ fromJSON $ responseBody response

-- Create a bet
createBet :: Manager -> Text -> Int -> Text -> Maybe Float -> IO Text
createBet manager contractId amount outcome limitProb = do
    let json = object [
            "amount" .= amount,
            "contractId" .= contractId,
            "outcome" .= outcome,
            "limitProb" .= limitProb
        ]
    response <- httpLbs (parseRequest_ $ baseUrl <> "/bet") manager
    return $ fromJSON $ responseBody response

-- Create a free response market
createFreeResponseMarket :: Manager -> Text -> Text -> Int -> Maybe [Text] -> IO LiteMarket
createFreeResponseMarket manager question description closeTime tags = do
    let json = object [
            "outcomeType" .= "FREE_RESPONSE",
            "question" .= question,
            
            "description" .= description,
            "closeTime" .= closeTime,
            "tags" .= tags
        ]
    response <- httpLbs (parseRequest_ $ baseUrl <> "/market") manager
    return $ fromJSON $ responseBody response

-- Create a multiple choice market
createMultipleChoiceMarket :: Manager -> Text -> Text -> Int -> [Text] -> Maybe [Text] -> IO LiteMarket
createMultipleChoiceMarket manager question description closeTime answers tags = do
    let json = object [
            "outcomeType" .= "MULTIPLE_CHOICE",
            "question" .= question,
            
            "description" .= description,
            "closeTime" .= closeTime,
            "tags" .= tags,
            "answers" .= answers
        ]
    response <- httpLbs (parseRequest_ $ baseUrl <> "/market") manager
    return $ fromJSON $ responseBody response

-- Create a numeric market
createNumericMarket :: Manager -> Text -> Text -> Int -> Int -> Int -> Bool -> Maybe Float -> Maybe [Text] -> IO LiteMarket
createNumericMarket manager question description closeTime minValue maxValue isLogScale initialValue tags = do
    let json = object [
            "outcomeType" .= "PSEUDO_NUMERIC",
            "question" .= question,
            
            "description" .= description,
            "closeTime" .= closeTime,
            "tags" .= tags,
            "minValue" .= minValue,
            "maxValue" .= maxValue,
            "isLogScale" .= isLogScale,
            "initialValue" .= initialValue
        ]
    response <- httpLbs (parseRequest_ $ baseUrl <> "/market") manager
    return $ fromJSON $ responseBody response

-- Create a binary market
createBinaryMarket :: Manager -> Text -> Text -> Int -> Maybe [Text] -> Maybe Int -> IO LiteMarket
createBinaryMarket manager question description closeTime tags initialProb = do
    let json = object [
            "outcomeType" .= "BINARY",
            "question" .= question,
            
            "description" .= description,
            "closeTime" .= closeTime,
            "tags" .= tags,
            "initialProb" .= initialProb
        ]
    response <- httpLbs (parseRequest_ $ baseUrl <> "/market") manager
    return $ fromJSON $ responseBody response

-- Resolve a market, with different inputs depending on its type
resolveMarket :: Manager -> LiteMarket -> IO Response
resolveMarket manager market = do
    if outcomeType market == "BINARY" then
        resolveBinaryMarket manager market
    else if outcomeType market == "FREE_RESPONSE" then
        resolveFreeResponseMarket manager market
    else if outcomeType market == "MULTIPLE_CHOICE" then
        resolveMultipleChoiceMarket manager market
    else if outcomeType market == "PSEUDO_NUMERIC" then
        resolvePseudoNumericMarket manager market
    else
        throw $ NotImplementedError "Invalid outcome type. Outcome should be one of: BINARY, FREE_RESPONSE, PSEUDO_NUMERIC, MULTIPLE_CHOICE"

-- Resolve a binary market
resolveBinaryMarket :: Manager -> LiteMarket -> IO Response
resolveBinaryMarket manager market = do
    let json = if probability market == 100 then
            object ["outcome" .= "YES"]
        else if probability market == 0 then
            object ["outcome" .= "NO"]
        else
           
            object ["outcome" .= "MKT", "probabilityInt" .= probability market]
    response <- httpLbs (parseRequest_ $ baseUrl <> "/market/" <> id market <> "/resolve") manager
    return $ fromJSON $ responseBody response

-- Resolve a free response market
resolveFreeResponseMarket :: Manager -> LiteMarket -> IO Response
resolveFreeResponseMarket manager market = do
    let json = if length (answers market) == 1 then
            object ["outcome" .= head (answers market)]
        else
            object ["outcome" .= "MKT", "resolutions" .= map (\answer -> object ["answer" .= answer, "pct" .= 100]) (answers market)]
    response <- httpLbs (parseRequest_ $ baseUrl <> "/market/" <> id market <> "/resolve") manager
    return $ fromJSON $ responseBody response

-- Resolve a multiple choice market
resolveMultipleChoiceMarket :: Manager -> LiteMarket -> IO Response
resolveMultipleChoiceMarket manager market = do
    let json = if
            length (answers market) == 1 then
            object ["outcome" .= head (answers market)]
        else
            object ["outcome" .= "MKT", "resolutions" .= map (\answer -> object ["answer" .= answer, "pct" .= 100]) (answers market)]
    response <- httpLbs (parseRequest_ $ baseUrl <> "/market/" <> id market <> "/resolve") manager
    return $ fromJSON $ responseBody response

-- Resolve a pseudo numeric market
resolvePseudoNumericMarket :: Manager -> LiteMarket -> IO Response
resolvePseudoNumericMarket manager market = do
    let prob = 100 * numberToProbCpmm1 (resolution market) (min market) (max market) (isLogScale market)
    let json = object ["outcome" .= "MKT", "value" .= resolution market, "probabilityInt" .= prob]
    response <- httpLbs (parseRequest_ $ baseUrl <> "/market/" <> id market <> "/resolve") manager
    return $ fromJSON $ responseBody response

-- Create a comment on a given market, using Markdown, HTML, or TipTap formatting
createComment :: Manager -> Text -> Text -> Text -> IO Response
createComment manager market comment mode = do
    let data = if mode == "tiptap" then
            object [
                "contractId" .= market,
                "content" .= comment
            ]
        else if mode == "html" then
            object [
                "contractId" .= market,
                "html" .= comment
            ]
        else if mode == "markdown" then
            object [
                "contractId" .= market,
                "markdown" .= comment
            ]
        else
            throw $ ValueError "Invalid format mode"
    response <- httpLbs (parseRequest_ $ baseUrl <> "/comment") manager
    return $ fromJSON $ responseBody response