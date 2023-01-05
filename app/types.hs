{-# LANGUAGE DuplicateRecordFields #-}

module Types (Bet (..), LiteMarket (..), Market (..), Group (..), LiteUser (..)) where
import Data.Text

-- Data types for the resources
data Bet = Bet {
    amount :: Int,
    contractId :: Text,
    
    createdTime :: Int,
    id :: Text,
    
    loanAmount :: Maybe Int,
    userId :: Maybe Text,
    userAvatarUrl :: Maybe Text,
    userUsername :: Maybe Text,
    userName :: Maybe Text,
    
    orderAmount :: Maybe Int,
    isCancelled :: Bool,
    isFilled :: Bool,
    fills :: Maybe [Fill],
    fees :: Maybe Fees
} deriving (Show, Eq)

data Comment = Comment {
    contractId :: Text,
    createdTime :: Int,
    id :: Text,
    text :: Text,
    
    
    userId :: Text,
    userName :: Text,
    userAvatarUrl :: Text,
    userUsername :: Text
} deriving (Show, Eq)

data LiteMarket = LiteMarket {
    id :: Text,
    
    creatorUsername :: Text,
    creatorName :: Text,
    createdTime :: Int,
    creatorAvatarUrl :: Maybe Text,
    
    closeTime :: Maybe Int,
    question :: Text,
    -- description :: Text,
    tags :: [Text],
    
    outcomeType :: Text,
    pool :: Maybe Text,
    volume7Days :: Float,
    volume24Hours :: Float,
    isResolved :: Bool,
    description :: Text,
    lastUpdatedTime :: Maybe Int,
    probability :: Maybe Float,
    resolutionTime :: Maybe Int,
    resolution :: Maybe Text,
    resolutionProbability :: Maybe Float,
    
    
    p :: Maybe Float,
    totalLiquidity :: Maybe Float,
    min :: Maybe Float,
    max :: Maybe Float,
    isLogScale :: Maybe Bool,
    
    
    url :: Maybe Text
} deriving (Show, Eq)

data Market = Market {
    bets :: [Bet],
    comments :: [Comment],
    answers :: Maybe [Text],
    
    id :: Text,
    
    creatorUsername :: Text,
    creatorName :: Text,
    createdTime :: Int,
    creatorAvatarUrl :: Maybe Text,
    
    closeTime :: Maybe Int,
    question :: Text,
    -- description :: Text,
    tags :: [Text],
    
    outcomeType :: Text,
    pool :: Maybe Text,
    volume7Days :: Float,
    volume24Hours :: Float,
    isResolved :: Bool,
    description :: Text,
    lastUpdatedTime :: Maybe Int,
    probability :: Maybe Float,
    resolutionTime :: Maybe Int,
    resolution :: Maybe Text,
    resolutionProbability :: Maybe Float,
    
    p :: Maybe Float,
    totalLiquidity :: Maybe Float,
    min :: Maybe Float,
    max :: Maybe Float,
    isLogScale :: Maybe Bool,
    
    url :: Maybe Text
} deriving (Show, Eq)

data Group = Group {
    name :: Text,
    creatorId :: Text,
    id :: Text,
    contractIds :: [Text],
    mostRecentActivityTime :: Int,
    anyoneCanJoin :: Bool,
    mostRecentContractAddedTime :: Int,
    createdTime :: Int,
    memberIds :: [Text],
    slug :: Text,
    about :: Text
} deriving (Show, Eq)

data LiteUser = LiteUser {
    id :: Text,
    createdTime :: Float,
    
    name :: Text,
    username :: Text,
    url :: Text,
    avatarUrl :: Maybe Text,
    
    bio :: Maybe Text,
    bannerUrl :: Maybe Text,
    website :: Maybe Text,
    twitterHandle :: Maybe Text,
    discordHandle :: Maybe Text,
    
    balance :: Float,
    totalDeposits :: Float,
    totalPnLCached :: Float,
    creatorVolumeCached :: Float
} deriving (Show, Eq)