{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Optics.RedditClone where
-- This module contains types and lens operations that might be 
-- required in an actual clone project.

import Control.Lens
import Data.Text (Text)
import GHC.Int (Int64,Int32)
import Data.Time (UTCTime(..),getCurrentTime,Day,utctDay,secondsToDiffTime)
import qualified Data.ByteString.Lazy as BSL

newtype EntryID = EntryID Int32 deriving (Show,Eq)
newtype CommunityID = CommunityID Int32 deriving (Show,Eq)
newtype UserID = UserID Int32 deriving (Show,Eq)
newtype Tag = Tag Text deriving (Show,Eq)

data Entry = Entry {
   _entryID          :: EntryID
 , _entryTitle       :: Text
 , _entryDescription :: Maybe Text
 , _tags             :: Maybe [Tag]
 , _numberOfLikes    :: Int64
 , _entryCommunityID :: CommunityID
 , _entryCommunityName  :: Text
 , _creatorID        :: UserID
 , _creatorName      :: Text
 , _numberOfComments :: Int32
 , _entryCreatedAt   :: UTCTime
 , _entryUpdatedAt   :: UTCTime
} deriving (Show,Eq)

data Community = Community {
    _communityID          :: CommunityID
  , _communityName        :: Text
  , _communityDescription :: Text
  , _numberOfMembers      :: Int64
  , _communityCreatedAt   :: UTCTime
  , _communityUpdatedAt   :: UTCTime
} deriving (Show,Eq)

data User = User {
    _userID :: UserID
  , _userName :: Text
  , _userEmail :: Text
  , _joinedOn :: UTCTime
  , _profileImage :: BSL.ByteString
} deriving (Show,Eq)

data PageConfig = PageConfig {
    _darkMode :: Bool
  , _cookieAllowed :: Bool
  , _clickCounts :: Int32   -- For some reasons :)
} deriving (Show,Eq)

data HomePageInfo = HomePageInfo {
    _entry :: Entry
  , _community :: Community
  , _user   :: User
  , _pageConfig :: PageConfig
} deriving (Show,Eq)

mkUser :: UTCTime -> User
mkUser currTime = User {
    _userID = UserID 1
  , _userName = "John"
  , _userEmail = "john@abc.com"
  , _joinedOn = currTime
  , _profileImage = ""
}

mkCommunity :: UTCTime -> Community
mkCommunity currTime = Community {
    _communityID = CommunityID 1
  , _communityName = ""
  , _communityDescription = ""
  , _numberOfMembers = 0
  , _communityCreatedAt = currTime
  , _communityUpdatedAt = currTime
}

mkEntry :: UTCTime -> Entry
mkEntry currTime = Entry {
    _entryID = EntryID 1
  , _entryTitle = ""
  , _entryDescription = Nothing
  , _tags = Nothing
  , _numberOfLikes = 0
  , _entryCommunityID = CommunityID 1
  , _entryCommunityName = ""
  , _creatorID = UserID 1
  , _creatorName = ""
  , _numberOfComments = 0
  , _entryCreatedAt = currTime
  , _entryUpdatedAt = currTime
}

mkPageConifg :: PageConfig
mkPageConifg = PageConfig {
    _darkMode = False
  , _cookieAllowed = False
  , _clickCounts = 0
}

mkHomePage :: User -> Community -> Entry -> PageConfig -> HomePageInfo
mkHomePage u c e pc = HomePageInfo {
    _entry = e
  , _community = c
  , _user = u
  , _pageConfig = pc
}


-- Writing Lens for user type manually
-- UserID
getUserID :: User -> UserID
getUserID = _userID

setUserID :: User -> UserID -> User
setUserID u uid = u { _userID = uid } 

userID :: Lens' User UserID
userID = lens getUserID setUserID

-- UserName 
getUserName :: User -> Text
getUserName = _userName

setUserName :: User -> Text -> User
setUserName u n = u { _userName = n }

userName :: Lens' User Text
userName = lens getUserName setUserName

-- UserEmail
getUserEmail :: User -> Text
getUserEmail = _userEmail

setUserEmail :: User -> Text -> User
setUserEmail u e = u { _userEmail = e }

userEmail :: Lens' User Text
userEmail = lens getUserEmail setUserEmail

-- JoinedOn
getJoinedOn :: User -> UTCTime
getJoinedOn = _joinedOn

setJoinedOn :: User -> UTCTime -> User
setJoinedOn u j = u { _joinedOn = j }

joinedOn :: Lens' User UTCTime
joinedOn = lens getJoinedOn setJoinedOn

-- Profile Image
getProfileImage :: User -> BSL.ByteString
getProfileImage = _profileImage

setProfileImage :: User -> BSL.ByteString -> User
setProfileImage u p = u { _profileImage = p }

profileImage :: Lens' User BSL.ByteString
profileImage = lens getProfileImage setProfileImage
-- end of writing your own lens for a type

makeLenses ''Entry
makeLenses ''PageConfig
makeLenses ''Community
makeLenses ''HomePageInfo
--makeLenses ''User

-- Virtual Field
joinDate :: Lens' User Day
joinDate = lens getter setter
  where
    getter = utctDay . view joinedOn
    setter user1 newVal = set joinedOn 
            (UTCTime newVal (secondsToDiffTime 0)) user1

main1 :: IO ()
main1 = do
    currTime <- getCurrentTime
    let user0 = mkUser currTime
        community0 = mkCommunity currTime
        entry0 = mkEntry currTime
        pageConfig0 = mkPageConifg
        homePageInfo0 = mkHomePage user0 community0 entry0 pageConfig0

    -- view
    print $ view userName user0
    print $ view (user . userName) homePageInfo0
    print $ homePageInfo0 ^. user . userName
 
    -- Set
    print $ set communityDescription "something" community0
    print $ community0 & communityName .~ "Something"
    print $ set (community . numberOfMembers) 23 homePageInfo0
    print $ homePageInfo0 & community . numberOfMembers .~ 23 

    -- Chaining Multiple Opeartions together
    print $ community0 & communityName .~ "some name"
                       & communityDescription .~ "some Description"
    -- Over
    print $ over clickCounts (+1) pageConfig0
    print $ pageConfig0 & clickCounts %~ (+1)

    print $ pageConfig0 & clickCounts %~ (+1)
                        & clickCounts %~ (+2)

    -- Using virtual field
    print $ view joinDate user0

    print "Hello Lens!"

