module SocialMediaPseudocode where



type SocialMediaOracle -- let this type represent some 
                       -- abstract environment that we can query for 
                       -- friend relationships, posts, etc if we have it
                       -- but we must also maintain it.
                       --
{- SOME MONADIC HELPER FUNCTIONS -}
-- let lift here be a way to call a monad further down a stack. 
-- e.g. let us call an IO action from a state monad if the state monad is a `StateT s IO`
lift :: (Monad m1, Monad m2) => m2 a -> m1 m2 a
get  :: StateT s IO s 
put  :: s -> StateT s IO ()


-- here is an imaginary program that cross posts recent blog posts to twitter for the user
program1 :: StateT SocialMediaOrcale IO () -- A State monad holding an oracle, and all computation gets evaluated to IO.
program1 = do
  orcale              <- get                            
  let user            =  userFrom oracle
  blogs               <- lift $ askInternetForBlogsBy user
  let mostRecentTweet =  head $ tweetsFrom oracle
  let recentBlogs     =  filter ((> (timeOf mostRecentTweet)) . timeOf) blogs
  mapM                (lift . (postToTwitter user) . (take 280)) (map textOf recentBlogs)
  tweets              <- lift $ askInternetForTweetsBy user 
  put                 $  oracle `withTweets` tweets
  return              ()
  where 
    userFrom               :: SocialMediaOracle -> User
    askInternetForBlogsBy  :: User -> IO [Blog]
    tweetsFrom             :: SocialMediaOracle -> [Tweet]
    timeOf                 :: Tweet -> TimeStamp 
    postToTwitter          :: User -> String -> IO (Either NetworkError ())
    withTweets             :: SocialMediaOracle -> [Tweet] -> SocialMediaOracle 
    askInternetForTweetsBy :: User -> IO [Tweet]
    textOf                 :: (Postable p) => p -> String

-- here is an imaginary program that queries recent social media posts and notifies the user of a reminder
program2 :: StateT SocialMediaOracle IO Tasks 
program2 = do
  oracle <- get
  let user = userFrom oracle
  blogs  <- lift $ askInternetForBlogsBy user 
  tweets <- lift $ askInternetForTweetsBy user 
  today  <- lift $ todaysDate 
  let humanOracle = -- hmm~, should I give this IO access?
    (learnFromModel oracle) `specializeFor` TaskPlanningForDate today 
  let tasks  <- filter isJust $ map (getTask humanOracle) $ (textOf tweets) ++ (textOf blogs)
  putStrLn $ show tasks
  return tasks
  where 
    userFrom :: SocialMediaOracle -> User 
    askInternetForBlogsBy :: User -> IO [Blog]
    askInternetForTweetsBy :: User -> IO [Tweet]
    todaysDate :: IO Date 
    learnFromModel :: (Humanable h) => h -> Model
    specalizeFor :: Model -> Criteria -> Model
    getTask :: Model -> String -> Maybe String
    textOf :: (Postable p) => p -> String 
    isJust :: Maybe a -> Bool
    
