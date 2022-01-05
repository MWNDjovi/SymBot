{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

module DiscordPort
where

import Control.Monad (when, void, forever)
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import UnliftIO (liftIO, try, IOException)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import UnliftIO.Concurrent
import Discord
import Discord.Types
import qualified Discord.Requests as R
import PdfSearcher (queryPdf, SearchResult (foundSentance), page)

main :: IO ()
main = pingpongExample

-- | Replies "pong" to every message that starts with "ping"
pingpongExample :: IO ()
pingpongExample = do
  tok <- TIO.readFile "./src/auth-token.secret"

  -- open ghci and run  [[ :info RunDiscordOpts ]] to see available fields
  err <- runDiscord $ def { discordToken = tok
                          , discordOnStart = changeAction "Witcher på netflix" ActivityTypeStreaming
                          , discordOnEnd = liftIO $ putStrLn "Ended"
                          , discordOnEvent = eventHandler
                          , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
                          }

  -- only reached on an unrecoverable error
  -- put normal 'cleanup' code in discordOnEnd
  TIO.putStrLn err

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
    MessageCreate m -> do
        when (not (fromBot m) && isPing m) $ do
            void $ changeAction "ping pong" ActivityTypeCompeting
            void $ restCall (R.CreateReaction (messageChannel m, messageId m) "pingpong")
            threadDelay (2 * 10^(6 :: Int))
            -- A very simple message.
            void $ restCall (R.CreateMessage (messageChannel m) "smack \n pong!")
            void $ changeAction "larm" ActivityTypeListening
        when (not (fromBot m) && isQuery m) $ do
            void $ restCall (R.CreateReaction (messageChannel m, messageId m) "book")
            void $ changeAction "speed reading" ActivityTypeCompeting
            let query = (T.unpack . T.unwords . drop 1 . T.words . messageText) m
            result <- liftIO $ queryPdf query "placeholder"
            let prettyResult = (printResult . take 2)result            
            void $ restCall (R.CreateMessage (messageChannel m) prettyResult )
            void $ changeAction "spændene eventyr" ActivityTypeListening
    _ -> return ()


changeAction :: T.Text -> ActivityType   -> DiscordHandler()
changeAction action activityType = do
    let activity = Activity {
        activityName = action,
        activityType = activityType,
        activityUrl = Nothing
        }
    let opts = UpdateStatusOpts { updateStatusOptsSince = Nothing
        , updateStatusOptsGame = Just activity
        , updateStatusOptsNewStatus = UpdateStatusOnline
        , updateStatusOptsAFK = False
    }
    sendCommand (UpdateStatus opts)


fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `T.isPrefixOf`) . T.toLower . messageText

isQuery :: Message -> Bool
isQuery = ("!search" `T.isPrefixOf`) . T.toLower . messageText


printResult :: [SearchResult] -> T.Text
printResult = foldl (\acc -> T.append acc . show' ) "I found the following: \n"

show' :: SearchResult -> T.Text
show' result = T.concat ["On page ", (T.pack . show . page) result,":\n" , T.dropWhile (' ' == ) $  foundSentance result ,".\n"]