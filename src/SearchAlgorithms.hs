module SearchAlgorithms
where

import qualified Data.Text as T
import Data.Char ( toLower )
import SharedTypes

searchText :: Query -> T.Text -> [T.Text]
searchText query = filter (searchMethod $ targetedText query )  . splitToSentances . preprocessing
    where
      searchMethod = case searchType query of
        Partial -> partialSearch
        Complete -> completeSearch
        Phonetic -> undefined
      preprocessing = case caseSensitive query of
        Sensitive  -> id
        Insensitive  -> lowerCase

partialSearch :: T.Text -> T.Text-> Bool
partialSearch = T.isInfixOf

completeSearch :: T.Text -> T.Text -> Bool
completeSearch query subject = case subjectSequence of
    []-> False
    x -> querySequence == (take queryLength subjectSequence)  || completeSearch query ((T.unwords . drop 1) subjectSequence)
    where
        queryLength = length querySequence
        querySequence = T.words query
        subjectSequence = T.words subject

splitToSentances :: T.Text -> [T.Text]
splitToSentances = let delimiter =  T.pack "." in T.splitOn delimiter

lowerCase :: T.Text -> T.Text
lowerCase = T.map toLower