{-# OPTIONS_GHC -Wno-duplicate-exports #-}
module SharedTypes
(
    QueryType(..),
    CaseSensitive(..),
    Query (..)
)where
  import qualified Data.Text as T
  
  data QueryType = Partial | Complete | Phonetic deriving (Enum)
  data CaseSensitive = Sensitive | Insensitive deriving (Enum)
  data Query = Query {searchType :: QueryType , targetedText :: T.Text, caseSensitive:: CaseSensitive} 
