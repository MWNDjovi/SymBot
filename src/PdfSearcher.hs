module PdfSearcher
    ( queryPdf,
    SearchResult(..)
    ) where
import SharedTypes
import qualified Pdf.Core.Object
import qualified Pdf.Core.Object.Util
import qualified Pdf.Core.XRef
import qualified Pdf.Core.Stream (knownFilters)
import qualified Pdf.Core.Util
import Pdf.Core.Exception
import qualified Pdf.Core.IO.Buffer as Buffer
import qualified Pdf.Core.File as File
import qualified Pdf.Core.Writer
import qualified Pdf.Document

-- Using the internals to switch from 'pdf-toolbox-document' level
-- to 'pdf-toolbox-core'
import qualified Pdf.Document.Internal.Types

import qualified Data.HashMap.Strict as HashMap
import Control.Monad
import System.IO
import Data.Char
import qualified Data.ByteString as ByteString
import qualified Data.Text as T
import Data.Text (Text, isInfixOf)
import Pdf.Document (pageExtractText)
import qualified Data.Text.Encoding as T

testFile :: [Char]
testFile = "C:/Users/Martin/Documents/Haskells/SymBot/src/input.pdf"

queryPdf :: String -> String -> IO [SearchResult]
queryPdf query book  = do
  openPdf $ do
    extractPages   
      (\pages -> pure $ concatMap (executeQuery query) (zip pages [1 .. length pages]) )  
    
  

openPdf :: (Pdf.Document.Internal.Types.PageNode -> IO a) -> IO a
openPdf action= do
  Pdf.Document.withPdfFile testFile $ \pdf -> do
      encrypted <- Pdf.Document.isEncrypted pdf
      when encrypted $ do
        ok <- Pdf.Document.setUserPassword pdf Pdf.Document.defaultUserPassword
        unless ok $
          fail "need password"
      doc <- Pdf.Document.document  pdf
      catalog <- Pdf.Document.documentCatalog doc
      rootNode <- Pdf.Document.catalogPageNode catalog
      action rootNode

extractPages :: ([T.Text] -> IO a) -> Pdf.Document.Internal.Types.PageNode -> IO a
extractPages action rootPageNode = do 
  pageCount <- Pdf.Document.pageNodeNKids rootPageNode
  pages <- forM [0 .. (pageCount-1)] (\n -> do
    page <-  Pdf.Document.pageNodePageByNum rootPageNode n
    Pdf.Document.pageExtractText page
    )  
  action pages
  
data SearchResult = SearchResult
  {
    page :: Int,
    foundSentance :: T.Text
  } deriving Show

executeQuery ::  String -> (Text,Int)-> [SearchResult]
executeQuery query (page,pageNumber)  = map (\result -> SearchResult{page = pageNumber, foundSentance = result}) (searchPage query page)

searchPage::String -> T.Text-> [Text]
searchPage query page  = filter (T.isInfixOf packedQuery ) sentances
    where
      sentances = T.splitOn (T.pack ".") $ T.unwords $ filter (not . T.isInfixOf (T.pack "\n"))  $ T.words $ T.map toLower page
      packedQuery = T.pack $ map toLower query











