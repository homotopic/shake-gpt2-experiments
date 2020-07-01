{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

import           Control.Comonad
import           Control.Lens hiding (view)
import           Data.Aeson.Lens
import           Data.List.Split
import           RIO hiding (some, try, many)
import qualified RIO.ByteString.Lazy as LBS
import           RIO.List
import           RIO.Partial
import           RIO.List.Partial
import qualified RIO.Set             as S
import qualified RIO.Text            as T
import qualified RIO.Text.Partial
import           RIO.State
import           Shakebook
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer
import           Text.Pandoc.Readers
import           Text.Pandoc.Definition
import           Text.Pandoc.Options
import           Text.Pandoc.Class
import           Text.Pandoc.Writers
import qualified Control.Exception as EUnsafe
import Control.Monad.Catch hiding (try)
import           Text.Pandoc.Walk

type Parser = Parsec Void Text

squiggles :: Parser a -> Parser a
squiggles = between (string "{{") (string "}}")

data Affiliation = Alliance | Horde | Neutral | Hostile
  deriving (Eq, Show, Read, Ord, Enum)

data NPC = NPC {
  npcAffiliation :: Affiliation
, npcName        :: Text
, npcRole        :: Maybe Text
, npcIcon        :: Text
} deriving (Eq, Show, Ord)

mediafield :: Parser Text
mediafield = T.pack <$> many (alphaNumChar <|> spaceChar <|> char '\'' <|> char '\"' <|> char '-')

npc :: Parser NPC
npc = squiggles $ do
  void $ string "NPC" >> char '|'
  npcAffiliation <- affiliation
  void $ char '|'
  npcName <- mediafield
  void $ char '|'
  npcRole <- optional . try $ do 
    a <- mediafield
    void $ char '|'
    return a
  void $ optional . try $ mediafield >> char '|'
  npcIcon <- iconmwfield
  return NPC{..}

affiliation :: Parser Affiliation
affiliation = choice [
   Alliance <$ string "Alliance"
 , Horde    <$ string "Horde"
 , Neutral  <$ string "Neutral"
 , Hostile  <$ string "combat"
 ]

iconmwfield :: Parser Text
iconmwfield = void (string "icon=") >> mediafield

authormwfield :: Parser Text
authormwfield = void (string "author=") >> mediafield

titlemwfield :: Parser Text
titlemwfield = void (string "title=") >> mediafield

publishermwfield :: Parser Text
publishermwfield = void (string "publisher=") >> mediafield

yearmwfield :: Parser Text
yearmwfield = void (string "year=") >> mediafield

isbnmwfield :: Parser Text
isbnmwfield = void (string "isbn=") >> mediafield

data CiteBook = CiteBook {
  cbAuthor :: Text
, cbTitle :: Text
, cbPublisher :: Text
, cbYear :: Text
, cbISBN :: Text
} deriving (Eq, Show, Ord)

readMediawikiFile :: (MonadAction m, MonadThrow m) => ReaderOptions -> Path Rel File -> m Pandoc
readMediawikiFile ropts src = readFile' src >>= runPandocA . readMediaWiki ropts

viewCmTitles = toListOf (key "query" . key "categorymembers" . values . key "title" . _String)

testNPC1 = "{{NPC|Neutral|Lovely|Happy Holaua's Companion|icon=MonkeyKing}}"
testNPC2 = "{{NPC|Horde|Enforcer Dakanji|icon=Zandalari Male}}"
testNPC3 = "{{NPC|Horde|Rastakhan||King Rastakhan|icon=Rastakhan}}"
testNPC4 = "{{NPC|Neutral|Gluk-Gluk|Innkeeper|icon=Hozen}}"

-- | Pandoc filter - strip all links and replace with raw link text.
stripLinks :: [Inline] -> [Inline]
stripLinks ((Link _ txt _) : xs) = txt <> stripLinks xs
stripLinks (x : xs)             = x : stripLinks xs
stripLinks []                   = []

-- | Pandoc filter - delete all images.
deleteImages :: [Inline] -> [Inline]
deleteImages ((Image _ _ _ ) : xs) = deleteImages xs
deleteImages (x : xs)              = x : deleteImages xs
deleteImages []                    = []

-- | Pandoc filter - delete all notes.
deleteNotes :: [Inline] -> [Inline]
deleteNotes ((Note _) : xs) = deleteNotes xs
deleteNotes (x : xs)        = x : deleteNotes xs
deleteNotes []              = []

-- | Pandoc filter - delete all citations.
deleteCites :: [Inline] -> [Inline]
deleteCites ((Cite _ _) : xs) = deleteCites xs
deleteCites (x : xs)          = x : deleteCites xs
deleteCites []                = []

junkSections = [ [Str "External", Space, Str "links"]
               , [Str "Fan art"]
               , [Str "Gallery"]
               , [Str "References"]
               , [Str "Patch", Space, Str "changes"]
               , [Str "See", Space, Str "also"]
               , [Str "Videos"]
               ]

isHeader :: Block -> Bool
isHeader (Header _ _ _) = True
isHeader _ = False

splitSections :: [Block] -> [[Block]]
splitSections = split (keepDelimsL $ whenElt isHeader)

isJunkHeader :: Block -> Bool
isJunkHeader (Header _ _ a') = elem a' junkSections
isJunkHeader _ = False

isJunkSection :: [Block] -> Bool
isJunkSection (x : xs) = isJunkHeader x
isJunkSection [] = True

stripJunkSections :: [Block] -> [Block]
stripJunkSections = join . filter (not . isJunkSection) . splitSections 

stripMWData :: [Inline] -> [Inline]
stripMWData t@((Str x) : xs) = if "{{#data:" `T.isPrefixOf` x then [] else t
stripMWData a = a

npcToPandoc :: NPC -> [Inline]
npcToPandoc x = [Str $ npcName x, Space] ++ (maybe [] (\k -> [Str "-", Space, Str k]) $ npcRole x)

convertNPCs :: [Block] -> [Block]
convertNPCs t@(x@(RawBlock b k) : xs) = (maybe x (Plain . npcToPandoc) $ parseMaybe npc k) : xs
convertNPCs a = a

stripRawInline :: [Inline] -> [Inline]
stripRawInline t@((RawInline _ _) : xs) = stripRawInline xs
stripRawInline x = x

stripRawBlock :: [Block] -> [Block]
stripRawBlock t@((RawBlock _ _) : xs) = stripRawBlock xs
stripRawBlock x = x

data ApiType = ApiType1 | ApiType2
  deriving (Eq, Show, Generic)

instance FromJSON ApiType

data WikiManifest = WikiManifest {
  api               :: Text
, apiType           :: ApiType
, includeCategories :: [Text]
, includePages      :: [Text]
} deriving (Eq, Show, Generic)

instance FromJSON WikiManifest

recCollectP :: (MonadUnliftAction m, Ord a) => (a -> m (Set a)) -> Set a -> a -> m (Set a)
recCollectP g exs x = do
  x' <- g x
  xs' <- flip forP (recCollectP g (S.union exs x')) (toList $ S.filter (not . (`S.member` exs)) x')
  return $ foldr S.union (S.union (S.singleton x) x') xs'

main :: IO ()
main = runSimpleShakePlus $ do

  jsonLookup <- addRemoteJSONOracleCache

  let pullJson :: Text -> RAction LogFunc Value
      pullJson x = do
       logInfo $ displayShow $ "Polling " <> x
       k <- jsonLookup . RemoteJSONLookup $ x
       logDebug $ displayShow $ "Receieved: " <> (T.pack $ show k)
       return k

  let subcatRequest u a x = pullJson $ "https://" <> u <> "/" <> a <> "?action=query&list=categorymembers&cmtitle=" <> x <> "&cmlimit=500&cmtype=subcat&format=json"
  let pagesRequest u a x = pullJson $ "https://" <> u <> "/" <> a <> "?action=query&list=categorymembers&cmtitle=" <> x <> "&cmlimit=500&cmtype=page&format=json"
  let contentRequest u a x = pullJson $ "https://" <> u <> "/" <> a <> "?action=query&prop=revisions&titles=" <> x <> "&rvslots=*&rvprop=content&formatversion=2&format=json"

      recSubcats u a x = recCollectP (fmap (S.fromList . viewCmTitles) . subcatRequest u a) mempty x

      apiFor x = do
        let x' = $(mkRelDir "manifests/original") </> x </> $(mkRelFile "api")
        d <- doesFileExist x'
        case d of
          False -> return "api.php"
          True -> head . T.lines <$> readFile' (x')

  "out.txt" %> \out -> do
    xs  <- getDirectoryFiles $(mkRelDir ".") ["processed/markdown//*.md"]
    xs' <- forM xs (evaluate <=< readFile')
    let ys = map (T.unlines . (\x -> ["<|startoftext|>"] ++ T.lines x ++ ["<|endoftext|>"])) xs'
    writeFile' out $ T.unlines ys

  "raw/mediawiki/*/*.mediawiki" %> \out -> do
      let k = T.pack . (!! 2) . splitOn "/" . toFilePath$ out
      k' <- parseRelDir (T.unpack k)
      a <- apiFor k'
      (x, _) <- splitExtension . filename $ out
      y <- contentRequest k a (T.pack . toFilePath $ x)
      let y' = view (key "query"
                   . key "pages" . values
                   . key "revisions" . values
                   . key "slots" 
                   . key "main"
                   . key "content" . _String) $ y
      writeFile' out y'

  ("*/*.md" `within` $(mkRelDir "processed/markdown")) %^> \out -> do
    src <- blinkAndMapM $(mkRelDir "raw/mediawiki") (replaceExtension ".mediawiki") out
    a <- readFile' (fromWithin src)
    l <- liftIO $ runIO $ readMediaWiki (def { readerExtensions = extensionsFromList [Ext_smart]}) a
    case l of
       Left x -> writeFile' (fromWithin out) ""
       Right x -> do
      --   logInfo $ displayShow x
         let x' = walk stripJunkSections . walk convertNPCs . walk (stripLinks . deleteImages . deleteNotes) $ x
         --k <- runPandocA $ writeMarkdown (def { writerExtensions = pandocExtensions})  $ x'
         k <- runPandocA $ writeMarkdown def $ x'
    --     logInfo $ displayShow  k
         writeFile' (fromWithin out) k

  ("*/include" `within` $(mkRelDir "manifests/derived/")) %^> \out -> do
    let k = T.pack . (!! 0) . splitOn "/" . toFilePath . extract $ out
    let src = blinkLocalDir $(mkRelDir "manifests/original/") out
    xs <- readFileWithin src
    es <- readFileWithin $ fmap ((</> $(mkRelFile "exclude")) . parent) src
    k' <- parseRelDir (T.unpack k)
    a <- apiFor k'
    ys <- forP (T.lines xs) $ recSubcats k a
    let ys' = foldr S.union S.empty ys
    logInfo $ displayShow $ ys'
    zs <- forP (T.lines xs <> toList ys') $ pagesRequest k a
    let zs' = filter (not .  T.isInfixOf "/") $ filter (not . (`elem` (T.lines es))) $ join $ viewCmTitles <$> zs
    writeFile' (fromWithin out) $ T.unlines zs'

  let wikiManifest :: HasLogFunc r => Text -> RAction r ()
      wikiManifest x = do
      logInfo $ displayShow $ "Opening Wiki Manifest for " <> x
      x' <- parseRelDir (T.unpack x)
      xs <- readFile' $ $(mkRelDir "manifests/derived") </> x' </> $(mkRelFile "include")
      need $ flip map (filter (/= "") $ T.lines xs) $ T.unpack . (\a -> "processed/markdown/" <> x <> "/" <> a <> ".md")

  phony "mortalkombat" $ wikiManifest "mortalkombat.fandom.com"
  phony "pokemon"      $ wikiManifest "pokemon.gamepedia.com"
  phony "rickandmorty" $ wikiManifest "rickandmorty.fandom.com"
  phony "startrek"     $ wikiManifest "memory-alpha.fandom.com"
  phony "wikipedia"    $ wikiManifest "en.wikipedia.org"
  phony "wow"          $ wikiManifest "wow.gamepedia.com"

  want ["out.txt"]
