{-# LANGUAGE MultiWayIf, OverloadedStrings, ScopedTypeVariables, CPP #-}

module Text.Hxml (parseHxml) where

--  Prelude-like
import Control.Monad
import Data.Maybe
import Data.Char
import Data.Bifunctor
import Data.List
import Data.String
import Data.Word
import Control.Applicative hiding (some, many)
import Control.Monad.Trans (lift)
import Data.Void
import qualified Data.Set as Set

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.ByteString.Builder
import qualified Data.Set as S
import Control.Monad.Trans.Except
import Control.Monad.State.Strict

import Text.Megaparsec hiding (State(..))
import qualified Text.Megaparsec as M
import Text.Megaparsec.Byte hiding (char)


type Byte = Word8
type Bytes = BL.ByteString
type Chunk = Builder

type TagData = (Bytes, [Attr])
type MyState = [(Bool, TagData)]
type MyError = ParseErrorBundle Bytes Void

--  Name and value including quotes.
type Attr = (Bytes, Maybe Bytes)

type Line = Pos


type Parser = ParsecT Void Bytes (State MyState)
type LineM = ExceptT MyError (State (Line, MyState))


tabWidth = 4 :: Int


__ :: Bool
__ = True

charToByte :: Char -> Byte
charToByte = toEnum . fromEnum
byteToChar :: Byte -> Char
byteToChar = toEnum . fromEnum

--  Treat bytes as chars.
char :: Char -> Parser Byte
char = single . charToByte

oneOfC :: String -> Parser Byte
oneOfC = oneOf . map charToByte

ctest :: (Char -> a) -> Byte -> a
ctest = (. byteToChar)

showb :: Builder -> String
showb = BLC.unpack . toLazyByteString

--  Tags that HTML5 requires have a separate closing tag.
nonVoids :: Set.Set Bytes
nonVoids = Set.fromList [
   "a", "abbr", "acronym", "address", "applet", "article", "aside", "audio",
   "b", "basefont", "bdi", "bdo", "big", "blockquote", "body", "button",
   "canvas", "caption", "center", "cite", "code", "colgroup",
   "data", "datalist", "dd", "del", "details", "dfn", "dialog", "dir", "div",
       "dl", "dt",
   "em",
   "fieldset", "figcaption", "figure", "font", "footer", "form", "frame",
       "frameset",
   "h1", "h2", "h3", "h4", "h5", "h6", "head", "header", "hgroup", "html",
   "i", "iframe", "ins", "isindex",
   "kbd",
   "label", "legend", "li",
   "main", "map", "mark", "menu", "menuitem", "meter",
   "nav", "noframes", "noscript",
   "object", "ol", "optgroup", "option", "output",
   "p", "pre", "progress",
   "q",
   "rb", "rp", "rt", "rtc", "ruby",
   "s", "samp", "script", "section", "select", "small", "source", "span",
       "strike", "strong", "style", "sub", "summary", "sup",
   "table", "tbody", "td", "template", "textarea", "tfoot", "th", "thead",
       "time", "title", "tr", "tt",
   "u", "ul",
   "var", "video",
   "svg"  -- since commonly used
   ]

lz :: Bytes -> Builder
lz = lazyByteString

ws :: [Byte] -> Builder
ws = lazyByteString . BL.pack

--  Don't know what this is, but I can make one.
lineToPosState :: s -> Pos -> PosState s
lineToPosState s lno = PosState s 0 ((initialPos "") { sourceLine=lno }) (mkPos tabWidth) ""

blockError :: Int -> String -> MyError
blockError lno err =
   ParseErrorBundle (pure (FancyError 0 (Set.singleton $ ErrorFail err))) $
      lineToPosState "" (mkPos lno)

-- newErrorMessage (Message err) (newPos "" lno 1)

--  Does first tag close second tag?
closes :: Maybe TagData -> TagData -> Bool
closes Nothing  _               = True
closes (Just (ct, ca)) (ot, oa) =
   ot==ct && S.fromList ca `S.isSubsetOf` S.fromList oa

parseComment :: Parser Builder
parseComment = do
   let (cs, ce) = ("<!--", "-->") -- :: IsString s => [s]
   void $ try (chunk cs)
   b <- manyTill anySingle ((eof >> fail "runaway comment") <|> try (chunk ce))
   pure $ lz cs <> ws b <> lz ce

parseSpecial :: Parser Builder
parseSpecial = do
   a <- try (chunk "<!") <|> try (chunk "<?")
   b <- manyTill anySingle ((eof >> fail ("runaway " <> show a)) <|> char '>')
   pure $ lz a <> ws b <> lz ">"

parseAttrVal :: Parser Bytes
parseAttrVal = parseQ <|> parseId where
   parseQ = do
      c <- oneOfC "'\"" -- <|> fail "attribute syntax error"
      ct <- takeWhileP Nothing (/=c)
      void $ anySingle
      pure $ let c' = BL.singleton c in c' <> ct <> c'

parseTag :: Parser Builder
parseTag = do
   void $ char '<'
   clo <- isJust <$> optional (char '/')
   tdatam <- optional $ do
      tag <- parseId
      attrs <- many $ try $ do  -- TODO avoid try here
         void $ some spaceChar -- <|> fail "tag syntax error"
         nm <- parseId
         attr <- optional $ char '=' >> parseAttrVal
         pure (nm, attr)
      pure (tag, attrs)
   if clo
   then do
      --  Close tag.
      void $ char '>'
      stk <- get
      case stk of
         ((False, tdata') : rest) | closes tdatam tdata' -> do
            put rest
            pure $ renderClose tdata'
         _  -> fail "unmatching close tag"
   else do
      tdata@(tag, _) <- maybe (fail "empty open tag") pure tdatam
      sp <- many spaceChar
      fin <- optional (char ':' <|> char '/')
      inner <- takeWhileP Nothing $ ctest (/='>')
      void $ char '>' <|> fail ("runaway <" <> show tag <> "> tag")
      let noInner = unless (BL.null inner) $
            fail $ "tag extraneity: " <> take 20 (show inner)
      case byteToChar <$> fin of
         --  Regular open tag.
         Nothing -> do
            noInner
            modify ((False, tdata) :)
            pure $ renderOpen tdata
         --  Self-closed.
         Just '/' -> do
            pure $ if
               | tag=="#" -> ""
               | BL.null inner && tag `Set.notMember` nonVoids
                          -> renderSelf tdata
               | __       ->
                  renderOpen tdata <> lz inner <> renderClose tdata
         --  Block start.
         _ {- Just ':' -} -> do
            noInner
            stk <- get
            case nub $ map fst stk of
               False : True : _ -> fail "illegal block tag mix"
               _                -> pure ()
            when (not$null sp) $
               void $ many $ oneOfC "\r\n"
            put $ ((True, tdata) : stk)
            pure $ renderOpen tdata

parseToken :: Parser Builder
parseToken = skipChommentBlock <|>
   (chommentFilter =<<
      parseComment <|> parseSpecial <|> parseText <|> parseTag)

--  The "line" may include intra-tag newlines.
parseLine :: Parser Builder
parseLine = mconcat <$> many parseToken

--  These control line numbering.
getLineNo = sourceLine <$> getSourcePos :: Parser Pos
-- setLineNo ln = setPosition =<< flip setSourceLine ln <$> getPosition :: Parser ()

--  Successively try numbers of lines till we're not mid-tag.
--  Return result + rest in Line Monad.
minMunch :: [Bytes] -> LineM (Bytes, [Bytes])
minMunch []     = error "minMunch called with nothing"
minMunch (s:ss) = do
   (ln, st) <- lift get
   let tup = liftA2 (,)
   let pstate = M.State s 0 (lineToPosState s ln)
#if MIN_VERSION_megaparsec(8,0,0)
         []
#endif
   case flip evalState st $ runParserT' (tup parseLine (tup getLineNo get)) pstate of
      (_, Left err) {- verify it's a runaway error? -} ->
         case ss of
            s':ss' -> minMunch (s<>s' : ss')
            _      -> throwE err
      (_, Right (ch, lnst)) -> do
         lift $ put lnst
         pure (toLazyByteString ch, ss)

renderAttrVal :: Bytes -> Builder
renderAttrVal v = case BLC.uncons v of
   Just (x, _) | x == '\'' || x == '\"' -> lz v
               | __                     -> "\"" <> lz v <> "\""

--  Inserts a preceding space always.
renderAttr :: Attr -> Builder
renderAttr (n, v)
   | "_" `BLC.isPrefixOf` n = ""
   | __   = " " <> lz n <> "=" <> renderAttrVal (fromMaybe n v)

--  First argument specifies if self-closing.
renderTag :: Bool -> TagData -> Builder
renderTag _   ("#", _)  = ""
renderTag clo (tag, attrs) =
   "<" <> lz tag <> foldMap renderAttr attrs <>
      (if clo then "/" else "") <> ">"

renderOpen = renderTag False
renderSelf = renderTag True

--  This should be fed the *opening* tag.
renderClose :: TagData -> Builder
renderClose ("#", _) = ""
renderClose (tag, _) = "</" <> lz tag <> ">"

--  Characters ok for identifiers.
--  Very permissive for now.
isIdChar :: Byte -> Bool
isIdChar c' = not (isAscii c) || isDigit c
   || (not (isSpace c) && c `notElem` (Set.fromList "<>'\"[]:=/.,"))
      where c = byteToChar c'

colonInId :: Parser Byte
colonInId = try $ do
   char ':' <* (lookAhead $ satisfy isIdChar)

parseId :: Parser Bytes
parseId = BL.pack <$> some (satisfy isIdChar <|> colonInId)

parseText :: Parser Builder
parseText = lz <$> takeWhile1P Nothing (/= charToByte '<')

--  "Chomment" is a hash-comment (as opposed to an html one).
chommentFilter :: Builder -> Parser Chunk
chommentFilter s = do
   b <- inChomment <$> get
   pure $ if b then "" else s

inChomment :: MyState -> Bool
inChomment = any ((=="#") . fst . snd)

inChommentBlock :: MyState -> Bool
inChommentBlock ((True, ("#", _)) : _) = True
inChommentBlock _                      = False

{-# INLINE isBlankLine #-}
isBlankLine :: (Eq s, IsString s) => s -> Bool
isBlankLine = flip elem $ map fromString ["", "\r\n", "\n"]

skipChommentBlock :: Parser Builder
skipChommentBlock = do
   guard =<< inChommentBlock <$> get
   void $ some $ anySingle
   pure ""

tabUp :: Int -> Int
tabUp i = (i `div` tabWidth + 1) * tabWidth

indentOf :: Bytes -> Bytes
indentOf l = BLC.takeWhile (`elem` Set.fromList " \t") l

indentSize :: Bytes -> Int
indentSize = BLC.foldl f 0 . indentOf where
   f i '\t' = tabUp i
   f i _    = i + 1

--  TODO combine these functions?
dedent :: Int -> Bytes -> Bytes
dedent d = loop 0 where
   loop i bs | i==d = bs
             | __   = case BLC.uncons bs of
      Just (' ',  bs') -> loop (i+1) bs'
      Just ('\t', bs') -> loop (tabUp i) bs'
      _                -> bs

--  First argument is basic line, all lines more indented than the next
--  non-blank line under it are then collected, skipping blank lines.
--  Returns split of indented set and rest.
handleIndent :: Bytes -> [Bytes] -> ([Bytes], [Bytes])
handleIndent l0 ls =
   case dropWhile isBlankLine ls of
      []   -> ([], ls)
      l1:_ -> if indentSize l0 >= ind then ([], ls) else loop ls where
         ind = indentSize l1
         proc l = indentOf l0 <> dedent ind l
         loop ls' =  -- ugly?
            case key of
               l:_ | indentSize l >= ind
                  -> let (a, b) = loop rest in (bls ++ proc l : a, b)
               _  -> ([], ls')
            where (bls, (key, rest)) = second (splitAt 1) $ span isBlankLine ls'

--  Add to end or before final newline sequence.
--  TODO combine with below somehow?
addAtEol :: Bytes -> Bytes -> Bytes
addAtEol add s = case BLC.unsnoc s of
   Just (s', '\n') -> case BLC.unsnoc s' of
      Just (s'', '\r') -> s'' <> add <> "\r\n"
      _                -> s' <> add <> "\n"
   _               -> s <> add

--  Find the eol character of Bytes.
findEol :: Bytes -> Bytes
findEol bts = case first (fmap snd . BLC.unsnoc) <$> BLC.unsnoc bts of
      Just (Just '\r', '\n') -> "\r\n"
      Just (_, '\n')         -> "\n"
      _                      -> ""

munchLoop :: [Bytes] -> LineM Bytes
munchLoop []        = pure ""
munchLoop ls@(l0:_) = do
   let lb = toLazyByteString
   let addPos x = mkPos . (+x) . unPos
   (_, prestk) <- lift get  -- hacky??
   (chunk, rest) <- minMunch ls
   (lno, stk) <- lift get
   (chunk', cont) <- let (inner, notinner) = break fst stk in
      if null notinner
      then pure (chunk, rest)  -- no block tags, continue.
      else do
         let (block, postblock) = handleIndent l0 rest
         let (bstack, outer) = first (map snd) $ span fst notinner
         chunk' <- if
            | inChommentBlock stk -> do
               put (addPos (length block) lno, outer)
               let eol = if null block then "" else findEol (last block)
               pure $ chunk <> lb (foldMap renderClose bstack) <> eol
            | inChomment prestk -> do
               put (addPos (length block) lno, outer)
               pure chunk
            | __ -> do
               -- dislike this "imperative" style
               put (lno, inner)
               ichunk <- munchLoop block
               (lno', stk') <- get
               when (not$null stk') $ throwE $ blockError (unPos lno') $
                  "block ended with unclosed " <> showb (renderOpen (snd $ head stk'))
               put (lno', outer)
               pure $ addAtEol (lb (foldMap renderClose bstack)) (chunk <> ichunk)
         pure (chunk', postblock)
   mappend chunk' <$> munchLoop cont

parseLines :: [Bytes] -> Either MyError Bytes
parseLines ls =
   case runState (runExceptT (munchLoop ls)) (mkPos 1, []) of
      (Left err, _)        -> Left err
      (_, (_, (_, tag):_)) ->
         Left $ blockError (length ls) $
            "file ended with unclosed " <> showb (renderOpen tag)
      (Right ch, _)        -> Right ch

--  Unlike lines, preserves final newlines.
lines' :: Bytes -> [Bytes]
lines' s = maybe [s] (\n -> let (a, b) = BL.splitAt n s in a : lines' b) $
         (+1) <$> BLC.elemIndex '\n' s

parseHxml :: Bytes -> Either MyError Bytes
parseHxml = parseLines . lines'

