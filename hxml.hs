{-# LANGUAGE MultiWayIf #-}

--  Prelude-like
import Control.Monad
import Data.Monoid
import Data.Maybe
import Data.Char
import Data.Bifunctor
import Data.List
import Data.String
import Control.Applicative
import Control.Monad.Trans (lift)

import System.IO
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Set as S
import Control.Monad.Trans.Except
import Control.Monad.State.Strict

import Text.Parsec hiding ((<|>), many, optional, State(..))
import Text.Parsec.Pos (newPos)
import Text.ParserCombinators.Parsec.Error

type Bytes = BL.ByteString
type Chunk = [Char]  -- more convenient than packing or whatever

type TagData = (Chunk, [Attr])
type MyState = [(Bool, TagData)]
type MyError = ParseError

--  Name and value including quotes.
type Attr = (Chunk, Maybe Chunk)


type Parser = Parsec Bytes MyState
type LineM = ExceptT MyError (State (Line, MyState))


tabWidth = 4 :: Int


__ = True

blockError :: Int -> String -> ParseError
blockError lno err = newErrorMessage (Message err) (newPos "" lno 1)

--  Does first tag close second tag?
closes :: Maybe TagData -> TagData -> Bool
closes Nothing  _               = True
closes (Just (ct, ca)) (ot, oa) =
   ot==ct && S.fromList ca `S.isSubsetOf` S.fromList oa

parseComment :: Parser Chunk
parseComment = do
   let (cs, ce) = ("<!--", "-->") -- :: IsString s => [s]
   void $ try (string cs)
   b <- manyTill anyChar ((eof >> fail "runaway comment") <|> try (string ce))
   return $ cs <> b <> ce

parseSpecial :: Parser Chunk
parseSpecial = do
   a <- try (string "<!") <|> try (string "<?")
   b <- manyTill anyChar ((eof >> fail ("runaway " <> a)) <|> char '>')
   return $ a <> b <> ">"

parseAttrVal :: Parser Chunk
parseAttrVal = parseQ <|> parseId where
   parseQ = do
      c <- oneOf "'\"" -- <|> fail "attribute syntax error"
      ct <- manyTill anyChar $ char c
      return $ c : ct ++ [c]

parseTag :: Parser Chunk
parseTag = do
   void $ char '<'
   clo <- isJust <$> optional (char '/')
   tdatam <- optional $ do
      tag <- parseId
      attrs <- many $ try $ do  -- TODO avoid try here
         void $ some (satisfy isSpace) -- <|> fail "tag syntax error"
         nm <- parseId
         attr <- optional $ char '=' >> parseAttrVal
         return (nm, attr)
      return (tag, attrs)
   if clo
   then do
      --  Close tag.
      void $ char '>'
      stk <- getState
      case stk of
         ((False, tdata') : rest) | closes tdatam tdata' -> do
            putState rest
            return $ renderClose tdata'
         _  -> fail "unmatching close tag"
   else do
      tdata@(tag, _) <- maybe (fail "empty open tag") pure tdatam
      sp <- many (satisfy isSpace)
      fin <- optional (char ':' <|> char '/')
      inner <- many $ noneOf ">"
      void $ char '>' <|> fail ("runaway <" <> tag <> "> tag")
      let noInner = unless (null inner) $
            fail $ "tag extraneity: " <> take 20 inner
      case fin of
         --  Regular open tag.
         Nothing -> do
            noInner
            modifyState ((False, tdata) :)
            return $ renderOpen tdata
         --  Self-closed.
         Just '/' -> do
            pure $ if
               | tag=="#"   -> ""
               | null inner -> renderSelf tdata  -- special case
               | __         ->
                  renderOpen tdata <> inner <> renderClose tdata
         --  Block start.
         _ {- Just ':' -} -> do
            noInner
            stk <- getState
            case nub $ map fst stk of
               False : True : _ -> fail "illegal block tag mix"
               _                -> pure ()
            when (not$null sp) $
               void $ some $ oneOf "\r\n"
            putState $ ((True, tdata) : stk)
            pure $ renderOpen tdata

parseToken :: Parser Chunk
parseToken = skipChommentBlock <|>
   (chommentFilter =<<
      parseComment <|> parseSpecial <|> parseText <|> parseTag)

--  The "line" may include intra-tag newlines.
parseLine :: Parser Chunk
parseLine = mconcat <$> many parseToken

--  These control line numbering.
getLineNo = sourceLine <$> getPosition :: Parser Line
setLineNo ln = setPosition =<< flip setSourceLine ln <$> getPosition :: Parser ()

--  Successively try numbers of lines till we're not mid-tag.
--  Return result + rest in Line Monad.
minMunch :: [Bytes] -> LineM (Chunk, [Bytes])
minMunch []     = error "minMunch called with nothing"
minMunch (s:ss) = do
   (ln, st) <- lift get
   let tup = liftA2 (,)
   case runParser (tup (setLineNo ln >> parseLine) (tup getLineNo getState)) st "" s of
      Left err {- verify it's a runaway error? -} ->
         case ss of
            s':ss' -> minMunch (s<>s' : ss')
            _      -> throwE err
      Right (ch, lnst) -> do
         lift $ put lnst
         pure (ch, ss)

renderAttrVal :: Chunk -> Chunk
renderAttrVal v@('\'':_) = v
renderAttrVal v@('"' :_) = v
renderAttrVal v          = "\"" <> v <> "\""

--  Inserts a preceding space always.
renderAttr :: Attr -> Chunk
renderAttr ('_':_, _) = ""
renderAttr (n, v)     = " " <> n <> "=" <> renderAttrVal (fromMaybe n v)

--  First argument specifis if self-closing.
renderTag :: Bool -> TagData -> Chunk
renderTag _   ("#", _)  = ""
renderTag clo (tag, attrs) =
   "<" <> tag <> concatMap renderAttr attrs <>
      (guard clo >> "/") <> ">"

renderOpen = renderTag False
renderSelf = renderTag True

--  This should be fed the *opening* tag.
renderClose :: TagData -> Chunk
renderClose (tag, _) = guard (tag/="#") >> ("</" <> tag <> ">")

--  Characters ok for identifiers.
--  Very permissive for now.
isIdChar :: Char -> Bool
isIdChar c = not (isAscii c) || isDigit c
   || (not (isSpace c) && c `notElem` "<>'\"[]:=/.,")

colonInId :: Parser Char
colonInId = try $ do
   char ':' <* (lookAhead $ satisfy isIdChar)

parseId :: Parser String
parseId = some (satisfy isIdChar <|> colonInId)

parseText :: Parser Chunk
parseText = some (noneOf "<")

--  "Chomment" is a hash-comment (as opposed to an html one).
chommentFilter :: Chunk -> Parser Chunk
chommentFilter s = do
   b <- inChomment <$> getState
   pure $ if b then "" else s

inChomment :: MyState -> Bool
inChomment = any ((=="#") . fst . snd)

inChommentBlock :: MyState -> Bool
inChommentBlock ((True, ("#", _)) : _) = True
inChommentBlock _                      = False

{-# INLINE isBlankLine #-}
isBlankLine :: (Eq s, IsString s) => s -> Bool
isBlankLine = flip elem $ map fromString ["", "\r\n", "\n"]

skipChommentBlock :: Parser Chunk
skipChommentBlock = do
   guard =<< inChommentBlock <$> getState
   void $ some $ anyChar
   pure ""

tabUp :: Int -> Int
tabUp i = (i `div` tabWidth + 1) * tabWidth

indentOf :: Bytes -> Bytes
indentOf l = BLC.takeWhile (`elem` " \t") l

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
--  line under it are then collected, skipping blank lines.
--  Returns split of indented set and rest.
handleIndent :: Bytes -> [Bytes] -> ([Bytes], [Bytes])
handleIndent _  []        = ([], [])
handleIndent l0 ls@(l1:_) = if indentSize l0 >= ind then ([], ls) else loop ls where
   ind = indentSize l1
   proc l = indentOf l0 <> dedent ind l
   loop ls' =  -- ugly?
      case key of
         l:_ | indentSize l >= ind
            -> let (a, b) = loop rest in (bls ++ proc l : a, b)
         _  -> ([], ls')
      where (bls, (key, rest)) = second (splitAt 1) $ span isBlankLine ls'

--  Add to end of line or before final newline sequence.
--  TODO nicer way to do this sort of thing?
addAtEol :: Chunk -> Chunk -> Chunk
addAtEol s = loop where
   loop x | isBlankLine x = s <> x
   loop (c:r)             = c : loop r

--  Find the eol character of Bytes.
findEol :: Bytes -> String
findEol bts = case first (fmap snd . BLC.unsnoc) <$> BLC.unsnoc bts of
      Just (Just '\r', '\n') -> "\r\n"
      Just (_, '\n')         -> "\n"
      _                      -> ""

munchLoop :: [Bytes] -> LineM Chunk
munchLoop []        = pure ""
munchLoop ls@(l0:_) = do
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
               put (lno + length block, outer)
               let eol = if null block then "" else findEol (last block)
               pure $ chunk <> (renderClose =<< bstack) <> eol
            | inChomment stk -> do
               put (lno + length block, outer)
               pure chunk
            | __ -> do
               -- dislike this "imperative" style
               put (lno, inner)
               ichunk <- munchLoop block
               (lno', stk') <- get
               when (not$null stk') $ throwE $ blockError lno' $
                  "block ended with unclosed " <> renderOpen (snd $ head stk')
               put (lno', outer)
               pure $ addAtEol (renderClose =<< bstack) (chunk <> ichunk)
         pure (chunk', postblock)
   mappend chunk' <$> munchLoop cont

parseLines :: [Bytes] -> Either MyError Chunk
parseLines ls =
   case runState (runExceptT (munchLoop ls)) (1, []) of
      (Left err, _)        -> Left err
      (_, (_, (_, tag):_)) ->
         Left $ blockError (length ls) $
            "file ended with unclosed " <> renderOpen tag
      (Right ch, _)        -> Right ch

--  Unlike lines, preserves final newlines.
lines' :: Bytes -> [Bytes]
lines' s = maybe [s] (\n -> let (a, b) = BL.splitAt n s in a : lines' b) $
         (+1) <$> BLC.elemIndex '\n' s

compileHxml :: Bytes -> Either MyError Bytes
compileHxml = fmap BLC.pack . parseLines . lines'

main :: IO ()
main = do
   inp <- BL.hGetContents stdin
   let put h str  = hSetBinaryMode h True >> hPutStr h str
   case compileHxml inp of
      Left err -> put stderr $ "error:\n" <> show err <> "\n"
      Right str -> BL.hPut stdout str

