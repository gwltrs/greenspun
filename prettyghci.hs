module PrettyGHCI (prettifyGHCI) where

------------------------------------------------------------
-- Parsing

data Node = Atom String | Comma | Group Char [Node]

parseAll :: String -> [Node]
parseAll s = case parseNodes s of
  (ns, [])    -> ns
  (ns, x:r)   -> ns ++ Atom [x] : parseAll r   -- stray closer

parseNodes :: String -> ([Node], String)
parseNodes [] = ([], [])
parseNodes s@(c:cs)
  | c `elem` ")]" = ([], s)
  | isSpace c     = parseNodes cs
  | c == ','      = cons Comma cs
  | c `elem` "([" =
      let (kids, rest) = parseNodes cs
          rest'        = drop 1 rest              -- consume the closer
          (ns, rest'') = parseNodes rest'
      in (Group c kids : ns, rest'')
  | c == '"'      = let (str, rest) = lexStr cs in cons (Atom ('"' : str)) rest
  | otherwise     = let (a, rest) = break stop s in cons (Atom a) rest
  where
    cons n r = let (ns, r') = parseNodes r in (n : ns, r')
    stop x   = isSpace x || x `elem` "()[],\""

-- rest of a string literal, including the closing quote
lexStr :: String -> (String, String)
lexStr ('\\':x:r) = let (a, b) = lexStr r in ('\\' : x : a, b)
lexStr ('"':r)    = ("\"", r)
lexStr (x:r)      = let (a, b) = lexStr r in (x : a, b)
lexStr []         = ("", "")

------------------------------------------------------------
-- Layout

width :: Int
width = 80

closeOf :: Char -> Char
closeOf '(' = ')'
closeOf _   = ']'

isAtom, isComma :: Node -> Bool
isAtom (Atom _) = True
isAtom _        = False
isComma Comma   = True
isComma _       = False

isSpace :: Char -> Bool
isSpace c = c `elem` " \t\n\r\f\v"

splitCommas :: [Node] -> [[Node]]
splitCommas ns = case break isComma ns of
  (a, [])  -> [a]
  (a, _:r) -> a : splitCommas r

nl :: Int -> String
nl k = '\n' : replicate k ' '

flat :: Node -> String
flat (Atom a)    = a
flat Comma       = ","
flat (Group o ns) = o : flatSeq ns ++ [closeOf o]

flatSeq :: [Node] -> String
flatSeq = intercalate ", " . map (unwords . map flat) . splitCommas

-- render a node whose first character sits at column i
node :: Int -> Node -> String
node i n
  | i + length f <= width = f
  where f = flat n
node i (Group o ns)
  | length chunks > 1 =
      o : nl (i + 2)
        ++ intercalate ("," ++ nl (i + 2)) (map (chunk (i + 2)) chunks)
        ++ nl i ++ [c]
  | otherwise = o : broken (i + 1) (i + 2) ns ++ [c]
  where chunks = splitCommas ns
        c      = closeOf o
node _ n = flat n

-- one comma-separated item, starting at the beginning of a line
chunk :: Int -> [Node] -> String
chunk i ns
  | i + length f + 1 <= width = f
  | otherwise                 = broken i (i + 2) ns
  where f = unwords (map flat ns)

prependToAll            :: a -> [a] -> [a]
prependToAll _   []     = []
prependToAll sep (x:xs) = sep : x : prependToAll sep xs

intersperse             :: a -> [a] -> [a]
intersperse _   []      = []
intersperse sep (x:xs)  = x : prependToAll sep xs

intercalate :: [a] -> [[a]] -> [a]
intercalate xs xss = concat (intersperse xs xss)

-- leading atoms stay on the first line, everything after goes on its own line
broken :: Int -> Int -> [Node] -> String
broken col ind ns = case span isAtom ns of
  (hs@(_:_), rest) -> unwords (map flat hs) ++ concatMap line rest
  _ -> case ns of
         []     -> ""
         (x:xs) -> node col x ++ concatMap line xs
  where line x = nl ind ++ node ind x

prettify :: String -> String
prettify = unlines . map (node 0) . parseAll

------------------------------------------------------------

prettifyGHCI :: Show a => a -> IO ()
prettifyGHCI x = do
  putStrLn ""
  putStrLn $ prettify $ show x
  putStrLn ""