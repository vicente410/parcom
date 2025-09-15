import Data.Char

newtype Parser a =
  Parser { runParser :: String -> Maybe (String, a) }

second :: (b -> c) -> (a, b) -> (a, c)
second f (a, b) = (a, f b)

instance Functor Parser where
  fmap f (Parser p) = Parser (fmap (second f) . p )

instance Applicative Parser where
  pure x = Parser (\input -> Just (input, x))
  (Parser p1) <*> (Parser p2) = Parser
    (\input -> (p1 input >>= (\(rest, f) -> second f <$> p2 rest)))

charP :: Char -> Parser Char
charP c = Parser f
  where f (x:xs) | c == x = Just (xs, x)
        f _ = Nothing

stringP :: String -> Parser String
stringP s = sequenceA $ map charP s

main :: IO ()
main = putStrLn $ show $ runParser (stringP "Hello") "Hello World"
