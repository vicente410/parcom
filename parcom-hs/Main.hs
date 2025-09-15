import Data.Char

newtype Parser a =
  Parser { runParser :: String -> Maybe (String, a) }

second :: (b -> c) -> (a, b) -> (a, c)
second f (a, b) = (a, f b)

instance Functor Parser where
  fmap f (Parser p) = Parser (fmap (second f) . p )

charP :: Char -> Parser Char
charP c = Parser f
  where f (x:xs) | c == x = Just (xs, x)
        f _ = Nothing

main :: IO ()
main = putStrLn $ show $ runParser (ord <$> charP 'H') "Hello World"
