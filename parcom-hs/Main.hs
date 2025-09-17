import Control.Applicative
import Control.Monad

newtype Parser a =
  Parser { runParser :: String -> Maybe (String, a) }

second :: (b -> c) -> (a, b) -> (a, c)
second f (a, b) = (a, f b)

instance Functor Parser where
  fmap = (<*>) . pure

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  p1 <*> p2 = p1 >>= (\f -> p2 >>= (\x -> pure (f x)))

instance Monad Parser where
  (Parser p) >>= f = Parser $
    \input -> p input >>= (\(rest, x) -> runParser (f x) rest)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

charP :: Char -> Parser Char
charP c = Parser f
  where f (x:xs) | c == x = Just (xs, x)
        f _ = Nothing

stringP :: String -> Parser String
stringP s = sequence $ map charP s

main :: IO ()
main = putStrLn $ show $ runParser (fmap length $ many $ stringP " ") "            Goodbye"
