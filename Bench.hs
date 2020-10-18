import Control.Applicative (Alternative (many, some, (<|>)))
import Criterion.Main
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Lexy

data Token = Key Text | Name Text | Whitespace Text | Comment Text

token :: Lexer Token
token = keywords <|> name <|> comment <|> whitespace
  where
    keywords =
      (Key "k1" <$ string "k1")
        <|> (Key "k2" <$ string "k2")
        <|> (Key "k3" <$ string "k3")
        <|> (Key "k4" <$ string "k4")
        <|> (Key "k5" <$ string "k5")
        <|> (Key "k6" <$ string "k6")
        <|> (Key "k7" <$ string "k7")
        <|> (Key "k8" <$ string "k8")
        <|> (Key "k9" <$ string "k9")
    name = fmap (Name . pack) ((:) <$> satisfies isAlpha <*> many (satisfies isAlphaNum))
    comment = fmap (Comment . pack) (string "--" *> many (satisfies (\c -> isSpace c && c /= '\n')))
    whitespace = fmap (Whitespace . pack) (some (satisfies isSpace))

main :: IO ()
main =
  let prog1 = "k1 k2 k3 k4 fooooooooo k1 k2 k3 k4 fooooooooo k1 k2 k3 k4 fooooooo  -- foo bar baz\n k1 k2 k3 k4 --\n--\n--\n"
      prog2 = Text.concat (take 100 (cycle [prog1]))
      prog3 = Text.concat (take 10000 (cycle [prog2]))
   in defaultMain
        [ bgroup
            "runLexerNaive"
            [ bench "small" $ whnf (\t -> runLexer token t) prog1,
              bench "mid" $ whnf (\t -> runLexer token t) prog2,
              bench "large" $ whnf (\t -> runLexer token t) prog3
            ]
        ]
