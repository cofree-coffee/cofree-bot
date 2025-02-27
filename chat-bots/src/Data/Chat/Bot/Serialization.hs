{-# LANGUAGE CPP #-}

-- | Bidirectional parsing to map 'Bot' I/O to 'Server' I/O.
--
-- TODO: Make monoidal-functor instances
module Data.Chat.Bot.Serialization where

--------------------------------------------------------------------------------

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
import Control.Monad ((>=>))
import Control.Monad.ListT (emptyListT)
import Data.Attoparsec.Text qualified as P
import Data.Bifunctor (first)
import Data.Chat.Bot (Bot (..))
import Data.Chat.Utils (can, type (/+\))
import Data.Text (Text)
import Data.These (These (..), these)

--------------------------------------------------------------------------------

applySerializer ::
  (Applicative m) =>
  Bot m s bi bo ->
  Serializer so si bo bi ->
  Bot m s so si
applySerializer (Bot bot) (Serializer parser printer) = Bot $ \s i ->
  case parser i of
    Nothing -> emptyListT
    Just i' -> first printer <$> bot s i'

--------------------------------------------------------------------------------

-- | Bidirectional serializer from 'Server' I/O to 'Bot' I/O.
data Serializer so si bo bi = Serializer
  {parser :: so -> Maybe bi, printer :: bo -> si}

-- | A 'Serializer' whose 'Server' I/O has been specialized to 'Text'.
type TextSerializer = Serializer Text Text

-- | Extend the parser portion of a 'TextSerializer' to consume a
-- prefix string.
prefix :: Text -> TextSerializer x y -> TextSerializer x y
prefix prefix' Serializer {..} =
  let prefixParser = P.string prefix' *> ":" *> P.skipSpace *> P.takeText
      parsePrefix = either (const Nothing) Just . P.parseOnly prefixParser
   in Serializer
        { parser = parsePrefix >=> parser,
          printer = printer
        }

infixr 6 /+\\

(/+\) :: TextSerializer o i -> TextSerializer o' i' -> TextSerializer (o /+\ o') (i /+\ i')
(/+\) (Serializer par1 pri1) (Serializer par2 pri2) = Serializer (par1 *|* par2) (pri1 +|+ pri2)

-- | Parse and tensor the components @a@ and @b@ of a 'These'.
infixr 6 *|*

(*|*) :: (Text -> Maybe a) -> (Text -> Maybe b) -> Text -> Maybe (a /+\ b)
(*|*) p1 p2 = these (fmap This) (fmap That) can . liftA2 These p1 p2

-- | Print and combine the components @a@ and @b@ of a 'These'.
infixr 6 +|+

(+|+) :: (a -> Text) -> (b -> Text) -> a /+\ b -> Text
(+|+) p1 p2 = these p1 p2 $ \a b -> p1 a <> "\n" <> p2 b
