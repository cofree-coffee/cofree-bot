cofree-bot
==========

[![cofree-bot::CI](https://github.com/cofree-coffee/cofree-bot/actions/workflows/nix.yml/badge.svg)](https://github.com/cofree-coffee/cofree-bot/actions/workflows/nix.yml)

`cofree-bot` is a toolkit for building chat bots compositionally. Bots
can be defined over arbitrary inputs or outputs, combined together and
then "lifted" to operate in any context.

Supported Chat Protocols:
-------------------------
- [x] Matrix
- [ ] Discord
- [ ] IRC
- [ ] Slack
- [ ] Zulip
- [x] CLI REPL

The Bot type
------------
```Haskell
newtype Bot m s i o = Bot {runBot :: s -> i -> ListT m (o, s)}
```

Building bot behaviors
----------------------
```Haskell
helloBot :: Monad m => Bot m s Text Text
helloBot = Bot $ \s msg ->
  let name = "cofree-bot"
   in if name `T.isInfixOf` msg
        then pure ("Are you talking to me, punk?", s)
        else emptyListT
```

Lifting Bots Over more complex inputs and outputs
-------------------------------------------------
```Haskell
liftSimpleBot :: Functor m => Bot m s Text Text -> Bot m s (RoomID, Event) (RoomID, Event)
liftSimpleBot (Bot bot) = Bot $ \s (rid, i) ->
  fmap (\(i', s') -> ((rid, mkMsg i'), s')) $ bot s (viewBody i)

viewBody :: Event -> T.Text
viewBody = view (_EventRoomMessage . _RoomMessageText . _mtBody)

mkMsg :: T.Text -> Event
mkMsg msg =
  EventRoomMessage $ RoomMessageText $ MessageText msg TextType Nothing Nothing
```

Combining Bot Behaviors
---------------------
```Haskell
type (/\) = (,)
type (\/) = Either
type a /+\ b = These a b
type a \*/ b = Maybe (Either a b)

(/\) :: Monad m => Bot m s i o -> Bot m s' i o' -> Bot m (s /\ s') i (o /\ o')
(/+\) :: Monad m => Bot m s i o -> Bot m s' i o' -> Bot m (s /\ s') i (o /+\ o')
(/.\) :: Monad m => Bot m s i o -> Bot m s' i o -> Bot m (s /\ s') i o
(\/) :: Monad m => Bot m s i o -> Bot m s i' o' -> Bot m s (i \/ i') (o \/ o')
nudge :: Monad m => Bot m s i o \/ Bot m s i' o' -> Bot m s (i \/ i') (o \*/ o')
nudgeLeft :: Monad m => Bot m s i o -> Bot m s (i \/ i') (o \*/ o')
nudgeRight :: Monad m => Bot m s i' o' -> Bot m s (i \/ i') (o \*/ o')
```
