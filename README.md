cofree-bot
==========

[![cofree-bot::CI](https://github.com/cofree-coffee/cofree-bot/actions/workflows/nix.yml/badge.svg)](https://github.com/cofree-coffee/cofree-bot/actions/workflows/nix.yml)

`cofree-bot` is a toolkit for building chat bots compositionally. Bots
can be defined over arbitrary inputs or outputs, combined together and
then "lifted" to operate in any context.

Supported Chat Protocols:
-------------------------
- [x] Matrix
- [ ] Zulip (in progress)
- [x] CLI REPL

The Bot type
------------
```Haskell
data BotAction s o = BotAction { responses :: o, nextState :: s }
newtype Bot m s i o = Bot { runBot :: i -> s -> m (BotAction s o) }
```

Building bot behaviors
----------------------
```Haskell
helloWorldBot :: Applicative m => Bot m () T.Text T.Text
helloWorldBot = Bot $ \input state ->
  let output = "Hello " <> input
  in pure $ BotAction output state
```

Lifting Bots Over more complex inputs and outputs
-------------------------------------------------
```Haskell
matrixHelloBot :: Bot IO () (RoomID, Event) (RoomID, Event)
matrixHelloBot = second' $ dimap viewBody mkMsg helloWorldBot
  where
    viewBody :: Event -> T.Text
    viewBody = (view (_EventRoomMessage . _RoomMessageText . _mtBody))

    mkMsg :: T.Text -> Event
    mkMsg msg = EventRoomMessage $ RoomMessageText $ MessageText msg TextType Nothing Nothing
```

Combing Bot Behaviors
---------------------
```Haskell
combinedBot :: Applicative m => Bot m () (Either T.Text Int) (Either T.Text Int)
combinedBot = helloWorldBot \/ calculatorBot
```
