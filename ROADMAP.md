# cofree-bot project goals and roadmap

## Protocol Support:
  - [x] Matrix
  - [x] CLI
  - [ ] Zulip
  - [ ] Slack
  - [ ] IRC
  - [ ] Twitter
  - [ ] Signal
  - [ ] Telegram
  - [ ] Reddit
  - [ ] Github
  - [ ] Gitlab
  - [ ] Gitea

## Short Term Bot Features
- [ ] A combine bots combinator to reduce the verbosity of expressions like this:
  ```haskell
  rmap (\(x :& y :& z) -> x <> y <> z)
    $  calcBot
    /\ helloBot
    /\ coinFlipBot'
  ```
- [ ] Optics equipment to modify bot inputs and outputs indepentently
- [ ] Printer/Parser classes to standardize and simplify I/O
      consumption/production and solve the partial parsing problem.
- [ ] Use `HKD` for Bot I/O.
- [ ] Fixed Point of `Bot`. This would abstract the bot's state
      parameter away which would make it easier to compose bots.
- [ ] Add option to read Auth Token from disk rather then an env var.
- [ ] Bots should accept matrix private message invitations.
- [ ] Introspect on enables behaviors of a bot. eg., ask the bot what
      behaviors are enabled in a context.
- [ ] Behaviors should be toggleable per room/context.
- [ ] Testing Harness. We should have a "protocol" for local unit
      testing of a bot behavior.

## Long Term Bot Features
### Wire API for bots
We want to use some wire protocol for bot I/O. This would uses to
build bot behaviors in any language.

### Bots as interacting agents
We want bots to be able to speak to each other. Protocols
implementions would then become specialized bots that other bots pass
messages into. This would allow interesting compound behaviors such as
messaging a Matrix Bot who would then message Github bot which would
perform an action on Github.

## CI/Deployment
### Short Term
- [ ] Settle on REPL deployment process
- [ ] Some sort of ability to introspect on the deployed bot's status

### Long Term
#### Bot Zoo
Some sort of website where users can upload bot behaviors. We could
also provide bots as a service where the user picks a bunch of
behaviors and how they want to compose them then we deploy them in our
cloud environment.
