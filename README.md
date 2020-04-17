# hif

## TODO
- more forgiving verb/noun combos
- whether on/off in inventory display
- break things - with a broken status
- synechdoche stuff - look under lid, for example
- repeat the mantra on the radio for special ending
- queries - what is X
- more verbs, even if non functional


CLEANUP
=======
- Remove specificity of things
- All messaging goes in a config, can be overridden by games
- Combinations are registered
- In fact, all action potentials are registered and stored
- We should have IDs for items - things more like simpleObj than anything else
- inventory management as helper func
- TurnOn/Off etc handlers attached to the things themselves
- Split engine and game into separate files
- Lots of duplication in enactInstruction - reduce this down
- Figure out MonadState again
- Move some functionality outside of App
