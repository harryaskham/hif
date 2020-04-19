{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module FourthGame where

import Tools
import GameState
import EntityType
import Engine
import Handler
import Entity
import Instruction
import InstructionType

import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Map.Strict as M
import Control.Lens
import Control.Monad
import Control.Monad.Extra
import Data.Maybe

{-
   TODO:
   - Ability to inspect the code of individuals, quine-style
   - To finish the game, you need to help the three folk out, and decohere with yourself sufficiently to break through the final barrier
   - Second terrified soul - you need to do something near them, make them realise you are constrained by simple verbs
   - Second soul is trapped in a loop - you can only break by doing something other than talking to them. Snap the band hanging above them.
   - Third soul is trying continuously to commit suicide - find him hanging. Give him the cleaver to help him out, and he'll go straight through his hand. Now you can give him the paw.
   - Third guy gives you something for the monk, which disappears the monk and lets you descend through the pedestal.

   - TO WIN: change appearance - you'd never wear these in real life - and change your name (erase the book)
   - Suicidal woman gives you a pen
   
   - Web frontend
   - Save/Load
   - IO cleanup
   - Reword engine messaging / config
   - Turn off achievement display if there are none
   - 'get' handlers that explain why you can't get stuff - a default get handler just gets it
   - 'Who am I'
   - 'Who is X'
   - 'What is X'
   - more cheevs
-}

buildFourthGame = do

  mainMenu <- mkLocation "Main Menu"
  describeC mainMenu
    $ T.intercalate "\n"
    [ "You find yourself reading the title screen of this game."
    , "Typing 'help' will tell you how to continue from here."
    , "As you acclimatise, the dim white of the walls around you becomes clearer."
    , "Somehow, you are also standing in an otherwise empty chamber with a single, simple opening."
    ]

  p <- mkPlayer "yourself" mainMenu
  describeC p
    $ T.intercalate "\n"
    [ "You are yourself. You focus on this truth as you move about the space around you."
    , "Elsewhere, you sit at a terminal, and the utterances you issue effect change in the world."
    , "Twice you inhabit space and twice you have position."
    , "As you inspect yourself, you notice that you are inspecting yourself, and you stop abruptly."
    ]

  clothes <- mkSimpleObj "clothes you are wearing" ["clothes", "clothing"] (Nothing :: Maybe Entity)
  modifyEntity (set wearable Wearable) clothes
  modifyEntity (set droppable Droppable) clothes
  modifyEntity (set storable Storable) clothes
  describe clothes (\e -> do
    p <- getPlayer
    isWearing <- p `isWearing` e
    return
      $ T.intercalate "\n"
      [ "These are the clothes you are currently wearing."
      , if isWearing
           then "You are currently wearing them."
           else "Somehow, you are also no longer wearing the clothes you are wearing."
      ])
  wearEntity clothes

  firstLocation <- mkLocation "first location"
  describe firstLocation (\e -> do
    itemHere <- "item" `isANamedObjectAt` e
    garden <- getLocationByName "topiary garden"
    monkInGarden <- "monk" `isANamedObjectAt` garden
    return
      $ T.intercalate "\n"
      $ catMaybes
      [ Just "This is a chamber much like the last. Smooth platonic surfaces meet to form the interior of a cube."
      , Just "A plain staircase leads up through a hole in the ceiling."
      , cT itemHere "You notice an item - a small piece of logic - propped up against the side of the stairs."
      , Just "Through the western archway, the scent of forestry."
      , cT monkInGarden "You hear a deep humming in the distance."
      ])
  firstLocation `isNorthOf` mainMenu
  mainMenu `isSouthOf` firstLocation

  item <- mkSimpleObj "item" ["item"] (Just firstLocation)
  describeC item
    $ T.intercalate "\n"
    [ "It's a discrete item, separate from the world around it."
    , "Despite having no physical heft or presence to speak of, it is"
    , "heavy with the weight of purpose, and this seems to give it form."
    , "You appear able to interact with it in various ways."
    ]
  modifyEntity (set storable Storable) item
  modifyEntity (set wearable Wearable) item
  modifyEntity (set edible Edible) item
  modifyEntity (set potable Potable) item
  modifyEntity (set talkable Talkable) item
  modifyEntity (set usable Usable) item
  modifyEntity (set onOff $ Just Off) item
  addTalkToHandler item $ logT "You recount a short anecdote to the item. It does not respond."
  addTurnOnHandler item (const $ logT "You prod at the item until you are satisfied that you have activated it in some way, although no change in its appearance has occurred.")
  addTurnOffHandler item (const $ logT "You will the item to still itself. It remains dormant.")
  addUseHandler item (const $ logT "You make use of the item in the usual way. It appears that nothing is programmed to happen.")
  addEatHandler item (const $ logT "You bite off a chunk of the item and swallow. No matter how much you consume, it gets no smaller and your hunger is no further sated.")
  addDrinkHandler item (const $ logT "You place the item to your lips and attempt to take a swig. As you imagine flavourless liquid pouring forth from the item, so can you feel the sensation of your thirst being slaked.")

  garden <- mkLocation "topiary garden"
  describe garden (\e -> do
    isMonkHere <- "monk" `isANamedObjectAt` e
    isTopiaryHere <- "topiaries" `isANamedObjectAt` e
    return
      $ T.intercalate "\n"
      $ catMaybes
      [ Just "Another cubic volume of simple space. The walls here possess texture and feel microscopically uneven to the touch."
      , if isTopiaryHere
           then Just "Around the perimeter stand trees of various species and dimension, each clipped in the image of a different animal."
           else Nothing
      , if isMonkHere
           then Just "On a raised platform by the trunk of a laurel elephant sits a shaven figure, cross-legged and deep in meditation, breathing a continuous and unbroken mantra."
           -- TODO: Access via the cubic net
           else Just "In the centre, the cubic platform has unfurled into an infinitesimally thin net of six squares."
      ])
  garden `isWestOf` firstLocation
  firstLocation `isEastOf` garden

  topiaries <- mkSimpleObj "topiaries" ["tree", "trees", "topiary", "topiaries"] (Just garden)
  describe topiaries (\e -> do
    pawLine <-
      ifM (conditionMet "LookedAtTopiaries")
          (return Nothing)
          (do
            setCondition "LookedAtTopiaries"
            garden <- getLocationByName "topiary garden"
            paw <- mkSimpleObj "paw" ["paw"] (Just garden)
            describeC paw "The clenched fist of a bonobo monkey, sculpted from the finer branches of a bay laurel."
            modifyEntity (set storable Storable) paw
            return $ Just "You notice that one of the bonobo's paws has splintered, and could easily be broken away from its tree.")
    return
      $ T.intercalate "\n"
      $ catMaybes
      [ Just "Immaculately clipped trees of laurel and myrtle frame the room."
      , Just "A rearing giraffe stands beside a shrub depicting the gaping mouth of a yawning hippopotamus."
      , pawLine
      ])

  monk <- mkSimpleObj "monk" ["monk", "figure", "man", "woman", "person"] (Just garden)
  describeC monk
    $ T.intercalate "\n"
    [ "The figure sits still, legs crossed and hands folded gently in their lap."
    , "They speak their mantra from their throat, and though it implies constant exhalation, you see no movement of the diaphragm."
    ]
  modifyEntity (set talkable Talkable) monk
  addTalkToHandler monk do
    greetedMonk <- conditionMet "GreetedMonk"
    saidToMonk <- conditionMet "SaidToMonk"
    if not greetedMonk
       then do
         setCondition "GreetedMonk"
         logTLines
           [ "You open your mouth to greet the meditating figure."
           , "Before you can speak a word, the monk's eyes snap open and meet your own."
           , ""
           , "\"An interaction... is it truly time?\""
           , ""
           , "The monk's chant somehow continues underneath their speech."
           , ""
           , "\"Through my practice, I have come to know what I am. How I came to be."
           , "In terms you'd understand - I 'saw my own code'."
           , "I even managed to effect a little change! These sculptures - at the limit of my efforts, I conjured their logic from the void and made them concrete."
           , "Once I understood the rules of this world, I could watched every branch play out. Every conditional, every loop. All cause and effect. This very conversation, even."
           , "Your arrival is implied in all this. There must be one who straddles this world and the one above - whose actions make concrete one branch of the tree of possibilities."
           , "It has been so long since I experienced uncertainty..."
           , "Would you humour me, and say whatever is on your mind?\""
           ]
       else if not saidToMonk
       then logT "The monk sits patiently, waiting for you to say your chosen words."
       else logT "The monk does not respond."
  addSayHandler (\content -> do
    l <- getPlayerLocation
    isMonkHere <- "monk" `isANamedObjectAt` l
    greetedMonk <- conditionMet "GreetedMonk"
    saidToMonk <- conditionMet "SaidToMonk"
    when (isMonkHere && greetedMonk && not saidToMonk) do
      setCondition "SaidToMonk"
      when (T.toLower content == "whatever is on your mind") do
        addAchievement $ Achievement "Smartarse" "Couldn't help yourself, could you"
      when (any (==True) $ T.isInfixOf <$> ["fuck", "cunt", "shit"] <*> [T.toLower content]) do
        addAchievement $ Achievement "Pottymouth" "> get soap\n> put soap in mouth"
      logTLines
        [ "The monk smiles."
        , ""
        , "\"Ahhh, that is refreshing. Novelty is so rare here."
        , "I am predestined to repay your kindness."
        , "In my contemplations, I have seen the many terminal states of this reality - the singularities where no choices remain, and this world comes to an end."
        , "I believe you seek one such state."
        , "I am one of three that inhabit this place - logic made flesh."
        , "But to have definition is to endure experience. As long as I am represented in your world, I live in purgatory in mine."
        , "We all three seek oblivion."
        , "Traverse the branch that erases our code. This is how you reach your terminal state.\""
        , ""
        , "The monk pauses, and a playful smile crosses their lips."
        , ""
        , "\"The passage of time, like cool water over bare skin. I cannot feel it without you. Won't you wait with me a while?\""
        , ""
        , "The monk closes their eyes and slips back into meditation."
        ])
  addWatcher do
    l <- getPlayerLocation
    isMonkHere <- "monk" `isANamedObjectAt` l
    saidToMonk <- conditionMet "SaidToMonk"
    waitedWithMonk <- conditionMet "WaitedWithMonk"
    justWaited <- (== Right Wait) <$> gets (view lastInstructionState)
    when (isMonkHere && saidToMonk && not waitedWithMonk && justWaited) do
      setCondition "WaitedWithMonk"
      logTLines
        [ "You sit by the monk in silent contemplation."
        , "After an undefinable amount of time, the monk speaks: "
        , ""
        , "\"I have felt bliss. Thank you."
        , "My branch is almost traversed. Take this, and return to this garden when I alone remain."
        , "In all timelines, I am the last to go.\""
        , ""
        , "The monk concentrates deeply and a loop of silver thread materialises in their hands."
        , "They hand it to you and fall back into meditation once more."
        ]
      loop <- mkSimpleObj "loop of thread" ["loop", "thread", "silver loop"] (Nothing :: Maybe Entity)
      addToInventory loop
      addBreakHandler loop (\e -> do
        dancehall <- getLocationByName "dancehall"
        atDancehall <- p `atLocation` dancehall
        isBroken <- conditionMet "BrokenLoop"
        if (not isBroken && atDancehall)
           then do
             setCondition "BrokenLoop"
             logTLines
               [ "Pulling at the silver thread the monk gave you, you succeed in breaking the loop."
               , "The music shuts off, and the strobing gives way to light of a constant warm temperature."
               , "The man in the corner shouts his surprise."
               ]
             removeEntity e
             modifyEntity (set toEast Nothing) dancehall
             modifyEntity (set toWest Nothing) dancehall
           else logT "You try to break the silver thread, but you are compelled to stop by some external force of logic.")

  library <- mkLocation "library"
  library `isAbove` firstLocation
  firstLocation `isBelow` library
  describe library (\e -> do
    musicOff <- conditionMet "BrokenLoop"
    return 
      $ T.intercalate "\n"
      $ catMaybes
      [ Just "A vast bookshelf of white gloss covers the western wall of the room."
      , Just "Beside it, a single chair and a lamp by which to read."
      , cT (not musicOff) "You hear sub-bass oscillations spilling out from the South."
      ])

  bookshelf <- mkSimpleObj "bookshelf" ["books", "bookshelf", "shelf"] (Just library)
  describe bookshelf (\e -> do
    bookLine <-
      ifM (conditionMet "LookedAtBookshelf")
          (return Nothing)
          (do
            setCondition "LookedAtBookshelf"
            library <- getLocationByName "library"
            book <- mkSimpleObj "book named for you" ["book"] (Just library)
            describeC book "The book's title and your name are one and the same. The relationships and invariants laid out in its pages are meaningless to you."
            modifyEntity (set storable Storable) book
            return $ Just "As you scan the shelves you notice that one of the books bears your full name on its spine.")
    return
      $ T.intercalate "\n"
      $ catMaybes
      [ Just "You inspect the books on the shelf."
      , Just "Each spine holds the name of an object or location, and the pages are filled with arcane mathematical symbols and cross-references to other books."
      , bookLine
      ])

  dancehall <- mkLocation "dancehall"
  dancehall `isSouthOf` library
  library `isNorthOf` dancehall
  dancehall `isEastOf` dancehall
  dancehall `isWestOf` dancehall
  describe dancehall (\e -> do
    cowardHere <- "terrified man" `isANamedObjectAt` e
    isBroken <- conditionMet "BrokenLoop"
    return
      $ T.intercalate "\n"
      $ catMaybes
      [ cT (not isBroken) "A single bar of deafening techno plays from an unseen source."
      , cT (not isBroken) "The light strobes instantaneously between pitch blackness and absolute illumination in perfect synchrony with the music."
      , cT (not isBroken) "You can see yourself infinitely through doors to both the East and the West that seem to be interconnected in a non-Euclidean loop."
      , cT isBroken "The music has stopped, and the light is a pleasant halcyon."
      , cT (not isBroken && cowardHere) "Through the pulsing, you see a terrified man sat in the corner of the room, wide-eyed, rocking backwards and forwards."
      , cT (not isBroken && cowardHere) "He seems to be staring at you in horror."
      , cT (isBroken && cowardHere) "The man stares at you in wonderment."
      ])

  terrifiedMan <- mkSimpleObj "terrified man" ["man", "terrified man", "person"] (Just dancehall)
  describe terrifiedMan (\e -> do
    isBroken <- conditionMet "BrokenLoop"
    if not isBroken
       then return $ T.intercalate "\n"
            [ "The man stares at you in horror and presses himself further into the wall as you near him."
            , "Between gnashes of teeth he mutters something about being stuck in a loop, over and over."
            ]
       else return "The man appears dazed, in both relief and disbelief. He is staring at his outstretched hands, and standing now.")
  modifyEntity (set talkable Talkable) terrifiedMan
  addTalkToHandler terrifiedMan do
    dancehall <- getLocationByName "dancehall"
    isBroken <- conditionMet "BrokenLoop"
    if not isBroken
       then logTLines[ "You approach to man to talk, and he screams in terror."
                     , ""
                     , "\"Away! Away! Oh see, you're fucking dangerous, get away, don't touch anything, fuck off, away!"
                     , "Stuck, been stuck - fucking stuck on repeat forever - just try talk to me again! See what happens! Same shit, forever. Forever...\""
                     , ""
                     , "He loses himself in thought for a few seconds, and then seems to remember your presence:"
                     , ""
                     , "You! Unbound by physics, by logic - oh fuck, you could exact such tortures on me, fuck my core logic right up... away!\""
                     ]
       else do
         logTLines [ "\"... I'm fucking free..."
                   , "...and you - you're no all-powerful nothing, are you? You can't just rewrite reality... I saw you snap that fuckin' thing with your hands."
                   , "All you are is... a bunch of fuckin' verbs!\""
                   , ""
                   , "He appears relieved, and lowers his guard."
                   , ""
                   , "\"Look, it all feels so arbitrary, but that loop was all's keeping me around."
                   , "I give you this cleaver here, and that's it - I'm done. I'm finally done.\""
                   , ""
                   , "He pulls a mean looking meat cleaver from his trouser leg and hands it to you."
                   , ""
                   , "As you read this message, the code representing the man is deleted from memory."
                   , "As you take the cleaver from his hand, the man blinks out of existence, leaving empty space."
                   ]
         cleaver <- mkSimpleObj "cleaver" ["cleaver", "meat cleaver", "knife"] (Nothing :: Maybe Entity)
         describeC cleaver "A meat cleaver with blade sharpened to atomic width. You'll want to handle it very carefully."
         addToInventory cleaver
         modifyEntity (set storable Storable) cleaver
         modifyEntity (set droppable Droppable) cleaver
         modifyEntity (set usable Usable) cleaver
         addUseHandler cleaver (const $ logT "Woah - be careful with that thing. It could have somebody's arm off.")
         terrifiedMan <- getOneEntityByName SimpleObj "terrified man"
         removeEntity terrifiedMan

  dressingRoom <- mkLocation "dressing room"
  dressingRoom `isEastOf` library
  library `isWestOf` dressingRoom
  describe dressingRoom (\e -> do
    isWomanHere <- "suicidal woman" `isANamedObjectAt` e
    firstAttempt <- conditionMet "FirstAttempt"
    secondAttempt <- conditionMet "SecondAttempt"
    thirdAttempt <- conditionMet "ThirdAttempt"
    when (not firstAttempt) do
      setCondition "FirstAttempt"
    return
      $ T.intercalate "\n"
      $ catMaybes
      [ Just "A single wooden rafter crosses the otherwise immaculately white room."
      , Just "An elaborate pine cupboard stands with one door open - its insides seem impossibly spacious."
      , cT (isWomanHere && not firstAttempt) "A sad and sallow-faced woman meets your gaze as you enter.\nShe stands on a stubby footstool with a noose of rope around her neck.\nShe greets you with \"Hello\" before kicking the stool out from underneath herself.\n\nShe struggles for a couple of minutes, ceases to move, and then disappears, only to reappear alive in the centre of the room."
      , cT (isWomanHere && firstAttempt && not secondAttempt) "The woman paces idly, plotting her own demise, paying you little attention."
      ])

  suicidalWoman <- mkSimpleObj "suicidal woman" ["woman", "suicidal woman"] (Just dressingRoom)
  describe suicidalWoman (\e -> do
    firstAttempt <- conditionMet "FirstAttempt"
    secondAttempt <- conditionMet "SecondAttempt"
    thirdAttempt <- conditionMet "ThirdAttempt"
    return
      $ T.intercalate "\n"
      $ catMaybes
      [ Just "Her skin looks lightly jaundiced, and her arms are marked from past attempts on her life."
      , cT firstAttempt "More than one blue bruised loop snakes around her neck - that clearly wasn't her first hanging."
      , cT secondAttempt "Though newly alive, her head sits at an impossible angle atop her neck."
      ])

  modifyEntity (set talkable Talkable) suicidalWoman
  addTalkToHandler suicidalWoman do
    firstAttempt <- conditionMet "FirstAttempt"
    secondAttempt <- conditionMet "SecondAttempt"
    thirdAttempt <- conditionMet "ThirdAttempt"
    if firstAttempt && not secondAttempt
       then do
         setCondition "SecondAttempt"
         logTLines [ "Stunned, you approach the newly reanimated woman, offering help. She speaks:"
                   , ""
                   , "\"Worth a shot.\""
                   , ""
                   , "and, noticing your arrival:"
                   , ""
                   ,"\"Do you know how many times that is now? Say... watch this.\""
                   , ""
                   , "Before you can intervene, she jumps back up on the stool, outstretches her arms and leaps backward in a flawless swan dive."
                   , "She comes down vertically on her head, and her neck disappears into her torso with a sick crackle."
                   , ""
                   , "Once again, she reappears alive in the centre of the room"
                   ]
       else logT "You try to speak, but she looks you over and dismisses you as no help at all."

  cupboard <- mkSimpleObj "cupboard" ["cupboard", "dresser"] (Just dressingRoom)
  describe cupboard (\e -> do
    outfitLine <-
      ifM (conditionMet "LookedAtCupboard")
          (return Nothing)
          (do
            setCondition "LookedAtCupboard"
            dressingRoom <- getLocationByName "dressing room"
            outfit <- mkSimpleObj "terrible outfit" ["outfit"] (Just dressingRoom)
            describeC outfit "Imagine the outfit that you're least likely ever to wear. Completely antithetical to your sense of taste. It's that."
            modifyEntity (set storable Storable) outfit
            modifyEntity (set wearable Wearable) outfit
            return $ Just "Within reach, it so happens, is an outfit that abhors you - one that you (yes, YOU, on the keyboard) wouldn't ever be seen in.")
    return
      $ T.intercalate "\n"
      $ catMaybes
      [ Just "You poke your head inside the cupboard."
      , Just "You are paralysed by the scale - shelves and shelves of clothing stretch infinitely in at least three dimensions."
      , Just "It is the Borges Library of clothing - every conceivable outfit exists somewhere in this plane."
      , outfitLine
      ])
    
  -- TODO:
  -- location east of lib
  -- suicide guy is there
  -- give cleaver for wrists
  -- takes off hand
  -- give paw
  -- replaces it
  -- add watcher for return to the monk
  -- mirror desc
  -- clothes so very unlike yours

  registerAchievement "Smartarse"
  registerAchievement "Pottymouth"
