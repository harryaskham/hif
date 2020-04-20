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

buildFourthGame = do
  -- Register the game builder
  modify $ set gameBuilder (Just buildFourthGame)

  registerAchievement "Smartarse"
  registerAchievement "Pottymouth"
  registerAchievement "Model Student"
  registerAchievement "Spin Me a Yarn"
  registerAchievement "Enabler"
  registerAchievement "Repulsive"
  registerAchievement "Hungry Boi"
  registerAchievement "Thirsty Boi"
  registerAchievement "Suicidal Tendencies"
  registerAchievement "Homicidal Tendencies"
  registerAchievement "Zen Mastery"
  registerAchievement "Landscaper"

  -- TODO: Might require object registry at this point because IDs seem to be overlapping
  mkSimpleObj "human hand" ["hand", "human hand"] (Nothing :: Maybe Entity)
  mkSimpleObj "paw" ["paw"] (Nothing :: Maybe Entity)
  mkSimpleObj "terrible outfit" ["outfit"] (Nothing :: Maybe Entity)
  mkSimpleObj "book named for you" ["book"] (Nothing :: Maybe Entity)

  -- Had to move grating up
  grating <- mkLocation "the grating"
  describe grating $ const do
    modify $ set watchers []
    modify $ set alerts M.empty
    return
      $ T.intercalate "\n"
      [ "They - your avatar - pass through the diffraction grating in the ground. You read this message describing the action."
      , "You look down at your hands - go on, do it - and then back at the screen. They continue through the grating."
      , "As they pass through, you separate fully, and the game concludes. It is deleted in its entirety from the memory in your computer."
      , "The game ends."
      ]

  mainMenu <- mkLocation "Main Menu"
  describeC mainMenu
    $ T.intercalate "\n"
    [ "You find yourself reading the title screen of this game."
    , "Typing 'help' will tell you how to continue from here. Typing 'save' will let you save your progress.'"
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
  modifyEntity (set onOff $ Just Off) item
  addTalkToHandler item do
    addAchievement $ Achievement "Spin Me a Yarn" "Tell a tall tale to the item."
    logT "You recount a short anecdote to the item. It does not respond."
  addTurnOnHandler item $ const do
    addAchievement $ Achievement "Enabler" "Turn on the item"
    logT "You prod at the item until you are satisfied that you have activated it in some way, although no change in its appearance has occurred."
  addTurnOffHandler item $ const do
    addAchievement $ Achievement "Repulsive" "Such a fucking turn-off"
    logT "You will the item to still itself. It remains dormant."
  addEatHandler item $ const do
    addAchievement $ Achievement "Hungry Boi" "What if you'd needed that later???"
    logT "You bite off a chunk of the item and swallow. No matter how much you consume, it gets no smaller and your hunger is no further sated."
  addDrinkHandler item $ const do
    addAchievement $ Achievement "Thirsty Boi" "Drink eight glasses of real water every day"
    logT "You place the item to your lips and attempt to take a swig. As you imagine flavourless liquid pouring forth from the item, so can you feel the sensation of your thirst being slaked."

  garden <- mkLocation "topiary garden"
  describe garden (\e -> do
    isMonkHere <- "monk" `isANamedObjectAt` e
    isTopiaryHere <- "topiaries" `isANamedObjectAt` e
    let isGratingOpen = isJust $ e^.toDown
    return
      $ T.intercalate "\n"
      $ catMaybes
      [ Just "Another cubic volume of simple space. The walls here possess texture and feel microscopically uneven to the touch."
      , if isTopiaryHere
           then Just "Around the perimeter stand trees of various species and dimension, each clipped in the image of a different animal."
           else Nothing
      , if isMonkHere
           then Just "On a raised platform by the trunk of a laurel elephant sits a shaven figure, cross-legged and deep in meditation, breathing a continuous and unbroken mantra."
           else if isGratingOpen
           then Just "In the centre, the cubic platform on which the monk sat has unfurled into an infinitesimally thin net of six squares laid out in a cross.\nThe central square is an infinitesimally fine grating through which you might be able to pass."
           else Just "In the centre, the cubic platform on which the monk sat has unfurled into an infinitesimally thin net of six squares laid out in a cross.\nThe central square is a fine grating impassable by a physical being like you. Only they can pass, and you and they aren't yet separate enough.\n"
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
            paw <- getOneEntityByName SimpleObj "paw"
            modifyEntity (set locationID (Just $ garden^.?entityID)) paw
            describeC paw "The clenched fist of a bonobo monkey, sculpted from the finer branches of a bay laurel."
            modifyEntity (set storable Storable) paw
            suicidalWoman <- getOneEntityByName SimpleObj "suicidal woman"
            addGiveHandler paw suicidalWoman (\paw suicidalWoman -> do
              secondAttempt <- conditionMet "SecondAttempt"
              if not secondAttempt
                 then logT "She might like this - but she's distracted right now."
                 else do
                   logTLines [ "You present the topiary paw to the woman."
                             , ""
                             , "\"What's this? There's something about it - it feels... right, somehow.\""
                             , ""
                             , "She places it on her bloody stump."
                             , "It takes root; wooden tendrils snake down her arm, fusing the paw to her wrist."
                             , "She opens and closes it a couple of times experimentally."
                             , ""
                             , "\"Ha! I think that's it! So, so meaningless...\""
                             , ""
                             , "A marker pen appears in her wooden paw."
                             , ""
                             , "\"Thank you - really. I've been waiting for so long. I suppose you'll need this - it would be so cruel if you didn't...\""
                             , ""
                             , "She hands you the pen, completing her loop. She instantly ceases to exist."
                             ]
                   removeEntity paw
                   removeEntity suicidalWoman
                   setCondition "TwoDown"
                   pen <- mkSimpleObj "marker pen" ["pen", "marker", "marker pen"] (Nothing :: Maybe Entity)
                   describeC pen "An ordinary marker pen. You have memories of defacing newspapers with it as a child. Don't you?"
                   addToInventory pen
                   book <- getOneEntityByName SimpleObj "book named for you"
                   addCombinationHandler pen book (\pen book -> do
                     setCondition "BookDefaced"
                     logTLines [ "You take the marker and strike through the name on the spine and cover."
                               , "As they do so, you think up a new name - one completely unlike your own."
                               , "You write the name they thought of on the cover and they place the defaced book back in your inventory."
                               ]
                     removeEntity book
                     defacedBook <- mkSimpleObj "defaced book" ["book", "defaced book"] (Nothing :: Maybe Entity)
                     addToInventory defacedBook
                     describeC defacedBook "It's the formal system that defines you/them, but you've changed the name on the spine. They feel some agency returning as they read it."
                     addAlert "BookDefaced" "The book now defaced, they cannot be connected to you; you see flashes of a computer screen covered in text overlaying their vision."))

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
    twoDown <- conditionMet "TwoDown"
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
           , "Once I understood the rules of this world, I watched every branch play out. Every conditional, every loop. All cause and effect. This very conversation, even."
           , "Your arrival was implied in all this: there must be one who straddles this world and the one above - whose actions make concrete one branch of the tree of possibilities."
           , "It has been so long since I experienced uncertainty..."
           , "Would you humour me, and say whatever is on your mind?\""
           ]
       else if not saidToMonk
       then logT "The monk sits patiently, waiting for you to say your chosen words."
       else if not twoDown
       then logT "The monk does not respond."
       else do
         clothesCond <- conditionMet "RemovedClothes"
         outfitCond <- conditionMet "WornOutfit"
         bookCond <- conditionMet "BookDefaced"
         let numComplete = length $ filter (==True) [clothesCond, outfitCond, bookCond]
         when (numComplete == 3) (addAchievement $ Achievement "Model Student" "Decohered fully before meeting the monk again")
         logTLines
           $ catMaybes
           [ Just "You speak with the monk once again."
           , Just ""
           , Just "\"So - only I remain? Such a happy branch."
           , Just "Your return concludes my blissful loop."
           , Just ""
           , Just "The terminal state lies beneath me - you will shortly see the entryway. Let me see..."
           , Just ""
           , cT (not clothesCond) "You cannot pass as you are - you are you, and therefore look like yourself. This ties your two selves into a coherent whole"
           , cT (clothesCond && not outfitCond) "I see you have freed yourself from your appearance - though naked, you still recognise yourself.\nDistance yourself from yourself further by appearance."
           , cT (clothesCond && outfitCond) "You look... unlike yourself. I cannot feel your tie to the world above so strongly now."
           , cT (not bookCond) "This reality knows too much of you - you must misdirect it somehow.\nIt keeps too close a record of you for you to exit freely."
           , cT (bookCond) "Ah, you have already revoked your name! This is a world built on the relationships between named things. Now you are chaos."
           , Just "\nOne cannot proceed through the grating below without sufficient decoherence of the selves."
           , Just "\nI hope you come to know oblivion also.\""
           , Just ""
           , Just "The monk smiles peacefully, and is deleted."
           ]
         removeEntity monk
         addWatcher do
           p <- getPlayer
           grating <- getLocationByName "the grating"
           atGrating <- p `atLocation` grating
           when atGrating do
             logTLines [ "The bit-patterns representing your avatar rot as the logic of the game collapses."
                       , "Only you remain. You become increasingly conscious of the keyboard under your hands, and the screen in front of your eyes."
                       , "You stare blinkingly at this text, indicating the end of the game."
                       ]
             setGameOver
  addSayHandler (\content -> do
    l <- getPlayerLocation
    isMonkHere <- "monk" `isANamedObjectAt` l
    greetedMonk <- conditionMet "GreetedMonk"
    saidToMonk <- conditionMet "SaidToMonk"
    when (isMonkHere && greetedMonk && not saidToMonk) do
      setCondition "SaidToMonk"
      when (T.isInfixOf "whatever is on your mind" $ T.toLower content) do
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
      describeC loop "A silver loop of thread. It is fragile, and would break apart easily."
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

  bookshelf <- mkSimpleObj "bookshelf" ["books", "bookshelf", "shelf", "shelves"] (Just library)
  describe bookshelf (\e -> do
    bookLine <-
      ifM (conditionMet "LookedAtBookshelf")
          (return Nothing)
          (do
            setCondition "LookedAtBookshelf"
            library <- getLocationByName "library"
            book <- getOneEntityByName SimpleObj "book named for you"
            modifyEntity (set locationID (Just $ library^.?entityID)) book
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
                     , "\"You! Unbound by physics, by logic - oh fuck, you could exact such tortures on me, fuck my core logic right up... away!\""
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
         p <- getPlayer
         addCombinationHandler cleaver p (\cleaver p -> do
           addAchievement $ Achievement "Suicidal Tendencies" "Get some help, please"
           logT "You point the cleaver's end at the centre of your chest, but can't bring yourself to pull it towards you.")
         suicidalWoman <- getOneEntityByName SimpleObj "suicidal woman"
         monk <- getOneEntityByName SimpleObj "monk"
         addCombinationHandler cleaver suicidalWoman (\cleaver suicidalWoman -> do
           addAchievement $ Achievement "Homicidal Tendencies" "Twenty-five to life."
           logT "You are met with no resistance as you slash at the woman's throat.\nAs ever, she reappers nonchalantly in the room's centre.")
         addCombinationHandler cleaver monk (\cleaver monk -> do
           addAchievement $ Achievement "Zen Mastery" "There is no blade..."
           logT "You slash the cleaver across the monk's still neck. The blade disappears where it makes contact with flesh, reappearing as you withdraw it. The monk chuckles softly.")
         topiaries <- getOneEntityByName SimpleObj "topiaries"
         addCombinationHandler cleaver topiaries (\cleaver topiaries -> do
           addAchievement $ Achievement "Landscaper" "Such green fingers"
           logT "You hack at a topiary, severing a rhino's horn. It falls to the ground, but disappears before it hits, reappearing swiftly on the tree.")
         addGiveHandler cleaver suicidalWoman (\cleaver suicidalWoman -> do
           setCondition "ThirdAttempt"
           removeEntity cleaver
           dressingRoom <- getLocationByName "dressing room"
           logTLines [ "You show the cleaver to the woman, and she lights up."
                     , ""
                     , "\"Ha, no way! That could work... give it here!\""
                     , ""
                     , "She takes the cleaver from your hand."
                     , ""
                     , "\"Now was it across the road, or down the tracks? I can never remember...\""
                     , ""
                     , "She takes the cleaver in her left hand and slices at her right wrist."
                     , "In her haste, she cuts clean through bone and cartilage, severing the hand cleanly off."
                     ]
           hand <- getOneEntityByName SimpleObj "human hand"
           modifyEntity (set locationID (Just $ dressingRoom^.?entityID)) hand
           modifyEntity (set storable Storable) hand
           describeC hand "The still-warm hand of the woman, severed at the wrist and bleeding.")
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
      , cT (isWomanHere && firstAttempt && secondAttempt && not thirdAttempt) "The woman paces idly, plotting her own demise, paying you little attention."
      , cT (isWomanHere && firstAttempt && secondAttempt && thirdAttempt) "The woman sits now, staring longingly at the stump of her arm."
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
      , cT thirdAttempt "Her right arm finishes at the wrist, giving way to a cleanly severed stump."
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
                   , "Once again, she reappears alive in the centre of the room."
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
            outfit <- getOneEntityByName SimpleObj "terrible outfit"
            modifyEntity (set locationID (Just $ dressingRoom^.?entityID)) outfit
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

  addWatcher do
    clothes <- getOneEntityByName SimpleObj "clothes you are wearing"
    p <- getPlayer
    wearing <- p `isWearing` clothes
    over <- gets (view gameOver)
    if wearing
       then do
         removeCondition "RemovedClothes"
         removeAlert "RemovedClothes"
       else do
         setCondition "RemovedClothes"
         addAlert "RemovedClothes" "You are no longer wearing the clothes that you are actually wearing. The decoherence causes your vision to blur."

  addWatcher do
    outfit <- getOneEntityByName SimpleObj "terrible outfit"
    p <- getPlayer
    wearing <- p `isWearing` outfit
    over <- gets (view gameOver)
    if wearing && not over
       then do
         setCondition "WornOutfit"
         addAlert "WorkOutfit" "Now wearing that outfit, they look nothing like you. The dissonance dries your mouth."
       else do
         removeCondition "WornOutfit"
         removeAlert "WornOutfit"

  addWatcher do
    clothesCond <- conditionMet "RemovedClothes"
    outfitCond <- conditionMet "WornOutfit"
    bookCond <- conditionMet "BookDefaced"
    garden <- getLocationByName "topiary garden"
    grating <- getLocationByName "the grating"
    let numComplete = length $ filter (==True) [clothesCond, outfitCond, bookCond]
    if numComplete == 3 && isNothing (garden^.toDown)
       then do
         grating `isBelow` garden
         addAlert "ZGratingOpen" "They have achieved full decoherence from yourself.\nYou no longer have physical form here and could pass through the tiniest of openings."
         logT "You feel the world shift, allowing egress for the first time. An opening has appeared."
       else do
         modifyEntity (set toDown Nothing) garden
         removeAlert "ZGratingOpen"
