module Game where

import Control.Lens

data EntityType = Human
                | Letter

type Name = String

type Description = String

data Location = Location { _locationName :: Name
                         , _description :: Description
                         , _toNorth :: Maybe Location
                         , _toEast :: Maybe Location
                         , _toSouth :: Maybe Location
                         , _toWest :: Maybe Location
                         , _toUp :: Maybe Location
                         , _toDown :: Maybe Location
                         }

data Entity = Entity { _entityType :: EntityType
                     , _entityName :: Name
                     , _location :: Location
                     , _storable :: Bool
                     , _inventory :: Inventory
                     , _usable :: Bool
                     , _edible :: Bool
                     , _potable :: Bool
                     }

type Inventory = [Entity]

data GameState = GameState { _entities :: [Entity]
                           , _clock :: Integer
                           }

data Instruction e = GoNorth
                   | GoEast
                   | GoSouth
                   | GoWest
                   | GoUp
                   | GoDown
                   | Wait
                   | Use e
                   | Get e
                   | Drop e
                   | Eat e
                   | Drink e
