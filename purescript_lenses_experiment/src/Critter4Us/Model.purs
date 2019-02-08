module Critter4Us.Model where

import Critter4Us.Animal (Animal)
import Critter4Us.Animal as Animal

import Data.Lens (Lens')
import Data.Lens.Setter (over) as Lens
import Data.Lens.At (at) as Lens
import Data.Lens.Record (prop) as Lens
import Data.Lens.Common (_Just) as Lens
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Prelude
import Type.Data.Symbol (SProxy (..))

-- | Model
type Model =
  { animals :: Map Animal.Id Animal
  }

-- | Lenses
_animals :: Lens' Model (Map Animal.Id Animal)
_animals = Lens.prop (SProxy :: SProxy "animals")

_animal :: Animal.Id -> Lens' Model (Maybe Animal)
_animal id = _animals <<< Lens.at id

-- | API
initialModel :: Model
initialModel = { animals: Map.empty }

addAnimal :: Animal.Id -> Animal.Name -> Model -> Model
addAnimal id name =
  Lens.over
    _animals
    (Map.insert id (Animal.named id name))

addAnimalTag :: Animal.Id -> Animal.Tag -> Model -> Model
addAnimalTag id tag =
  Lens.over
    (_animal id <<< Lens._Just)
    (Animal.addTag tag)

