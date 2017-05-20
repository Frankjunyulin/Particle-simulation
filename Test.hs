module Test where

import World
import Test.QuickCheck
import Physics
import Simulation
import TestSupport
prop_EnergyConservation :: World -> Bool
prop_EnergyConservation w = ( realToFrac(abs(e2-e1)/e1)< World.epsilon)
  where
    e1 = worldEnergy w
    e2 = worldEnergy (advanceWorld 0 0.001 w)

