module Simulation (moveParticle, accelerate, advanceWorld) where
  
import World
import Physics
import Control.Parallel.Strategies
--import Control.Seq
-- Move a particle according to its velocity for the given number of (simulated) seconds.
--error "You need to implement Simulation.moveParticle"
moveParticle :: Float -> Particle -> Particle
moveParticle delta_t (Particle mass (x,y) (vx,vy))= Particle mass ((x + delta_t * vx),(y + delta_t * vy)) (vx,vy)
--    where
--      delta_r = (vel) * delta_t


-- Accelerate a particle in dependence on the gravitational force excerted by all other particles for
-- the given number of (simulated) seconds.
--
--[Particle mass (x,y) (vx,vy)] = [Particle mass (x,y) (vx,vy)]
accelerate :: Float -> [Particle] -> [Particle]
accelerate delta_t pars = 
    parMap rseq acc pars
  where
    acc par =
      foldl addAcc par pars
    addAcc thisPar@(Particle mass pos (vx,vy)) otherPar =
      Particle mass pos ((vx + delta_t *ax),(vy + delta_t *ay))
      where
        (ax,ay) = force thisPar otherPar
--      let (ax,ay) = force thisPar otherPar
--      in
       
-- Progressing the world state
--
advanceWorld :: unused -> Float -> World -> World
advanceWorld unused delta_t (World f1 f2 f3 pars) = World f1 f2 f3 newpars
  where
    dt = delta_t * f3
    newpars = map (moveParticle dt) (accelerate dt pars)
  
  
  --error "You need to implement Simulation.advanceWorld"
