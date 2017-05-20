# Particle-simulation
The purpose of the simulation is, given a set of particles, to compute their motion over time under the influence of the 
gravitational force acting between the particles. This is an important simulation in computational physics and many highly 
sophisticated algorithms have been developed to reduce the computational complexity of particle simulations. 
To favour simplicity, we will however, implement a direct gravitational scheme, which has quadratic complexity. 
(In each simulation step, we compute the gravitational force for all pairs of particles.) 

 World.hs
This module defines basic types of physical measures and defines the World state threaded through the simulation. You will use the type World to instantiate the polymorphic model type of the function simulate.

Visualise.hs
This module defines the function mapping a World configuration to its graphical representation.

Physics.hs
This module defines the force function that computes the gravitational force that one particle exerts on another. To be precise, the function does not compute the physical force explicitly, but directly computes the acceleration that one particle receives due to the gravitational pull of another particle.

Simulation.hs
Functions for the particle simulation.

Main.hs
This modules animates the simulation in the playground in Haskell for Mac and in a separate window with Gloss otherwise.
TestSupport.hs
This modules defines a random test value generator for Particles and Worlds. 
