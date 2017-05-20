# Particle-simulation
The purpose of the simulation is, given a set of particles, to compute their motion over time under the influence of the 
gravitational force acting between the particles. This is an important simulation in computational physics and many highly 
sophisticated algorithms have been developed to reduce the computational complexity of particle simulations. 
To favour simplicity, we will however, implement a direct gravitational scheme, which has quadratic complexity. 
(In each simulation step, we compute the gravitational force for all pairs of particles.) 
