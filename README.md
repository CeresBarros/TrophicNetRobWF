# TrophicNetRobWF - Trophic Networks' Robustness workflow

This repository contains the full R-based workflow used in Dansereau et al. (in prep.), 
Box 3.

The [full workflow](global.html) runs all necessary scripts to:

1.  Build baseline networks and networks under scenarios of change

2.  Run a sensitivity analysis of baseline network properties to different 
    species-specific thresholds of extinction (the minimum number or prey 
    each species needs to survive)
    
3.  Re-build baseline networks given the thresholds of extinction

4.  Build networks under two scenarios of change -- climate change and
    failure to protect endagered species
    
5.  Prepare outputs and make figures used in publication (and others). 
    
Workflow code is available [here](global.Rmd).
