#!/bin/bash

echo "starting load_and_save.sh"

cd /n/home08/collinoconnor/R/TBD_ABM

# Submit the pipeline as a background process with ./run.sh
module load R # Uncomment if R is an environment module.
Rscript Code/Post_sim_analysis/Simplified_analysis.R

# Change the nice level above as appropriate
# for your situation and system.

# Removing .RData is recommended.
 rm -f .RData

echo "ending load_and_save.sh"