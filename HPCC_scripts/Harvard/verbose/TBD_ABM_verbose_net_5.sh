#!/bin/bash

echo "starting TBD_ABM_run_net_5.sh"

# Submit the pipeline as a background process with ./run.sh
# module load R # Uncomment if R is an environment module.

Rscript ./R/TBD_ABM/Code/Master/Simplified_Master_5.R

# Change the nice level above as appropriate
# for your situation and system.

# Removing .RData is recommended.
rm -f .RData

echo "ending TBD_ABM_run_net_5.sh"