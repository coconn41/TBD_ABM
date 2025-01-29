#!/bin/bash

echo "starting TBD_ABM_run_net_4.sh"

# Submit the pipeline as a background process with ./run.sh
module load R # Uncomment if R is an environment module.
nohup nice -4 R CMD BATCH ./Cluster_TBD_ABM/Code/Master/Network_specific/Network_4_Master.R &

# Change the nice level above as appropriate
# for your situation and system.

# Removing .RData is recommended.
 rm -f .RData

echo "ending TBD_ABM_run_net_4.sh"