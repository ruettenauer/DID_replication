#!/bin/bash

#SBATCH --time=47:30:00         # Walltime
#SBATCH --nodes=1               # Use 1 Node (Unless code is multi-node parallelized)
#SBATCH --ntasks=1  
#SBATCH --cpus-per-task=16
#SBATCH --mem=64000
#SBATCH -o slurm-%j.out
#SBATCH --job-name=DID9
#SBATCH --constraint=XEON_SP_6126

# Export file and working directory
export FILENAME=$HOME/DID/01_Script/09_Run-Simulation-set_hpc_het-treat_twotime.R
export FILENAME2=$HOME/DID/01_Script/01_DiD-Smulation-Program.R

export WORK_DIR=$HOME/DID/02_Data

# Load the default version of R
module unload R
module load R/4.3

# Take advantage of all the threads (linear algebra)
export OMP_NUM_THREADS=$SLURM_CPUS_ON_NODE

# Create scratch & copy everything over to scratch
# mkdir -p $SCR_DIR
# cd $SCR_DIR
# cp -p $WORK_DIR/* . 

cd $WORK_DIR

# Copy second script into wd
cp $FILENAME2 .

# Run the R script in batch
Rscript $FILENAME > $FILENAME.out

# Copy results over + clean up
# cd $WORK_DIR
# cp -pR $SCR_DIR/* .
# rm -rf $SCR_DIR

echo "End of program at `date`"
