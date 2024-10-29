#!/bin/bash

# User Details
#SBATCH --mail-user=maximilian.d.rohde@vanderbilt.edu 
#SBATCH --mail-type=FAIL                         

# Compute details
#SBATCH --partition=production   
#SBATCH --nodes=1                
#SBATCH --ntasks=1               
#SBATCH --cpus-per-task=1        
#SBATCH --time=00:05:00           
#SBATCH --mem=10G
#SBATCH --constraint=cascadelake              

# Name of output file
#SBATCH --output=status_reports/out-%A-%a.out   

# Number of jobs in SLURM job array
#SBATCH --array=1-10   

# Print CPU information
lscpu

# Load R
module load GCC/11.3.0 OpenMPI/4.1.4 R/4.2.1    

# Run R script
Rscript --no-save bug_test.R
