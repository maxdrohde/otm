#!/bin/bash

for SETTING in {1..25}
do
  sbatch --export=SETTING=$SETTING submit.slurm
done

echo "All jobs submitted successfully."
