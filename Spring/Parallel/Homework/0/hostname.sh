#!/bin/bash
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -t 01:00:00
#SBATCH -o mer3ef_output_hw_0.txt 
#SBATCH -p training 
#SBATCH -A parallelcomputing 

/bin/hostname
grep "LYNN GRAMBO" /home/ag8t/data/records.txt
