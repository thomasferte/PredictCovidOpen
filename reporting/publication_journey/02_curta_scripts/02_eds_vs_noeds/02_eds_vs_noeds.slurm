#!/bin/bash

#############################
# les directives Slurm vont ici:

# Your job name (displayed by the queue)
#SBATCH -J PREDCOV

# walltime (hh:mm::ss)
#SBATCH -t 05:00:00

# Specify the number of nodes(nodes=) and the number of cores per nodes(tasks-pernode=) to be used
#SBATCH --ntasks=1 --cpus-per-task 32

# change working directory
# SBATCH --chdir=.

### In filenames, %j=jobid, %a=index in job array
#SBATCH -o PredictCovid_data/results_simu/result_bash/predcov_HOSP_%j_%a.out # standard out goes to this file
#SBATCH -e PredictCovid_data/results_simu/result_bash/predcov_HOSP_%j_%a.out # standard err goes to this file

# fin des directives PBS
#############################

# useful informations to print
echo "#############################"
echo "User:" $USER
echo "Date:" `date`
echo "Host:" `hostname`
echo "Directory:" `pwd`
echo "SLURM_JOBID:" $SLURM_JOBID
echo "SLURM_SUBMIT_DIR:" $SLURM_SUBMIT_DIR
echo "SLURM_JOB_NODELIST:" $SLURM_JOB_NODELIST
echo "#############################"

#############################

module load R/4.1.0

Rscript PredictCovid/reporting/publication_journey/02_curta_scripts/02_eds_vs_noeds/02_eds_vs_noeds.R
