#!/bin/bash
#SBATCH -J my_job_name
#SBATCH -o %J.out
#SBATCH -e %J.err
#SBATCH -n 24
#SBATCH -p small
#SBATCH -t 5

aprun /homeappl/home/lautuppi/appl_sisu/CSCss/summerschool/unix/prog

