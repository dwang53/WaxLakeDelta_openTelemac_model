#!/bin/bash
#----------------------------------------------------
# Sample Slurm job script
#   for TACC Stampede2 SKX nodes
#
#   *** MPI Job on SKX Normal Queue ***
# 
# Last revised: 20 Oct 2017
#
# Notes:
#
#   -- Launch this script by executing
#      "sbatch skx.mpi.slurm" on Stampede2 login node.
#
#   -- Use ibrun to launch MPI codes on TACC systems.
#      Do not use mpirun or mpiexec.
#
#   -- Max recommended MPI ranks per SKX node: 48
#      (start small, increase gradually).
#
#   -- If you're running out of memory, try running
#      fewer tasks per node to give each task more memory.
#
#----------------------------------------------------

#SBATCH -J myjob           # Job name
#SBATCH -o myjob.o%j       # Name of stdout output file
#SBATCH -e myjob.e%j       # Name of stderr error file
#SBATCH -p normal     # Queue (partition) name
#SBATCH -N 48               # Total # of nodes 
#SBATCH -n 2688              # Total # of mpi tasks
#SBATCH -t 48:00:00        # Run time (hh:mm:ss)
#SBATCH --mail-user=dwang53@illinois.edu
#SBATCH --mail-type=all    # Send email at begin and end of job
#SBATCH -A CTS22005       # Allocation name (req'd if you have more than 1)

# Other commands must follow all #SBATCH directives...
##source $HOME/opentelemac/bin/telemac-v8p1r2.sh
source /work2/04680/dwang53/frontera/opentelemac/bin/load_v8p4r0.sh
module list
cd $PWD
pwd
date

# Launch MPI code... 
telemac2d.py -s t2d_cvsm1.cas
#####ibrun ./mycode.exe         # Use ibrun instead of mpirun or mpiexec

# ---------------------------------------------------
