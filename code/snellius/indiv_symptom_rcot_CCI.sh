#!/bin/bash
#Set job requirements (note set time 1.5 to 2x longer than expected)
#SBATCH -J sym_rcot_CCI

#SBATCH -t 120:00:00 
#SBATCH -p genoa
#SBATCH --exclusive 
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --cpus-per-task=128

#Send email at start en end
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=k.park@uva.nl

#SBATCH --output=helius/log/%x-%A_%a.out  # %A is the main job ID, %a is the task ID
#SBATCH --error=helius/log/%x-%A_%a.err   # where to store the output and error messages

#Loading modules
module load 2023
module load R-bundle-CRAN/2023.12-foss-2023a

# Set R_LIBS to include both directories
export R_LIBS="/gpfs/home1/kpark/rpackages:/gpfs/home1/kpark/R/x86_64-pc-linux-gnu-library/4.3:$R_LIBS"


# Run the R script with the specified alpha and threshold
Rscript $HOME/helius/code/individual_sym_CCI.R 

