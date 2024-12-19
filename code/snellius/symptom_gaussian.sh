#!/bin/bash
#Set job requirements (note set time 1.5 to 2x longer than expected)
#SBATCH -J sym_gaussian

#SBATCH -t 50:00:00 
#SBATCH -p genoa
#SBATCH --exclusive 
#SBATCH --nodes=1
#SBATCH --tasks-per-node=4  
#SBATCH --cpus-per-task=30
#SBATCH --array=0-3 # Total jobs based on parameter combinations

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


# Define parameters
alphas=("0.01" "0.05")
thresholds=("0.6" "0.7")

# Calculate indices for combinations
alpha_index=$((SLURM_ARRAY_TASK_ID / 2))
threshold_index=$((SLURM_ARRAY_TASK_ID % 2))
alpha=${alphas[$alpha_index]}
threshold=${thresholds[$threshold_index]}

# Log the chosen parameters (for debugging purposes)
echo "Running with alpha=$alpha and threshold=$threshold"

# Run the R script with the specific alpha and threshold
Rscript $HOME/helius/code/sym_gaussian.R "$alpha" "$threshold"
