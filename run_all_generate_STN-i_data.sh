#!/bin/bash

# ==============================================================================
# Script: run_all_generate_STN-i_data.sh
# Description: Generates .RData files from STN-i .txt files for all experiments.
#
# Usage:
#   1. Make the script executable:
#        chmod +x run_all_generate_STN-i_data.sh
#
#   2. Run the script:
#        ./run_all_generate_STN-i_data.sh
#
#   Output:
#     - Log file: Logs/run_generate_STN-i.log
#     - RData outputs: one per experiment/level
# ==============================================================================

# Create log directory if it doesn't exist
LOG_DIR="./Logs"
mkdir -p "$LOG_DIR"

# Set log file path
LOG_FILE="$LOG_DIR/run_generate_STN-i.log"
echo "=== STN-i data generation started at $(date) ===" > "$LOG_FILE"

# Function to run the Rscript and log output
run_generate_rdata() {
  local args=("$@")
  local output_file=""
  for arg in "${args[@]}"; do
    [[ $arg == --output_file=* ]] && output_file="${arg#--output_file=}"
  done

  echo ">> Generating: $output_file" | tee -a "$LOG_FILE"

  if ! Rscript R/generate_STN-i_data.R "${args[@]}" >> "$LOG_FILE" 2>&1; then
    echo "❌ Error: Failed to generate $output_file" | tee -a "$LOG_FILE"
  else
    echo "✅ Success: Generated $output_file" | tee -a "$LOG_FILE"
  fi
}

# Define algorithms and experiments
declare -A experiments
#experiments["ACOTSP"]="E1-BL-WSR-2000 E2-BL-SR-2000 E3-BH-WSR-2000 E4-BH-SR-2000"
#experiments["MMASQAP"]="E1-BL-WSR-60 E2-BL-SR-60 E3-BH-WSR-60 E4-BH-SR-60"
#experiments["PSO-X"]="E1-BL-WSR-Mix E2-BL-SR-Mix E3-BH-WSR-Mix E4-BH-SR-Mix E5-BL-WSR-Mul E6-BL-SR-Mul E7-BH-WSR-Mul E8-BH-SR-Mul E9-BL-WSR-Uni E10-BL-SR-Uni E11-BH-WSR-Uni E12-BH-SR-Uni"
experiments["ACOTSP"]="E5-BL-22-2000 E6-BL-45-2000 E7-BH-45-2000 E8-BH-90-2000"
experiments["PSO-X"]="E13-BH-32-Mix E14-BH-65-Mix"

# Define levels to generate per experiment
#levels="L1 L2 L3"
levels="L1 L2 L3 L4 L5"

# Loop over all combinations of algorithm, experiment, and level
for alg in "${!experiments[@]}"; do
  echo "=== Processing algorithm: $alg ===" | tee -a "$LOG_FILE"

  for exp in ${experiments[$alg]}; do
    for lvl in $levels; do

      input_path="Experiments/$alg/$exp/Data/STN-i-$exp-$lvl.txt"
      output_dir="Experiments/$alg/$exp/STNs-i"
      output_file="STN-i-$exp-$lvl.RData"
      network_base="${exp#*-}"
      network_base="${network_base%-*}"
      network_name="${network_base}"

      run_generate_rdata \
        --input="$input_path" \
        --output="$output_dir" \
        --output_file="$output_file" \
        --problem_type=min \
        --network_name="$network_name"

    done
  done
done

echo "=== STN-i data generation finished at $(date) ===" >> "$LOG_FILE"
