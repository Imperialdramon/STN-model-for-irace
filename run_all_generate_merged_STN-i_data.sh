#!/bin/bash

# ==============================================================================
# Script: run_all_generate_merged_STN-i_data.sh
# Description: Generates merged STN-i .RData files for specified merged experiments
#              for each algorithm and L1, L2, L3 levels.
#
# Usage:
#   1. Make the script executable:
#        chmod +x run_all_generate_merged_STN-i_data.sh
#
#   2. Run the script:
#        ./run_all_generate_merged_STN-i_data.sh
#
#   Output:
#     - Merged .RData files per merged pair/level
#     - Log file: Logs/generate_merged_stn_data_logs.log
# ==============================================================================

# Create Logs directory if it doesn't exist
LOG_DIR="./Logs"
mkdir -p "$LOG_DIR"

# Log file path
LOG_FILE="$LOG_DIR/generate_merged_stn_data_logs.log"
echo "=== Merged STN-i generation started at $(date) ===" > "$LOG_FILE"

# Function to run Rscript with logging
run_merge_rscript() {
  local args=("$@")
  local output_file=""
  for arg in "${args[@]}"; do
    [[ $arg == --output_file=* ]] && output_file="${arg#--output_file=}"
  done

  echo ">> Generating: $output_file" | tee -a "$LOG_FILE"
  if ! Rscript R/generate_merged_STN-i_data.R "${args[@]}" >> "$LOG_FILE" 2>&1; then
    echo "❌ Error: Failed to generate $output_file" | tee -a "$LOG_FILE"
  else
    echo "✅ Success: Generated $output_file" | tee -a "$LOG_FILE"
  fi
}

# Define merged cases per algorithm
declare -A merged_experiments
merged_experiments["ACOTSP"]="Merged-E1-E2-2000 Merged-E3-E4-2000"
merged_experiments["MMASQAP"]="Merged-E1-E2-60 Merged-E3-E4-60"
merged_experiments["PSO-X"]="Merged-E1-E2-Mix Merged-E3-E4-Mix Merged-E5-E6-Mul Merged-E7-E8-Mul Merged-E9-E10-Uni Merged-E11-E12-Uni"

# Levels to process
levels="L1 L2 L3"

# Iterate over each algorithm and its merged cases
for alg in "${!merged_experiments[@]}"; do
  echo "=== Processing algorithm: $alg ===" | tee -a "$LOG_FILE"

  for merged_case in ${merged_experiments[$alg]}; do
    for lvl in $levels; do

      input_path="Experiments/$alg/$merged_case/Data/$lvl"
      output_dir="Experiments/$alg/$merged_case/Merged-STNs-i"
      output_file="merged-STN-i-${merged_case#Merged-}-$lvl.RData"

      run_merge_rscript \
        --input="$input_path" \
        --output="$output_dir" \
        --output_file="$output_file" \
        --criteria=mean
    done
  done
done

echo "=== Merged STN-i generation finished at $(date) ===" >> "$LOG_FILE"
