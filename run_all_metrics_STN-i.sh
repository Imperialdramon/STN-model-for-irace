#!/bin/bash

# ==============================================================================
# Script: run_all_metrics_STN-i.sh
# Description: Generates CSV metric files for each STN-i .RData file across
#              all experiments and levels. Logs output to Logs/run_metrics_logs.log.
#
# Usage:
#   1. Make the script executable:
#        chmod +x run_all_metrics_STN-i.sh
#
#   2. Run the script:
#        ./run_all_metrics_STN-i.sh
#
#   Output:
#     - One metrics CSV file per experiment and level
#     - Log file: Logs/run_metrics_logs.log
# ==============================================================================

# Ensure Logs directory exists
LOG_DIR="./Logs"
mkdir -p "$LOG_DIR"

# Define log file path
LOG_FILE="$LOG_DIR/run_metrics_logs.log"
echo "=== STN-i metrics generation started at $(date) ===" > "$LOG_FILE"

# Function to execute Rscript with logging
run_metrics_rscript() {
  local args=("$@")
  local output_file=""
  for arg in "${args[@]}"; do
    [[ $arg == --output_file=* ]] && output_file="${arg#--output_file=}"
  done

  echo ">> Generating metrics: $output_file" | tee -a "$LOG_FILE"

  if ! Rscript R/metrics_STN-i.R "${args[@]}" >> "$LOG_FILE" 2>&1; then
    echo "❌ Error: Failed to generate $output_file" | tee -a "$LOG_FILE"
  else
    echo "✅ Success: Generated $output_file" | tee -a "$LOG_FILE"
  fi
}

# Define experiments per algorithm
declare -A experiments
experiments["ACOTSP"]="E1-BL-WSR-2000 E2-BL-SR-2000 E3-BH-WSR-2000 E4-BH-SR-2000"
experiments["MMASQAP"]="E1-BL-WSR-60 E2-BL-SR-60 E3-BH-WSR-60 E4-BH-SR-60"
experiments["PSO-X"]="E1-BL-WSR-Mix E2-BL-SR-Mix E3-BH-WSR-Mix E4-BH-SR-Mix E5-BL-WSR-Mul E6-BL-SR-Mul E7-BH-WSR-Mul E8-BH-SR-Mul E9-BL-WSR-Uni E10-BL-SR-Uni E11-BH-WSR-Uni E12-BH-SR-Uni"

# Levels to process
levels="L1 L2 L3"

# Loop through all combinations
for alg in "${!experiments[@]}"; do
  echo "=== Processing algorithm: $alg ===" | tee -a "$LOG_FILE"

  for exp in ${experiments[$alg]}; do
    for lvl in $levels; do

      input_path="Experiments/$alg/$exp/STNs-i/STN-i-$exp-$lvl.RData"
      output_dir="Experiments/$alg/$exp/Metrics"
      output_file="metrics-STN-i-$exp-$lvl.csv"

      run_metrics_rscript \
        --input="$input_path" \
        --output="$output_dir" \
        --output_file="$output_file"

    done
  done
done

echo "=== STN-i metrics generation finished at $(date) ===" >> "$LOG_FILE"
