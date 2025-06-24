#!/bin/bash

# ==============================================================================
# Script: run_all_metrics_merged_STN-i.sh
# Description: Generates CSV metrics files from merged STN-i .RData files
#              for each merged experiment group and level.
#
# Usage:
#   1. Make the script executable:
#        chmod +x run_all_metrics_merged_STN-i.sh
#
#   2. Run the script:
#        ./run_all_metrics_merged_STN-i.sh
#
#   Output:
#     - One metrics CSV per merged pair and level (e.g., metrics-Merged-STN-i-E1-E2-2000-L1.csv)
#     - Log file: Logs/run_metrics_merged_logs.log
# ==============================================================================

# Create Logs directory if it doesn't exist
LOG_DIR="./Logs"
mkdir -p "$LOG_DIR"

# Set log file path
LOG_FILE="$LOG_DIR/run_metrics_merged_logs.log"
echo "=== Merged STN-i metrics generation started at $(date) ===" > "$LOG_FILE"

# Function to execute the R script with logging
run_merged_metrics_rscript() {
  local args=("$@")
  local output_file=""
  for arg in "${args[@]}"; do
    [[ $arg == --output_file=* ]] && output_file="${arg#--output_file=}"
  done

  echo ">> Generating metrics: $output_file" | tee -a "$LOG_FILE"

  if ! Rscript R/metrics_merged_STN-i.R "${args[@]}" >> "$LOG_FILE" 2>&1; then
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

# Loop through each merged case and combine metrics into one CSV per case and level
for alg in "${!merged_experiments[@]}"; do
  echo "=== Processing algorithm: $alg ===" | tee -a "$LOG_FILE"

  for merged_case in ${merged_experiments[$alg]}; do
    merged_label="${merged_case#Merged-}"  # Extract E1-E2-2000

    for level in $levels; do
      input_path="Experiments/$alg/$merged_case/Merged-STNs-i/merged-STN-i-$merged_label-$level.RData"
      output_dir="Experiments/$alg/$merged_case/Metrics"
      output_file="metrics-Merged-STN-i-$merged_label-$level.csv"

      run_merged_metrics_rscript \
        --input="$input_path" \
        --output="$output_dir" \
        --output_file="$output_file"
    done
  done
done

echo "=== Merged STN-i metrics generation finished at $(date) ===" >> "$LOG_FILE"
