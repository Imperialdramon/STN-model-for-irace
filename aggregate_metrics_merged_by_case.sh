#!/bin/bash

# ==============================================================================
# Script: aggregate_metrics_merged_by_case.sh
# Description: Aggregates merged metrics across levels into a single CSV per
#              merged experiment (e.g., E1-E2-2000), preserving naming.
#
# Output:
#   Metrics-Merged-STN-i/metrics-Merged-STN-i-<merged_case>.csv
# ==============================================================================

# Output directory
OUTPUT_DIR="./Metrics-Merged-STN-i"
mkdir -p "$OUTPUT_DIR"

# Define merged cases per algorithm
declare -A merged_experiments
merged_experiments["ACOTSP"]="Merged-E1-E2-2000 Merged-E3-E4-2000"
merged_experiments["MMASQAP"]="Merged-E1-E2-60 Merged-E3-E4-60"
merged_experiments["PSO-X"]="Merged-E1-E2-Mix Merged-E3-E4-Mix Merged-E5-E6-Mul Merged-E7-E8-Mul Merged-E9-E10-Uni Merged-E11-E12-Uni"

# Levels to process
levels="L1 L2 L3"

# Iterate through each merged case
for alg in "${!merged_experiments[@]}"; do
  for merged_case in ${merged_experiments[$alg]}; do

    merged_label="${merged_case#Merged-}"  # e.g., E1-E2-2000
    output_file="$OUTPUT_DIR/$alg/metrics-Merged-STN-i-${merged_label}.csv"
    header_written=false

    for level in $levels; do
      metrics_file="Experiments/$alg/$merged_case/Metrics/metrics-Merged-STN-i-${merged_label}-${level}.csv"

      if [[ -f "$metrics_file" ]]; then
        if ! $header_written; then
          head -n 1 "$metrics_file" > "$output_file"
          header_written=true
        fi
        tail -n +2 "$metrics_file" >> "$output_file"
      else
        echo "⚠️ Warning: Missing file $metrics_file"
      fi
    done

  done
done

echo "✅ Aggregation of merged STN-i metrics complete. Files saved in: $OUTPUT_DIR"
