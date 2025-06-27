#!/bin/bash

# ==============================================================================
# Script: aggregate_all_merged_metrics_per_algorithm.sh
# Description: Aggregates merged STN-i metrics into one file per algorithm,
#              preserving naming based on the merged case size (2000, 60, Mix, etc.).
#
# Output:
#   Metrics-Merged-STN-i/Metrics-Merged-<Algorithm>-STN-i.csv
# ==============================================================================

# Output directory
FINAL_OUTPUT_DIR="./Metrics-Merged-STN-i"
mkdir -p "$FINAL_OUTPUT_DIR"

# Define merged cases per algorithm
declare -A merged_experiments
merged_experiments["ACOTSP"]="Merged-E1-E2-2000 Merged-E3-E4-2000"
merged_experiments["MMASQAP"]="Merged-E1-E2-60 Merged-E3-E4-60"
merged_experiments["PSO-X"]="Merged-E1-E2-Mix Merged-E3-E4-Mix Merged-E5-E6-Mul Merged-E7-E8-Mul Merged-E9-E10-Uni Merged-E11-E12-Uni"

# Levels to process
levels="L1 L2 L3"

# Loop over algorithms
for alg in "${!merged_experiments[@]}"; do

  # Get size token from first merged case (e.g., 2000, 60, Mix, etc.)
  first_case=${merged_experiments[$alg]%% *}
  size_token=$(echo "$first_case" | rev | cut -d'-' -f1 | rev)

  output_file="$FINAL_OUTPUT_DIR/Metrics-Merged-${alg}-STN-i.csv"
  header_written=false

  for merged_case in ${merged_experiments[$alg]}; do
    merged_label="${merged_case#Merged-}"

    for lvl in $levels; do
      metrics_file="Experiments/$alg/$merged_case/Metrics/metrics-Merged-STN-i-${merged_label}-${lvl}.csv"

      if [[ -f "$metrics_file" ]]; then
        if ! $header_written; then
          head -n 1 "$metrics_file" > "$output_file"
          header_written=true
        fi
        tail -n +2 "$metrics_file" >> "$output_file"
      else
        echo "⚠️ Missing file: $metrics_file"
      fi
    done
  done

  echo "✅ Saved: $output_file"
done

echo "🎉 All merged metrics aggregated to $FINAL_OUTPUT_DIR"
