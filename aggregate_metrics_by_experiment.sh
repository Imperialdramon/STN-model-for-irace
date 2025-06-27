#!/bin/bash

# ==============================================================================
# Script: aggregate_metrics_by_experiment.sh
# Description: Aggregates metrics across levels into a single CSV per experiment,
#              preserving original experiment names.
#
# Output:
#   Metrics-STN-i/metrics-STN-i-<experiment>.csv
# ==============================================================================

# Output directory
OUTPUT_DIR="./Metrics-STN-i"
mkdir -p "$OUTPUT_DIR"

# Define experiments per algorithm
declare -A experiments
experiments["ACOTSP"]="E1-BL-WSR-2000 E2-BL-SR-2000 E3-BH-WSR-2000 E4-BH-SR-2000"
experiments["MMASQAP"]="E1-BL-WSR-60 E2-BL-SR-60 E3-BH-WSR-60 E4-BH-SR-60"
experiments["PSO-X"]="E1-BL-WSR-Mix E2-BL-SR-Mix E3-BH-WSR-Mix E4-BH-SR-Mix E5-BL-WSR-Mul E6-BL-SR-Mul E7-BH-WSR-Mul E8-BH-SR-Mul E9-BL-WSR-Uni E10-BL-SR-Uni E11-BH-WSR-Uni E12-BH-SR-Uni"

# Levels to process
levels="L1 L2 L3"

# Loop through all experiments
for alg in "${!experiments[@]}"; do
  for exp in ${experiments[$alg]}; do

    output_file="$OUTPUT_DIR/$alg/metrics-STN-i-${exp}.csv"
    header_written=false

    for lvl in $levels; do
      metrics_file="Experiments/$alg/$exp/Metrics/metrics-STN-i-${exp}-${lvl}.csv"

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

echo "✅ Aggregation complete. Files saved in $OUTPUT_DIR"
