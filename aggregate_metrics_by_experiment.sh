#!/bin/bash

# ==============================================================================
# Script: aggregate_all_metrics_per_algorithm.sh
# Description: Aggregates ALL experiment metrics (across levels and experiments)
#              into one file per algorithm (e.g., Metrics-ACOTSP-STN-i-2000.csv)
#
# Output:
#   Metrics-STN-i/Metrics-<Algorithm>-STN-i.csv
# ==============================================================================

# Final output directory
FINAL_OUTPUT_DIR="./Metrics-STN-i"
mkdir -p "$FINAL_OUTPUT_DIR"

# Define experiments per algorithm
declare -A experiments
experiments["ACOTSP"]="E1-BL-WSR-2000 E2-BL-SR-2000 E3-BH-WSR-2000 E4-BH-SR-2000"
experiments["MMASQAP"]="E1-BL-WSR-60 E2-BL-SR-60 E3-BH-WSR-60 E4-BH-SR-60"
experiments["PSO-X"]="E1-BL-WSR-Mix E2-BL-SR-Mix E3-BH-WSR-Mix E4-BH-SR-Mix E5-BL-WSR-Mul E6-BL-SR-Mul E7-BH-WSR-Mul E8-BH-SR-Mul E9-BL-WSR-Uni E10-BL-SR-Uni E11-BH-WSR-Uni E12-BH-SR-Uni"

# Levels used in each experiment
levels="L1 L2 L3"

# Loop over algorithms
for alg in "${!experiments[@]}"; do

  # Detect unique size token (2000, 60, Mix, etc.) from first experiment
  first_exp=${experiments[$alg]%% *}  # First item of the list
  size_token=$(echo "$first_exp" | rev | cut -d'-' -f1 | rev)

  output_file="$FINAL_OUTPUT_DIR/Metrics-${alg}-STN-i.csv"
  header_written=false

  for exp in ${experiments[$alg]}; do
    for lvl in $levels; do
      metrics_file="Experiments/$alg/$exp/Metrics/metrics-STN-i-${exp}-${lvl}.csv"

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

echo "🎉 All algorithm-wise metrics aggregated to $FINAL_OUTPUT_DIR"
