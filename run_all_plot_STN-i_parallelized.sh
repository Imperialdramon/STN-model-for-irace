#!/bin/bash

# ==============================================================================
# Script: run_all_plot_STN-i_parallelized.sh
# Description: Plots STN-i graphs from .RData files for all experiments and levels,
#              using multiple layout, visibility, and zoom configurations.
#              This version supports parallel execution up to MAX_JOBS at a time.
#
# Usage:
#   1. Make the script executable:
#        chmod +x run_all_plot_STN-i_parallelized.sh
#
#   2. Run the script:
#        ./run_all_plot_STN-i_parallelized.sh
#
#   Output:
#     - Multiple PDF plots per STN-i file
#     - Log file: Logs/run_plot_stn_parallelized_logs.log
# ==============================================================================

# Maximum number of parallel jobs
MAX_JOBS=4

LOG_DIR="./Logs"
mkdir -p "$LOG_DIR"
LOG_FILE="$LOG_DIR/run_plot_stn_parallelized_logs.log"
echo "=== STN-i plotting started at $(date) ===" > "$LOG_FILE"

layouts=("fr" "kk" "graphopt")

show_combinations=(
  "TRUE TRUE"
  "TRUE FALSE"
  "FALSE FALSE"
)

#zoom_levels=("NA" "0.25" "0.5" "0.75")

# Define zoom levels
# NA: No zoom, original size
# 0.25: 25% of the best nodes, 25% of the original size
zoom_levels=("NA" "0.25")

# Function to wait if too many background jobs are running
wait_for_jobs() {
  while (( $(jobs -rp | wc -l) >= MAX_JOBS )); do
    sleep 1
  done
}

# Function to execute plotting and log results
run_plot_rscript() {
  local args=("$@")
  local output_file=""
  for arg in "${args[@]}"; do
    [[ $arg == --output_file=* ]] && output_file="${arg#--output_file=}"
  done

  echo ">> Plotting: $output_file" | tee -a "$LOG_FILE"

  if ! Rscript R/plot_STN-i.R "${args[@]}" >> "$LOG_FILE" 2>&1; then
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

levels="L1 L2 L3"

# Loop over all combinations and run in parallel
for alg in "${!experiments[@]}"; do
  echo "=== Processing algorithm: $alg ===" | tee -a "$LOG_FILE"

  for exp in ${experiments[$alg]}; do
    for lvl in $levels; do
      input_path="Experiments/$alg/$exp/STNs-i/STN-i-$exp-$lvl.RData"
      output_dir="Experiments/$alg/$exp/Plots/$lvl"
      mkdir -p "$output_dir"

      for layout in "${layouts[@]}"; do
        for show_pair in "${show_combinations[@]}"; do
          read -r show_regular show_start_regular <<< "$show_pair"

          for zoom in "${zoom_levels[@]}"; do
            zoom_label="${zoom//./}"
            output_file="plotted-STN-i-$exp-$lvl-$layout-${show_regular:0:1}-${show_start_regular:0:1}-$zoom.pdf"

            wait_for_jobs
            run_plot_rscript --input="$input_path" \
              --output="$output_dir" \
              --output_file="$output_file" \
              --layout_type="$layout" \
              --show_regular="$show_regular" \
              --show_start_regular="$show_start_regular" \
              --palette=1 \
              --zoom_quantile="$zoom" &
          done
        done
      done
    done
  done
done

wait  # Wait for all background jobs to finish
echo "=== STN-i plotting finished at $(date) ===" >> "$LOG_FILE"
