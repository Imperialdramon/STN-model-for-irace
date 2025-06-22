#!/bin/bash

# ==============================================================================
# Script: run_all_plot_merged_STN-i.sh
# Description: Generates PDF plots for all merged STN-i graphs with layout,
#              visibility, and zoom configuration combinations.
#
# Usage:
#   chmod +x run_all_plot_merged_STN-i.sh
#   ./run_all_plot_merged_STN-i.sh
#
# Output:
#   - PDF plots in the corresponding Plots/ folder
#   - Log file: Logs/run_plot_merged_stn_logs.log
# ==============================================================================

# Create Logs directory
LOG_DIR="./Logs"
mkdir -p "$LOG_DIR"

# Log file path
LOG_FILE="$LOG_DIR/run_plot_merged_stn_logs.log"
echo "=== Merged STN-i plotting started at $(date) ===" > "$LOG_FILE"

# Layouts to use
layouts=("fr" "kk" "graphopt")

# Zoom quantile values
zoom_levels=("NA" "0.25" "0.5" "0.75")

# Boolean combinations for merged plotting
show_combinations=(
  "TRUE TRUE TRUE TRUE"
  "TRUE TRUE TRUE FALSE"
  "TRUE TRUE FALSE TRUE"
  "FALSE TRUE FALSE FALSE"
  "FALSE FALSE FALSE FALSE"
  "TRUE FALSE TRUE TRUE"
  "TRUE FALSE TRUE FALSE"
)

# Function to call plot_merged_STN-i.R with logging
run_plot_merged_rscript() {
  local args=("$@")
  local output_file=""
  for arg in "${args[@]}"; do
    [[ $arg == --output_file=* ]] && output_file="${arg#--output_file=}"
  done

  echo ">> Plotting: $output_file" | tee -a "$LOG_FILE"

  if ! Rscript R/plot_merged_STN-i.R "${args[@]}" >> "$LOG_FILE" 2>&1; then
    echo "❌ Error: Failed to generate $output_file" | tee -a "$LOG_FILE"
  else
    echo "✅ Success: Generated $output_file" | tee -a "$LOG_FILE"
  fi
}

# Define merged experiments
declare -A merged_experiments
merged_experiments["ACOTSP"]="Merged-E1-E2-2000 Merged-E3-E4-2000"
merged_experiments["MMASQAP"]="Merged-E1-E2-60 Merged-E3-E4-60"
merged_experiments["PSO-X"]="Merged-E1-E2-Mix Merged-E3-E4-Mix Merged-E5-E6-Mul Merged-E7-E8-Mul Merged-E9-E10-Uni Merged-E11-E12-Uni"

levels="L1 L2 L3"

# Loop through all combinations
for alg in "${!merged_experiments[@]}"; do
  echo "=== Processing algorithm: $alg ===" | tee -a "$LOG_FILE"

  for merged_case in ${merged_experiments[$alg]}; do
    merged_label="${merged_case#Merged-}"

    for lvl in $levels; do
      input_path="Experiments/$alg/$merged_case/Merged-STNs-i/merged-STN-i-$merged_label-$lvl.RData"
      output_dir="Experiments/$alg/$merged_case/Plots/$lvl"
      mkdir -p "$output_dir"

      for layout in "${layouts[@]}"; do
        for zoom in "${zoom_levels[@]}"; do
          for show_config in "${show_combinations[@]}"; do
            read -r shared_reg shared_mix reg start_reg <<< "$show_config"

            # Encode short version for filename
            short_flags="${shared_reg:0:1}-${shared_mix:0:1}-${reg:0:1}-${start_reg:0:1}"
            zoom_clean="${zoom//./}"  # Remove dot for file name if needed

            output_file="plotted-Merged-STN-i-$merged_label-$lvl-$layout-$short_flags-$zoom_clean.pdf"

            run_plot_merged_rscript \
              --input="$input_path" \
              --output="$output_dir" \
              --output_file="$output_file" \
              --layout_type="$layout" \
              --show_shared_regular="$shared_reg" \
              --show_shared_mixed="$shared_mix" \
              --show_regular="$reg" \
              --show_start_regular="$start_reg" \
              --palette=1 \
              --zoom_quantile="$zoom"
          done
        done
      done
    done
  done
done

echo "=== Merged STN-i plotting finished at $(date) ===" >> "$LOG_FILE"
