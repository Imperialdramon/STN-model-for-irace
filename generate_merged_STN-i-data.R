#########################################################################
# STN-i Merge Script
# Author: Pablo Estobar
#
# Description:
# This script merges multiple Search Trajectory Networks (STN-i) created from 
# independent irace executions of different algorithms into a single network.
# It supports the merging of at least 2 STN-i files stored in a folder, enriches
# the node and edge attributes, and classifies shared and elite nodes.
#
# Usage:
# Rscript merge_STN-i.R --input=<input_folder> --output=<output_folder> [--output_file=<output_file_name>]
#
# Arguments:
# --input       : Path to the folder containing 2 or more .RData files, each defining an STN-i object named 'STN-i'.
# --output      : Path to the folder where the merged file will be saved.
# --output_file : (Optional) Name for the output file (default is '<input_folder_name>-merged.RData').
#
# Behavior:
# - Loads each `.RData` file and extracts the `STN-i` graph object.
# - Assigns algorithm identifiers based on filenames (prefix before first underscore).
# - Merges the networks using `igraph::graph.union()`.
# - Consolidates vertex attributes (Fitness, Type, Quality, etc.) and 
#   assigns a unified classification (`Category`) to shared nodes:
#     - shared-regular
#     - shared-elite
#     - shared-mixed
#     - algorithm-elite
#     - algorithm-regular
# - Consolidates edge weights and algorithms.
# - Saves the merged network as `<input_folder>-merged.RData`.
#
# Requirements:
# - R with the following packages installed:
#     - igraph
#     - dplyr
#     - tidyr
#
# Notes:
# - The input folder must contain 2 or more `.RData` files.
# - Each `.RData` must define a graph object named `STN-i`.
# - The merged object is saved as `merged_STN_i` along with metadata (nruns, algn, bmin, best).
#########################################################################

# ---------- Validate required packages ----------
if (!requireNamespace("igraph", quietly = TRUE)) {
  stop("Error: The igraph package is not installed. Please install it with 'install.packages(\"igraph\")'", call. = FALSE)
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop("Error: The dplyr package is not installed. Please install it with 'install.packages(\"dplyr\")'", call. = FALSE)
}
if (!requireNamespace("tidyr", quietly = TRUE)) {
  stop("Error: The tidyr package is not installed. Please install it with 'install.packages(\"tidyr\")'", call. = FALSE)
}

# ---------- Load the required packages ----------
library(igraph)
library(dplyr)
library(tidyr)

# ---------- Load utility functions ----------
source("utils.R")

# ---------- Parse command line arguments ----------
parse_arguments <- function(args) {
  parsed <- list()
  for (arg in args) {
    if (grepl("^--", arg)) {
      parts <- strsplit(sub("^--", "", arg), "=")[[1]]
      if (length(parts) == 2) {
        parsed[[parts[1]]] <- parts[2]
      } else {
        stop(paste("Invalid argument format:", arg), call. = FALSE)
      }
    }
  }
  return(parsed)
}

args <- commandArgs(trailingOnly = TRUE)
params <- parse_arguments(args)

# ---------- Validate required arguments ----------
required_args <- c("input", "output")
for (param_name in required_args) {
  if (is.null(params[[param_name]])) {
    stop(paste("Missing required argument: --", param_name, sep = ""), call. = FALSE)
  }
}

# ---------- Assign and normalize paths ----------
input_folder <- normalizePath(params$input, mustWork = TRUE)
output_folder <- normalizePath(params$output, mustWork = TRUE)

# Check if input file exists
if (!dir.exists(input_folder)) {
  stop(paste("Input forlder does not exist:", input_folder), call. = FALSE)
}

# Check if the output folder exists, if not create it
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# ---------- Optional parameters ----------
if (!is.null(params$output_file)) {
  output_file_name <- params$output_file
} else {
  input_basename <- tools::file_path_sans_ext(basename(input_folder))
  output_file_name <- paste0(input_basename, "_merged_stn_i.RData")
}

# ---------- Process input files ----------

stns_i_data <- get_stns_i_data(input_folder)

merged_stn_i_result <- merge_stns_i_data(stns_i_data)


# TODO: Adjust the processing using the utility functions




# ---------- Save output file ----------

# Check if output file name has a valid extension
if (!grepl("\\.RData$", output_file_name)) {
  output_file_name <- paste0(output_file_name, ".RData")
}

# Construct the full output file path
output_file_path <- file.path(output_folder, output_file_name)

# Save the merged STN-i object
save_merged_stn_i_data(merged_stn_i_result, output_file_path)




# ----------------------------- Load STN-i Data ---------------------------



# ----------------------------- Prioritize Node Type ----------------------

prioritize_topology <- function(types) {
  types <- unique(types[types != ""])
  if ("END" %in% types) return("END")
  if ("START" %in% types) return("START")
  return("STANDARD")
}

# ----------------------------- Main Script -------------------------------


alg <- data$graphs
algn <- data$names
bmin <- data$bmin
num_alg <- length(alg)

stnm <- graph.union(alg)

# Fill missing attributes
for (i in 1:num_alg) {
  for (attr in c("Type", "Alg", "Quality", "Fitness", "Count")) {
    attr_name <- paste0(attr, "_", i)
    if (!attr_name %in% vertex_attr_names(stnm)) {
      V(stnm)[[attr_name]] <- NA
    }
  }
  for (attr in c("weight", "Alg")) {
    attr_name <- paste0(attr, "_", i)
    if (!attr_name %in% edge_attr_names(stnm)) {
      E(stnm)[[attr_name]] <- NA
    }
  }
}

# Assign unified attributes
type_matrix <- do.call(cbind, lapply(1:num_alg, function(i) V(stnm)[[paste0("Type_", i)]]))
V(stnm)$Type <- apply(type_matrix, 1, prioritize_topology)

V(stnm)$Fitness <- Reduce(coalesce, lapply(1:num_alg, function(i) V(stnm)[[paste0("Fitness_", i)]]))
V(stnm)$Count <- rowSums(do.call(cbind, lapply(1:num_alg, function(i) V(stnm)[[paste0("Count_", i)]])), na.rm = TRUE)

alg_df <- do.call(cbind, lapply(1:num_alg, function(i) V(stnm)[[paste0("Alg_", i)]]))
V(stnm)$Alg <- unite(as.data.frame(alg_df), "Alg", sep = "", remove = TRUE)$Alg

qual_df <- do.call(cbind, lapply(1:num_alg, function(i) V(stnm)[[paste0("Quality_", i)]]))
V(stnm)$Quality <- unite(as.data.frame(qual_df), "Quality", sep = "", remove = TRUE)$Quality

# Remove old vertex attributes
old_vattr <- unlist(lapply(1:num_alg, function(i) paste0(c("Fitness_", "Count_", "Type_", "Alg_", "Quality_"), i)))
for (a in old_vattr) stnm <- delete_vertex_attr(stnm, name = a)

# Edge weights
E(stnm)$weight <- rowSums(do.call(cbind, lapply(1:num_alg, function(i) E(stnm)[[paste0("weight_", i)]])), na.rm = TRUE)
alg_df_e <- do.call(cbind, lapply(1:num_alg, function(i) E(stnm)[[paste0("Alg_", i)]]))
E(stnm)$Alg <- unite(as.data.frame(alg_df_e), "Alg", sep = "", remove = TRUE)$Alg
old_eattr <- unlist(lapply(1:num_alg, function(i) paste0(c("weight_", "Alg_"), i)))
for (a in old_eattr) stnm <- delete_edge_attr(stnm, name = a)

# Shared node classification
V(stnm)$Shared <- TRUE
for (i in 1:num_alg) {
  V(stnm)[V(stnm)$Alg == algn[i]]$Shared <- FALSE
}

V(stnm)$Category <- NA
for (v in V(stnm)) {
  q_parts <- unlist(strsplit(V(stnm)[v]$Quality, split = ""))
  is_shared <- grepl(",", V(stnm)[v]$Alg)
  is_elite  <- grepl("ELITE", V(stnm)[v]$Quality)
  elite_count <- sum(grepl("ELITE", unlist(strsplit(V(stnm)[v]$Quality, split = ""))))
  if (is_shared && elite_count == 0) {
    V(stnm)[v]$Category <- "shared-regular"
  } else if (is_shared && elite_count == num_alg) {
    V(stnm)[v]$Category <- "shared-elite"
  } else if (is_shared && elite_count > 0 && elite_count < num_alg) {
    V(stnm)[v]$Category <- "shared-mixed"
  } else if (!is_shared && is_elite) {
    V(stnm)[v]$Category <- "algorithm-elite"
  } else {
    V(stnm)[v]$Category <- "algorithm-regular"
  }
}

# Save output
ofname <- paste0(infolder, "-merged.RData")
cat("Output file:", ofname, "\n")
save(stnm, num_alg, algn, bmin, file = ofname)