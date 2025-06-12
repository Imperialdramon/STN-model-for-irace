#' Parse command line arguments
#' 
#' This function parses command line arguments in the format `--key=value` and returns a named list of parameters.
#' 
#' @param args A character vector of command line arguments.
#' 
#' @return A named list where each key corresponds to an argument name and its value is the argument value.
#' 
#' @examples
#' \dontrun{
#' args <- c("--input=path/to/input.txt", "--output=path/to/output.txt")
#' parsed_args <- parse_arguments(args)
#' print(parsed_args)
#' }
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

#' Add an entry to the location trend
#' 
#' This function adds an entry to the location trend list, ensuring that the elite and type values are valid and accumulating origin values.
#' 
#' @param loc_trend A list representing the location trend.
#' @param location A string representing the location key.
#' @param elite_val A string representing the elite value (e.g., "ELITE", "REGULAR").
#' @param type_val A string representing the type value (e.g., "START", "STANDARD", "END").
#' @param origin_elite_val A string representing the origin elite value.
#' @param origin_type_val A string representing the origin type value.
#' 
#' @return A modified list with the new entry added or updated.
#' 
#' @examples
#' \dontrun{
#' loc_trend <- list()
#' loc_trend <- add_loc_entry(loc_trend, "Location1", "ELITE", "START", "REGULAR", "STANDARD")
#' }
add_loc_entry <- function(loc_trend, location, elite_val, type_val, origin_elite_val, origin_type_val) {
  location <- as.character(location)  # Asegura que la clave sea string

  # Validación opcional (puedes omitirla si estás seguro de que siempre son válidos)
  valid_elites <- c("ELITE", "REGULAR")
  valid_types  <- c("START", "STANDARD", "END")
  
  if (!(elite_val %in% valid_elites)) {
    warning(paste("Invalid elite value:", elite_val, "in node", location))
    elite_val <- "REGULAR"
  }
  
  if (!(type_val %in% valid_types)) {
    warning(paste("Invalid type value:", type_val, "in node", location))
    type_val <- "STANDARD"
  }

  if (!(origin_elite_val %in% valid_elites)) {
    warning(paste("Invalid origin elite value:", origin_elite_val, "in node", location))
    origin_elite_val <- "REGULAR"
  }

  if (!(origin_type_val %in% valid_types)) {
    warning(paste("Invalid origin type value:", origin_type_val, "in node", location))
    origin_type_val <- "STANDARD"
  }

  # If the location is not already in the list, initialize it
  if (!(location %in% names(loc_trend))) {
    loc_trend[[location]] <- list(
      Elite = elite_val,
      Type = type_val,
      Origin_Elite = c(origin_elite_val),
      Origin_Type  = c(origin_type_val)
    )
  } else {
    # Ensure that Elite and Type are defined
    if (is.null(loc_trend[[location]]$Elite)) loc_trend[[location]]$Elite <- elite_val
    if (is.null(loc_trend[[location]]$Type))  loc_trend[[location]]$Type  <- type_val

    # Accumulate the origin values
    loc_trend[[location]]$Origin_Elite <- c(loc_trend[[location]]$Origin_Elite, origin_elite_val)
    loc_trend[[location]]$Origin_Type  <- c(loc_trend[[location]]$Origin_Type, origin_type_val)
  }

  return(loc_trend)
}

#' Create a STN-i from a trace file
#' 
#' This function reads a trace file and creates a STN-i (Solution Trace Network) graph.
#' 
#' @param input_file A string specifying the path to the input trace file.
#' @param problem_type A string specifying the type of problem, either "min" for minimization or "max" for maximization. Default is "min".
#' @param best_known_solution A numeric value representing the best known solution for the problem. Default is NA, which means it will be calculated from the data.
#' 
#' @param number_of_runs An integer specifying the number of runs to consider from the trace file. Default is NA, which means it will use the maximum run number found in the data.
#' @param separator A string specifying the separator used in the input file. Default is "".
#' 
#' @return A list containing the STN-i graph, problem type, best known solution, number of runs, and summaries of elite and type origins.
#' 
#' @examples
#' \dontrun{
#' stn_i_result <- stn_i_create("path/to/trace_file.txt", problem_type = "min", best_known_solution = 0.5)
#' }
stn_i_create <- function(input_file, problem_type = "min", best_known_solution = NA, number_of_runs = NA, separator = "") {
  # Load the input file data
  trace_all <- read.table(input_file, header=T, sep = separator, colClasses=c("integer", "logical", "numeric", "character", "character", "character", "character", "character", "integer", "numeric", "character", "character", "character", "character", "character", "integer"), stringsAsFactors = F)

  # Check if the number of runs is specified, if not, use the maximum run number in the data
  if (is.na(number_of_runs)) {
    number_of_runs <- max(trace_all$Run, na.rm = TRUE)
  } else {
    if (number_of_runs < 1) {
      stop("number_of_runs must be a positive integer.", call. = FALSE)
    }
  }

  trace_all <- trace_all[trace_all$Run <= number_of_runs,]
  lnodes <- vector("list", number_of_runs)
  ledges <- vector("list", number_of_runs)
  loc_trend <- list() # Initialize an empty list to store location trends

  for (i in (1:number_of_runs)) {  # combine all runs in a single network
    trace <- trace_all[which(trace_all$Run==i),c(-1)] # take by run and remove first column run number
    colnames(trace) <- c("path",
                        "fit1", "node1", "elite1", "origin_elite1", "type1", "origin_type1", "iteration1",
                        "fit2", "node2", "elite2", "origin_elite2", "type2", "origin_type2", "iteration2")

    # Initialize lists to store nodes and edges for the current run
    lnodes_run <- list()

    for (j in 1:nrow(trace)) {
      # Add only the right node (node2) to the location trend
      # because it's to avoid over-representing the left node (node1) in the network
      loc_trend <- add_loc_entry(
        loc_trend,
        trace$node2[j],
        trace$elite2[j],
        trace$type2[j],
        trace$origin_elite2[j],
        trace$origin_type2[j]
      )
      lnodes_run[[length(lnodes_run) + 1]] <- data.frame(
        Node = trace$node2[j],
        Fitness = trace$fit2[j],
        Elite = trace$elite2[j],
        Type = trace$type2[j],
        stringsAsFactors = FALSE
      )
    }

    # Remove self loops with path information
    lnodes[[i]] <- do.call(rbind, lnodes_run)
    ledges[[i]] <- trace[trace$path == TRUE, c("node1", "node2")]
  }

  # Create a data frame for the location trends with the elite and type information
  loc_flat <- do.call(rbind, lapply(names(loc_trend), function(n) {
    data.frame(
    Node = n,
    Elite = loc_trend[[n]]$Elite,
    Type = loc_trend[[n]]$Type,
    Origin_Elite = loc_trend[[n]]$Origin_Elite,
    Origin_Type  = loc_trend[[n]]$Origin_Type,
    stringsAsFactors = FALSE
    )
  }))

  # Calculate global origin proportions for elite and type
  origin_elite_global <- prop.table(table(loc_flat$Origin_Elite))
  origin_type_global <- prop.table(table(loc_flat$Origin_Type))

  # Create summaries for elite and type by location
  elite_summary <- as.data.frame(table(Elite_Real = loc_flat$Elite, Origen = loc_flat$Origin_Elite))
  elite_summary <- elite_summary %>%
    group_by(Elite_Real) %>%
    mutate(Proporcion = round(Freq / sum(Freq), 4))

  # Create a summary for type by location
  type_summary <- as.data.frame(table(Type_Real = loc_flat$Type, Origen = loc_flat$Origin_Type))
  type_summary <- type_summary %>%
    group_by(Type_Real) %>%
    mutate(Proporcion = round(Freq / sum(Freq), 4))

  # Combine the list of nodes into one dataframe and
  # group by (Node, Fitness) to identify unique nodes and count them
  nodes <- ddply((do.call("rbind", lnodes)), .(Node, Fitness, Elite, Type), nrow) # Cambiar forma de contar nodos
  colnames(nodes) <- c("Node", "Fitness", "Elite", "Type", "Count")

  # Delete duplicates from nodes dataframe based on Node
  nodesu<- nodes[!duplicated(nodes$Node), ]

  # Combine the list of edges into one dataframe and
  # group by (node1, node2) to identify unique edges and count them
  edges <- ddply(do.call("rbind", ledges), .(node1, node2), nrow)
  colnames(edges) <- c("Start", "End", "weight")

  # Create STN_i graph and remove self loops
  STN_i <- graph_from_data_frame(d = edges, directed = T, vertices = nodesu)
  STN_i <- simplify(STN_i, remove.multiple = FALSE, remove.loops = TRUE)

  # Obtain the fitness values from graph
  fitness_vals <- V(STN_i)$Fitness

  # If best known solution is not provided, calculate it based on the problem type
  if (is.na(best_known_solution)) {
    best_known_solution <- if (problem_type == "min") {
      min(fitness_vals, na.rm = TRUE)
    } else {
      max(fitness_vals, na.rm = TRUE)
    }
  }

  # Obtain the topology ids
  standard_ids <- which(V(STN_i)$Type == "STANDARD")
  start_ids <- which(V(STN_i)$Type == "START")
  end_ids <- which(V(STN_i)$Type == "END")

  # Set the topology types: START, STANDARD and END
  V(STN_i)$Topology <- NA
  V(STN_i)[standard_ids]$Topology <- "STANDARD"
  V(STN_i)[start_ids]$Topology <- "START"
  V(STN_i)[end_ids]$Topology <- "END"

  # Obtain the quality ids (REGULAR, ELITE and BEST)
  regular_ids <- which(V(STN_i)$Elite == "REGULAR")
  elite_ids <- which(V(STN_i)$Elite == "ELITE")
  best_ids <- if (problem_type == "min") {
    which(fitness_vals <= best_known_solution)
  } else {
    which(fitness_vals >= best_known_solution)
  }

  # Set the quality types: REGULAR, ELITE and BEST
  V(STN_i)[regular_ids]$Quality <- "REGULAR"
  V(STN_i)[elite_ids]$Quality <- "ELITE"
  V(STN_i)[best_ids]$Quality <- "BEST"

  # Return all data without save
  return(list(
    STN_i = STN_i,
    problem_type = problem_type,
    best_known_solution = best_known_solution,
    number_of_runs = number_of_runs,
    origin_elite_global = origin_elite_global,
    origin_type_global = origin_type_global,
    elite_summary = elite_summary,
    type_summary = type_summary
  ))
}

#' Save the STN_i-i data to a R file
#'
#' This function saves the STN_i-i data to a specified output folder with a given file name.
#'
#' @param stn_i_result The result of the STN_i-i analysis, typically an object containing the STN_i graph and related data.
#' @param output_file_path A string specifying the path to the output folder where the RData file will be saved.
#'
#' @return None. The function writes the file to disk.
#'
#' @examples
#' \dontrun{
#' save_stn_i_data(stn_i_result, "output/", "custom_name.RData")
#' }
save_stn_i_data <- function(stn_i_result, output_file_path) {

  # Save the STN-i result as an RData file
  save(stn_i_result, file = output_file_path)

  cat("General STN-i summary:\n")

  cat("Problem Type:", stn_i_result$problem_type, "\n")
  cat("Number of Runs:", stn_i_result$number_of_runs, "\n")
  cat("Best Known Solution:", stn_i_result$best_known_solution, "\n")
  cat("Number of Nodes:", vcount(stn_i_result$STN_i), "\n")
  cat("Number of Edges:", ecount(stn_i_result$STN_i), "\n")
  cat("Number of Elite Nodes:", sum(V(stn_i_result$STN_i)$Quality == "ELITE"), "\n")
  cat("Number of Regular Nodes:", sum(V(stn_i_result$STN_i)$Quality == "REGULAR"), "\n")
  cat("Number of Best Nodes:", sum(V(stn_i_result$STN_i)$Quality == "BEST"), "\n")


  cat("\nSummary of the STN-i graph:\n")

  cat("Global summary elite origin:\n")
  print(round(stn_i_result$origin_elite_global, 4))
  cat("Global summary type origin:\n")
  print(round(stn_i_result$origin_type_global, 4))
  cat("\nSummary by location elite status:\n")
  print(stn_i_result$elite_summary)
  cat("\nSummary by location type:\n")
  print(stn_i_result$type_summary)

  # Print a message indicating where the file was saved
  message(paste("STN-i data saved to:", output_file_path))
}

#' Load STN-i data from a file
#' 
#' This function loads STN-i data from a specified input file and validates its structure.
#' 
#' @param input_file A string specifying the path to the input file containing the STN-i data.
#' 
#' @return A list containing the loaded STN-i data, including the graph, problem type, best known solution, number of runs, and summaries.
#' 
#' @examples
#' \dontrun{
#' stn_i_data <- get_stn_i_data("path/to/stn_i_file.RData")
#' }
get_stn_i_data <- function(input_file) {
  # Check if file exists
  if (!file.exists(input_file)) {
    stop(paste("Input file does not exist:", input_file), call. = FALSE)
  }

  # Load the object and retrieve its name
  loaded_name <- load(input_file)
  stn_i_result <- get(loaded_name)

  # Validate structure
  expected_fields <- c(
    "STN_i",
    "problem_type",
    "best_known_solution",
    "number_of_runs",
    "origin_elite_global",
    "origin_type_global",
    "elite_summary",
    "type_summary"
  )

  if (!is.list(stn_i_result) || !all(expected_fields %in% names(stn_i_result))) {
    stop("The loaded file does not contain a valid STN-i result structure.", call. = FALSE)
  }

  return(stn_i_result)
}

#' Get palette colors for STN-i visualization
#'
#' This function returns a list of colors for nodes and edges based on the specified palette option.
#'
#' @param palette A numeric value specifying the color palette option. Default is 1.
#'
#' @return A list containing node and edge colors for the specified palette.
#'
#' @examples
#' \dontrun{
#' colors <- get_stn_i_palette_colors(2)
#' print(colors)
#' }
get_stn_i_palette_colors <- function(palette = 1) {
  palette_colors <- switch(as.character(palette),
    "1" = list(
      node = list(regular = "gray70", elite = "orange", best = "red", shapes = "black"),
      edge = list(improving = "gray50",
                  equal     = rgb(0, 0, 250, max = 255, alpha = 180),
                  worsening = rgb(0, 250, 0, max = 255, alpha = 180))
    ),
    "2" = list(
      node = list(regular = "lightblue", elite = "blue", best = "darkblue", shapes = "black"),
      edge = list(improving = "gray50",
                  equal     = rgb(0, 0, 250, max = 255, alpha = 180),
                  worsening = rgb(0, 250, 0, max = 255, alpha = 180))
    ),
    "3" = list(
      node = list(regular = "lightgreen", elite = "green", best = "darkgreen", shapes = "black"),
      edge = list(improving = "gray50",
                  equal     = rgb(0, 0, 250, max = 255, alpha = 180),
                  worsening = rgb(0, 250, 0, max = 255, alpha = 180))
    ),
    "4" = list(
      node = list(regular = "lightpink", elite = "pink", best = "darkred", shapes = "black"),
      edge = list(improving = "gray50",
                  equal     = rgb(0, 0, 250, max = 255, alpha = 180),
                  worsening = rgb(0, 250, 0, max = 255, alpha = 180))
    ),
    "5" = list(
      node = list(regular = "lightgray", elite = "gray", best = "black", shapes = "black"),
      edge = list(improving = "gray50",
                  equal     = rgb(0, 0, 250, max = 255, alpha = 180),
                  worsening = rgb(0, 250, 0, max = 255, alpha = 180))
    ),
    stop("Invalid palette option. Choose from: 1, 2, 3, 4, 5.")
  )

  return(palette_colors)
}

#' Get layout data for a given STN-i graph
#'
#' This function retrieves layout data for a given STN-i graph object based on the specified layout type.
#'
#' @param STN_i A graph object of class igraph representing the STN-i.
#' @param layout A string specifying the layout type. Options include "fr" (Fruchterman-Reingold), "kk" (Kamada-Kawai), "circle", "grid", and "sphere". Default is "fr".
#'
#' @return A list containing the layout title, coordinates, and layout type.
#'
#' @examples
#' \dontrun{
#' layout_data <- get_stn_i_layout_data(STN_i, layout = "kk")
#' }
get_stn_i_layout_data <- function(STN_i, layout = "fr") {
  layout_data <- switch(layout,
    "fr" = list(
      title = "Fruchterman-Reingold Layout",
      coords = layout.fruchterman.reingold(STN_i),
      layout_type = "fr"
    ),
    "kk" = list(
      title = "Kamada-Kawai Layout",
      coords = layout.kamada.kawai(STN_i),
      layout_type = "kk"
    ),
    "circle" = list(
      title = "Circle Layout",
      coords = layout.circle(STN_i),
      layout_type = "circle"
    ),
    "grid" = list(
      title = "Grid Layout",
      coords = layout.grid(STN_i),
      layout_type = "grid"
    ),
    "sphere" = list(
      title = "Sphere Layout",
      coords = layout.sphere(STN_i),
      layout_type = "sphere"
    ),
    stop("Invalid layout option. Choose from: fr, kk, circle, grid, sphere.")
  )

  return(layout_data)
}

#' Decorate nodes and edges an STN for visualising a single algorithm STN
#' 
#' This function decorates the nodes and edges of an STN graph object for visualization purposes.
#' 
#' @param STN_i Graph object
#' @param problem_type Boolean indicating minimisation or not
#' @param show_regular Boolean indicating whether to show regular nodes
#' @param palette_colors List of colors for the different node and edge types
#' 
#' @return Decorated STN-i graph object
#' 
#' @examples
#' \dontrun{
#' STN_i <- stn_i_decorate(STN_i, problem_type = "min", show_regular = TRUE, palette_colors = get_stn_i_palette_colors(1))
#' }
stn_i_decorate <- function(STN_i, problem_type = "min", show_regular = TRUE, palette_colors) {
  # Optionally remove REGULAR nodes for visualization purposes
  if (!show_regular) {
    STN_i <- delete_vertices(STN_i, V(STN_i)[Quality == "REGULAR"])
  }

  # Retrieve edge list and corresponding fitness values
  edge_list <- as_edgelist(STN_i)
  node_fitness <- V(STN_i)$Fitness
  node_names <- V(STN_i)$name

  # Extract fitness of origin and destination nodes for each edge
  fitness_from <- node_fitness[match(edge_list[, 1], node_names)]
  fitness_to   <- node_fitness[match(edge_list[, 2], node_names)]

  # Assign edge types based on fitness comparison
  if (problem_type == "min") {
    E(STN_i)[fitness_to < fitness_from]$Type <- "IMPROVING"
    E(STN_i)[fitness_to > fitness_from]$Type <- "WORSENING"
  } else {
    E(STN_i)[fitness_to > fitness_from]$Type <- "IMPROVING"
    E(STN_i)[fitness_to < fitness_from]$Type <- "WORSENING"
  }
  E(STN_i)[fitness_to == fitness_from]$Type <- "EQUAL"

  # Assign edge colors based on type
  E(STN_i)$color[E(STN_i)$Type == "IMPROVING"] <- palette_colors$edge$improving
  E(STN_i)$color[E(STN_i)$Type == "WORSENING"] <- palette_colors$edge$worsening
  E(STN_i)$color[E(STN_i)$Type == "EQUAL"]     <- palette_colors$edge$equal

  # Set edge width proportional to number of visits (weight)
  E(STN_i)$width <- E(STN_i)$weight

  # Assign node shapes based on Topology type
  V(STN_i)[V(STN_i)$Topology == "STANDARD"]$shape <- "circle"
  V(STN_i)[V(STN_i)$Topology == "START"]   $shape <- "square"
  V(STN_i)[V(STN_i)$Topology == "END"]     $shape <- "triangle"

  # Assign node colors based on Quality
  V(STN_i)[V(STN_i)$Quality == "REGULAR"]$color <- palette_colors$node$regular
  V(STN_i)[V(STN_i)$Quality == "ELITE"]  $color <- palette_colors$node$elite
  V(STN_i)[V(STN_i)$Quality == "BEST"]   $color <- palette_colors$node$best

  # Set node size proportional to in-degree (number of incoming visits)
  V(STN_i)$size <- strength(STN_i, mode = "in") + 1

  # Slightly increase the size of BEST nodes for visibility
  V(STN_i)[V(STN_i)$Quality == "BEST"]$size <- V(STN_i)[V(STN_i)$Quality == "BEST"]$size + 0.5

  return(STN_i)
}

#' Function to create a plot for STN-i data
#'
#' This function processes the input STN-i file, decorates the STN object, and prepares layout data for plotting.
#'
#' @param input_file Path to the input STN-i file.
#' @param show_regular Boolean indicating whether to show regular nodes in the plot (default is TRUE).
#' @param palette_colors List of colors for the different node and edge types.
#'
#' @return A decorated STN-i graph object ready for plotting.
#'
#' @examples
#' \dontrun{
#' result <- stn_i_plot_create("path/to/stn_i_file.RData", show_regular = TRUE, palette_colors = get_stn_i_palette_colors(1))
#' }
stn_i_plot_create <- function(input_file, show_regular = TRUE, palette_colors) {

  # Load the STN-i data
  stn_i_result <- get_stn_i_data(input_file)

  # Obtain the STN object and problem type
  STN_i <- stn_i_result$STN_i
  problem_type <- stn_i_result$problem_type

  # Decorate the STN-i object
  STN_i <- stn_i_decorate(STN_i, problem_type, show_regular, palette_colors)

  # Return everything needed for external plotting
  return(STN_i)
}

#' Save a plot of the STN-i graph
#' 
#' This function saves a plot of the STN-i graph to a specified PDF file, applying custom shapes and colors for nodes and edges.
#' 
#' @param output_file_path A string specifying the path to the output PDF file.
#' @param STN_i A graph object of class igraph representing the STN-i.
#' @param layout_data A list containing layout information, including coordinates and title for the plot.
#' @param palette_colors A list of colors for nodes and edges, as returned by `get_stn_i_palette_colors()`.
#' @param nsizef A numeric factor to adjust node sizes (default is 1).
#' @param ewidthf A numeric factor to adjust edge widths (default is 0.5).
#' @param asize A numeric value for the arrow size of edges (default is 0.3).
#' @param ecurv A numeric value for the curvature of edges (default is 0.3).
#' 
#' @return None. The function saves the plot to the specified PDF file.
#' 
#' @examples
#' \dontrun{
#' save_stn_i_plot("output/stn_i_plot.pdf", STN_i, layout_data, palette_colors, nsizef = 1, ewidthf = 0.5, asize = 0.3, ecurv = 0.3)
#' }
save_stn_i_plot <- function(output_file_path, STN_i, layout_data, palette_colors, nsizef = 1, ewidthf = 0.5, asize = 0.3, ecurv = 0.3) {
  # Ensure the file name ends in .pdf
  if (!grepl("\\.pdf$", output_file_path)) {
    output_file_path <- paste0(output_file_path, ".pdf")
  }

  # Define triangle shape
  mytriangle <- function(coords, v = NULL, params) {
    vertex.color <- params("vertex", "color")
    if (length(vertex.color) != 1 && !is.null(v)) {
      vertex.color <- vertex.color[v]
    }
    vertex.size <- 1 / 200 * params("vertex", "size")
    if (length(vertex.size) != 1 && !is.null(v)) {
      vertex.size <- vertex.size[v]
    }
    symbols(x = coords[, 1], y = coords[, 2], bg = vertex.color, col = vertex.color,
            stars = cbind(vertex.size, vertex.size, vertex.size),
            add = TRUE, inches = FALSE)
  }
  # Register triangle as a custom shape (reuses circle clipping)
  add_shape("triangle", clip = shapes("circle")$clip, plot = mytriangle)

  # Define legend components based on the palette
  legend.txt <- c("Start", "Standard", "End", "Regular", "Elite", "Best", "Improve", "Equal", "Worse")
  legend.col <- c(
    palette_colors$node$shapes, palette_colors$node$shapes, palette_colors$node$shapes,
    palette_colors$node$regular, palette_colors$node$elite, palette_colors$node$best,
    palette_colors$edge$improving, palette_colors$edge$equal, palette_colors$edge$worsening
  )
  legend.shape <- c(15, 21, 17, 21, 21, 21, NA, NA, NA)
  legend.lty <- c(NA, NA, NA, NA, NA, NA, 1, 1, 1)

  # Open PDF device
  pdf(output_file_path)

  # Compute adjusted node sizes
  maxns <- max(V(STN_i)$size)
  if (maxns > 100) {
    nsize <- nsizef * sqrt(V(STN_i)$size) + 1
  } else if (maxns > 10) {
    nsize <- nsizef * 0.5 * V(STN_i)$size + 1
  } else {
    nsize <- nsizef * V(STN_i)$size
  }

  ewidth <- ewidthf * E(STN_i)$width
  title <- paste(layout_data$title, "Nodes:", vcount(STN_i), "Edges:", ecount(STN_i), "Comp:", components(STN_i)$no)

  # Plot graph
  plot(STN_i,
       layout = layout_data$coords,
       vertex.label = "",
       vertex.size = nsize,
       main = title,
       edge.width = ewidth,
       edge.arrow.size = asize,
       edge.curved = ecurv)

  # Add legend
  legend("topleft", legend.txt, pch = legend.shape, col = legend.col,
         pt.bg = legend.col, lty = legend.lty,
         cex = 0.7, pt.cex = 1.35, bty = "n")

  # Close PDF device
  dev.off()
}

#' Load and merge multiple STN-i data files from a specified folder.
#'
#' This function reads multiple `.RData` files containing STN-i graph objects,
#' merges them into a single graph, and extracts relevant metadata such as
#' algorithm names, problem type, best known solutions, and number of runs.
#'
#' @param input_folder The folder containing the `.RData` files.
#'
#' @return A list containing:
#' - `graphs`: A list of merged STN-i graph objects.
#' - `names`: Names of the algorithms extracted from the file names.
#' - `problem_type`: The type of problem (min or max) from the first file.
#' - `best_known_solutions`: Best known solutions from each file.
#' - `number_of_runs`: Number of runs for each algorithm.
#'
#' @examples
#' \dontrun{
#' input_folder <- "path/to/stn_i_data"
#' merged_data <- load_stn_i_data(input_folder)
#' }
get_stns_i_data <- function(input_folder) {
  # Check if the input folder contains at least 2 .RData files
  files <- list.files(input_folder, pattern = "\\.RData$", full.names = TRUE)
  if (length(files) < 2) {
    stop("At least 2 .RData files required to merge STN-i data.")
  }

  # Initialize lists to store graphs and metadata
  graphs <- list()
  names <- character(length(files))
  problem_types <- c()
  best_known_solutions <- c()
  number_of_runs_vec <- c()

  for (i in seq_along(files)) {
    input_file <- files[i]

    alg_name <- strsplit(basename(input_file), "_")[[1]][1]

    # Obtain the STN-i data from the file
    stn_i_result <- get_stn_i_data(input_file)

    STN_i <- stn_i_result$STN_i
    V(STN_i)$Alg <- alg_name
    E(STN_i)$Alg <- alg_name

    graphs[[i]] <- STN_i
    names[i] <- alg_name
    problem_types <- c(problem_types, stn_i_result$problem_type)
    best_known_solutions <- c(best_known_solutions, stn_i_result$best_known_solution)
    number_of_runs_vec <- c(number_of_runs_vec, stn_i_result$number_of_runs)
  }

  if (length(unique(problem_types)) != 1) {
    stop("All input STN-i files must have the same problem type.")
  }

  return(list(
    graphs = graphs,
    names = alg_names,
    problem_type = unique(problem_types),
    best_known_solutions = best_known_solutions,
    number_of_runs = number_of_runs_vec
  ))
}

# TODO: Add merged functions