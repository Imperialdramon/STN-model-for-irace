#########################################################################
# Based on the analysis of Search Trajectory Networks (STN), adapted here as STN-i for irace.
# Inspired by the work of Gabriela Ochoa, Katherine Malan, and Christian Blum.
# This version has been modified and extended for specific use with irace.
# Input: Folder containing trace files from executions, number of runs.
# Output: STN-i graph objects saved in the output folder.
#########################################################################

# ---------- Processing inputs from command line ----------

args = commandArgs(trailingOnly=TRUE)   # Take command line arguments
#  Test if there are two arguments if not, return an error
if (length(args) < 1) {
  stop("The first argument is required, arguments 2 to 4 are optional: \ 
    1) Name of the input folder 
    2) Boolean indicating minimisation (1) or maximisation (0). If no argument is given, minimisation (i.e 1) is assumed.
    3) The evaluation of the global optimum (or best-knwon solution). For continous optimisation a desired precision can be given. 
    If no argument is given, the best evaluation in the set of input files is used.
    4) The number of runs from the data files to be used. This should be a number between 1 up to total number of runs within in the raw data files. 
    If no argument is given, the largest run number in the input files is used."
    , call.=FALSE)
}

infolder <- args[1]

if (!dir.exists(infolder) ) {
  stop("Error: Input folder does not exist", call.=FALSE)
}

# Default values of parameters if not given in command line.
bmin <- 1
best <- NA   # Not given in command line, taken from data
nruns <-NA   # Not given om command line, taken from data

if (length(args) > 1){
  bmin <- as.integer(args[2])
  if (is.na(bmin)) {
    stop("Error: 2nd argument is not a number", call.=FALSE)
  }
}

if (length(args) > 2) {
  best <- as.numeric(args[3])
  if (is.na(best)) {
    stop("Error: 3rd argument is not a number", call.=FALSE)
  }
}

if (length(args) > 3){
  nruns <- as.integer(args[4]) 
  if (is.na(nruns)) {
    stop("Error: 4th argument is not a number", call.=FALSE)
  }
}

# Create out-folder folder to save STN objects  -- rule append "-stn" to input folder

outfolder <- paste0(infolder,"-stn")

if (!dir.exists(outfolder) ){
  dir.create(outfolder)
}
cat("Output folder: ", outfolder, "\n")

## Packages required
# igraph: tools handling graph objects
# plyr:   tools for Splitting, Applying and Combining Data
# dplyr:  tools for data manipulation and summarisation

packages = c("igraph", "plyr", "dplyr")

## If a package is installed, it will be loaded. If any are not, 
## the missing package(s) will be installed from CRAN and then loaded.

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# Function to add an entry to the location trend
add_loc_entry <- function(loc_trend, location, elite_val, type_val, origin_elite_val, origin_type_val) {
  location <- as.character(location)  # Asegura que la clave sea string

  # Validación opcional (puedes omitirla si estás seguro de que siempre son válidos)
  valid_elites <- c("ELITE", "REGULAR")
  valid_types  <- c("START", "STANDARD", "END")
  
  if (!(elite_val %in% valid_elites)) {
    warning(paste("Elite inválido:", elite_val, "en nodo", location))
    elite_val <- "REGULAR"  # valor por defecto
  }
  
  if (!(type_val %in% valid_types)) {
    warning(paste("Type inválido:", type_val, "en nodo", location))
    type_val <- "STANDARD"  # valor por defecto
  }

  if (!(origin_elite_val %in% valid_elites)) {
    warning(paste("Origin_Elite inválido:", origin_elite_val, "en nodo", location))
    origin_elite_val <- "REGULAR"
  }

  if (!(origin_type_val %in% valid_types)) {
    warning(paste("Origin_Type inválido:", origin_type_val, "en nodo", location))
    origin_type_val <- "STANDARD"
  }

  # Si el nodo no existe, lo inicializamos
  if (!(location %in% names(loc_trend))) {
    loc_trend[[location]] <- list(
      Elite = elite_val,
      Type = type_val,
      Origin_Elite = c(origin_elite_val),
      Origin_Type  = c(origin_type_val)
    )
  } else {
    # Asegurar que Elite y Type estén definidos
    if (is.null(loc_trend[[location]]$Elite)) loc_trend[[location]]$Elite <- elite_val
    if (is.null(loc_trend[[location]]$Type))  loc_trend[[location]]$Type  <- type_val

    # Acumular los valores de origen
    loc_trend[[location]]$Origin_Elite <- c(loc_trend[[location]]$Origin_Elite, origin_elite_val)
    loc_trend[[location]]$Origin_Type  <- c(loc_trend[[location]]$Origin_Type, origin_type_val)
  }

  return(loc_trend)
}

#-----------------------------------------------------------------------------------------------
# Function for Creating the STN of a given instance/algorithm 
# Read data from text input file and construct the STN network model
# Saves the STN oject in a file within the outfolder
# Returns numebr of nodes of the STN

stn_create <- function(instance)  {
  fname <- paste0(infolder,"/",instance)
  print(fname)
  file_ext <- substr(fname, nchar(fname)-3, nchar(fname))
  mysep <- ifelse(file_ext == ".csv", ",","")
  trace_all <- read.table(fname, header=T, sep = mysep, colClasses=c("integer", "logical", "numeric", "character", "character", "character", "character", "character", "integer", "numeric", "character", "character", "character", "character", "character", "integer"), stringsAsFactors = F)
  trace_all <- trace_all[trace_all$Run <= nruns,]
  lnodes <- vector("list", nruns)
  ledges <- vector("list", nruns)
  loc_trend <- list() # Initialize an empty list to store location trends

  for (i in (1:nruns)) {  # combine all runs in a single network
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

    # Remove auto-loops from the trace data
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

  cat("Global summary elite origin:\n")
  origin_elite_global <- prop.table(table(loc_flat$Origin_Elite))
  print(round(origin_elite_global, 4))

  cat("Global summary type origin:\n")
  origin_type_global <- prop.table(table(loc_flat$Origin_Type))
  print(round(origin_type_global, 4))

  cat("\nSummary by location elite status:\n")
  elite_summary <- as.data.frame(table(Elite_Real = loc_flat$Elite, Origen = loc_flat$Origin_Elite))
  elite_summary <- elite_summary %>%
    group_by(Elite_Real) %>%
    mutate(Proporcion = round(Freq / sum(Freq), 4))
  print(elite_summary)

  cat("\nSummary by location type:\n")
  type_summary <- as.data.frame(table(Type_Real = loc_flat$Type, Origen = loc_flat$Origin_Type))
  type_summary <- type_summary %>%
    group_by(Type_Real) %>%
    mutate(Proporcion = round(Freq / sum(Freq), 4))
  print(type_summary)

  print("Location trends:")
  #print(loc_trend)

  # combine the list of nodes into one dataframe and
  # group by (Node,Fitness) to identify unique nodes and count them
  nodes <- ddply((do.call("rbind", lnodes)), .(Node, Fitness, Elite, Type), nrow) # Cambiar forma de contar nodos
  colnames(nodes) <- c("Node", "Fitness", "Elite", "Type", "Count")

  # Delete duplicates from nodes dataframe based on Node
  nodesu<- nodes[!duplicated(nodes$Node), ]

  # combine the list of edges into one dataframe and
  # group by (node1,node2) to identify unique edges and count them
  edges <- ddply(do.call("rbind", ledges), .(node1, node2), nrow)
  colnames(edges) <- c("Start", "End", "weight")

  # Create STN graph
  STN <- graph_from_data_frame(d = edges, directed = T, vertices = nodesu)

  # Remove self loops
  STN <- simplify(STN, remove.multiple = FALSE, remove.loops = TRUE)

  # Obtain the start, standard and end nodes ids
  standard_ids <- which(V(STN)$Type == "STANDARD")
  start_ids <- which(V(STN)$Type == "START")
  end_ids <- which(V(STN)$Type == "END")

  # Three types of topology: START, END and STANDARD
  V(STN)$Topology <- NA
  V(STN)[standard_ids]$Topology <- "STANDARD"
  V(STN)[start_ids]$Topology <- "START"
  V(STN)[end_ids]$Topology <- "END"

  # Obtain the elite and regular nodes ids
  regular_ids <- which(V(STN)$Elite == "REGULAR")
  elite_ids <- which(V(STN)$Elite == "ELITE")
  # If it's a minimisation problem, take the best minimum nodes nodes
  if (bmin) {
    best_ids <- which(V(STN)$Fitness <= best)
  }
  # If it's a maximisation problem, take the best maximum nodes nodes
  else { 
    best_ids <- which(V(STN)$Fitness >= best)
  }

  # Three types of quality: Elite, Best and Regular
  V(STN)[regular_ids]$Quality <- "REGULAR"
  V(STN)[elite_ids]$Quality <- "ELITE"
  V(STN)[best_ids]$Quality <- "BEST"

  # Removes (last 4 characters, .ext) from file to use as name
  fname <- gsub('.{4}$', '', instance)
  fname <- paste0(outfolder,"/",fname,"_stn.RData")

  # Store STN, wether it is a minimisation problem and the best-known given
  save(STN,nruns, bmin, best, origin_elite_global, origin_type_global, elite_summary, type_summary, file=fname)
  return(vcount(STN))
}

#--------------------------------------------------------------------------------
# Extracts the required data fro the input file
# Input:  String with name of file
# Output: Data frame with trace data, 2) name of output file .Rdata
get_data <- function(instance) {
  file_ext <- substr(instance, nchar(instance)-3, nchar(instance))
  mysep <- ifelse(file_ext == ".csv", ",", "")
  # TODO: Agregar las nuevas columnas
  trd <- read.table(paste0(infolder,"/",instance), header=T, sep = mysep, colClasses=c("integer", "logical", "numeric", "character", "character", "character", "character", "character", "integer", "numeric", "character", "character", "character", "character", "character", "integer"), stringsAsFactors = F)
  return (trd)
}

# ---- Process all data files in the given input folder ----------------
data_files <- list.files(infolder)

# This is only executed if the nruns or best parameters are not given
if (is.na(best) | is.na(nruns))  {
  dfs <- lapply(data_files, get_data)  # Store data in a list, so is an argument to create function 
  # If best its not given, determine it from all files in the folder
  if (is.na(best)) {
    l <- lapply(dfs, function(x) {x[c("Fitness2")]})  # Extract Fitness
    v <- unlist(l, recursive = T)   # Take all the fitness values
    best <- ifelse(bmin, min(v), max(v))
    cat("Best value in data:", best, "\n")
  }

  # If nruns its not given, determine it from all files 
  if (is.na(nruns)) {
    l <- lapply(dfs, function(x) {x[c("Run")]})  # Extract Runs
    nruns <- max(unlist(l, recursive = T))
    cat("Number of runs in data:", nruns, "\n")
  }
  remove(dfs)
} 

# Applies stn_create function to all files
nsizes <- lapply(data_files, stn_create)
print("Number of nodes in the STNs created:")
print(as.numeric(nsizes))
