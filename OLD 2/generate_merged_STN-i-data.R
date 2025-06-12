
#########################################################################
# Network Analysis of Search Trajectory Networks (STN)
# Authors: Gabriela Ochoa, Katherine Malan, Christian Blum
# Date: May 2021 (modificado en 2025)
# Construction of merged STN network of several algorithms (STN-i version)
# Adaptado para priorización de nodos por tipo (END > START > STANDARD)
#########################################################################

# ---------- Processing input from command line ----------

args = commandArgs(trailingOnly=TRUE)
if (length(args) < 1) {
   stop("One argument is required: the input folder with STNs files to merge.", call.=FALSE)
}
infolder <- args[1]
if (!dir.exists(infolder)) {
  stop("Input folder does not exist", call.=FALSE)
}

packages = c("igraph", "dplyr", "tidyr")
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      suppressWarnings(library(x, character.only = TRUE))
    }
  }
)

data_files <- list.files(infolder)
cat("Input folder: ", infolder, "\n")
num_alg <- length(data_files)
if (num_alg < 2 || num_alg > 3) {
  stop("Number of algorithms to merge can only be 2 or 3", call.=FALSE)
}

alg <- vector(mode = "list", length = num_alg)
algn <- vector(mode = "character", length = num_alg)
i <- 1
for (f in data_files) {
  alg_name <- strsplit(f,"_")[[1]][1]
  print(alg_name)
  fname <- paste0(infolder,"/",f)
  load(fname, verbose = F)
  V(STN)$Alg <- alg_name
  E(STN)$Alg <- alg_name
  algn[i] <- alg_name
  alg[[i]] <- STN
  i <- i + 1
}

stnm <- graph.union(alg)

# Preparar y combinar atributos
V(stnm)$Type_1 <- replace_na(V(stnm)$Type_1, "")
V(stnm)$Type_2 <- replace_na(V(stnm)$Type_2, "")
V(stnm)$Alg_1 <- replace_na(V(stnm)$Alg_1, "")
V(stnm)$Alg_2 <- replace_na(V(stnm)$Alg_2, "")
V(stnm)$Quality_1 <- replace_na(V(stnm)$Quality_1, "")
V(stnm)$Quality_2 <- replace_na(V(stnm)$Quality_2, "")

if (num_alg == 3) {
  V(stnm)$Type_3 <- replace_na(V(stnm)$Type_3, "")
  V(stnm)$Alg_3 <- replace_na(V(stnm)$Alg_3, "")
  V(stnm)$Quality_3 <- replace_na(V(stnm)$Quality_3, "")
}

# Asignar Type según prioridad END > START > STANDARD
type_matrix <- if (num_alg == 3) {
  cbind(V(stnm)$Type_1, V(stnm)$Type_2, V(stnm)$Type_3)
} else {
  cbind(V(stnm)$Type_1, V(stnm)$Type_2)
}
prioritize_type <- function(types) {
  types <- unique(types[types != ""])
  if ("END" %in% types) return("END")
  if ("START" %in% types) return("START")
  return("STANDARD")
}
V(stnm)$Type <- apply(type_matrix, 1, prioritize_type)

# Fitness y Count
if (num_alg == 2) {
  V(stnm)$Fitness <- coalesce(V(stnm)$Fitness_1, V(stnm)$Fitness_2)
  V(stnm)$Count <-  rowSums(cbind(V(stnm)$Count_1, V(stnm)$Count_2), na.rm=TRUE)
} else {
  V(stnm)$Fitness <- coalesce(V(stnm)$Fitness_1, V(stnm)$Fitness_2, V(stnm)$Fitness_3)
  V(stnm)$Count <-  rowSums(cbind(V(stnm)$Count_1, V(stnm)$Count_2, V(stnm)$Count_3), na.rm=TRUE)
}

# Concatenar Alg
dfa <- if (num_alg == 3) {
  data.frame(V(stnm)$Alg_1, V(stnm)$Alg_2, V(stnm)$Alg_3)
} else {
  data.frame(V(stnm)$Alg_1, V(stnm)$Alg_2)
}
dfa <- unite(dfa, "Alg", remove = TRUE, sep = "")
V(stnm)$Alg <- as.vector(dfa$Alg)

# Concatenar Quality
dfe <- if (num_alg == 3) {
  data.frame(V(stnm)$Quality_1, V(stnm)$Quality_2, V(stnm)$Quality_3)
} else {
  data.frame(V(stnm)$Quality_1, V(stnm)$Quality_2)
}
dfe <- unite(dfe, "Quality", remove = TRUE, sep = "")
V(stnm)$Quality <- as.vector(dfe$Quality)

# Eliminar atributos antiguos
old_vattr = c("Fitness_1", "Fitness_2", "Count_1", "Count_2",
              "Type_1", "Type_2", "Alg_1", "Alg_2", "Quality_1", "Quality_2")
if (num_alg == 3) {
  old_vattr = c(old_vattr, "Fitness_3", "Count_3", "Alg_3", "Type_3", "Quality_3")
}
for (i in old_vattr) {
  stnm <- delete_vertex_attr(stnm, name = i)
}

# Edges: combinar peso y algoritmos
if (num_alg == 2) {
  E(stnm)$weight <- rowSums(cbind(E(stnm)$weight_1, E(stnm)$weight_2), na.rm=TRUE)
} else {
  E(stnm)$weight <- rowSums(cbind(E(stnm)$weight_1, E(stnm)$weight_2, E(stnm)$weight_3), na.rm=TRUE)
}

E(stnm)$Alg_1 <- replace_na(E(stnm)$Alg_1, "")
E(stnm)$Alg_2 <- replace_na(E(stnm)$Alg_2, "")
if (num_alg == 3) {
  E(stnm)$Alg_3 <- replace_na(E(stnm)$Alg_3, "")
  dfa <- data.frame(E(stnm)$Alg_1, E(stnm)$Alg_2, E(stnm)$Alg_3)
} else {
  dfa <- data.frame(E(stnm)$Alg_1, E(stnm)$Alg_2)
}
dfa <- unite(dfa, "Alg", remove = TRUE, sep = "")
E(stnm)$Alg <- as.vector(dfa$Alg)

old_eattr = c("weight_1", "weight_2", "Alg_1", "Alg_2")
if (num_alg == 3) {
  old_eattr = c(old_eattr, "weight_3", "Alg_3")
}
for (i in old_eattr) {
  stnm <- delete_edge_attr(stnm, name = i)
}

# Shared nodes
V(stnm)$Shared <- TRUE
for (i in 1:num_alg) {
  V(stnm)[V(stnm)$Alg == algn[i]]$Shared <- FALSE
}

# Clasificación extendida de nodos compartidos
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

# Save
ofname <- paste0(infolder,"-merged.RData")
cat("Output file: ", ofname, "\n")
save(stnm, nruns, num_alg, algn, bmin, best, file=ofname)
