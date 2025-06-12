#########################################################################
# Network Analysis of Search Trajectory Networks (STN)
# Authors: Gabriela Ochoa, Katherine Malan, Christian Blum
# Adapted to support merged STN-i visualization by Pablo Estobar
# Input:  File name with merged STN graph object (RData file)
# Output: Network plots (pdf) saved in current folder
#########################################################################

library(igraph)

# ---------- Processing inputs ----------
args = commandArgs(trailingOnly=TRUE)
if (length(args) < 1) stop("Missing input merged STN .RData file")
infile <- args[1]
if (!file.exists(infile)) stop("Input file does not exist")

size_factor <- ifelse(length(args) > 1, as.integer(args[2]), 1)
if (is.na(size_factor)) stop("2nd argument must be numeric")

# Colors & Styles
start_ncol     <- "gold"
end_run_ncol   <- "gray30"
best_ncol      <- "red"
shared_col     <- "black"
shared_elite   <- "green"
shared_mixed <- "purple"

# Base palette for algorithms
alg_base_col <- c("#fc8d62", "#377eb8", "#4daf4a", "#984ea3", "#a65628")

# Load merged STN
load(infile, verbose = F)
stnm <- simplify(stnm, remove.multiple = FALSE, remove.loops = TRUE)

# Considerar el caso donde no todos los algoritmos comparten nodos elite y son nodos shared-mixed nodes

# Categorize nodes according to merged STN-i model
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

# Prepare dual color palette (regular/elite) per algorithm
alg_colors <- list()
for (i in 1:length(algn)) {
  base <- alg_base_col[i]
  elite <- adjustcolor(base, alpha.f = 0.6)
  alg_colors[[algn[i]]] <- list(
    elite = elite,
    regular = base
  )
}

# Triangle shape for END
mytriangle <- function(coords, v=NULL, params) {
  vertex.color <- params("vertex", "color")
  vertex.size  <- 1/200 * params("vertex", "size")
  if (length(vertex.color) != 1 && !is.null(v)) vertex.color <- vertex.color[v]
  if (length(vertex.size) != 1 && !is.null(v)) vertex.size <- vertex.size[v]
  symbols(x=coords[,1], y=coords[,2], bg=vertex.color, col=vertex.color,
          stars=cbind(vertex.size, vertex.size, vertex.size), add=TRUE, inches=FALSE)
}
add_shape("triangle", clip=shapes("circle")$clip, plot=mytriangle)

# Decoration function
stn_decorate <- function(N) {
  V(N)$color <- "gray80"

  # Categoría compartida
  V(N)[V(N)$Category == "shared-mixed"]$color <- shared_mixed
  V(N)[V(N)$Category == "shared-regular"]$color <- shared_col
  V(N)[V(N)$Category == "shared-elite"]$color   <- shared_elite

  # Algoritmos exclusivos
  for (alg in names(alg_colors)) {
    elite_idx <- which(V(N)$Category == "algorithm-elite" & V(N)$Alg == alg)
    reg_idx   <- which(V(N)$Category == "algorithm-regular" & V(N)$Alg == alg)
    V(N)[elite_idx]$color  <- alg_colors[[alg]]$elite
    V(N)[reg_idx]$color    <- alg_colors[[alg]]$regular
  }

  # Formas topológicas
  V(N)$shape <- "circle"
  V(N)[V(N)$Type == "START"]$shape <- "square"
  V(N)[V(N)$Type == "END"]$shape   <- "triangle"

  # Tamaño proporcional a fuerza entrante
  V(N)$size <- strength(N, mode="in") + 1
  V(N)[V(N)$Quality == "BEST"]$size <- V(N)[V(N)$Quality == "BEST"]$size + 1
  V(N)[V(N)$Type == "END"]$size     <- V(N)[V(N)$Type == "END"]$size + 0.3

  #V(N)[V(N)$Category == "shared-mixed"]$size <- 2

  # Bordes
  V(N)$frame.color <- V(N)$color
  V(N)[V(N)$Quality == "BEST"]$frame.color <- "white"

  # Aristas
  E(N)$color <- shared_col
  for (i in 1:num_alg) {
    E(N)[E(N)$Alg == algn[i]]$color <- alg_base_col[i]
  }
  E(N)$width <- E(N)$weight

  return(N)
}

# Leyenda dinámica
legend.txt <- c("Start", "Standard", "End", "Best")
legend.col <- c("black", "black", "black", best_ncol)
legend.shape <- c(15, 16, 17, 16)

# Agrega algoritmo-regular / algoritmo-elite
for (i in 1:num_alg) {
  legend.txt   <- c(legend.txt, paste0(algn[i], "-regular"), paste0(algn[i], "-elite"))
  legend.col   <- c(legend.col, alg_colors[[algn[i]]]$regular, alg_colors[[algn[i]]]$elite)
  legend.shape <- c(legend.shape, 16, 16)
}

# Agrega compartidos
legend.txt   <- c(legend.txt, "Shared-Regular", "Shared-Elite", "Shared-Mixed")
legend.col   <- c(legend.col, shared_col, shared_elite, shared_mixed)
legend.shape <- c(legend.shape, 16, 16, 16)

# Plotting function
plotNet <- function(N, tit, nsizef, ewidthf, asize, ecurv, mylay, bleg = T) {
  nsize <- nsizef * V(N)$size
  ewidth <- ewidthf * E(N)$width
  plot(N, layout = mylay, vertex.label = "", vertex.size = nsize,
       edge.width = ewidth, main = tit,
       edge.arrow.size = asize, edge.curved = ecurv)
  if (bleg) {
    legend("topleft", legend.txt, pch = legend.shape, col = legend.col,
           cex = 0.7, pt.cex=1.4, bty = "n")
  }
}

# Decorar y plotear
stnm <- stn_decorate(stnm)
lfr <- layout_with_fr(stnm)
lkk <- layout_with_kk(stnm)

ofname <- paste0(gsub('.{6}$', '', infile), "-plot.pdf")
pdf(ofname)

nf <- sqrt(sqrt(size_factor) * 0.3)
ef <- sqrt(size_factor) * 0.5

plotNet(stnm, tit="FR layout", nsizef=0.4, ewidthf=ef*0.1, asize=0.08, ecurv=0.15, mylay=lfr)
plotNet(stnm, tit="KK layout", nsizef=0.5, ewidthf=ef*0.1, asize=0.08, ecurv=0.15, mylay=lkk)

dev.off()
cat("Merged STN number of nodes:\n")
print(vcount(stnm))
