#########################################################################
# Network Analysis Search Trajectory Networks
# Author: Gabriela Ochoa
# Date: May 2021
# STN Visualisation, Includes decoration
# Plots for single algorithm, as opposed to merging several algorithms
# Input:  Folder with STN graph objects for a single algorithm
# Output: Network plots (pdf) saved in folder
#########################################################################

# ---------- Processing inputs from command line ----------
args = commandArgs(trailingOnly=TRUE)   # Take command line arguments
if (length(args) < 1) { #  Test if there are two arguments if not, return an error
  stop("One rgument is required: the input folder with stn objects. \\
        A 2nd argument can be given, a size numeric factor for nodes and edges", call.=FALSE)
}
infolder <- args[1]

if (!dir.exists(infolder) ){
  stop("Input folder does not exist", call.=FALSE)
}

# Create outfolder folder to save STN objects  -- rule append "-plot" to input folder
outfolder <- paste0(infolder,"-plot")

if (!dir.exists(outfolder) ){
  dir.create(outfolder)
}
cat("Output folder: ", outfolder, "\n")

if (length(args) > 1) {
  size_factor <- as.integer(args[2])
} else {
  size_factor <- 1
}

if (is.na(size_factor)) {
  stop("2nd argument is not a number", call.=FALSE)
}

if (!require("igraph", character.only = TRUE)) {
  install.packages("igraph", dependencies = TRUE)
  library("igraph", character.only = TRUE)
}

# Functions for visualisation, and Default Colours -------------------------------- 

# Node Colors
# STNs model for irace has 3 types of quality nodes: best, regular, elite
best_ncol  <- "red"
regular_ncol <- "gray70"
elite_ncol <- "orange"
shapes_ncol <- "black"

# Edge Colors
# STNs  model has 3 types of perturbation edges: 3 Types: (i)improvement, (e)equal, (w)worsening
# alpha is for transparency: (as an opacity, 0 means fully transparent,  max (255) opaque)
impru_ecol <- "gray50"
equal_ecol <- rgb(0,0,250, max = 255, alpha = 180)  # transparent blue for worsening edges
worse_ecol <- rgb(0,250,0, max = 255, alpha = 180)  # transparent green worsening edges

#################################################################
# Triangle vertex shape: because igraph does not have a native
# triangle shape, a function is provided to have a triangle shape
mytriangle <- function(coords, v=NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size <- 1/200 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  symbols(x=coords[,1], y=coords[,2], bg=vertex.color, col = vertex.color,
          stars=cbind(vertex.size, vertex.size, vertex.size),
          add=TRUE, inches=FALSE)
}
# clips as a circle
add_shape("triangle", clip=shapes("circle")$clip, plot=mytriangle)

# Legends for the plot
legend.txt <- c("Start", "Standar", "End", "Regular", "Elite", "Best", "Improve", "Equal", "Worse")
# Colors for legend
legend.col <- c(shapes_ncol, shapes_ncol, shapes_ncol, regular_ncol, elite_ncol, best_ncol, impru_ecol, equal_ecol, worse_ecol)
legend.shape <- c(15,21,17,21,21,21,NA,NA,NA)
# Line style, NA for nodes, solid line for edges
legend.lty <-  c(NA,NA,NA,NA,NA,NA,1,1,1)

# Plot Networks 
# N: Graph objec
# tit: type of 
# ewidthf: factor for vector with 
# asize: arrow size for plots
# nsizef: factor to multiply for node sizes
# ecurv: curvature of the edges (0 = non, 1 = max)
# mylay: graph layout as a parameter, as differet situatuons require diff. layuts

plotNet <-function(N, tit, nsizef, ewidthf, asize, ecurv, mylay) 
{
  maxns <-  max(V(N)$size)
  if (maxns  > 100) {   # for very large nodes use sqrt for scaling size
    nsize <-  nsizef * sqrt(V(N)$size)  + 1
  } else {
    if (maxns  > 10) {   # for large nodes, use half of size
      nsize <- nsizef * 0.5*V(N)$size   + 1
    } else {
      nsize <-  nsizef * V(N)$size
    }  
  }  
  ewidth <- ewidthf * E(N)$width
  title <- paste(tit,'Nodes:',vcount(N), 'Edges:',ecount(N), 'Comp:', components(N)$no)
  print(title)
  plot(N, layout = mylay, vertex.label = "", vertex.size = nsize, main = title,
       edge.width = ewidth, edge.arrow.size = asize, edge.curved = ecurv)
  legend("topleft", legend.txt, pch = legend.shape, col = legend.col, 
         pt.bg=legend.col, lty = legend.lty,
         cex = 0.7, pt.cex=1.35, bty = "n")
}

# Decorate nodes and edges an STN for visualising a single algorithm STN
# N: Graph object
# bmin: Boolean indicating minimisation or not

stn_decorate <- function(N, bmin)  {
  el<-as_edgelist(N)
  fits<-V(N)$Fitness
  names<-V(N)$name
  ## get the fitness values at each endpoint of an edge
  f1<-fits[match(el[,1],names)]
  f2<-fits[match(el[,2],names)]
  if (bmin) {  # minimisation problem 
    E(N)[which(f2<f1)]$Type = "improving"   # improving edges - Minimisation
    E(N)[which(f2>f1)]$Type = "worsening"   # worsening edges - Minimisation
  } else {
    E(N)[which(f2>f1)]$Type = "improving"   # improving edges - Maximisation
    E(N)[which(f2<f1)]$Type = "worsening"   # worsening edges - Maximisation
  }
  E(N)[which(f2==f1)]$Type = "equal"  # equal fitness edges
  
  # Coloring nodes and edges. Also give size to nodes
  E(N)$color[E(N)$Type=="improving"] = impru_ecol
  E(N)$color[E(N)$Type=="equal"] = equal_ecol
  E(N)$color[E(N)$Type=="worsening"] = worse_ecol
  
  # width of edges proportional to weight - times visited
  E(N)$width <- E(N)$weight
  
  # Shape of nodes
  V(N)[V(N)$Type == "standard"]$shape = "circle"  # circle is the default shape
  V(N)[V(N)$Type == "start"]$shape = "square"  # Square for start nodes
  V(N)[V(N)$Type == "end"]$shape = "triangle"  # Triangle for start nodes

  # Color of Nodes
  V(N)[V(N)$Quality == "regular"]$color = regular_ncol  # Color of regular nodes
  V(N)[V(N)$Quality == "elite"]$color = elite_ncol  # Color of elite nodes
  V(N)[V(N)$Quality == "best"]$color = best_ncol   # Color of best nodes
  
  # Frame colors are the same as node colors. White  frame for best nodes to highlight them
  #V(N)$frame.color <- V(N)$color
  #V(N)[V(N)$Type == "best"]$frame.color <- "white"
  #V(N)[V(N)$Elite == "T"]$frame.color <- "orange"
  
  # Size of Nodes Proportional to  incoming degree, 
  V(N)$size <- strength(N, mode="in") + 1   # nodes with strength 0 have at least size 1
  V(N)[V(N)$Quality == "best"]$size = V(N)[V(N)$Quality == "best"]$size + 0.5 # Increase a bit size of best node
  
  return(N)
}

stn_plot <- function(inst)  {
  print(inst)
  fname <- paste0(infolder,"/",inst)
  load(fname, verbose = F)
  STN <- stn_decorate(STN, bmin)
  lkk <-layout.kamada.kawai(STN)
  lfr <-layout.fruchterman.reingold(STN)
  
  fname <-  gsub('.{5}$', '', inst) # removes  (last 5 characters, RData) from file to use as name
  fname <- paste0(outfolder,"/",fname,"pdf")
  pdf(fname) 
  print(fname)
  plotNet(N = STN, tit="FR Layout.", nsizef=size_factor, ewidthf=size_factor *.5, asize=0.3, ecurv=0.3, mylay=lfr)
  plotNet(N = STN, tit="KK Layout.", nsizef=size_factor * 0.8, ewidthf=size_factor *.4, asize=0.2, ecurv=0.3, mylay=lkk)
  dev.off()
  return(vcount(STN))
}

# ---- Process all datasets in the given inpath folder ----------------
dataf <- list.files(infolder)
print(infolder)
print(dataf)
nsizes <- lapply(dataf, stn_plot)  # plot all STNs in folder, return node sizes
print("Numer of nodes in STNs ploted:")
print(as.numeric(nsizes))
