#Matthias Ronnau
#Customer and Social Analytics-277
#Professor Dewan
#March 28, 2021

#Final Project

#Set Working Directory
setwd("~/The University of California, Irvine/Winter Quarter/Customer and Social Analytics-277/Final Project")

#Set options
options(scipen=999)

###Load Packages
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(igraph)

#Read the Data into R
g_subcomponent <- read_csv("subcomponent.csv")
#samp_size = ceiling(0.2 * nrow(g_subcomponent))
g_subcomponent_graph <- graph_from_data_frame(sample_frac(g_subcomponent, 0.01))
g_subcomponent <- graph_from_data_frame(g_subcomponent)

diameter(g_subcomponent)
diam <- get_diameter(g_subcomponent_graph)
diam
vcol <- rep("blue", vcount(g_subcomponent_graph))
vcol[diam] <- "red"
ecol <- rep("gray80", ecount(g_subcomponent_graph))
ecol[E(g_subcomponent_graph, path = diam)] <- "orange"
plot(g_subcomponent_graph, vertex.size = degree(g_subcomponent_graph) * 0.4, 
     vertex.color = vcol, vertex.label = NA, edge.arrow.size = 0.1,
     layout = layout.kamada.kawai)
