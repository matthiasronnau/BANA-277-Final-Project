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
library(dplyr)
library(igraph)
library(graphdata)
#devtools::install_github("matthiasronnau/graphdata", force = TRUE)
library(ggplot2)
library(ggcorrplot)

#Read the Data into R
data <- read_csv("Data/2020-Apr.csv")
ids <- read_csv("product_ids.csv")
g <- read_csv("subcomponent.csv")

set.seed(277)
samp <- sample_frac(data, 0.2)
rm(data)
subcomponent_data <- subset(samp, samp$product_id %in% ids$id)
rm(samp)
data_no_missing <- na.omit(subcomponent_data)
rm(subcomponent_data)
#purchases <- read_csv("purchases_no_missing.csv")

g_subcomponent <- graph_from_data_frame(g)

#Calculate the edge density
edge_density(g_subcomponent)

#Calculate the reciprocity
reciprocity(g_subcomponent)

#Calculate the closeness centrality
close <- as.data.frame(closeness(g_subcomponent, mode = "all"))
close$id <- as.numeric(rownames(close))
rownames(close) <- c()
colnames(close) <- c("closeness", "id")
head(close)

#Calculate the betweenness centrality
between <- as.data.frame(betweenness(g_subcomponent, directed = TRUE))
between$id <- as.numeric(rownames(between))
rownames(between) <- c()
colnames(between) <- c("betweenness", "id")
head(between)

#Calculate the hub score
hub <- as.data.frame(hub_score(g_subcomponent)$vector)
hub$id <- as.numeric(rownames(hub))
rownames(hub) <- c()
colnames(hub) <- c("hub_score", "id")
head(hub)

#Calculate the authority score
authority <- as.data.frame(authority_score(g_subcomponent)$vector)
authority$id <- as.numeric(rownames(authority))
rownames(authority) <- c()
colnames(authority) <- c("authority_score", "id")
head(authority)

#Combine the relevant stats into a single dataframe
network_stats <- inner_join(inner_join(inner_join(close, between, by = "id"), hub, by = "id"), authority, by = "id")
head(network_stats)

