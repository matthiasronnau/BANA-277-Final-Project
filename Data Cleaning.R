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
data <- read_csv("Data/2020-Apr.csv")
head(data)

#Data Cleaning
unique(data$event_type)

purchases <- subset(data, data$event_type == "purchase")
rm(data)
purchases$event_time <- as_datetime(purchases$event_time)
head(purchases)

purchases_no_missing <- na.omit(purchases)

grouped <- group_by(purchases_no_missing, user_id)
head(grouped)
length(unique(grouped$user_id))
a <- summarize(grouped, nodes = list(product_id))
head(a)
a$nodes

a$num_nodes <- sapply(a$nodes, length)
a$unique <- sapply(sapply(a$nodes, unique), length)

c <- subset(a, a$num_nodes > 1 & a$unique > 1)
head(c)

verts <- c()
for(i in 1:nrow(c)){
  cat(round(i / nrow(c) * 100, 2), "%    \r")
  node_list <- c$nodes[[i]]
  nodes <- c(node_list[1])
  #print(node_list)
  len <- length(node_list)
  for(j in 2:len){
    node <- node_list[j]
    prior <- node_list[j - 1]
    if(node == prior){
      next
    } else if(j == len | (length(unique(node_list[j:len]))) == 1){
      nodes <- c(nodes, node)
    } else{
      nodes <- c(nodes, node, node)
    }
  }
  verts <- c(verts, nodes)
}

rm(i, j, len, node, nodes, num_nodes)

verts <- as.character(verts)
#verts
g = make_graph(verts)
degree(g, mode = "out")
degree(g, mode = "in")
print(g)
#plot(g)
graph <- as_data_frame(g)
head(graph)
colnames(graph) <- c("Source", "Target")
graph$Source <- as.double(graph$Source)
graph$Target <- as.double(graph$Target)

in_degree <- degree(g, mode = "in")
in_degree_dataframe <- as.data.frame(in_degree)
in_degree_dataframe$Source <- as.numeric(rownames(in_degree_dataframe))
rownames(in_degree_dataframe) <- c()
graph <- left_join(graph, in_degree_dataframe, by = "Source")
head(graph)

out_degree <- degree(g, mode = "out")
out_degree_dataframe <- as.data.frame(out_degree)
out_degree_dataframe$Source <- as.numeric(rownames(out_degree_dataframe))
rownames(out_degree_dataframe) <- c()
graph <- left_join(graph, out_degree_dataframe, by = "Source")
head(graph)

graph$Degree <- graph$in_degree + graph$out_degree
head(graph[order(- graph$Degree), ])

subcomponents <- subcomponent(g, v = "1005100", mode = "all")
g_subcomponent <- induced_subgraph(g, subcomponents)
item_subcomponent <- as_data_frame(g_subcomponent)
colnames(item_subcomponent) <- c("Source", "Target")

write.table(item_subcomponent, row.names = FALSE, col.names = c("Source", "Target"), sep = ",", file = "subcomponent.csv")


















