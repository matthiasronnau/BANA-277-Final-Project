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
#data$event_time <- as_datetime(data$event_time)
unique(data$event_type)

purchases <- subset(data, data$event_type == "purchase")
rm(data)
purchases$event_time <- as_datetime(purchases$event_time)
head(purchases)

purchases_no_missing <- na.omit(purchases)
#purchases_no_missing$event_time <- as_datetime(purchases_no_missing$event_time)

grouped <- group_by(purchases_no_missing, user_id)
head(grouped)
length(unique(grouped$user_id))
a <- summarize(grouped, nodes = list(product_id))
head(a)
a$nodes

num_nodes <- sapply(a$nodes, length)
a$num_nodes <- num_nodes

c <- subset(a, a$num_nodes > 1)

#b <- a[1:100, ]
#head(b, 15)

#ifelse(length(a$nodes) > 1, TRUE, FALSE)

verts <- c()
for(i in 1:nrow(c)){
  cat(round(i / nrow(c) * 100, 2), "%    \r")
  nodes <- c()
  len = length(c$nodes[[i]])
  for(j in 1:len){
    node <- c$nodes[[i]][j]
    if(j == 1 | j == len){
      nodes <- c(nodes, node)
    }
    else {
      nodes <- c(nodes, node, node)
    }
  }
  verts <- c(verts, nodes)
}
rm(i, j, node, nodes, num_nodes)

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




















