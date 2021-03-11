#Matthias Ronnau
#Customer and Social Analytics-277
#Professor Dewan
#March 28, 2021

#Final Project

#Set Working Directory
setwd("~/The University of California, Irvine/Winter Quarter/Customer and Social Analytics-277/Final Project")

#Set options
options(scipen = 999)

###Load Packages
library(readr)
library(lubridate)
library(dplyr)
library(igraph)
library(graphdata)

#Read the Data into R
purchases_no_missing <- read_csv("Data/Cleaned Data/purchases_no_missing.csv")

#Make iGraph object
g <- graph_data(data = purchases_no_missing, id = "user_id", product_id = "product_id")
print(g)

graph <- as_data_frame(g)
head(graph)
colnames(graph) <- c("Source", "Target")
graph$Source <- as.double(graph$Source)
graph$Target <- as.double(graph$Target)

#Calculate the degree of each object
in_degree <- degree(g, mode = "in")
in_degree_dataframe <- as.data.frame(in_degree)
in_degree_dataframe$Source <- as.numeric(rownames(in_degree_dataframe))
rownames(in_degree_dataframe) <- c()
write.table(in_degree_dataframe, row.names = FALSE, col.names = colnames(in_degree_dataframe), sep = ",", file = "Data/Cleaned Data/in_degree_dataframe.csv")
graph <- left_join(graph, in_degree_dataframe, by = "Source")
head(graph)

out_degree <- degree(g, mode = "out")
out_degree_dataframe <- as.data.frame(out_degree)
out_degree_dataframe$Source <- as.numeric(rownames(out_degree_dataframe))
rownames(out_degree_dataframe) <- c()
write.table(out_degree_dataframe, row.names = FALSE, col.names = colnames(out_degree_dataframe), sep = ",", file = "Data/Cleaned Data/out_degree_dataframe.csv")
graph <- left_join(graph, out_degree_dataframe, by = "Source")
head(graph)

graph$Degree <- graph$in_degree + graph$out_degree
head(graph[order(- graph$Degree), ])

subcomponents <- subcomponent(g, v = "1005100", mode = "all")
g_subcomponent <- induced_subgraph(g, subcomponents)
item_subcomponent <- as_data_frame(g_subcomponent)
colnames(item_subcomponent) <- c("Source", "Target")

#Export ids into a csv
product_ids <- unique(c(item_subcomponent$Source, item_subcomponent$Target))
write.table(product_ids, row.names = FALSE, col.names = "id", sep = ",", file = "Data/Cleaned Data/product_ids.csv")

#Export graph data into a csv
write.table(item_subcomponent, row.names = FALSE, col.names = c("Source", "Target"), sep = ",", file = "Data/Cleaned Data/subcomponent.csv")














