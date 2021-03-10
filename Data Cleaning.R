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
library(igraph)
#library(Rtools)
devtools::install_github("matthiasronnau/graphdata", force = TRUE)

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
write.table(purchases_no_missing, row.names = FALSE, col.names = colnames(purchases_no_missing), sep = ",", file = "purchases_no_missing.csv")

g <- graphdata::graph_data(data = purchases_no_missing, id = "user_id", product_id = "product_id")
print(g)

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


















