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
library(lubridate)
library(chron)
library(ggplot2)
library(ggcorrplot)

#Read the Data into R
g <- read_csv("subcomponent.csv")
purchases <- read_csv("purchases_no_missing.csv")

set.seed(277)
g_sample <- sample_frac(g, 0.01)
write.table(g_sample, row.names = FALSE, col.names = colnames(g_sample), sep = ",", file = "g_sample.csv")
g_subcomponent_graph <- graph_from_data_frame(g_sample)
g_subcomponent <- graph_from_data_frame(g)

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

dat <- rep(0:11452, round(degree_distribution(g_subcomponent) * 1000))
ggplot() + geom_histogram(aes(dat), binwidth = 1, fill = "blue") + ggtitle("Distribution of Degrees Throughout the Network") + xlab("Degree") + 
  ylab("Frequency") + theme_minimal()# + scale_y_continuous(label = comma)

#Visualize Items
product_ids <- c(g$Source, g$Target)
purchases <- subset(purchases, purchases$product_id %in% product_ids)
head(purchases)
purchases$date <- as_date(purchases$event_time)
purchases$time <- chron(times = format(purchases$event_time, format = "%H:%M:%S"))

min(purchases$time)
mean(purchases$time)
median(purchases$time)
max(purchases$time)
hist(purchases$time)
ggplot(purchases, aes(x = time)) + geom_histogram(fill = "blue") + ggtitle("Distribution of Times Purchases Are Made") + xlab("Time") + 
  ylab("Frequency") + theme_bw() + scale_x_continuous(labels = c("0.00" = "00:00:00", "0.25" = "06:00:00", "0.50" = "12:00:00", "0.75" = "18:00:00", "1.00" = "24:00:00"))

mean(purchases$event_time)


































