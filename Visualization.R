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
library(dplyr)
library(igraph)
library(lubridate)
library(chron)
library(ggplot2)
library(stringr)

#Read the Data into R
purchases <- read_csv("Data/Cleaned Data/purchases_no_missing.csv")
g <- read_csv("Data/Cleaned Data/subcomponent.csv")
ids <- read_csv("Data/Cleaned Data/product_ids.csv")
samp <- read_csv("Data/Cleaned Data/sample.csv")

#Generate a sample of the data to use for network visualization
set.seed(277)
g_sample <- sample_frac(g, 0.01)
write.table(g_sample, row.names = FALSE, col.names = colnames(g_sample), sep = ",", file = "g_sample.csv")
g_subcomponent_graph <- graph_from_data_frame(g_sample)
g_subcomponent <- graph_from_data_frame(g)

#Visualize the network
diameter(g_subcomponent)

diam <- get_diameter(g_subcomponent_graph)

vcol <- rep("blue", vcount(g_subcomponent_graph))
vcol[diam] <- "red"
ecol <- rep("gray80", ecount(g_subcomponent_graph))
ecol[E(g_subcomponent_graph, path = diam)] <- "orange"

#Generate a plot visualizing the network and save the image
plot(g_subcomponent_graph, vertex.size = degree(g_subcomponent_graph) * 0.4, 
     vertex.color = vcol, vertex.label = NA, edge.arrow.size = 0.1,
     layout = layout.kamada.kawai)
dev.copy(png, "Visualizations/network.png")
dev.off()

#Visualize the degree distribution and save the image
dat <- rep(0:11452, round(degree_distribution(g_subcomponent) * 1000))
ggplot() + geom_histogram(aes(dat), binwidth = 1, col = "black", fill = "#0064A4") + ggtitle("Distribution of Degrees Throughout the Network") + xlab("Degree") + 
  ylab("Frequency") + theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
dev.copy(png, "Visualizations/degree.png")
dev.off()

#Visualize Items
purchases <- subset(purchases, purchases$product_id %in% ids$id)
head(purchases)
purchases$time <- chron(times = format(purchases$event_time, format = "%H:%M:%S"))

#Histogram of Time Purchases are Made
min(purchases$time)
mean(purchases$time)
median(purchases$time)
max(purchases$time)

ggplot(purchases, aes(x = time)) + geom_histogram(col = "black", fill = "#0064A4") + ggtitle("Distribution of Times Purchases Are Made") + xlab("Time") + 
  ylab("Frequency") + scale_x_continuous(labels = c("0.00" = "00:00:00", "0.25" = "06:00:00", "0.50" = "12:00:00", "0.75" = "18:00:00", "1.00" = "24:00:00")) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
dev.copy(png, "Visualizations/time_hist.png")
dev.off()

#Barplot Showing the Days on Which Purchases Are Made
ggplot(purchases, aes(x = weekday)) + geom_bar(stat = "count", fill = "#0064A4") +
  labs(title = "Purchases by Weekday", x = "Weekday", y = "Count") + 
  theme_test() + theme(plot.title = element_text(hjust = 0.5))
dev.copy(png, "Visualizations/purchases_day.png", width = 800)
dev.off()

#Boxplot of Product Price by Category
ggplot(purchases, aes(y = price)) + geom_boxplot(color = "#0064A4", fill = "#FFD200") + ggtitle("Boxplot of Product Prices by Category") + ylab("Average Price") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), axis.ticks.x = element_blank(),
                                         axis.text.x = element_blank()) + facet_grid(. ~ category)
dev.copy(png, "Visualizations/price_boxplot.png", width = 1000, height = 400)
dev.off()

#Visualize the types of events
samp$category <- str_to_title(sub("[[:punct:]].+", "", samp$category_code))

ggplot(samp, aes(x = factor(event_type, levels = c("view", "cart", "purchase"),
                            labels = c("View", "Cart", "Purchase")), 
                 fill = factor(event_type, levels = c("view", "cart", "purchase"),
                               labels = c("View", "Cart", "Purchase")))) + geom_bar(stat = "count") +
  labs(title = "Event Type") +
  scale_fill_manual(values = c("#6AA2B8", "#FFD200", "#0064A4"), name = "Event Type", labels = c("View", "Cart", "Purchase")) +
  scale_y_continuous(name = "Percentage", labels = c("0.00%", paste(as.character(round(20000/nrow(samp) * 100)), "%", sep = ""),
                                                   paste(as.character(round(40000/nrow(samp) * 100)), "%", sep = ""),
                   paste(as.character(round(60000/nrow(samp) * 100)), "%", sep = ""))) + theme_test() +
    theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_blank())
dev.copy(png, "Visualizations/event_barplot.png")
dev.off()

#Visualize event breakdown by category
ggplot(samp, aes(x = category)) + geom_bar(position = "fill", 
                                           aes(fill = factor(event_type, levels = c("view", "cart", "purchase"), 
                                                             labels = c("View", "Cart", "Purchase")))) + labs(title = "Event Type", x = "Product Category", y = "Percentage") +
  scale_fill_manual(values = c("#6AA2B8", "#FFD200", "#0064A4"), name = "Event Type", labels = c("View", "Cart", "Purchase")) + 
  scale_y_continuous(labels = c("0.00%", "25%", "50%", "75%", "100%")) +
  theme_test() + theme(plot.title = element_text(hjust = 0.5))
dev.copy(png, "Visualizations/event_category_barplot.png", width = 1000, height = 400)
dev.off()



