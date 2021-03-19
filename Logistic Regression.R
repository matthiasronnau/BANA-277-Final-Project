#Matthias Ronnau
#Customer and Social Analytics-277
#Professor Dewan
#March 28, 2021

#Final Project

#Set Working Directory
setwd("~/The University of California, Irvine/Winter Quarter/Customer and Social Analytics-277/Final Project")

#Set options
options(scipen = 999)
set.seed(277)

###Load Packages
library(readr)
library(lubridate)
library(chron)
library(dplyr)
library(ROSE)
library(igraph)
library(fastDummies)

#Read the Data into R
samp <- read_csv("Data/Cleaned Data/sample.csv")
ids <- read_csv("Data/Cleaned Data/product_ids.csv")
g <- read_csv("Data/Cleaned Data/subcomponent.csv")
in_degree_dataframe <- read_csv("Data/Cleaned Data/in_degree_dataframe.csv")
out_degree_dataframe <- read_csv("Data/Cleaned Data/out_degree_dataframe.csv")

#Get data from the sample that is part of the subcomponent
data <- subset(samp, samp$product_id %in% ids$id)
rm(samp, ids)
data_no_missing <- na.omit(data)
data_no_missing$purchase <- ifelse(data_no_missing$event_type == "purchase", 1, 0)
data_no_missing$event_time <- as_datetime(data_no_missing$event_time)
data_no_missing$time <- chron(times = format(data_no_missing$event_time, format = "%H:%M:%S"))
data_no_missing$weekday <- wday(data_no_missing$event_time, label = TRUE)
rm(data)
nrow(subset(data_no_missing, data_no_missing$purchase == 1))
nrow(data_no_missing)

#Oversample the data for unbiased estimates for logistic regression
over <- sample_frac(ovun.sample(purchase ~ ., data = data_no_missing, method = "over")$data, 0.5)
nrow(subset(over, over$purchase == 1))
nrow(over)
rm(data_no_missing)

#Get Network Statistics
g_subcomponent <- graph_from_data_frame(g)

#Calculate the edge density
edge_density(g_subcomponent)

#Calculate the reciprocity
reciprocity(g_subcomponent)

#Calculate the closeness centrality
close_df <- as.data.frame(closeness(g_subcomponent, mode = "all"))
close_df$id <- as.numeric(rownames(close_df))
rownames(close_df) <- c()
colnames(close_df) <- c("closeness", "id")
head(close_df)

#Calculate the betweenness centrality
between_df <- as.data.frame(betweenness(g_subcomponent, directed = TRUE))
between_df$id <- as.numeric(rownames(between_df))
rownames(between_df) <- c()
colnames(between_df) <- c("betweenness", "id")
head(between_df)

#Calculate the hub score
hub_df <- as.data.frame(hub_score(g_subcomponent)$vector)
hub_df$id <- as.numeric(rownames(hub_df))
rownames(hub_df) <- c()
colnames(hub_df) <- c("hub_score", "id")
head(hub_df)

#Calculate the authority score
authority_df <- as.data.frame(authority_score(g_subcomponent)$vector)
authority_df$id <- as.numeric(rownames(authority_df))
rownames(authority_df) <- c()
colnames(authority_df) <- c("authority_score", "id")
head(authority_df)

#Combine the relevant stats into a single dataframe
network_stats <- inner_join(inner_join(inner_join(close_df, between_df, by = "id"), hub_df, by = "id"), authority_df, by = "id")
head(network_stats)
rm(close_df, between_df, hub_df, authority_df)

#Join both datasets together to be used for the logistic regression
merged <- inner_join(g, over, by = c("Source" = "product_id"))
merged$view <- ifelse(merged$event_type == "view", 1, 0)
merged$cart <- ifelse(merged$event_type == "cart", 1, 0)
merged$purchase <- ifelse(merged$event_type == "purchase", 1, 0)
rm(g)

target_source <- subset(merged, merged$Target %in% unique(merged$Source))
neighbor_stats <- summarize(group_by(target_source, Target), nghb_mn_price = mean(price), 
                            nghb_avg_pct_views = mean(view),
                            nghb_avg_pct_cart = mean(cart),
                            nghb_avg_pct_purchase = mean(purchase))
rm(target_source)
names(neighbor_stats)[names(neighbor_stats) == "Target"] <- "product_id"
head(neighbor_stats)

logistic_data <- left_join(left_join(left_join(left_join(neighbor_stats, over, by = "product_id"), in_degree_dataframe, by = c("product_id" = "Source")), out_degree_dataframe,
                    by = c("product_id" = "Source")), network_stats, by = c("product_id" = "id"))
logistic_data <- dummy_cols(logistic_data, select_columns = "weekday")
rm(over, neighbor_stats, in_degree_dataframe, out_degree_dataframe, network_stats)

logistic_model <- glm(purchase ~ log(price) + time + weekday_Mon + weekday_Tue + 
                        weekday_Wed + weekday_Thu + weekday_Fri + weekday_Sat + 
                        in_degree + out_degree + closeness + betweenness + 
                        hub_score + authority_score + nghb_mn_price + 
                        nghb_avg_pct_views + nghb_avg_pct_cart + 
                        nghb_avg_pct_purchase, family = "binomial", data = logistic_data)
summary(logistic_model)

updated_logistic_model <- glm(purchase ~ log(price) + time + weekday_Mon + 
                                weekday_Tue + weekday_Wed + weekday_Thu + 
                                weekday_Fri + weekday_Sat + in_degree + 
                                closeness + betweenness + hub_score + 
                                nghb_avg_pct_views, family = "binomial", data = logistic_data)
summary(updated_logistic_model)







