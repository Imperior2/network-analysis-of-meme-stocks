library("igraph")
library("tidyverse")
library("ggplot2")
library("reshape2")
#@Author Marvin Kruber
############################# Layout ########################################################################################
#These themes are used in ordinary scatter plots.
descriptiveTheme <- theme(
  #Styles axis
  plot.title = element_text(colour = "black", size = 15, face = "bold"),
  axis.title.x = element_text(colour = "black", size = 12, face = "bold"),
  axis.title.y = element_text(colour= "black", size = 12, face = "bold"),
  axis.text.x = element_text( colour = "grey15", size = 8, face = "bold"), #angle = 45, vjust = 1, hjust = 1,
  axis.text.y = element_text(colour = "grey15", size = 8, face ="bold"),
  axis.line = element_line(linewidth = 1, colour = "grey65", linetype="solid"),
  legend.title = element_text(colour = "black",  size = 12, face = "bold"),
  legend.text = element_text(colour = "grey15", size = 8, face = "bold"),
  # #Style panel
  panel.background = element_rect(fill = "white")
)
gridTheme <- theme(
  panel.grid.major.x = element_line(linewidth = 0.4, colour = "gray40", linetype = "dashed" ), 
  panel.grid.major.y = element_line(linewidth = 0.4, colour = "grey85", linetype = "solid" )
)
################################ Functions ###################################################################################
#Calculates a matrix (ranking) with the Top X candidates for each metric of the passed matrix for the entire time frame.
# matrix - contains metric values of each node of the entire time frame
# nr - specifies how many candidates are selected
# returns ranking of the top X candidates for the corresponding metric
calculateTopX <- function(matrix, nr) {
  #Extract names of metrics and append "Ticker" in the next columns
  names <- c()
  for(name in dimnames(matrix)[[2]]){
    names <- append(names, name)
    names <- append(names, "Ticker")
  }
  ranking <- matrix(nrow = nr, ncol = (2*ncol(matrix)), dimnames = list(c(), names)) #dimnames = dimnames(matrix)
  
  if(nrow(matrix) == 0){
    return(ranking)
  }
  
  #These vectors are used for error handling so that each output matrix has exactly nr rows even if nr > nrows(matrix).
  values <- rep(NA, nr)
  names <- rep(NA, nr)
  end <- if(nrow(matrix) < nr) nrow(matrix) else nr
  
  #Extract values and Ticker ranking
  for(i in 1:ncol(matrix)) {
    values[1:end] <- head(sort(matrix[,i], decreasing =TRUE), nr)
    names[1:end] <- names(head(sort(matrix[,i], decreasing =TRUE), nr))
    
    ranking[,(2*i-1)] <- values
    ranking [,(2*i)] <- names
  }
  return(ranking)
}

#Calculates the edges based on tickers which were jointly referenced. The weights are derived from the total number
#of individual mentionings of the corresponding ticker.
# temporal_subs - Submissions within the respective time frame
# returns list which contains two elements: 
#           (1) the edge list following the model assumptions and 
#           (2) all tickers and their corresponding number of references in the time period
calculateEdgeList <- function(temporal_subs) {
  #Determine all submissions whith multiple tickers
  dups <- temporal_subs$id[duplicated(temporal_subs$id)] %>% unique()
  sub_mult <- temporal_subs %>% filter((id %in% dups))
  unique_tickers <- sub_mult$Ticker %>% unique()
  
  # Calculate how often each multiple named ticker is referenced within the time frame
  tic_occ <- temporal_subs %>% count(Ticker) %>% filter(Ticker %in% unique_tickers)
  names(tic_occ)[2] <- "Occurences"
  
  #Join tic_occ and sub_mult via Ticker in order to get the number of occurences for each ticker.
  weighted_subs <- inner_join(x=sub_mult, y=tic_occ, by="Ticker")
  
  # Determine edges based on joint reference in submission and determine the weight of an edge.
  el <- inner_join(x=weighted_subs, y=weighted_subs, by="id", relationship="many-to-many") %>% filter(!(Ticker.x == Ticker.y))
  names(el) <- c("ID", "from", "Weight1","to", "Weight2")
  #Opsahl2010 also used the idea of inverting the weight to keep the idea of closeness.
  #Returns 1 / number of occurences.
  el <- el %>% transmute(from, to, weight = (1 /(Weight1 + Weight2)))
  
  return(list(el, tic_occ))
}

#This function creates the network following the model, i.e. building connections between stocks which were mentioned together.
# submissions - dataset of submissions
# returns largest component (i.e. with the highest total number of references) of the network
createNetwork <- function(submissions){
  res <- calculateEdgeList(submissions)
  edge_list <- res[[1]]
  tic_occ <- res[[2]]
  network <- graph_from_data_frame(d=edge_list, directed=FALSE)
  #Remove multi-egdes because of the join leads to two edges in the edge list with the same weight A -- B, B -- A
  #edge.attr.comb has to be set to "first", otherwise it would sum the edge weights. => @see https://igraph.org/r/doc/igraph-attribute-combination.html
  network <- igraph::simplify(network, edge.attr.comb = "first")
  
  V(network)$Occurence <- tic_occ[match(V(network)$name, tic_occ$Ticker),]$Occurences
  #Determine largest connected component
  #return(network)
  return(removeIsolated_Components(network))
}

#This function collects the network measures for a specific day (date).
# subs - set of submissions
# date - date to filter for
# returns list containing the centralites of each node in the network (first entry), minimal spanning tree (second entry)
# and the global clustering coefficient of the network (third entry)
collectNetworkStatisticsPerDay <- function(subs, date){
  data <- subs %>% filter(NY_Trading_Day == date) %>% select(-NY_Trading_Day) 
  g <- createNetwork(data)
  
  #Error prevention in case of an empty graph
  mst <- make_empty_graph(n=0, directed = FALSE)
  
  if(length(E(g)) > 0) {
    mst <- mst(g)
  }
  
  centralities_graph <- matrix(c(degree(g), strength(g, weights = 1/E(g)$weight), betweenness(g), closeness(g), 
                                                     page_rank(g, weights = 1/E(g)$weight)$vector, transitivity(g, type = "local")), nrow=length(V(g)), ncol = 6,
                                                   dimnames = list(V(g)$name, c("Degree", "Strength" ,"Betweenness", "Closeness", "Eigenvector","Clustering Coefficient")))
  
  centralities_mst <- matrix(c(degree(mst), strength(mst, weights = 1/E(mst)$weight), betweenness(mst), closeness(mst), 
                               page_rank(mst, weights = 1/E(mst)$weight)$vector, transitivity(mst, type = "local")), nrow=length(V(mst)), ncol = 6,
                             dimnames = list(V(mst)$name, c("Degree", "Strength" ,"Betweenness", "Closeness", "Eigenvector","Clustering Coefficient")))
  global_cc <- transitivity(g)
  return(list(centralities_graph, centralities_mst, global_cc))
}

#This function calculates an entire dataset of the metrics of each node for each day and returns a list of data frames.
#The first 12 data frames contain the node metrics during the last depicts the overall clustering coefficient over time.
# subs - submission (SHOULD BE FILTERED for TIME RANGE!!!)
# mult_ref_Tickers - ordered ticker set of all tickers which were jointly referenced in submissions in the time frame
# returns list of data frames with all metric values over time for each individual node
storeNetworkStatistics <- function(subs, mult_ref_Tickers){
  Tickers <- mult_ref_Tickers$Ticker
  dates <- subs$NY_Trading_Day %>% unique() %>% sort()
  dates <- as.character(dates)
  
  metrics <- list(c())
  df <- data.frame(Ticker = Tickers)
  # d_graph, d_mst, s_graph, s_mst, b_graph, b_mst, c_graph, c_mst, ev_graph, ev_mst
  for (i in 1:12) {
    metrics[[i]] <- df
  }
  # global_cc_graph
  metrics[[13]] <- data.frame()
  non_empty_days <- c()
  
  for(date in dates){
    print(date)
    
    ls <- collectNetworkStatisticsPerDay(subs, date)
    
    #Preparation for join => Convert from matrix to data frame
    c_graph <- as.data.frame(ls[[1]])
    c_graph$Ticker <- row.names(ls[[1]])
    c_mst <- as.data.frame(ls[[2]])
    c_mst$Ticker <- row.names(ls[[2]])
    global_cc <- ls[[3]]
    
    #Skip the JOIN if there are no tickers which were referenced jointly on that day.
    if(length(c_graph$Ticker) == 0) {
      next
    }
    
    non_empty_days[length(non_empty_days) + 1] <- date 
    
    #Store all metric values for this day
    for(i in 1:6) {
      temp <- data.frame(Ticker = c_graph$Ticker, c_graph[i])
      metrics[[2*i-1]] <- left_join(x=metrics[[2*i-1]], y=temp, by="Ticker")
      temp <- data.frame(Ticker = c_graph$Ticker, c_mst[i])
      metrics[[2*i]] <- left_join(x=metrics[[2*i]], y=temp, by="Ticker")
    }
    #Store the global clustering coefficient
    new_row = data.frame(Day = date, Global_CC = global_cc)
    metrics[[13]] <- rbind(metrics[[13]], new_row)
  }
  
  #Change column names of each data frame
  non_empty_days <- as.character(non_empty_days)
  non_empty_days <- non_empty_days %>% sort()
  for (i in 1:12){
    colnames(metrics[[i]]) <- append(c("Ticker"), non_empty_days)
  }
  return(metrics)
}

#Adapted from: https://stackoverflow.com/questions/64344845/getting-the-biggest-connected-component-in-r-igraph
#This functions determines the largest connected component by taking the number of references of each node into account.
#Problem is that closeness centrality is evaluated on separate components which leads to biases in a ranking.
# graph - network with eventually multiple components
# returns largest component of the network
removeIsolated_Components <- function(graph) {
  components <- igraph::components(graph, mode="weak")
  
  #Find cluster ID of the component with the most references
  df <- data.frame(cID = components$membership, values = V(graph)$Occurence)
  df <- df %>% group_by(cID) %>% summarise(values = sum(values))
  biggest_cluster_id <- df[which.max(df$values),]$cID
  
  # ids
  vert_ids <- V(graph)[components$membership == biggest_cluster_id]
  # subgraph
  return(igraph::induced_subgraph(graph, vert_ids))
}

#Changes format of metrics[[i]] data frame from: [Ticker | Day 1 | Day 2 | ... | Day n] 
#to [Date | Ticker 1 | Ticker 2 | ... | Ticker n].
#In addition, all NAs are replaced with 0.
# df - dataframe of metrics list, i.e. metrics[[i]]
# returns inverted data frame
invertMetrics <- function (df){
  #The first line has to be removed after transposing the data frame since it contains the ticker.
  temp <- data.frame(t(df)[-1,])
  colnames(temp) <- df$Ticker
  temp["Date"] <- as.Date(rownames(temp))
  temp <- temp %>% select(Date, everything())
  temp <- temp %>% replace(is.na(.), 0)
  for (i in 2:(length(temp))) {
    temp[i] <- as.numeric(unlist(temp[i]))
  }
  return(temp)
}

#Implements the logic of a sliding window. 
# df_metric - [Ticker | Day 1 | Day 2 | ... | Day n]
# NR_Days - window size
# returns - data frame with aggregated values [Day 1 | ... | Day n | Ticker]
slidingWindow <- function(df_metric, NR_DAYS = 20) {
  #Weighting the single days via (x/20)^2 => This assigns a higher significance to more recent values so that evaluation is more
  #trend sensitive. Furthermore the modified values are no problem because we only evaluate the ordinality and not the value!!!
  #It also follows the idea of Reddits ranking algorithm, i.e. that the display order of submissions also depends on their creation date. 
  weights = (c(1:NR_DAYS)/NR_DAYS)^2
  
  #Format data and create "results" data.frame
  temp <- invertMetrics(df_metric)
  
  #Stores all temporal distances to predecessor date
  dates <- determineTemporalDistances(temp$Date)
  temp <- inner_join(x = temp, y = dates, by = "Date") %>% select(Date, Dist_to_prev_day, everything())
  temp <- temp %>% arrange(Date)
  
  #Iterate over columns (i.e. Tickers) starting at column three because the first two column are "Date" and "Dist_to_prev_day".
  for(col in 3: length(temp)) {
    prev_day <- temp$Date[1]
    
    #Metric values of the past days. Initialized with 0s.
    values = rep(0, NR_DAYS)
    
    #Iterate over dates
    for(row in 1:length(temp$Date)) {
      day <- temp$Date[row]
      temporal_diff <- temp$Dist_to_prev_day[row]
      
      if(temporal_diff == 1){
        #Case for a one day distance (and also used for the start of the sliding window)
        values <- c(values[2:NR_DAYS], temp[row, col])
        
      } else if (temporal_diff < NR_DAYS) {
        #Case of normal delay between days and smaller than window-size.
        #Insert 0s for days in the middle
        values <- c(values[(temporal_diff+1):NR_DAYS], rep(0, temporal_diff-1), temp[row, col])
        
      } else {
        #Case temporal_diff >= NR_DAYS
        values <- c(rep(0, NR_DAYS-1), temp[row, col])
      }
      
      temp[row, col] <- sum(weights * values)
      prev_day <- day
    }
  }
  #Bring the data frame to the right format, i.e. [Day 1 | ... | Day n | Ticker].
  #Remove Date and Dist_to_prev_day as rows after transposing the data frame.
  final_df <- data.frame(t(temp)[3:length(temp),])
  colnames(final_df) <- temp$Date
  final_df$Ticker <- rownames(final_df)
  
  for (h in 1:(length(final_df)-1)){
    final_df[,h] <- as.numeric(final_df[,h])
  }
  return(final_df)
}

#Creates a dictionary that contains the temporal distance of each date to its predecessor.
#Weekends are ignored, i.e. the temporal distance between Friday and Monday is 1.
# all_dates - vector with all consecutive dates
# returns - data.frame[Date | Dist_to_prev_day]
determineTemporalDistances <- function (all_dates) {
  #position_in_week stores the number of the weekday [Sunday is 0]
  weekdays_dictionary <- data.frame(Date = as.Date(all_dates), position_in_week = as.numeric(format(as.Date(all_dates), "%w")))
  weekdays_dictionary$position_in_week <-  replace(weekdays_dictionary$position_in_week, 
                                                   weekdays_dictionary$position_in_week == 0, 7)
  
  #Remove weekend entries
  weekdays_dictionary <- weekdays_dictionary %>% filter(position_in_week < 6)
  
  #Sort to ensure the correct order and initialize with 0s.
  weekdays_dictionary <- weekdays_dictionary %>% arrange(Date)
  weekdays_dictionary$Dist_to_prev_day <- rep(0, length(weekdays_dictionary$Date))
  
  #Set start to one => required for performance optimization in determineDailyValue()
  weekdays_dictionary$Dist_to_prev_day[1] <- 1
  prev_day <-  weekdays_dictionary$Date[1]
  
  for(row in 2:length( weekdays_dictionary$Date)) {
    day <-  weekdays_dictionary$Date[row]
    
    #Calculate difference to previous date. 
    time_dif <- as.numeric(difftime(day, prev_day))
    
    #Determine weekends
    nr_weekend_days <- floor(time_dif / 7) * 2
    time_dif <- time_dif - nr_weekend_days
    
    #If weekday of day < prev_day an additional weekend is enclosed. Do -> Mi
    if (weekdays_dictionary$position_in_week[row] < weekdays_dictionary$position_in_week[row-1]) {
      time_dif <- time_dif-2
    }
    
    #Store temporal difference and update prev_day
    weekdays_dictionary$Dist_to_prev_day[row] <- time_dif
    prev_day <- day
  }
  return(select(weekdays_dictionary, -position_in_week))
}

#Determines the top X stocks per day according to the underlying metric.
# df - resulting data frame of slidingWindow() => Format: [Day 1 | ... | Day n | Ticker]
# TOP_X - number of ranks => Default: 10
# returns ranking for the corresponding metric on a daily base
dailyRanking <- function(df, TOP_X = 10){
  # Safety check
  if(length(df$Ticker) < TOP_X) {
    TOP_X <- length(df$Ticker)
  }
  
  previous_Top_X <- rep("", TOP_X)
  ranking <- data.frame(Position = c(1:TOP_X))
  temp <- df
  temp$Rank <- rep(0, length(temp$Ticker))
  end <- length(temp)
  
  #Iterate over all days => columns Ticker and Rank are excluded
  for(j in 1:(end-2)){
    
    date <- colnames(temp)[j]
    temp$Rank <- dense_rank(desc(as.numeric(temp[,j])))
    candidates <- (temp %>% filter(Rank <= TOP_X))[c(j, (end-1), end)] %>% arrange(Rank) #=> Formats to [Value | Ticker | Rank]
    candidates[,1] <- as.numeric(candidates[,1])
    
    #Solve ties
    if (candidates[1,1] == 0){
      #Keep the previous TOP_X if there was no metric value observed on this day
      candidates <- candidates %>% filter(Ticker %in% previous_Top_X)
      #Sort them appropriately
      candidates <- candidates[match(candidates$Ticker, previous_Top_X),]
      
    } else if (length(candidates$Ticker) > TOP_X) { # Case of a Tie
      #Determine rank of the 10th entry and filter for all Tickers which have this rank. 
      #Then prefer those who are in previous_Top_X.
      rank <- candidates$Rank[TOP_X]
      tie_stocks <- candidates %>% filter(Rank == rank)
      already_known <- tie_stocks %>% filter(Ticker %in% previous_Top_X)
      unknown_stocks <- tie_stocks %>% filter(! (Ticker %in% previous_Top_X))
      
      if(length(already_known$Ticker) > 0) {
        #Determine Priority of stocks in the tie which were already mentioned one day before
        already_known$Priority <- match(already_known$Ticker, previous_Top_X)
        already_known <- already_known %>% arrange(Priority) %>% select(-Priority)
        prio_candidates <- candidates %>% filter(Rank < rank) %>% arrange(Rank)
        #Add elements according to their priority on the previous day
        candidates <- head(rbind(prio_candidates, already_known), TOP_X)
      } else {
        #Case that none of the tie_stocks was previously in the Top X
        candidates <- head(candidates, TOP_X)
      }
      #Fill candidates with stocks which were not mentioned before if all previously mentioned stocks were included (in case of a tie)
      l <- length(candidates$Ticker)
      if(l < TOP_X){
        candidates <- rbind(candidates, head(unknown_stocks, (TOP_X - l)))
      }
    }
    ranking[date] <- candidates$Ticker
    previous_Top_X <- candidates$Ticker
  }
  return(ranking)
}
################################# Application Logic #################################################################################
#reddit_final is a dataset where submissions with multiple tickers have been replicated so that there is a submission for each ticker.
#The data set belongs to Felix Reichenbach and Martin Walther. “Financial recommendations on Reddit,stock returns and cumulative prospect theory”. In: Digital Finance (2023), pp. 1–28.

# Select the required fields from the original data set
# submissions <- read.csv("./res/submissions/reddit_final.csv")
# submissions <- submissions %>% select(id, Ticker, NY_Trading_Day)

submissions <- read.csv("./res/submissions/data.csv")
submissions <- transform(submissions, NY_Trading_Day = as.Date(NY_Trading_Day))

summary(submissions)

#Determine all submissions which contains multiple tickers
duplicates <- submissions$id[duplicated(submissions$id)] %>% unique()
submissions_mult <- submissions %>% filter((id %in% duplicates))

#================================================ Descriptive statistics ====================================================
#Calulate how often a ticker is referenced within the entire time frame
ticker_occ <- submissions %>% count(Ticker) %>% arrange(Ticker)
names(ticker_occ)[names(ticker_occ)=="n"] <- "Occurences"
summary(ticker_occ)

#Shows which tickers are mainly referenced. Many of them are meme-stocks
print(ticker_occ %>% filter(Occurences >= 100))
#Number of all tickers in the dataset
NR_TICKERS <- length(ticker_occ$Ticker) 
#Display the most discussed stocks
print(ticker_occ %>% arrange(desc(Occurences)))

ggplot(ticker_occ, aes(x=Ticker, y=log(Occurences))) +
  geom_point(fill="#a60a94", size = 2, shape=21, alpha=0.5) + labs(x = "Ticker", y = "Occurences (logarithmic)") +
  scale_y_continuous(breaks=seq(0,11, by=1)) +
  scale_x_discrete() +
  descriptiveTheme + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ggsave("Ticker_Occ.pdf", width = 5.0, height = 5.0, units = "in")

#Determine the frequency distribution of ticker occurences i.e. how often an arbitrary ticker is stated n-times
occ_f <- ticker_occ %>% count(Occurences)
names(occ_f)[names(occ_f)=="n"] <- "Frequence"
summary(occ_f) #Here Occurences has different Quartile values because the columns has been aggregated/group_by via count

ggplot(occ_f, aes(x= log(Occurences), y=Frequence)) +
  geom_point(fill="#a60a94",  size = 2, shape=21, alpha=0.5) + labs(x = "Occurences (logarithmic)", y = "Frequence") +
  scale_x_continuous(breaks=seq(0,11, by=1)) +
  descriptiveTheme + gridTheme
ggsave("Frequency_Distribution.pdf", width = 5.0, height = 5.0, units = "in")

#Shows how many stocks has been mentioned per day => Illustrates the activity
tickers_per_day <- submissions %>%
  group_by(NY_Trading_Day) %>%
  count()
names(tickers_per_day)[names(tickers_per_day)=="n"] <- "Tickers_per_Day"
summary(tickers_per_day)

ggplot(tickers_per_day, aes(x=NY_Trading_Day, y=log(Tickers_per_Day))) +
  geom_point(fill="#a60a94",  size = 2, shape=21, alpha=0.5) + labs(x = "Date", y = "Number of Tickers per Day (logarithmic)") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", date_minor_breaks = "3 month", limits = c(as.Date("2007-01-01"), as.Date("2022-09-01"))) +
  scale_y_continuous(breaks=seq(0,11, by=1)) +
  descriptiveTheme + gridTheme
ggsave("Tickers_per_Day.pdf", width = 5.0, height = 5.0, units = "in")

#Maximum activity is reached on 2021-01-28
tickers_per_day %>% filter(Tickers_per_Day == max(tickers_per_day$Tickers_per_Day))

#--------------------- Statistics of tickers which were mentioned in together in one post (duplicate set)------------------------
#Calulate how often a ticker is referenced within the entire time frame
ticker_occ_dup <- submissions_mult %>% count(Ticker) %>% arrange(Ticker)
names(ticker_occ_dup)[names(ticker_occ_dup)=="n"] <- "Occurences"
summary(ticker_occ_dup)

#Many are meme-stocks
print(ticker_occ_dup %>% filter(Occurences >= 100)) 
#Number of mentioned tickers
NR_TICKERS_DUP <- length(ticker_occ_dup$Ticker) 

#Number of recommendations per ticker (duplicate set)
ticker_occ_dup %>% arrange(desc(Occurences))
ggplot(ticker_occ_dup, aes(x=Ticker, y=log(Occurences))) +
  geom_point(fill="#d46d06", size = 2, shape=21, alpha=0.5) + labs(x = "Tickers", y = "Occurences (logarithmic)") +
  scale_y_continuous(breaks=seq(0,11, by=1)) +
  scale_x_discrete() +
  descriptiveTheme + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ggsave("Ticker_Occ_DUP.pdf", width = 5.0, height = 5.0, units = "in")

#Determine the frequency distribution of ticker occurences i.e. how often an arbitrary (duplicate) ticker is stated n-times.
occ_f_dup <- ticker_occ_dup %>% count(Occurences)
names(occ_f_dup)[names(occ_f_dup)=="n"] <- "Frequence"
summary(occ_f_dup) # Here Occurences has different Quartile values because the columns has been aggregated/group_by via count

ggplot(occ_f_dup, aes(x= log(Occurences), y=Frequence)) +
  geom_point(fill="#d46d06",  size = 2, shape=21, alpha=0.5) + labs(x = "Occurences (logarithmic)", y = "Frequence") +
  scale_x_continuous(breaks=seq(0,11, by=1)) +
  descriptiveTheme + gridTheme
ggsave("Frequency_Distribution_DUP.pdf", width = 5.0, height = 5.0, units = "in")

#Shows how many (duplicate) stocks has been mentioned per day => Illustrates the activity
tickers_per_day_dup <- submissions_mult %>%
  group_by(NY_Trading_Day) %>%
  count()
names(tickers_per_day_dup)[names(tickers_per_day_dup)=="n"] <- "Tickers_per_Day"
summary(tickers_per_day_dup)

ggplot(tickers_per_day_dup, aes(x=NY_Trading_Day, y=log(Tickers_per_Day))) +
  geom_point(fill="#d46d06",  size = 2, shape=21, alpha=0.5) + labs(x = "Date", y = "Number of Tickers per Day (logarithmic)") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", date_minor_breaks = "3 month", limits = c(as.Date("2007-01-01"), as.Date("2022-09-01"))) +
  scale_y_continuous(breaks=seq(0,11, by=1)) +
  descriptiveTheme + gridTheme
ggsave("Tickers_per_Day_DUP.pdf", width = 5.0, height = 5.0, units = "in")

summary(tickers_per_day_dup)
tickers_per_day_dup %>% filter(Tickers_per_Day == max(tickers_per_day_dup$Tickers_per_Day))
#Represents the percentage of days wich contain posts with multiple ticker references
length(tickers_per_day_dup$NY_Trading_Day) / length(tickers_per_day$NY_Trading_Day)
#=================================== Creation of the network: Entire Time Frame =============================================
network <- createNetwork(select(submissions, -NY_Trading_Day))

#Determine minimal spanning tree (MST)
mtree <- mst(network)
layout_tree <- layout_nicely(mtree)

#Contains top 10 tickers according to each network measure at network level and MST-level 
candidates <- unique(c(values_MST[,2], values_MST[,4], values_MST[,6], values_MST[,8], values_MST[,10], values_NW[,2], values_NW[,4], values_NW[,6], values_NW[,8], values_NW[,10]))

#Remove margin of network plots
par(mar=c(0,0,0,0)+.1)

#Visualization of the network over the entire period
plot(network, vertex.size = 2.5, 
     vertex.label=ifelse(V(network)$name %in% candidates, V(network)$name, NA), 
     vertex.label.color=c("#d46d06"),
     vertex.label.font=2,
     vertex.color = "purple3",
     vertex.label.cex=c(1.2))

#Visualize top 10 according to degree metric at MST
plot(mtree, layout=layout_tree, vertex.size = 2.5, vertex.label=ifelse(V(mtree)$name %in% values_MST[,2], V(mtree)$name, NA), vertex.label.color=c("#d46d06"),
     vertex.label.font=2,
     vertex.color = "purple3",
     vertex.label.cex=c(1.2))
#Visualize top 10 according to strength metric
plot(mtree, layout=layout_tree, vertex.size = 2.5, vertex.label=ifelse(V(mtree)$name %in% values_MST[,4], V(mtree)$name, NA), vertex.label.color=c("#d46d06"),
     vertex.label.font=2,
     vertex.color = "purple3",
     vertex.label.cex=c(1.2))
#Visualize top 10 according to betweenness metric
plot(mtree, layout=layout_tree, vertex.size = 2.5, vertex.label=ifelse(V(mtree)$name %in% values_MST[,6], V(mtree)$name, NA), vertex.label.color=c("#d46d06"),
     vertex.label.font=2,
     vertex.color = "purple3",
     vertex.label.cex=c(1.2))
#Visualize top 10 according to closeness metric
plot(mtree, layout=layout_tree, vertex.size = 2.5, vertex.label=ifelse(V(mtree)$name %in% values_MST[,8], V(mtree)$name, NA), vertex.label.color=c("#d46d06"),
     vertex.label.font=2,
     vertex.color = "purple3",
     vertex.label.cex=c(1.2))
#Visualize top 10 according to eigenvector metric
plot(mtree, layout=layout_tree, vertex.size = 2.5, vertex.label=ifelse(V(mtree)$name %in% values_MST[,10], V(mtree)$name, NA), vertex.label.color=c("#d46d06"),
     vertex.label.font=2,
     vertex.color = "purple3",
     vertex.label.cex=c(1.2))
#Visualize top 10 according to local clustering coefficient
plot(mtree, layout=layout_tree, vertex.size = 2.5, vertex.label=ifelse(V(mtree)$name %in% values_NW[,12], V(mtree)$name, NA), vertex.label.color=c("#d46d06"),
     vertex.label.font=2,
     vertex.color = "purple3",
     vertex.label.cex=c(1.2))


#Visualizes all candidates of all centrality measures (i.e. without the local clustering coefficient)
plot(mtree, layout=layout_tree, vertex.size = 2.5, 
     vertex.label=ifelse(V(mtree)$name %in% candidates, V(mtree)$name, NA), 
     vertex.label.color=c("#d46d06"),
     vertex.label.font=2,
     vertex.color = "purple3",
     vertex.label.cex=c(1.2))

#Create ego-network of GME
ego_gme <- make_ego_graph(network, order=1, nodes = V(network)["GME"], mode ="all")[[1]]
layout_ego <- layout_with_fr(ego_gme)


#Plots ego network of GME. The nodes are arranged in dependence of their distance.
par(mar=c(0,0,0,0)+.1)
plot(ego_gme, layout=layout_ego, vertex.label.color=c("#d46d06"),
     vertex.size = ifelse(V(ego_gme)$name == "GME", 10, 2.5),
     vertex.label.font=2,
     vertex.label=ifelse(V(ego_gme)$name =="GME", V(ego_gme)$name, NA),
     vertex.color = ifelse(V(ego_gme)$name == "GME","red","purple3"), 
     vertex.label.cex=c(1.2))

#-------------------------------------------- Centrality measures on network level-----------------------------------------
degree_c <- degree(network)
#Summing up the edge weights of the adjacent edges for each vertex. => Interpretes them as strength
w_degree_c <- strength(network, weights = 1/(E(network)$weight))
between_c <- betweenness(network)
#igraph interpretes weights as distances. (@see https://igraph.org/r/doc/closeness.html )
closeness_c <- closeness(network)
#igraph uses automatically the weights of a graph but it interprets them as strength of the tie.
#For this reason the weights have to be inverted to keep the logic.
eigenv_c <- page_rank(network, weights = 1/E(network)$weight)$vector
clustering_c <- transitivity(network, type ="local")
#Show the local clustering coefficient of previous candidates
clustering_c[candidates]

#Store centralities and ranking of the network
centralities_NW <- matrix(c(degree_c, w_degree_c, between_c, closeness_c, eigenv_c, clustering_c), nrow = length(V(network)), ncol=6,
                          dimnames = list(V(network)$name, c("Degree", "Strength", "Betweenness", "Closeness", "Eigenvector", "Clustering Coefficient")))
values_NW <- calculateTopX(centralities_NW, 10)

#Global clustering coefficient
print(transitivity(network))

#Centrality measures on MST-level
centralities_MST <- matrix(c(degree(mtree), strength(mtree, weights = 1/E(mtree)$weight), betweenness(mtree), closeness(mtree), 
                         page_rank(mtree, weights = 1/E(mtree)$weight)$vector, transitivity(mtree, type = "local")), nrow=length(V(mtree)), ncol = 6,
                       dimnames = list(V(mtree)$name, c("Degree", "Strength" ,"Betweenness", "Closeness", "Eigenvector","Clustering Coefficient")))
values_MST <- calculateTopX(centralities_MST, 10)

#Global clustering coefficient
print(transitivity(mtree))

write.table(values_NW, file = "results_network_new_approach_entire_time_frame.csv", append = TRUE)
write.table(values_MST,file = "results_network_new_approach_entire_time_frame.csv", append = TRUE)

#=================================== Modularization for time intervals ======================================================
#Calculate Statistics for specific day if needed
#collectNetworkStatisticsPerDay(submissions,"2021-01-28")

#Calculate all statistics for all days (Runtime ~ 1,5h)
metrics <- storeNetworkStatistics(submissions, ticker_occ_dup)

#Write metrics to file as backup
for(i in 1:13){
  write.csv(metrics[[i]], file = sprintf("metrics_%s.csv", i))
  print(sprintf("Completed: %s of 13", i))
}
#=================================== Sliding window ======================================================
#Plots and saves weighting function
ggplot(data.frame(x = c(1:20), y = ((c(1:20))/20)^2), aes(x, y)) +
  geom_point(fill="#d46d06",  size = 2, shape=21, alpha=0.5) + labs(x = "Day", y = "Weight") +
  descriptiveTheme + gridTheme
ggsave("Weighting_function.pdf", width = 11.69, height = 8.27, units = "in")

#Determine the ranking for all metrics. Here only 11 metrics are considered because the clustering coefficient on
#the minimal spanning tree is always 0 for all nodes.
agg_metrics <- list(c())
agg_rankings <- list(c())
for (iterator in 1:11) {#(Runtime ~ 8h)
  agg_metrics[[iterator]] <- slidingWindow(metrics[[iterator]], 20)
  print(sprintf("Completed Sliding Window: %s of 11", iterator))
  agg_rankings[[iterator]] <- dailyRanking(agg_metrics[[iterator]])
  print(sprintf("Completed Daily Ranking: %s of 11", iterator))
}
