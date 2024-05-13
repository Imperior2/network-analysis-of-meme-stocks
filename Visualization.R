#@Author Marvin Kruber
library("ggtext")
library("viridis")
############################# Layout ########################################################################################
#Used for Gantt-Charts
myTheme <- theme(
  #Styles axis
  plot.title = element_text(colour = "black", size = 15, face = "bold"),
  axis.title.x = element_text(colour = "black", size = 12, face = "bold"),
  axis.title.y = element_text(colour= "black", size = 12, face = "bold"),
  axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, colour = "grey15", size = 8),
  axis.text.y = element_text(colour = "grey15", size = 8 ),
  legend.title = element_text(colour = "black",  size = 12, face = "bold"),
  legend.text = element_text(colour = "grey15", size = 8, face = "bold"),
  # #Style panel
  panel.background = element_rect(fill = "white", colour = "grey65", linewidth  = 1.3, linetype = "solid"),
  panel.grid.major.x = element_line(linewidth = 0.4, colour = "gray40", linetype = "solid" ), 
  panel.grid.major.y = element_line(linewidth = 0.4, colour = "grey85", linetype = "solid" ), 
  panel.grid.minor.x = element_line(linewidth = 0.1, colour = "gray70", linetype = "dotted"), 
  panel.grid.minor.y = element_blank(),
  strip.background = element_blank(),
  strip.text = element_textbox(
    size = 12, face = "bold",
    fill = "white", box.color = "white", color = "red4",
    halign = .5, linetype = 1, r = unit(0, "pt"), width = unit(1, "npc"),
    padding = margin(2, 0, 1, 0), margin = margin(0, 1, 3, 1),
  )
)

################################## Functions ################################################################################
#Plots stock metric for a passed set of tickers. A time interval can be optionally defined.
# df - data frame which contains the  values of the node metrics for all time periods
# ticker_list - vector of tickers which shall be plotted
# metric_name - name of the metric (used as title in the plot)
# start_date - OPTIONAL: Start of a specific time frames (DEFAULT: NA -> no limitation)
# end_date - OPTIONAL: End of a specific time frames (DEFAULT: NA -> no limitation)
plot_stock_metrics <- function(df, ticker_list, metric_name,  start_date=NA, end_date=NA){
  # Convert data frame to needed data format containing only the selected tickers
  temp <- df %>% filter(Ticker %in% ticker_list)
  temp <- temp[match(ticker_list, temp$Ticker),]
  values <- invertMetrics(temp)
  values <- values %>% melt(id.vars = 'Date', variable.name = 'Stock')
  
  #Plot diagram
  aes_mapping <- aes(x = Date, y = (if(metric_name == "Eigenvector Centrality" || metric_name == " Local Clustering Coefficient") value else log(value)), 
                     colour = Stock)
  
  pl <- ggplot(values, aes_mapping) +
    geom_line() +
    #ggtitle(metric_name) +
    xlab("Date") +
    ylab((if(metric_name == "Eigenvector Centrality" || metric_name == "Local Clustering Coefficient") metric_name else paste("(Logarithmic) ", metric_name))) + 
    scale_color_viridis(option = "plasma", discrete = TRUE) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year", date_minor_breaks = "3 month", limits = c(as.Date(start_date), as.Date(end_date))) +
    theme(legend.position = "none") +
    myTheme
  
  if(length(ticker_list) > 3) {
    pl <- pl +
      facet_wrap(~ Stock, dir = "v", scales = "free", ncol=2)
  }
    print(pl)
}

#This function determines how long each stock holds its position in the ranking.
#It considers the temporal order, i.e. position switches are depicted.
# ranking - result of dailyRanking()
# returns ranking in long format [Stock | Position | Start | End]
determineRankDurations <- function(ranking) {
  temp <- data.frame(t(ranking)[-1,])
  colnames(temp) <- ranking$Position
  temp["Date"] <- as.Date(rownames(temp))
  temp <- temp %>% select(Date, everything())
  #temp <- temp %>% replace(is.na(.), 0)
  
  #Bring to long format and ensure that Date is treated correctly.
  temp <- temp %>% melt(id.vars = 'Date', variable.name = 'Position') %>% 
    select(Date, Position, Stock = value)
  temp$Date <- as.Date(temp$Date)
  
  #Determine time frames
  temp <- aggregateRanking(temp)
  
  return(temp)
}

#This function determines the time frame in which a stock holds its position in the ranking.
# ranking - long format of a daily ranking, i.e. [Date | Position | Stock]
# returns ranking in long format [Stock | Position | Start | End]
aggregateRanking <- function(ranking) {
  #Ensures required order
  temp <- ranking %>% arrange(Position, Date)
  temp$GROUP_ID <- rep(0, length(ranking$Stock))
  
  #All entries of with the same Ticker and the same POSITION have an equal GROUP_ID
  id <- 0
  prev_stock <- ""
  prev_pos <- 0
  
  #A simple group_by would not work because it would ignore the temporal order.
  for(row in 1:length(temp$Stock)){
    pos <- temp[row,2]
    stock <- temp[row,3]
    
    if(is.na(stock)) {
      next 
    }
    
    # If the Ticker changes or the position than increase the GROUP_ID and update other variables
    if(!(stock == prev_stock && pos == prev_pos)) {
      id <- id + 1
      prev_stock <- stock
      prev_pos <- pos
    }
    
    temp[row, 4] <- id
  }
  #Determine Start date and End date per group
  temp <- temp %>% group_by(GROUP_ID, Position, Stock) %>% summarize(Start = as.Date(min(Date)), End = as.Date(max(Date)))
  
  #End of measurements
  max_date <- as.Date(max(temp$Start) + 1)
  
  #Use the start date of the following entry as the end date of the current entry.
  #This has to be done position wise. Otherwise there would be a temporal bias due to the  
  #last entry with position x and the first entry with position y.
  for(pos in 1:prev_pos){
    indices <- which(temp$Position == pos)
    min_inx <- min(indices)
    max_inx <- max(indices)
    temp$End[min_inx:max_inx] <- c(temp$Start[(min_inx + 1):max_inx], max_date)
  }
  return(temp)
}
#Plots a Gantt-Chart for a given daily_ranking.
# daily_ranking - daily ranking of one metrics in long-format
# candidates - list of stocks which shall be visualized (LIMITATION is necessary)
# measure - used for the title of the diagram
# start_date - string of format YY-MM-DD
# end_date - string of format YY-MM-DD
# MODE - indicates whether a facet wrap per year (=1), per stock (=2) or none (=0)  should be conducted
visualizeGanttChart <- function(daily_ranking_long, candidates, measure, start_date, end_date, MODE = 1){
  df <- daily_ranking_long %>% filter(Stock %in% candidates)
  
  #Safety check
  MODE <- if(format(as.Date(start_date), "%Y") == format(as.Date(end_date), "%Y")) 0 else MODE
  
  gantt <- df %>% filter(Start >= as.Date(start_date) && Start <= as.Date(end_date)) %>% ggplot(aes(x=Start, xend=End,  y=Position, yend=Position, color = Stock)) +
    geom_segment(linewidth=5) +
    ggtitle(measure) +
    xlab("Date") +
    ylab("Position") + 
    scale_color_viridis(option = "plasma", discrete = TRUE) +
    myTheme
  
  gantt <- if (MODE == 1) (gantt + 
    facet_wrap(.~format(Start,"%Y"), scales = "free_x", ncol=2) +
    scale_x_date(date_labels = "%m", date_breaks = "1 month") #+scale_x_date(date_labels = "%d-%m")
    
    )  else if (MODE == 2) (gantt + 
                      facet_wrap(~ Stock, dir = "v", scales = "free", ncol=2)  +
                      scale_x_date(date_labels = "%Y", date_breaks = "1 year", date_minor_breaks = "3 month", limits = c(as.Date(start_date), as.Date(end_date))) +
                      theme(legend.position = "none")
    ) else (gantt +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year", date_minor_breaks = "1 month", limits = c(as.Date(start_date), as.Date(end_date))))
  
  print(gantt)
  #ggsave("GanttChart.pdf", width=8, height=6, units=c("in"), dpi=300)
}
#Plots a Gantt-Chart for all "Position 1"-Stocks of a given daily_ranking for the specified frame.
# daily_ranking - daily ranking of one metrics in long-format
# candidates - list of stocks which shall be visualized (LIMITATION is necessary)
# measure - used for the title of the diagram
# start_date - string of format YY-MM-DD
# end_date - string of format YY-MM-DD
# minimal_duration - minimal number of days on rank 1 (=> used for filtering the stock)
visualizeNumberOneChanges <- function(daily_ranking_long, candidates, measure, start_date, end_date, minimal_duration){
  df <- daily_ranking_long %>% filter(Position == 1)
  
  #Choose only stocks wich hold Position 1 for at least minimal duration
  durations_per_stock <-  df %>% mutate(Stock, TimeDiff = as.numeric(difftime(End, Start, units="days"))) %>% 
    group_by(Stock) %>% 
    summarise(Duration = sum(TimeDiff)) %>%
    filter(Duration >= minimal_duration)
  df <- df %>% filter(Stock %in% durations_per_stock$Stock)
  
  df <- df %>% mutate(Is_Top_10 = if(Stock %in% candidates) TRUE else FALSE)                                  
  
  gantt <- df %>% ggplot() +
    geom_segment(aes(x=Stock, xend=Stock,  y=Start, yend=End, color = Is_Top_10), linewidth=5) +
    myTheme +
    ggtitle(measure) +
    labs(y = "Date", color = "Top 10\nCandidate") +
    scale_y_date(date_labels = "%Y", date_breaks = "1 year", date_minor_breaks = "3 month", limits = c(as.Date(start_date), as.Date(end_date))) +
    scale_color_manual(values=c("purple4", "red1")) +
    theme(panel.grid.minor.y = element_line(linewidth = 0.1, colour = "gray70", linetype = "dotted"))
  
  print(gantt)
}

#This functions automatically creates the Gantt-Charts for each metric as well as the Nr_One_Changes_Chart
#and stores them as pdf.
# list_daily_rankings - list with ranking data (i.e. agg_rankings)
# values_network - matrix which contains the Top X candidates in the network over the entire time 
# values_mst - matrix which contains the Top X candidates in the minimal spanning tree over the entire time
createAndSaveGanttChartsAndNrOneChanges <- function (list_daily_rankings, values_network, values_mst){
  counter <- 1
  measures <- c("Degree Centrality", "Strength", "Betweenness Centrality", "Closeness Centrality", "Eigenvector Centrality", "Local Clustering Coefficient")
  for (measure in measures){
    rank_long <- determineRankDurations(list_daily_rankings[[2*counter-1]])
    
    #Plot ranking and nr one changes of network
    visualizeGanttChart(rank_long, values_network[, 2*counter], paste(measure, " (Network)"), "2012-01-01", "2022-09-01", 2)
    ggsave(gsub(" ", "_",sprintf("%s_Ranking_Network.pdf", measure)), height = 11.69, width = 8.27, units = "in")
    
    visualizeNumberOneChanges(rank_long, values_network[, 2*counter], paste(measure, " (Network)"), "2008-01-01", "2022-09-01", 20)
    ggsave(gsub(" ", "_",sprintf("%s_Nr_one_Changes_Network.pdf", measure)), height = 11.69, width = 8.27, units = "in")
    
    #There is no 12th entry
    if (counter == 6) {
      break
    }
    
    #Plot ranking and nr one changes of minimal spanning tree
    rank_long <- determineRankDurations(list_daily_rankings[[2*counter]])
    visualizeGanttChart(rank_long, values_mst[, 2*counter], paste(measure, " (MST)"), "2012-01-01", "2022-09-01", 2)
    ggsave(gsub(" ", "_",sprintf("%s_Ranking_MST.pdf", measure)), height = 11.69, width = 8.27, units = "in")
    
    visualizeNumberOneChanges(rank_long, values_mst[, 2*counter], paste(measure, " (MST)"), "2008-01-01", "2022-09-01", 20)
    ggsave(gsub(" ", "_",sprintf("%s_Nr_one_Changes_MST.pdf", measure)), height = 11.69, width = 8.27, units = "in")
    
    counter <- counter + 1
  }
}
####################################### Application #########################################################################
#Stocks from Michele Costola, Matteo Iacopini, and Carlo RMA Santagiustina. “On the “mementum of meme stocks”. In: Economics Letters 207 (2021), p. 110021.
counter <- 1
measures <- c("Degree Centrality", "Strength", "Betweenness Centrality", "Closeness Centrality", "Eigenvector Centrality", "Local Clustering Coefficient")
#Creates plots for each network measures at network and MST level 
for (measure in measures){
  #Plot metric of graph
  plot_stock_metrics(metrics[[2*counter-1]], c("GME", "AMC", "KOSS","MCO", "PFE", "DIS"), measure, start_date = "2015-01-01")#, start_date = "2017-01-01"
  ggsave(gsub(" ", "_", sprintf("%s_Network.pdf", measure)), height = 11.69,  width = 8.27, units = "in")
  #Plot metric of MST
  plot_stock_metrics(metrics[[2*counter]], c("GME", "AMC", "KOSS","MCO", "PFE", "DIS"), measure, start_date = "2015-01-01")#, start_date = "2017-01-01"
  ggsave(gsub(" ", "_", sprintf("%s_MST.pdf", measure)), height = 11.69,  width = 8.27, units = "in")
  
  counter <- counter + 1
}

# Plot global clustering coefficient on a daily base over the entire period
print(ggplot(metrics[[13]], aes(x = as.Date(Day), y = as.numeric(Global_CC))) +
        geom_point(fill="#d46d06",  size = 2, shape=21, alpha=0.5) + labs(x = "Date", y = "Global Clustering Coefficient") +
        scale_x_date(date_labels = "%Y", date_breaks = "1 year", date_minor_breaks = "3 month", limits = c(as.Date("2007-01-01"), as.Date("2022-09-01"))) +
        scale_y_continuous(breaks=seq(0,1, by=0.1)) +
        descriptiveTheme + gridTheme)
ggsave("Global_CC.pdf", width = 11.69, height = 8.27, units = "in")

#Show diagram of ticker_per_day_dup and the daily number of submissions with multiple tickers ( => Explains global Clustering Coefficient)
dup_subs_per_day <- unique(submissions_mult[,c("id", "NY_Trading_Day")]) %>% count(NY_Trading_Day) %>% select(Date = NY_Trading_Day, Nr = n)
print(ggplot(tickers_per_day_dup, aes(x = NY_Trading_Day, y = log(Tickers_per_Day))) +
        geom_point(fill="#d46d06",  size = 2, shape=21, alpha=0.5) + labs(x = "Date", y = "Number of Tickers per Day (logarithmic)") +
        scale_x_date(date_labels = "%Y", date_breaks = "1 year", date_minor_breaks = "3 month", limits = c(as.Date("2007-01-01"), as.Date("2022-09-01"))) +
        scale_y_continuous(breaks=seq(0,11, by=1)) +
        descriptiveTheme + gridTheme)

#Illustrates the daily number of submission from the duplicate set
print(ggplot(dup_subs_per_day, aes(x = Date, y = log(Nr))) +
        geom_point(fill="#d46d06",  size = 2, shape=21, alpha=0.5) + labs(x = "Date", y = "Number of Submissions per Day (logarithmic)") +
        scale_x_date(date_labels = "%Y", date_breaks = "1 year", date_minor_breaks = "3 month", limits = c(as.Date("2007-01-01"), as.Date("2022-09-01"))) +
        scale_y_continuous(breaks=seq(0,11, by=1)) +
        descriptiveTheme + gridTheme)
ggsave("Dup_Subs_Per_Day.pdf", width = 11.69, height = 8.27, units = "in")
#---------------------------------------- Gantt-Charts --------------------------------------------------------------------
createAndSaveGanttChartsAndNrOneChanges(agg_rankings, values_NW, values_MST)