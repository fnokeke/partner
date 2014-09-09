################
# Fabian Okeke
# Aug 6th, 2014
#
################

#
# NB: call setup() to get started
#

# random function just because it's like unix clear() command
clear <- function() {
  cat("\014")
}

# create all global variables needed
create_globals <- function() {
  assign("g.all_df", NULL, envir=.GlobalEnv)
  assign("g.all_graphs", NULL, envir=.GlobalEnv)
  assign("g.lsm", NULL, envir=.GlobalEnv)
}

# plot graph of given dataframe
draw_chart <- function(df_wt_lsm) {
  graph <- ggplot(data=df_wt_lsm, aes(x=cumm_duration)) +
    #geom_line(aes(y=personal_diff, color="personal")) + 
    geom_line(aes(y=intensifier_diff, color="intensifier")) 
  geom_line(aes(y=lexical_diff, color="lexical")) +
    xlab("Cummulative hours spent over weeks") + 
    ylab("LSM Value")  
  
  # update global graphs variable
  all_graphs <- c(g.all_graphs, graph)
  assign("g.all_graphs", all_graphs, envir=.GlobalEnv)
  print(graph)
}

# TO-DO: clean up this function
factor_to_char <- function(dataframe) {
  indices <- sapply(dataframe, is.factor)
  dataframe[indices] <- lapply(dataframe[indices], as.character)
  dataframe
}

# clean up date to specific format
# remove 'T' that comes befoer time
# eliminate the last part of date with .000-04:00 or .999-04:00
# change to POSIXct date format
format_date <- function(df, removeTime=FALSE) {
  if (removeTime) {
    df <- strtrim(df, 10)
  } 
  else {
    df <- sapply(df, gsub, pattern="T", replacement=" ")  
    df <- sapply(df, gsub, pattern=".000-04:00", replacement="")  
    df <- sapply(df, gsub, pattern=".999-04:00", replacement="")    
  }
  
  df <- as.POSIXct(df)
  df
}

#convert list to dataframe
list_to_df <- function(your_list) {
  do.call(rbind.data.frame, your_list)
}

# return list of all dataframes
get_all_df <- function() {
  return(g.all_df)
}

# obtain types of data in each col
get_col_classes <- function(dataframe) {
    initial <- dataframe[100,]
    classes <- sapply(initial, class)
    classes
}


get_col_names <- function(df, vec_list) {
  name <- deparse(substitute(df))
  vec <- paste(name, vec_list, sep=".")
  return(vec)
}

#get date matches for both two dataframes
get_date_match <- function(df1, df2, col) {
  isMatch <- df1[,col] %in% df2[,col]
  df1[isMatch,]
}

# retrieve matching date and matching timestamp in same and reverse orders
get_matching_data <- function() {
 
  # combine their place data
  dfx <- merge(andy.rPlace.cPlace, deb.rPlace.cPlace, all=T, by='date')
  dfx <- dfx[complete.cases(dfx),]
  
  #update col names to better represent their values
  names(dfx) <- c( "date", "andy.start", "andy.end", "andy.placeid", "andy.lat", "andy.lon", 
                   "deb.start", "deb.end", "deb.placeid", "deb.lat", "deb.lon")
  
  # get only rows where their locations are relatively close
  diff_limit <- 0.001
  dfx <- dfx[dfx$andy.lat - dfx$deb.lat <= diff_limit &
             dfx$andy.lon - dfx$deb.lon <= diff_limit,]
  
  # get only rows where their time at same place intersects for each day 
  dfx <- dfx[ !(dfx$andy.start > dfx$deb.end) &
              !(dfx$andy.end < dfx$deb.start),]
  
  # find the actual time intersection 
  # round off duration of intersection in hours (divide by 3600)
  # change duration format from 'difftime' to 'numeric'
  dfx$startIntersect <- pmax(dfx$andy.start, dfx$deb.start)
  dfx$endIntersect   <- pmin(dfx$andy.end, dfx$deb.end)
  dfx$duration       <- (dfx$endIntersect - dfx$startIntersect) / 3600
  dfx$duration       <- round(dfx$duration, 2)
  dfx$duration       <- as.numeric(dfx$duration)

  # select specific columns and reshape dataframe
  dfx <- dfx[,c("date", "startIntersect", "endIntersect", "duration")]
  dfx <- ddply(dfx, .(date), summarize, duration=sum(duration))
  dfx$date <- as.character(dfx$date)
  
  #create global variable
  assign("match.AndyDeb", dfx, envir = .GlobalEnv)

  # lsm is weekly while match.AndyDeb is daily so
  # use cut() to combine both dates
  dd <- as.Date(match.AndyDeb$date)
  max_date <- max(dd)
  break_period <- c(as.Date(lsm$date), max_date)
  new_col <- cut(dd, breaks=break_period, include.lowest=TRUE)
  match.AndyDeb$lsm_date <- new_col
  
  # summarize and add up duration(hours) belonging to a date range
  # rename col so merge can be done on date
  # merge and re-order col names
  dfx <- ddply(match.AndyDeb, .(lsm_date), summarize, duration=sum(duration))
  names(dfx)[names(dfx) == 'lsm_date'] <- 'date'
  dfx <- merge(lsm, dfx)
  
  # add new columns then reorder
  dfx$intensifier_diff  <- abs(dfx$intensifier_fraction_A - dfx$intensifier_fraction_B)
  dfx$personal_diff     <- abs(dfx$personal_fraction_A - dfx$personal_fraction_B)
  dfx$lexical_diff      <- abs(dfx$lexical_density_A - dfx$lexical_density_B)
  dfx$iqv_diff          <- abs(dfx$iqv_A - dfx$iqv_B)
  dfx$entropy_diff      <- abs(dfx$entropy_A - dfx$entropy_B)
  dfx$cumm_duration     <- cumsum(dfx$duration)
  dfx <- dfx[,c("date", "cumm_duration", "personal_diff", "intensifier_diff", "lexical_diff", 
                "iqv_diff", "entropy_diff", "counts_A", "counts_B")]
  assign("match.lsmAndyDeb", dfx, envir = .GlobalEnv)
  
}


# retrieve matching date and matching timestamp in same and reverse orders
get_matched_df <- function(df1, df2, lsm_df=g.lsm) {
  
  # combine their place data
  merged.df <- merge(df1, df2, all=T, by='date')
  merged.df <- merged.df[complete.cases(merged.df),]
  
  #update col names to better represent their values
  vec = c("start", "end", "placeid", "lat", "lon")
  df1.vec = get_col_names(df1, vec)
  df2.vec = get_col_names(df2, vec)
  names(merged.df) <- c( "date", df1.vec, df2.vec)
  
  #TO-DO: get rid of magic numbers
  # get only rows where their locations are relatively close
  # df1.lat - df2.lat <= diff_limit
  # df1.lon - df2.lon <= diff_limit
  diff_limit <- 0.001
  merged.df <- merged.df[ abs( merged.df[,5] - merged.df[,10] ) <= diff_limit &
                          abs( merged.df[,6] - merged.df[,11] ) <= diff_limit,]
  
  # get only rows where their time at same place intersects for each day 
  #dfx <- dfx[ !(dfx$andy.start > dfx$deb.end) &
  #              !(dfx$andy.end < dfx$deb.start),]
  merged.df <- merged.df[ !(merged.df[,2] > merged.df[,8]) &
                          !(merged.df[,3] < merged.df[,7]),]
  
  # find the actual time intersection 
  # round off duration of intersection in hours (divide by 3600)
  # change duration format from 'difftime' to 'numeric'
  merged.df$startIntersect <- pmax( merged.df[,2], merged.df[,7] )
  merged.df$endIntersect   <- pmin( merged.df[,3], merged.df[,8] )
  merged.df$duration       <- (merge.df$endIntersect - merged.df$startIntersect) / 3600
  merged.df$duration       <- round(merged.df$duration, 2)
  merged.df$duration       <- as.numeric(merged.df$duration)
  
  # select specific columns and reshape dataframe
  merged.df <- merged.df[,c("date", "startIntersect", "endIntersect", "duration")]
  merged.df <- ddply(dfx, .(date), summarize, duration=sum(duration))
  
  # TO-DO: REMOVE
  # REDUNDANT
  merged.df$date <- as.character(merged.df$date)
  
  
  #create global variable
  #NO NEED
  #assign("match.AndyDeb", dfx, envir = .GlobalEnv)
  
  # lsm is weekly while match.AndyDeb is daily so
  # use cut() to combine both dates
  #dd <- as.Date(match.AndyDeb$date)
  #max_date <- max(dd)
  #break_period <- c(as.Date(lsm$date), max_date)
  #new_col <- cut(dd, breaks=break_period, include.lowest=TRUE)
  #match.AndyDeb$lsm_date <- new_col
  
  # TO-DO: REMOVE
  # REDUNDANT
  dd <- as.Date(merged.df$date)
  
  # COMBINE MERGED DATA WITH LSM
  max_date <- max(dd)
  break_period <- c(as.Date(lsm_df$date), max_date)
  new_col <- cut(dd, breaks=break_period, include.lowest=TRUE)
  merged.df$lsm_date <- new_col
  
  # summarize and add up duration(hours) belonging to a date range
  # rename col so merge can be done on date
  # merge and re-order col names
  merge.df.lsm <- ddply(merged.df, .(lsm_date), summarize, duration=sum(duration))
  dfx <- merge.df.lsm
  names(dfx)[names(dfx) == 'lsm_date'] <- 'date'
  dfx <- merge(lsm, dfx)
  
  # add new columns then reorder
  dfx$intensifier_diff  <- abs(dfx$intensifier_fraction_A - dfx$intensifier_fraction_B)
  dfx$personal_diff     <- abs(dfx$personal_fraction_A - dfx$personal_fraction_B)
  dfx$lexical_diff      <- abs(dfx$lexical_density_A - dfx$lexical_density_B)
  dfx$iqv_diff          <- abs(dfx$iqv_A - dfx$iqv_B)
  dfx$entropy_diff      <- abs(dfx$entropy_A - dfx$entropy_B)
  dfx$cumm_duration     <- cumsum(dfx$duration)
  dfx <- dfx[,c("date", "cumm_duration", "personal_diff", "intensifier_diff", "lexical_diff",  
                "iqv_diff", "entropy_diff", "counts_A", "counts_B")]
  merge.df.lsm <- dfx
  return(merge.df.lsm)
}


# load deb's data from JSON to dataframe format
load_andy <- function() {
  print("Loading data for andy...")
  jsonAndy <- fromJSON("~/dev/research/partner//datasets//andy_moves.json")
  
  # json data contains two columns: "metadata" and "data"
  # these columns are individually dataframes
  # FOCUS is on "data" dataframe as the information of 
  # "metadata" can be extracted from "data"
  assign("andy", jsonAndy[,"data"], envir = .GlobalEnv)
  
  # select only rows where type == "Place"
  assign("andy.rPlace", andy[andy$type=="Place",], envir = .GlobalEnv)
  
  # select only rows where type == "Move"
  assign("andy.rMove", andy[andy$type=="Move",], envir = .GlobalEnv) 
  
  # rows where "type" == "Place", 
  # select only "times" and "place" columns
  # display selected contents of nested dataframes
  assign("andy.rPlace.cPlace", andy.rPlace[,c("startTime", "endTime", "place")], envir = .GlobalEnv)
  assign("andy.rPlace.cPlace", 
         data.frame(
           "date"       = format_date( andy.rPlace.cPlace[,"endTime"], removeTime=TRUE ),
           "startTime"  = format_date( andy.rPlace.cPlace[,"startTime"] ), 
           "endTime"    = format_date( andy.rPlace.cPlace[,"endTime"] ) , 
           "place.id"   = andy.rPlace.cPlace[,"place"][,"id"], 
           "lat"        = andy.rPlace.cPlace[,"place"][,"location"][,"lat"],
           "lon"        = andy.rPlace.cPlace[,"place"][,"location"][,"lon"] 
         ), 
         envir = .GlobalEnv)
  
  # rows where "type" == "Place", 
  # select only col=="activities"
  assign("andy.rPlace.cActivities", andy.rPlace[,"activities"], envir = .GlobalEnv)
  
  # rows where "type" == "Move", 
  # select only col=="activities"
  assign("andy.rMove.cActivities", andy.rMove[,"activities"], envir = .GlobalEnv)
  
  print ("andy dataframe is now available.")
}

# load all data
load_data <- function() {
  create_globals()
  load_moves("~/dev/research/partner//datasets//latest//all_moves_files.txt")
  load_lsm("~/dev/r/datasets/deborah.estrin-changun.tw.stats.csv")
}

# load andy's data from JSON to dataframe format
# TO-DO: change magic numbers to variables, 
#        change deb.onlydate retrieval form
load_deb <- function() {
  print("Loading data for deb...")
  jsonDeb <- fromJSON("~/dev/research/partner//datasets//deborah_moves.json")
  
  #
  #TO-DO: get rid of magic row_range hack
  #
  row_range <- 1060:2701  
  
  # json data contains two columns: "metadata" and "data"
  # these columns are individually dataframes
  # FOCUS is on "data" dataframe as the information of 
  # "metadata" can be extracted from "data"
  assign("deb", jsonDeb[row_range,"data"], envir = .GlobalEnv)
  
  # select only rows where type == "Place"
  assign("deb.rPlace", deb[deb$type=="Place",], envir = .GlobalEnv)
  
  # select only rows where type == "Move"
  assign("deb.rMove", deb[deb$type=="Move",], envir = .GlobalEnv) 
  
  # rows where "type" == "Place", 
  # select only "times" and "place" columns
  # display selected contents of nested dataframes
  assign("deb.rPlace.cPlace", deb.rPlace[,c("startTime", "endTime", "place")], envir = .GlobalEnv)
  assign("deb.rPlace.cPlace", 
         data.frame(
           "date"       = format_date( deb.rPlace.cPlace[,"endTime"], removeTime=TRUE ),
           "startTime"  = format_date( deb.rPlace.cPlace[,"startTime"] ), 
           "endTime"    = format_date( deb.rPlace.cPlace[,"endTime"] ) , 
           "place.id"   = deb.rPlace.cPlace[,"place"][,"id"], 
           "lat"        = deb.rPlace.cPlace[,"place"][,"location"][,"lat"],
           "lon"        = deb.rPlace.cPlace[,"place"][,"location"][,"lon"] 
         ), 
         envir = .GlobalEnv)
  
  # rows where "type" == "Place", 
  # select only col=="activities"
  assign("deb.rPlace.cActivities", deb.rPlace[,"activities"], envir = .GlobalEnv)
  
  # rows where "type" == "Move", 
  # select only col=="activities"
  assign("deb.rMove.cActivities", deb.rMove[,"activities"], envir = .GlobalEnv)
  
  # get date summary showing earliest time at a place 
  # and the latest time at same place for particular date
  # NB: places with same 'place.id' have same lat and lon
  dfx <- ddply(deb.rPlace.cPlace, .(date, place.id, lat, lon), summarize, min=min(startTime), max=max(endTime))
  assign("deb.dateSummary", dfx, envir = .GlobalEnv)
  
  print ("deb dataframe is now available.")
}

# load necessary libraries
load_library <- function() {
  require(ggplot2)
  require(jsonlite)
  require(plyr)
  require(reshape2)
}

# TO-DO: have all file names saved in a different file
# then import that file
# load lsm data
# change date format from 'mo/day/yr' to 'yr-mo-day' 
load_lsm <- function(location) {
  df <- read.csv(location)
  assign("lsm_orig", df, envir = .GlobalEnv)
  
  # drop some cols
  # rename one col
  # change 'date' format from %m/%d/%Y to POSIX format 
  # change date type from 'POSIXlt' to 'char'
  df <- subset(df, select = -c(time, X.1wk.break.))
  df <- rename(df, c("readable.time"="date"))
  df$date <- strptime(df$date, "%m/%d/%Y")
  df$date <- as.character(df$date)
  
  #update global variable
  assign("g.lsm", df[85:110,], envir=.GlobalEnv)
}

# load moves data files
# read.table() used so comments in file are ignored
# convert format read to a list with their corresponding names
load_moves <- function(location) {
  file_list <- read.table(location)
  file_list <- as.character(file_list$V1)
  
  # create dataframes of each of the files
  lapply(file_list, FUN=make_df_from_json)
}

# draw different graphs/charts
load_visuals <- function() {
  graph.bar <-ggplot(data=match.lsmAndyDeb, aes(x=cumm_duration, y=personal_diff, color=date)) + 
      geom_bar(stat = "identity")#+ 
      #xlab("Date together in same location") + 
      #ylab("Cummulative hours spent)") +
      ggtitle("Andy & Deborah")
            
  # Change color of both line and points
  # Change line type and point type, and use thicker line and larger points
  # Change points to circles with white fill
  graph.line1 <-ggplot(data=match.lsmAndyDeb, aes(x=cumm_duration, y=personal_diff, group=1)) + 
      geom_line(colour="blue", linetype="dotted", size=0.5) + 
      geom_point(color="black", size=4, shape=21, fill="white") 
      #xlab("Date together in same location") + 
      #ylab("Duration (No of hours)") +
      #ggtitle("Date vs Personal_dff for Andy & Deborah")

  graph.line2 <-ggplot(data=match.lsmAndyDeb, aes(x=cumm_duration, y=intensifier_diff, group=1)) + 
    geom_line(colour="red", linetype="dotted", size=0.5) + 
    geom_point(color="black", size=4, shape=21, fill="white")        
  
  graph.line3 <-ggplot(data=match.lsmAndyDeb, aes(x=cumm_duration, y=lexical_diff, group=1)) + 
    geom_line(colour="red", linetype="dotted", size=0.5) + 
    geom_point(color="black", size=4, shape=21, fill="white")        
  
  multiplot(graph.line1, graph.line2, graph.line3, col=1)
}

# make global dataframe variable from json file path
# so name of df made will be formed from file path 
make_df_from_json <- function(file_path) {
  cat("Loading data from", file_path, "...")
  name <- substrRight(file_path, 11)
  json_data <- fromJSON(file_path)
  
  # json data contains two columns: "metadata" and "data"
  # these columns are individually dataframes
  # FOCUS is on "data" dataframe as the information of 
  # "metadata" can be extracted from "data"
  # only specific cols of dataframe are currently needed
  # so shrink dataframe to produce only needed parts
  dfx <- json_data[, "data"]
  dfx <- shrink_df(dfx)
  assign(name, dfx, envir=.GlobalEnv)
  cat(name, "dataframe now exists.\n")
  
  #update global list of all df
  all_df <- c(g.all_df, name)
  assign("g.all_df", all_df, envir=.GlobalEnv)
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#change to workspace directory
set_dir <- function() {
  dir = getwd()
  setwd(dir)
  print("Working directory changed.")
}

# prepare the environment
setup <- function() {
  clear()
  print("Welcome. Setting up environment...")
  set_dir()
  load_library()
  load_data()
  
  #TO-DO
  #mm <- load_moves()
  #lsm <- load_lsm()
  #graph(mm,lsm)
  
  all_df <- get_all_df()
  mix_matrix <- combn(all_df, 2)
  
  for (i in 1:ncol(mix_matrix)) {
    combo_df <- mix_matrix[,i]
    combo_df1 <- eval(parse(text=combo_df[1]))
    combo_df2 <- eval(parse(text=combo_df[2]))
    df_wt_lsm  <- get_matched_df(combo_df1, combo_df2)
    load_visuals(df_wt_lsm)
    paste("Loaded visuals for", combo_df[1], "vs", combo_df[2])
  }
  
  print("Done! Enjoy :)")
}

# shrink df by selecting specific cols 
shrink_df <- function(dataframe) {
  df <- dataframe
  
  # select only rows where type == "Place"
  df.rPlace <- df[df$type=="Place",]
  
  # rows where "type" == "Place", 
  # select only the two "time" and "place" columns
  # display selected contents of nested dataframes
  df.rPlace.cPlace <- df.rPlace[, c("startTime", "endTime", "place")]
  df.rPlace.cPlace <- 
        data.frame(
           "date"       = format_date( df.rPlace.cPlace[,"endTime"], removeTime=TRUE ),
           "startTime"  = format_date( df.rPlace.cPlace[,"startTime"] ), 
           "endTime"    = format_date( df.rPlace.cPlace[,"endTime"] ) , 
           "place.id"   = df.rPlace.cPlace[,"place"][,"id"], 
           "lat"        = df.rPlace.cPlace[,"place"][,"location"][,"lat"],
           "lon"        = df.rPlace.cPlace[,"place"][,"location"][,"lon"] 
         )
  
  # return shrunk dataframe  
  df <- df.rPlace.cPlace
  df
}


# substring last n characters in a string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}