##
# Fabian Okeke
# Aug 6th, 2014
##

# clear command line
clear <- function() {
  cat("\014")
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

# obtain types of data in each col
get_col_classes <- function(dataframe) {
    initial <- dataframe[100,]
    classes <- sapply(initial, class)
    classes
}

#get date matches for both two dataframes
get_date_match <- function(df1, df2, col) {
  isMatch <- df1[,col] %in% df2[,col]
  df1[isMatch,]
}

# retrieve matching date and matching timestamp in same and reverse orders
get_matching_data <- function() {
 
  # combine their place data
  dfx <- merge(andy.rPlace.cPlace, deb.rPlace.cPlace, all=T, by = 'date')
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
  
  # merge LSM with match.AndyDeb
  # and match both data by date
  dfx <- merge(lsm, match.AndyDeb, all=T, by='date')
  dfx <- dfx[complete.cases(dfx),]
  assign("match.lsmAndyDeb", dfx, envir = .GlobalEnv)
  
}

# load deb's data from JSON to dataframe format
load_andy <- function() {
  print("Loading data for andy...")
  jsonAndy <- fromJSON("datasets/andy_moves.json")
  
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
  load_andy()
  load_deb()
  load_lsm()
}

# load andy's data from JSON to dataframe format
# TO-DO: change magic numbers to variables, 
#        change deb.onlydate retrieval form
load_deb <- function() {
  print("Loading data for deb...")
  jsonDeb <- fromJSON("datasets/deborah_moves.json")
  
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
load_lsm <- function() {
  lsm_file = "~/dev/r/datasets/deborah.estrin-changun.tw.stats.csv"
  df <- read.csv(lsm_file)
  
  # drop some cols
  # rename one col
  # change 'date' format from %m/%d/%Y to POSIX format 
  # change date type from 'POSIXlt' to 'char'
  df <- subset(df, select = -c(time, X.1wk.break.))
  df <- rename(df, c("readable.time"="date"))
  df$date <- strptime(df$date, "%m/%d/%Y")
  df$date <- as.character(df$date)
  
  assign("lsm", df, envir = .GlobalEnv)
}

# draw different graphs/charts
load_visuals <- function() {
  #BAR GRAPH

  
  #LINE GRAPH
  # Change color of both line and points
  # Change line type and point type, and use thicker line and larger points
  # Change points to circles with white fill
  graph <-ggplot(data=match.lsmAndyDeb, aes(x=date, y=duration, group=1)) + 
              geom_line(colour="red", linetype="dotted", size=0.5) + 
              geom_point(color="black", size=4, shape=21, fill="white") +
              xlab("Date together in same location") + 
              ylab("Duration (No of hours)") +
              ggtitle("Date vs Duration for Andy & Deborah")
  
  ggplot(data=match.lsmAndyDeb, aes(x=intensifier_fraction_A, y=duration, colour=date)) +
        geom_line(size=1.5) +
        geom_point() +
        xlab("Date together in same location") + 
        ylab("Duration (No of hours)") +
        ggtitle("Andy vs Deborah")          
  
  assign("graph.together", graph, envir=.GlobalEnv)  
  graph

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
  print("Getting matching rows for Deb & Andy...")
  get_matching_data() 
  load_visuals()
  print("Done! Ready to roll.")
}
