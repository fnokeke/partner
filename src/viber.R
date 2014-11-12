#####FUNCTIONS#########
# clean up date to specific format
format_date <- function(df) {
  df <- strptime(as.character(df), "%d/%m/%Y")
}

####### viber message analysis#######
file_path = "Dev/partner/datasets/viber/Fabian.csv"
col_names = c("Date", "Time", "Sender", "PhoneNo", "Msg")

# extract message for both 
vid_fab.df <- read.csv2(file_path, header=F, sep=",")
names(vid_fab.df) <- col_names

# extract message by only Vid
# keep only date and msg columns
vid.df <- vid_fab.df[vid_fab.df$Sender =="Me",]
vid.msg <- vid.df[,c("Date", "Msg")]
vid.msg$Date <- format_date(vid.msg$Date)

# extract message by only Fab
# keep only date and msg columns
fab.df <- vid_fab.df[vid_fab.df$Sender =="Fabian",]
fab.msg <- fab.df[,c("Date", "Msg")]
fab.msg$Date <- format_date(fab.msg$Date)

# write out msgs of both to files
fab_msg_file = "Dev/partner/datasets/viber_cleaned/fab.msg.csv"
vid_msg_file = "Dev/partner/datasets/viber_cleaned/vid.msg.csv"
write.csv2(fab.msg$Msg, fab_msg_file, row.names=F)
write.csv2(vid.msg$Msg, vid_msg_file, row.names=F)

# use cut() to split messages into break periods
ff <- as.Date(fab.msg$Date)
max_date <- max(ff)
min_date <- min(ff)
break_period <- seq(min_date, max_date+7, by = "1 week")
new_col <- cut(ff, breaks=break_period, include.lowest=TRUE)
fab.msg$wk_date <- new_col
fab.msg$Msg <- as.character(fab.msg$Msg)

# use cut() to split messages into break periods
vv <- as.Date(vid.msg$Date)
max_date <- max(vv)
min_date <- min(vv)
break_period <- seq(min_date, max_date+7, by = "1 week")
new_col <- cut(vv, breaks=break_period, include.lowest=TRUE)
vid.msg$wk_date <- new_col
vid.msg$Msg <- as.character(vid.msg$Msg)

# summarize and add up messages belonging to a date range
library(plyr)

#fab
fab.summary <- ddply(fab.msg, 
                     .(wk_date), 
                     summarize,  
                     No_of_Msgs=length(wk_date), 
                     comboMsg=paste(unlist(Msg), collapse ="--"))

#vid
vid.summary <- ddply(vid.msg, 
                     .(wk_date), 
                     summarize, 
                     No_of_Msgs=length(wk_date), 
                     comboMsg=paste(unlist(Msg), collapse ="--"))


# write out msgs of both to files
fab_summary_file = "Dev/partner/datasets/viber_cleaned/fab.summary.csv"
vid_summary_file = "Dev/partner/datasets/viber_cleaned/vid.summary.csv"
write.csv2(fab.summary$comboMsg, fab_summary_file, row.names=F)
write.csv2(vid.summary$comboMsg, vid_summary_file, row.names=F)