#####FUNCTIONS#########
# clean up date to specific format
# convert to POSIXct to use in data frames / ddply
format_date <- function(df) {
  df <- strptime(as.character(df), "%d/%m/%Y")
  df <- as.POSIXct(df)
}

####### viber message analysis#######
partner = "Himanshu Jindal Ipad"
file_path = "Dev/partner/datasets/viber/Himanshu Jindal Ipad.csv"
col_names = c("Date", "Time", "Sender", "PhoneNo", "Msg")

# extract message for both 
vid_partner.df <- read.csv2(file_path, header=F, sep=",")
names(vid_partner.df) <- col_names

# extract message by only Vid
# keep only date and msg columns
vid.df <- vid_partner.df[vid_partner.df$Sender =="Me",]
vid.msg <- vid.df[,c("Date", "Msg")]
vid.msg$Date <- format_date(vid.msg$Date)

# extract message by only partner
# keep only date and msg columns
partner.df <- vid_partner.df[vid_partner.df$Sender ==partner,]
partner.msg <- partner.df[,c("Date", "Msg")]
partner.msg$Date <- format_date(partner.msg$Date)
if (nrow(partner.df)==0)
  stop("Cannot continue because partner row size is 0.")

# write out msgs of both to files
partner_msg_file = "Dev/partner/datasets/viber_cleaned/partner.msg.csv"
vid_msg_file = "Dev/partner/datasets/viber_cleaned/vid.msg.csv"
write.csv2(partner.msg$Msg, partner_msg_file, row.names=F)
write.csv2(vid.msg$Msg, vid_msg_file, row.names=F)

# use cut() to split messages into break periods
ff <- as.Date(partner.msg$Date)
max_date <- max(ff)
min_date <- min(ff)
break_period <- seq(min_date, max_date+7, by = "1 week")
new_col <- cut(ff, breaks=break_period, include.lowest=TRUE)
partner.msg$wk_date <- new_col
partner.msg$Msg <- as.character(partner.msg$Msg)

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

#partner
partner.summary <- ddply(partner.msg, 
                         .(wk_date), 
                         summarize,  
                         No_of_Msgs=length(wk_date), 
                         comboMsg=paste(unlist(Msg), collapse=" "))

#vid
vid.summary <- ddply(vid.msg, 
                     .(wk_date), 
                     summarize, 
                     No_of_Msgs=length(wk_date), 
                     comboMsg=paste(unlist(Msg), collapse=" "))


# write out msgs of both to files
partner_summary_file = "Dev/partner/datasets/viber_cleaned/partner.summary.csv"
vid_summary_file = "Dev/partner/datasets/viber_cleaned/vid.summary.csv"
write.csv2(partner.summary$comboMsg, partner_summary_file, row.names=F)
write.csv2(vid.summary$comboMsg, vid_summary_file, row.names=F)