################
# Fabian Okeke
# Nov 3rd, 2014
# Moves data format changed so need to change data handling script
# TO-DO: script should be made more robust
# you specify the header format and it does the cleaning up of the data

################

#
# NB: call main() to get started
#



########################
# ALL FUNCTIONS USED IN MAIN
# DEFINED BELOW
#######################
load_data = function(file_path) {
  json_list = fromJSON(file_path)
  
  # get individual dataframes for all four cols: date(1), summary(2), segments(3), lastUpdate(4)
  #date
  df.date       = get_df(json_list, "date")
  
  #summary
  df.summary    = get_df(json_list, "summary")  
  assign("df.summary", df.summary, .GlobalEnv)
  
  #segments
  df.segments   = get_df(json_list, "segments")
  assign("df.segments", df.segments, .GlobalEnv)
  
  #lastUpdate
  df.lastUpdate = get_df(json_list, "lastUpdate")  
} 
  
load_libraries = function() {
  require(RJSONIO)
}

get_df = function(li, col_name) {
  if (col_name == "date") { get_date_df(li) }
  else if (col_name == "summary") { get_summary_df(li) }
  else if (col_name == "segments") {get_segments_df(li) }
  else if (col_name == "lastUpdate") {get_lastUpdate_df(li) }    
}

get_date_df = function(json_list) {
  rr = c()
  for (i in 1:length(json_list)) {
    new_entry = json_list[[i]]$date
    rr = c(rr, new_entry)
  }
  return(data.frame(date=rr))
}

get_lastUpdate_df = function(json_list) {
  rr = c()
  for (i in 1:length(json_list)) {
    new_entry = json_list[[i]]$lastUpdate
    rr = c(rr, new_entry)
  }
  return(data.frame(lastUpdate=rr))
}


# every row has data in the summary column
# work on one row at a time. 
get_segments_df = function(json_list) {
  mm = matrix(nrow=1, ncol=6)
  new_list = list()
  for (i in 1:length(json_list)) {
    #one row at a time
    row = json_list[[i]]$summary 
    row.date = json_list[[i]]$date
    no_of_lists = length(row)
    
    # a row summary is a list of lists
    # example row[[i]]: "walking" "walking" "2285" "1966" "2614"
    for (i in 1:no_of_lists) {
      new_list = row[[i]]
      full_col_names = c("activity", "group", "duration", "distance", "steps")
      
      # usually 4 elements means activity was 'transport' without col 'steps'
      # and 5 elements means activity was 'walking' as with full_col_names
      # so for transport, add col 'steps' and set to NULL
      if (length(new_list) == 4) {
        length(new_list) = 5
        names(new_list) = full_col_names
      } 
      
      if (length(new_list) == 5) {    
        # add new entry to matrix with corresponding date
        new_list = append(row.date, new_list)
        names(new_list) = c("date", "activity", "group", "duration", "distance", "steps")
        mm = rbind(mm, new_list)
      } 
    }
  }
  # remove row names and empty first col
  # then convert matrix to dataframe
  rownames(mm) = NULL
  mm = mm[-1,]
  return(data.frame(mm))
}

# every row has data in the summary column
# work on one row at a time. 
get_summary_df = function(json_list) {
  mm = matrix(nrow=1, ncol=6)
  new_list = list()
  for (i in 1:length(json_list)) {
    #one row at a time
    row = json_list[[i]]$summary 
    row.date = json_list[[i]]$date
    no_of_lists = length(row)
    
    # a row summary is a list of lists
    # example row[[i]]: "walking" "walking" "2285" "1966" "2614"
    for (i in 1:no_of_lists) {
      new_list = row[[i]]
      full_col_names = c("activity", "group", "duration", "distance", "steps")
      
      # usually 4 elements means activity was 'transport' without col 'steps'
      # and 5 elements means activity was 'walking' as with full_col_names
      # so for transport, add col 'steps' and set to NULL
      if (length(new_list) == 4) {
        length(new_list) = 5
        names(new_list) = full_col_names
      } 
      
      if (length(new_list) == 5) {    
        # add new entry to matrix with corresponding date
        new_list = append(row.date, new_list)
        names(new_list) = c("date", "activity", "group", "duration", "distance", "steps")
        mm = rbind(mm, new_list)
      } 
    }
  }
  # remove row names and empty first col
  # then convert matrix to dataframe
  rownames(mm) = NULL
  mm = mm[-1,]
  return(data.frame(mm))
}

##################
# test script here
# main function 
#################
main = function() {
  load_libraries()
  file_path = "~/Dev/partner/datasets/latest/fab-moves-data.json"
  load_data(file_path)
}
main()


