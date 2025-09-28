padding_weeks <- function(data, 
                          year){
  weeks_of_play <- ifelse(year == "2020", nrow(data), 12)
for(i in 2:weeks_of_play){
  past_week <- mdy(trimws(str_split( paste(data$date[i-1], year), ",")[[1]][2]))
  current_week <- mdy(trimws(str_split( paste(data$date[i], year), ",")[[1]][2])) 
  current_week <- ifelse(is.na(current_week),  ymd(paste0(year, "11-30")), current_week)  

  if(past_week - current_week + 13 == 0 | 
     past_week - current_week + 12 == 0 | 
     past_week - current_week + 11 == 0 | 
     past_week - current_week + 10 == 0 | 
     past_week - current_week + 9 == 0 |
     past_week - current_week + 8 == 0 |
     past_week - current_week + 7 == 0 |
     past_week - current_week + 6 == 0 | 
     past_week - current_week + 5 == 0 | 
     past_week - current_week + 4 == 0 | 
     past_week - current_week + 3 == 0 | 
     past_week - current_week + 2 == 0 | 
     past_week - current_week + 1 == 0  ){
    print("Games week to week, no bye week"
    )
  } else { 
    print("Bye week")
    bye_week <- c(format(past_week + 7, "%a, %b %d"), 
                  data$Team[i], 
                  data$Team_id[i], 
                  "Bye", 
                  "Bye", 
                  FALSE, 
                  NA, 
                  0, 
                  0, 
                  0, 
                  0, 
                  0, 
                  0, 
                  "Bye")
    data <- rbind(data[1:i-1,], 
                  bye_week, 
                  data[i:nrow(data), ])
    
    }
  
}

data$week <- if(year != "2020"){ 
  1:nrow(data)
} else { 
  rev(6:(7-nrow(data)))
  }
return(data)
}