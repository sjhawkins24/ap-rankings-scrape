library(rvest)
library(tidyverse)
year <- c("2024", 
          "2023", 
          "2022", 
          "2021", 
          "2020")
teams <- import("2025 Teams.csv")

for(i in 1:length(year)){
  html <- read_html(paste0("https://www.espn.com/college-football/fpi/_/view/resume/season/",
                           year[i]))
  
  bin <- tryCatch({ html %>% 
      # html_element("tbody") %>%
      html_table() 
  },  error=function(cond) {
    
  })
  
  bin <- do.call(cbind, bin)
  
  colnames(bin) <- bin[1,]
  rankings <- bin[-1,] 
  for(j in 1:nrow(teams)){
    html <- read_html(paste0("https://www.espn.com/college-football/team/schedule/_/id/",
                             teams$code[j], 
                             "/season/", 
                             year[i]) )
    schedule <- tryCatch({ html %>% 
        html_element("tbody") %>%
        html_table()  %>%
        as.data.frame()
    },  error=function(cond) {
      
    })
    colnames(schedule) <- schedule[2,]
    schedule <- schedule[-2,]
    schedule <- schedule[,-ncol(schedule)]
    schedule <- schedule %>%
      as.data.frame() %>%
      filter(!(DATE == "DATE" | 
                 DATE == "Regular Season")) %>%
      filter(RESULT != "Canceled") %>%
      filter(grepl("[0-9]",RESULT)) %>%
      mutate(RESULT = ifelse(substr(RESULT,1,1) == "W" | substr(RESULT,1,1) == "L", RESULT, "NA-NA")) %>%
      mutate(win_loss = substr(RESULT,1,1), 
             points_winner = map_chr(str_split(RESULT, "-"), 1), 
             points_winner = gsub("W", "", points_winner),
             points_loser = map_chr(str_split(RESULT, "-"),2),
             points_loser = gsub("[^0-9]", "", points_loser), 
             `HI PASS` = gsub("\\s{2}", " ", `HI PASS`), 
             `HI RUSH` = gsub("\\s{2}", " ", `HI RUSH`),
             `HI REC` = gsub("\\s{2}", " ", `HI REC`),
             `HI REC` = ifelse(is.na(`HI REC`), `HI PASS`, `HI REC`), 
             `HI PASS` = ifelse(!grepl("[0-9]", `HI PASS`), paste(trimws(`HI PASS`), "0"), `HI PASS`),
             `HI RUSH` = ifelse(!grepl("[0-9]", `HI RUSH`), paste(trimws(`HI RUSH`), "0"), `HI RUSH`),
             `HI REC` = ifelse(!grepl("[0-9]", `HI REC`), paste(trimws(`HI REC`), "0"), `HI REC`),
             pass = map_chr(str_split(`HI PASS`, " "), 2),
             pass = ifelse(pass == "as", NA, pass),
             rush = map_chr(str_split(`HI RUSH`, " "), 2),
             rush = ifelse(rush == "as", NA, rush), 
             rec = map_chr(str_split(`HI REC`, " "), 2), 
             rec = ifelse(rec == "as", NA, rec), 
             points_allowed = ifelse(win_loss == "W", points_loser, points_winner),  
             points_scored = ifelse(win_loss == "W", points_winner, points_loser), 
             point_differential = as.numeric(points_scored) - as.numeric(points_allowed), 
             home_game = grepl("vs", OPPONENT), 
             OPPONENT = gsub("vs", "", OPPONENT), 
             OPPONENT = gsub("@", "", OPPONENT), 
             OPPONENT = gsub(".*? ", "", OPPONENT), 
             Team = teams$Team[j]) %>%
      select(date = "DATE", 
             Team, 
             opponent = "OPPONENT", 
             win_loss, 
             pass, 
             rush, 
             rec, 
             points_allowed, 
             points_scored, 
             point_differential, 
             home_game)
    
    if(exists("all_schedule")){
      all_schedule <- rbind(all_schedule, 
                            schedule)
      rm(schedule)
    } else { 
      all_schedule <- schedule
      rm(schedule)
    }
    print(paste("Done with", teams$Team[j]))
  }
  all_schedule <- all_schedule %>%
    mutate(season = year[i]) %>% 
    merge(bin) %>%
    select(-`REM SOS`) 
  
  if(exists("all_schedule_all_year")){ 
    all_schedule_all_year <- rbind(all_schedule_all_year, 
                                   all_schedule)
    rm(all_schedule)
  } else { 
    all_schedule_all_year <- all_schedule
    rm(all_schedule)
  }
  print(paste0("Done with", year[i]))
}

write.csv(all_schedule_all_year, "College Football Rankings 24 to 20.csv", row.names = FALSE)


#Now getting the current year's data 

year <- c("2025")
teams <- import("2025 Teams.csv")

for(i in 1:length(year)){
  html <- read_html(paste0("https://www.espn.com/college-football/fpi/_/view/resume/season/",
                           year[i]))
  
  bin <- tryCatch({ html %>% 
      # html_element("tbody") %>%
      html_table() 
  },  error=function(cond) {
    
  })
  
  bin <- do.call(cbind, bin)
  
  colnames(bin) <- bin[1,]
  rankings <- bin[-1,] 
  for(j in 1:nrow(teams)){
    html <- read_html(paste0("https://www.espn.com/college-football/team/schedule/_/id/",
                             teams$code[j], 
                             "/season/", 
                             year[i]) )
    schedule <- tryCatch({ html %>% 
        html_element("tbody") %>%
        html_table()  %>%
        as.data.frame()
    },  error=function(cond) {
      
    })
    colnames(schedule) <- schedule[2,]
    schedule <- schedule[-2,]
    schedule <- schedule[,-ncol(schedule)]
    schedule <- schedule %>%
      as.data.frame() %>%
      filter(!(DATE == "DATE" | 
                 DATE == "Regular Season")) %>%
      filter(RESULT != "Canceled") %>%
      filter(grepl("[0-9]",RESULT)) %>%
      mutate(RESULT = ifelse(substr(RESULT,1,1) == "W" | substr(RESULT,1,1) == "L", RESULT, "NA-NA")) %>%
      mutate(win_loss = substr(RESULT,1,1), 
             points_winner = map_chr(str_split(RESULT, "-"), 1), 
             points_winner = gsub("W", "", points_winner),
             points_loser = map_chr(str_split(RESULT, "-"),2),
             points_loser = gsub("[^0-9]", "", points_loser), 
             `HI PASS` = gsub("\\s{2}", " ", `HI PASS`), 
             `HI RUSH` = gsub("\\s{2}", " ", `HI RUSH`),
             `HI REC` = gsub("\\s{2}", " ", `HI REC`),
             `HI REC` = ifelse(is.na(`HI REC`), `HI PASS`, `HI REC`), 
             `HI PASS` = ifelse(!grepl("[0-9]", `HI PASS`), paste(trimws(`HI PASS`), "0"), `HI PASS`),
             `HI RUSH` = ifelse(!grepl("[0-9]", `HI RUSH`), paste(trimws(`HI RUSH`), "0"), `HI RUSH`),
             `HI REC` = ifelse(!grepl("[0-9]", `HI REC`), paste(trimws(`HI REC`), "0"), `HI REC`),
             pass = map_chr(str_split(`HI PASS`, " "), 2),
             pass = ifelse(pass == "as", NA, pass),
             rush = map_chr(str_split(`HI RUSH`, " "), 2),
             rush = ifelse(rush == "as", NA, rush), 
             rec = map_chr(str_split(`HI REC`, " "), 2), 
             rec = ifelse(rec == "as", NA, rec), 
             points_allowed = ifelse(win_loss == "W", points_loser, points_winner),  
             points_scored = ifelse(win_loss == "W", points_winner, points_loser), 
             point_differential = as.numeric(points_scored) - as.numeric(points_allowed), 
             home_game = grepl("vs", OPPONENT), 
             OPPONENT = gsub("vs", "", OPPONENT), 
             OPPONENT = gsub("@", "", OPPONENT), 
             OPPONENT = gsub(".*? ", "", OPPONENT), 
             Team = teams$Team[j]) %>%
      select(date = "DATE", 
             Team, 
             opponent = "OPPONENT", 
             win_loss, 
             pass, 
             rush, 
             rec, 
             points_allowed, 
             points_scored, 
             point_differential, 
             home_game)
    
    if(exists("all_schedule")){
      all_schedule <- rbind(all_schedule, 
                            schedule)
      rm(schedule)
    } else { 
      all_schedule <- schedule
      rm(schedule)
    }
    print(paste("Done with", teams$Team[j]))
  }
  all_schedule <- all_schedule %>%
    mutate(season = year[i]) %>% 
    merge(bin) %>%
    select(-`REM SOS`) 
  
  if(exists("all_schedule_all_year")){ 
    all_schedule_all_year <- rbind(all_schedule_all_year, 
                                   all_schedule)
    rm(all_schedule)
  } else { 
    all_schedule_all_year <- all_schedule
    rm(all_schedule)
  }
  print(paste0("Done with", year[i]))
}
write.csv(all_schedule_all_year, "College Football Rankings 2025.csv", row.names = FALSE)