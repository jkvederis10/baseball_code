library(dplyr)
library(baseballr)
library(writexl)
library(lubridate)
setwd("C:/Users/jkved/OneDrive/Desktop/Sports Analytics/MLB")


#load attendance data
mlb_attendance <- readxl::read_excel("MLB_Attendance_Fan_Analysis.xlsx")
mlb_attendance$date <- as.Date(mlb_attendance$date)
mlb_attendance$temperature <- as.numeric(mlb_attendance$temperature)
#load win percentage data
mlb_win_pct <- as.data.frame(readxl::read_excel("MLB_WinPct_L365.xlsx"))
mlb_win_pct$date <- as.Date(mlb_win_pct$date)
mlb_win_pct_season <- as.data.frame(readxl::read_excel("MLB_WinPct_Season.xlsx"))
mlb_win_pct_season$date <- as.Date(mlb_win_pct_season$date)
mlb_win_pct_L3Y <- as.data.frame(readxl::read_excel("MLB_WinPct_L3Y.xlsx"))
mlb_win_pct_L3Y$date <- as.Date(mlb_win_pct_L3Y$date)
#load stadium capacities
mlb_stadiums <- readxl::read_excel("MLB_Attendance_Fan_Analysis.xlsx", sheet = "Capacities")
#load local populations
mlb_teams <- readxl::read_excel("MLB_Attendance_Fan_Analysis.xlsx", sheet = "Teams")
#load world series bumps
mlb_WS <- readxl::read_excel("MLB_Attendance_Fan_Analysis.xlsx", sheet = "WorldSeries")
#load yearly payrolls
mlb_payrolls <- readxl::read_excel("MLB_Attendance_Fan_Analysis.xlsx", sheet = "Payrolls")


#remove 2014 and 2020 data
mlb_attendance <- mlb_attendance[-which(mlb_attendance$season == "2020" | mlb_attendance$season == "2014" ), ]

#select only 2021-2025 data
#mlb_attendance <- mlb_attendance[which(mlb_attendance$season == "2021" | mlb_attendance$season == "2022" | mlb_attendance$season == "2023" | mlb_attendance$season == "2024" | mlb_attendance$season == "2025"), ]

#remove Rays and Athletics 2025 seasons
mlb_attendance <- mlb_attendance[-which(mlb_attendance$season == "2025" & mlb_attendance$teams_home_team_id == 139), ]
mlb_attendance <- mlb_attendance[-which(mlb_attendance$season == "2025" & mlb_attendance$teams_home_team_id == 133), ]

#remove where attendance is blank
mlb_attendance <- mlb_attendance[-which(is.na(mlb_attendance$attendance)), ]

#remove uncommon stadiums
mlb_attendance <- mlb_attendance[which(mlb_attendance$venue_id %in% mlb_stadiums$venue_id[which(mlb_stadiums$standard == 'Yes')]), ]

#remove outside of a certain temperature band
#mlb_attendance <- mlb_attendance[which(mlb_attendance$temperature >= 45 & mlb_attendance$temperature <= 90),]

#remove games after July 31 (trade deadline)
#mlb_attendance <- mlb_attendance[which(month(as.POSIXlt(mlb_attendance$date)) <= 7 ), ]


#add month and day of week
mlb_attendance$game_month <- as.factor(month(as.POSIXlt(mlb_attendance$date, format="%Y-%M-%D")))
mlb_attendance$day_of_week <- as.factor(weekdays(mlb_attendance$date))

#add L365 win percentages to attendance data
mlb_attendance$away_L365_winpct <- NA
mlb_attendance$home_L365_winpct <- NA
mlb_attendance$away_Season_winpct <- NA
mlb_attendance$home_Season_winpct <- NA
mlb_attendance$away_L3Y_winpct <- NA
mlb_attendance$home_L3Y_winpct <- NA
mlb_attendance$worldSeries <- NA
mlb_attendance$home_payroll <- NA
mlb_attendance$away_payroll <- NA

#for loop to fill in L365 win pct
for (i in 1:nrow(mlb_attendance)) {
  
  #find away team L365 win pct
  mlb_attendance$away_L365_winpct[i] <- mlb_win_pct[which(mlb_win_pct$date == mlb_attendance$date[i]),which(names(mlb_win_pct) == mlb_attendance$teams_away_team_id[i])]
  #find home team L365 win pct
  mlb_attendance$home_L365_winpct[i] <- mlb_win_pct[which(mlb_win_pct$date == mlb_attendance$date[i]),which(names(mlb_win_pct) == mlb_attendance$teams_home_team_id[i])]
  
  #find away team L3Y win pct
  mlb_attendance$away_L3Y_winpct[i] <- mlb_win_pct_L3Y[which(mlb_win_pct_L3Y$date == mlb_attendance$date[i]),which(names(mlb_win_pct_L3Y) == mlb_attendance$teams_away_team_id[i])]
  #find home team L3Y win pct
  mlb_attendance$home_L3Y_winpct[i] <- mlb_win_pct_L3Y[which(mlb_win_pct_L3Y$date == mlb_attendance$date[i]),which(names(mlb_win_pct_L3Y) == mlb_attendance$teams_home_team_id[i])]
  
  #check if date is season win percentage data
  if(mlb_attendance$date[i] %in% mlb_win_pct_season$date){
    #find away team Season win pct
    mlb_attendance$away_Season_winpct[i] <- mlb_win_pct_season[which(mlb_win_pct_season$date == mlb_attendance$date[i]),which(names(mlb_win_pct_season) == mlb_attendance$teams_away_team_id[i])]
    #find home team Season win pct
    mlb_attendance$home_Season_winpct[i] <- mlb_win_pct_season[which(mlb_win_pct_season$date == mlb_attendance$date[i]),which(names(mlb_win_pct_season) == mlb_attendance$teams_home_team_id[i])]
  }
  
  #add if the team has won a World Series in the last 2 years. 1 if last year, 0.5 if two years ago
  mlb_attendance$worldSeries[i] <- as.numeric(mlb_WS[which(mlb_WS$Season == mlb_attendance$season[i]),which(names(mlb_WS) == mlb_attendance$teams_home_team_id[i])])
  
  #add payroll data
  mlb_attendance$home_payroll[i] <- as.numeric(mlb_payrolls$Payroll[which(mlb_payrolls$Season == mlb_attendance$season[i] & mlb_payrolls$teams_home_team_id == mlb_attendance$teams_home_team_id[i])])
  mlb_attendance$away_payroll[i] <- as.numeric(mlb_payrolls$Payroll[which(mlb_payrolls$Season == mlb_attendance$season[i] & mlb_payrolls$teams_home_team_id == mlb_attendance$teams_away_team_id[i])])
  
}

#add local populations
mlb_attendance <- mlb_attendance %>%
  left_join(mlb_teams[,c(1,3,4)], by = 'teams_home_team_id')


#regression to predict capacity pct
capacity_reg_L365 <- lm(capacity_pct ~ home_L365_winpct + away_L365_winpct + home_payroll + 
                          away_payroll + pop_within_50km + temperature + day_night + day_of_week + 
                          game_month + worldSeries, data = mlb_attendance)
summary(capacity_reg_L365)
capacity_residuals <- residuals(capacity_reg_L365)

#add residuals to the dataset
mlb_attendance$cap_pct_residual <- NA
for (i in 1:nrow(mlb_attendance)) {
  mlb_attendance$cap_pct_residual[i] <- capacity_residuals[i]
}

#summarise residual data
mlb_residuals <- mlb_attendance %>%
  group_by(teams_home_team_id) %>%
  summarise(median_residual = median(cap_pct_residual), total_residual = sum(cap_pct_residual))

#write data to an excel file to work with
write_xlsx(mlb_residuals, "MLB_Best_Fans.xlsx")



#regression to predict capacity pct
capacity_reg_L3Y <- lm(capacity_pct ~ home_L3Y_winpct + away_L3Y_winpct + home_payroll + 
                          away_payroll + pop_within_50km + temperature + day_night + day_of_week + 
                          game_month + worldSeries, data = mlb_attendance)
summary(capacity_reg_L3Y)
capacity_residuals_L3Y <- residuals(capacity_reg_L3Y)

#add residuals to the dataset
mlb_attendance$cap_pct_residual_L3Y <- NA
for (i in 1:nrow(mlb_attendance)) {
  mlb_attendance$cap_pct_residual_L3Y[i] <- capacity_residuals_L3Y[i]
}

#summarise residual data
mlb_residuals_L3Y <- mlb_attendance %>%
  group_by(teams_home_team_id) %>%
  summarise(median_residual = median(cap_pct_residual_L3Y), total_residual = sum(cap_pct_residual_L3Y))

#write data to an excel file to work with
write_xlsx(mlb_residuals_L3Y, "MLB_Best_Fans.xlsx")



#regression to predict capacity pct using in-season win percentage
capacity_reg_Season <- lm(capacity_pct ~ home_L365_winpct + away_L365_winpct + home_payroll + 
                            away_payroll + pop_within_50km + temperature + day_night + day_of_week + 
                            game_month + worldSeries, 
                          data = mlb_attendance[-which(is.na(mlb_attendance$away_Season_winpct)),])
summary(capacity_reg_Season)
capacity_residuals_Season <- residuals(capacity_reg_Season)


#remove March and April games
mlb_attendance_2 <- mlb_attendance[which(mlb_attendance$game_month %in% 5:10), ]

#add residuals to the dataset
mlb_attendance_2$cap_pct_residual <- NA
for (i in 1:nrow(mlb_attendance_2)) {
  mlb_attendance_2$cap_pct_residual[i] <- capacity_residuals_Season[i]
}

#summarise residual data
mlb_residuals_Season <- mlb_attendance_2 %>%
  group_by(teams_home_team_id) %>%
  summarise(median_residual = median(cap_pct_residual), total_residual = sum(cap_pct_residual))

#write data to an excel file to work with
write_xlsx(mlb_residuals_Season, "MLB_Best_Fans.xlsx")






#create a data frame with the teams
mlb_team_regs <- unique(mlb_attendance[which(mlb_attendance$season == "2024"), c('teams_home_team_id','teams_home_team_name')])

#add columns for regression outputs
mlb_team_regs$avg_attendance <- NA
mlb_team_regs$avg_capacity_pct <- NA
mlb_team_regs$wp_coeff <- NA
mlb_team_regs$wp_sderr <- NA
mlb_team_regs$wp_pval <- NA
mlb_team_regs$r2 <- NA
mlb_team_regs$wp_lowCI <- NA
mlb_team_regs$wp_highCI <- NA

#for loop to populate the regression output for all teams
for (i in 1:nrow(mlb_team_regs)) {
  
  #calculate average attendance and average capacity pcercentage over the dataset
  mlb_team_regs$avg_attendance[i] <- mean(mlb_attendance$attendance[which(mlb_attendance$teams_home_team_id == mlb_team_regs$teams_home_team_id[i])])
  mlb_team_regs$avg_capacity_pct[i] <- mean(mlb_attendance$capacity_pct[which(mlb_attendance$teams_home_team_id == mlb_team_regs$teams_home_team_id[i])])
  
  #run a linear regression on capacity pct and last 365 win pct
  reg_attendance <- lm(capacity_pct ~ home_L365_winpct, data = mlb_attendance[which(mlb_attendance$teams_home_team_id == mlb_team_regs$teams_home_team_id[i]),])
  
  #get coefficient estimate, std error, and p value & r2
  mlb_team_regs$wp_coeff[i] <- summary(reg_attendance)$coefficients[2,1]
  mlb_team_regs$wp_sderr[i] <- summary(reg_attendance)$coefficients[2,2]
  mlb_team_regs$wp_pval[i] <- summary(reg_attendance)$coefficients[2,4]
  mlb_team_regs$r2[i] <- summary(reg_attendance)$adj.r.squared
  
  #get confidence intervals
  mlb_team_regs$wp_lowCI[i] <- confint(reg_attendance)[2,1]
  mlb_team_regs$wp_highCI[i] <- confint(reg_attendance)[2,2]
  
}

#write data to an excel file to work with
write_xlsx(mlb_team_regs, "MLB_Best_Fans.xlsx")




#run a linear regression on capacity pct and last 365 win pct
reg_attendance <- lm(capacity_pct ~ home_L365_winpct, data = mlb_attendance[which(mlb_attendance$teams_home_team_id == 134),])




##### CODE TO PULL ATTENDANCE, OTHER GAME INFO DATA


#mlb games
mlb_games <- mlb_schedule(season = 2012, level_ids = "1")

#vars to keep from game dataset
games_vars <- c('game_pk','date','game_type','season','game_number','double_header','gameday_type',
                'day_night','series_description','teams_away_score','teams_away_team_id',
                'teams_away_team_name','teams_home_score','teams_home_team_id','teams_home_team_name',
                'teams_home_is_winner','venue_id','venue_name')
mlb_games <- mlb_games[which(mlb_games$game_type == 'R'), games_vars]

#add columns for attendance, weather, and start time
mlb_games$attendance <- NA
mlb_games$temperature <- NA
mlb_games$other_weather <- NA
mlb_games$wind <- NA
mlb_games$start_time <- NA


#loop to pull game info
for (i in 1:nrow(mlb_games)) {
  
  #pull game info
  mlb_game_data <- try(mlb_game_info(mlb_games$game_pk[i]))
  
  if (exists('mlb_game_data') && is.data.frame(get('mlb_game_data')) && nrow(mlb_game_data) > 0) {
    mlb_games$attendance[i] <- mlb_game_data$attendance
    mlb_games$temperature[i] <- mlb_game_data$temperature
    mlb_games$other_weather[i] <- mlb_game_data$other_weather
    #mlb_games$wind[i] <- mlb_game_data$wind
    mlb_games$start_time[i] <- mlb_game_data$start_time
  }
}


#write to excel file
library(writexl)
setwd("C:/Users/jkved/OneDrive/Desktop/Sports Analytics/MLB")
write_xlsx(mlb_games, "MLB_Attendance.xlsx")




##### CODE TO PRODUCE THE DAILY, LAST 365 WIN PERCENTAGES


##create a dataset with all dates
mlb_win_pct <- as.data.frame(seq(as.Date("2015-03-01"), as.Date("2025-11-30"), by="days"))
names(mlb_win_pct)[1] <- "date"

#get unique team ids into an array
team_ids <- unique(mlb_attendance$teams_away_team_id)

#add a column for each team ID
mlb_win_pct <- cbind(mlb_win_pct, setNames( lapply(team_ids, function(x) x=NA), team_ids) )

#for loop to populate team ID columns with team win% over the last 365 days
for (i in 1:nrow(mlb_win_pct)) {
  
  #filter dataset for last 365 days
  attendance_data <- mlb_attendance[which(mlb_attendance$date < mlb_win_pct$date[i] & mlb_attendance$date >= mlb_win_pct$date[i]-365), ]
  
  for (j in 2:31) {
    
    #calculate the number of games the team played in during the last 365 days
    games <- nrow(attendance_data[which(attendance_data$teams_away_team_id == names(mlb_win_pct)[j]),]) +
      nrow(attendance_data[which(attendance_data$teams_home_team_id == names(mlb_win_pct)[j]),])
    
    #calculate the number of wins for the team during the last 365 days
    wins <- nrow(attendance_data[which(attendance_data$teams_away_team_id == names(mlb_win_pct)[j] & attendance_data$teams_home_is_winner == FALSE),]) +
      nrow(attendance_data[which(attendance_data$teams_home_team_id == names(mlb_win_pct)[j] & attendance_data$teams_home_is_winner == TRUE),])
    
    #calculate win % for last 365 days for the team ID in column j
    mlb_win_pct[i,j] <- wins / games
    
  }
}

#write win percentage data to an excel file
write_xlsx(mlb_win_pct, "MLB_WinPct_L365.xlsx")




##### CODE TO PRODUCE THE DAILY, LAST 3 YEARS WIN PERCENTAGES


##create a dataset with all dates
mlb_win_pct_L3Y <- as.data.frame(seq(as.Date("2015-03-01"), as.Date("2025-11-30"), by="days"))
names(mlb_win_pct_L3Y)[1] <- "date"

#get unique team ids into an array
team_ids <- unique(mlb_attendance$teams_away_team_id)

#add a column for each team ID
mlb_win_pct_L3Y <- cbind(mlb_win_pct_L3Y, setNames( lapply(team_ids, function(x) x=NA), team_ids) )

#for loop to populate team ID columns with team win% over the last 365 days
for (i in 1:nrow(mlb_win_pct_L3Y)) {
  
  #filter dataset for last 365 days
  attendance_data <- mlb_attendance[which(mlb_attendance$date < mlb_win_pct_L3Y$date[i] & mlb_attendance$date >= mlb_win_pct_L3Y$date[i]-1095), ]
  
  for (j in 2:31) {
    
    #calculate the number of games the team played in during the last 365 days
    games <- nrow(attendance_data[which(attendance_data$teams_away_team_id == names(mlb_win_pct_L3Y)[j]),]) +
      nrow(attendance_data[which(attendance_data$teams_home_team_id == names(mlb_win_pct_L3Y)[j]),])
    
    #calculate the number of wins for the team during the last 365 days
    wins <- nrow(attendance_data[which(attendance_data$teams_away_team_id == names(mlb_win_pct_L3Y)[j] & attendance_data$teams_home_is_winner == FALSE),]) +
      nrow(attendance_data[which(attendance_data$teams_home_team_id == names(mlb_win_pct_L3Y)[j] & attendance_data$teams_home_is_winner == TRUE),])
    
    #calculate win % for last 365 days for the team ID in column j
    mlb_win_pct_L3Y[i,j] <- wins / games
    
  }
}

#write win percentage data to an excel file
write_xlsx(mlb_win_pct_L3Y, "MLB_WinPct_L3Y.xlsx")





##### CODE TO PRODUCE THE DAILY WIN PERCENTAGES FOR THE CURRENT SEASON


##create a dataset with all dates
mlb_win_pct_season <- as.data.frame(seq(as.Date("2015-05-01"), as.Date("2025-11-30"), by="days"))
names(mlb_win_pct_season)[1] <- "date"

#get unique team ids into an array
team_ids <- unique(mlb_attendance$teams_away_team_id)

#add a column for each team ID
mlb_win_pct_season <- cbind(mlb_win_pct_season, setNames( lapply(team_ids, function(x) x=NA), team_ids) )

#remove months outside of May to October
mlb_win_pct_season <- mlb_win_pct_season[which(month(as.POSIXlt(mlb_win_pct_season$date, format="%Y-%M-%D")) %in% c(5:10)),]

#for loop to populate team ID columns with team win% over the last 365 days
for (i in 1:nrow(mlb_win_pct_season)) {
  
  #filter dataset for last 365 days
  attendance_data <- mlb_attendance[which(mlb_attendance$season == year(as.POSIXlt(mlb_win_pct_season$date[i], format="%Y-%M-%D")) & mlb_attendance$date < mlb_win_pct_season$date[i] ), ]
  
  for (j in 2:31) {
    
    #calculate the number of games the team played in during the last 365 days
    games <- nrow(attendance_data[which(attendance_data$teams_away_team_id == names(mlb_win_pct_season)[j]),]) +
      nrow(attendance_data[which(attendance_data$teams_home_team_id == names(mlb_win_pct_season)[j]),])
    
    #calculate the number of wins for the team during the last 365 days
    wins <- nrow(attendance_data[which(attendance_data$teams_away_team_id == names(mlb_win_pct_season)[j] & attendance_data$teams_home_is_winner == FALSE),]) +
      nrow(attendance_data[which(attendance_data$teams_home_team_id == names(mlb_win_pct_season)[j] & attendance_data$teams_home_is_winner == TRUE),])
    
    #calculate win % for last 365 days for the team ID in column j
    mlb_win_pct_season[i,j] <- wins / games
    
  }
}

#write win percentage data to an excel file
write_xlsx(mlb_win_pct_season, "MLB_WinPct_Season.xlsx")


