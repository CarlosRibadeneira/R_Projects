
#Carlos Ribadeneira 


#Get the data files 
library(tidyverse)
library(lubridate)
 
EPL_Standings <- function(Datef, Seasonf){
  #Date
  if(Seasonf == "2020/21"){
    #Get information 
    Datef <- mdy(Datef)
    season_2020_2021 <- read.csv(url("http://www.football-data.co.uk/mmz4281/2021/E0.csv"))
    season_2020_2021 <- select(season_2020_2021, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
    season_2020_2021$Date <- dmy(season_2020_2021$Date)
   
  
     #Points
    x<-mdy("10/18/2024")
    x
    
    season_2020_2021 <- mutate( season_2020_2021, Home_Points = ifelse(FTR == "H", 3, ifelse(FTR == "D", 1, 0)))%>%
      mutate(Away_Points = ifelse(FTR == "A", 3, ifelse(FTR == "D", 1, 0))) %>%
       filter(Date<=x)
    #Home
    Home0 <- select(season_2020_2021,Date, HomeTeam, FTHG, FTAG, FTR, Home_Points)
    Home<- Home0 %>% rename(TeamName = HomeTeam, FTHG_h = FTHG, FTAG_h = FTAG, FTR_h = FTR)
    Home_stats <- Home %>% group_by(TeamName) %>% 
      summarize(Points_h = sum(Home_Points), Wins_h = sum(FTR_h == "H"), Defeats_h = sum(FTR_h == "A"), Ties_h = sum(FTR_h == "D"), Goals_made_h = sum(FTHG_h), Goals_against_h = sum(FTAG_h))
    
    #Away
    Away0 <- select(season_2020_2021,Date, AwayTeam, FTHG, FTAG, FTR, Away_Points)
    Away <- Away0 %>% rename(TeamName = AwayTeam, FTHG_a = FTHG, FTAG_a = FTAG, FTR_a = FTR)
    Away_stats <- Away %>% group_by(TeamName) %>% 
      summarize(Points_a = sum(Away_Points), Wins_a = sum(FTR_a == "A"), Defeats_a = sum(FTR_a == "H"), Ties_a = sum(FTR_a == "D"), Goals_made_a = sum(FTHG_a), Goals_against_a = sum(FTAG_a))
    
  
  Home_and_Away <- full_join(Home_stats, Away_stats, by = "TeamName")
  Home_and_Away_stats <- Home_and_Away %>% mutate(Points = Points_h + Points_a) %>%
    mutate(MatchesPlayed = Wins_a + Wins_h + Defeats_a + Defeats_h + Ties_h + Ties_a) %>%
    mutate(PtPct = Points / 3 * MatchesPlayed, PPM = Points/MatchesPlayed, GS = Goals_made_a + Goals_made_h) %>%
    mutate(GSM = GS / MatchesPlayed, GA = Goals_against_a + Goals_against_h, GAM = GA / MatchesPlayed) %>%
    mutate(wins = Wins_h + Wins_a, loses = Defeats_a + Defeats_h, ties = Ties_h + Ties_a) %>%
    mutate(HomeRec = paste0(Wins_h,"-", Defeats_h, "-",Ties_h), AwayRec = paste0(Wins_a,"-", Defeats_a, "-",Ties_a), Record = paste0(wins,"-", loses, "-", ties))
  
  
  Home1 <- select(season_2020_2021,Date, HomeTeam, FTHG, FTAG, FTR)
    Home10<- Home1 %>% rename(TeamName = HomeTeam)
   
   Away1 <- select(season_2020_2021,Date, AwayTeam, FTHG, FTAG, FTR)
    Away10 <- Away1 %>% rename(TeamName = AwayTeam)
    
 
  Home_and_Away_10 <- rbind(Home10, Away10)
  class(Home_and_Away_10$Date)
  Home_and_Away_10f<- Home_and_Away_10[order(Home_and_Away_10$Date, decreasing = TRUE),]
  Home_and_Away_10ff<-top_n(Home_and_Away_10f, 10, wt = Date)
  Home_and_Away_10ff_Stats<- Home_and_Away_10ff %>% group_by(TeamName) %>% 
      summarize(Wins_10 = sum(FTR == "A" | FTR == 'H'), Ties_10 = sum(FTR == "D"))
  HA10_record <- Home_and_Away_10ff_Stats %>% mutate(Last10_Wins_Ties = paste0(Wins_10, "-", Ties_10))%>%
    select(TeamName, Last10_Wins_Ties)
  
  Home_and_Away_stats_F <- Home_and_Away_stats[order(-Home_and_Away_stats$PPM,-Home_and_Away_stats$wins,-Home_and_Away_stats$GSM, Home_and_Away_stats$GAM),]
  Home_and_Away_stats_final <- left_join(Home_and_Away_stats_F, HA10_record, by = "TeamName")
  
  EPL <- Home_and_Away_stats_final %>% select(TeamName, Record, HomeRec, AwayRec, MatchesPlayed, Points, PPM, PtPct, GS, GSM, GA, GAM, Last10_Wins_Ties)
  
  
  return(data.frame(EPL))
  
  } else if (Seasonf == "2019/20") {
      
    
      #Get information 
      Datef <- mdy(Datef)
      season_2019_2020 <- read.csv(url("http://www.football-data.co.uk/mmz4281/1920/E0.csv"))
      season_2019_2020 <- select(season_2019_2020, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
      season_2019_2020$Date <- dmy(season_2019_2020$Date)
  
      #Points
     
      season_2019_2020 <- mutate( season_2019_2020, Home_Points = ifelse(FTR == "H", 3, ifelse(FTR == "D", 1, 0)))%>%
        mutate(Away_Points = ifelse(FTR == "A", 3, ifelse(FTR == "D", 1, 0))) %>%
      
        filter(Date<=Datef)
      #Home
      Home0 <- select(season_2019_2020,Date, HomeTeam, FTHG, FTAG, FTR, Home_Points)
      Home<- Home0 %>% rename(TeamName = HomeTeam, FTHG_h = FTHG, FTAG_h = FTAG, FTR_h = FTR)
      Home_stats <- Home %>% group_by(TeamName) %>% 
        summarize(Points_h = sum(Home_Points), Wins_h = sum(FTR_h == "H"), Defeats_h = sum(FTR_h == "A"), Ties_h = sum(FTR_h == "D"), Goals_made_h = sum(FTHG_h), Goals_against_h = sum(FTAG_h))
      
      #Away
      Away0 <- select(season_2019_2020,Date, AwayTeam, FTHG, FTAG, FTR, Away_Points)
      Away <- Away0 %>% rename(TeamName = AwayTeam, FTHG_a = FTHG, FTAG_a = FTAG, FTR_a = FTR)
      Away_stats <- Away %>% group_by(TeamName) %>% 
        summarize(Points_a = sum(Away_Points), Wins_a = sum(FTR_a == "A"), Defeats_a = sum(FTR_a == "H"), Ties_a = sum(FTR_a == "D"), Goals_made_a = sum(FTHG_a), Goals_against_a = sum(FTAG_a))
      
      Home_and_Away <- full_join(Home_stats, Away_stats, by = "TeamName")
  Home_and_Away_stats <- Home_and_Away %>% mutate(Points = Points_h + Points_a) %>%
    mutate(MatchesPlayed = Wins_a + Wins_h + Defeats_a + Defeats_h + Ties_h + Ties_a) %>%
    mutate(PtPct = Points / 3 * MatchesPlayed, PPM = Points/MatchesPlayed, GS = Goals_made_a + Goals_made_h) %>%
    mutate(GSM = GS / MatchesPlayed, GA = Goals_against_a + Goals_against_h, GAM = GA / MatchesPlayed) %>%
    mutate(wins = Wins_h + Wins_a, loses = Defeats_a + Defeats_h, ties = Ties_h + Ties_a) %>%
    mutate(HomeRec = paste0(Wins_h,"-", Defeats_h, "-",Ties_h), AwayRec = paste0(Wins_a,"-", Defeats_a, "-",Ties_a), Record = paste0(wins,"-", loses, "-", ties))
  
  
  Home1 <- select(season_2019_2020,Date, HomeTeam, FTHG, FTAG, FTR)
    Home10<- Home1 %>% rename(TeamName = HomeTeam)
   
   Away1 <- select(season_2019_2020,Date, AwayTeam, FTHG, FTAG, FTR)
    Away10 <- Away1 %>% rename(TeamName = AwayTeam)
    
 
  Home_and_Away_10 <- rbind(Home10, Away10)
  class(Home_and_Away_10$Date)
  Home_and_Away_10f<- Home_and_Away_10[order(Home_and_Away_10$Date, decreasing = TRUE),]
  Home_and_Away_10ff<-top_n(Home_and_Away_10f, 10, wt = Date)
  Home_and_Away_10ff_Stats<- Home_and_Away_10ff %>% group_by(TeamName) %>% 
      summarize(Wins_10 = sum(FTR == "A" | FTR == 'H'), Ties_10 = sum(FTR == "D"))
  HA10_record <- Home_and_Away_10ff_Stats %>% mutate(Last10_Wins_Ties = paste0(Wins_10, "-", Ties_10))%>%
    select(TeamName, Last10_Wins_Ties)
  
  Home_and_Away_stats_F <- Home_and_Away_stats[order(-Home_and_Away_stats$PPM,-Home_and_Away_stats$wins,-Home_and_Away_stats$GSM, Home_and_Away_stats$GAM),]
  Home_and_Away_stats_final <- left_join(Home_and_Away_stats_F, HA10_record, by = "TeamName")
  
  EPL <- Home_and_Away_stats_final %>% select(TeamName, Record, HomeRec, AwayRec, MatchesPlayed, Points, PPM, PtPct, GS, GSM, GA, GAM, Last10_Wins_Ties)
  
  
  return(data.frame(EPL))
  } else if (Seasonf == "2018/19") {
    
    
    #Get information 
    Datef <- mdy(Datef)
    season_2018_2019 <- read.csv(url("http://www.football-data.co.uk/mmz4281/1819/E0.csv"))
    season_2018_2019 <- select(season_2018_2019, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
    season_2018_2019$Date <- dmy(season_2018_2019$Date)
    
    #Points
    
    season_2018_2019 <- mutate( season_2018_2019, Home_Points = ifelse(FTR == "H", 3, ifelse(FTR == "D", 1, 0)))%>%
      mutate(Away_Points = ifelse(FTR == "A", 3, ifelse(FTR == "D", 1, 0))) %>%
      filter(Date<=Datef)
    #Home
    Home0 <- select(season_2018_2019,Date, HomeTeam, FTHG, FTAG, FTR, Home_Points)
    Home<- Home0 %>% rename(TeamName = HomeTeam, FTHG_h = FTHG, FTAG_h = FTAG, FTR_h = FTR)
    Home_stats <- Home %>% group_by(TeamName) %>% 
      summarize(Points_h = sum(Home_Points), Wins_h = sum(FTR_h == "H"), Defeats_h = sum(FTR_h == "A"), Ties_h = sum(FTR_h == "D"), Goals_made_h = sum(FTHG_h), Goals_against_h = sum(FTAG_h))
    
    #Away
    Away0 <- select(season_2018_2019,Date, AwayTeam, FTHG, FTAG, FTR, Away_Points)
    Away <- Away0 %>% rename(TeamName = AwayTeam, FTHG_a = FTHG, FTAG_a = FTAG, FTR_a = FTR)
    Away_stats <- Away %>% group_by(TeamName) %>% 
      summarize(Points_a = sum(Away_Points), Wins_a = sum(FTR_a == "A"), Defeats_a = sum(FTR_a == "H"), Ties_a = sum(FTR_a == "D"), Goals_made_a = sum(FTHG_a), Goals_against_a = sum(FTAG_a))
    
    Home_and_Away <- full_join(Home_stats, Away_stats, by = "TeamName")
  Home_and_Away_stats <- Home_and_Away %>% mutate(Points = Points_h + Points_a) %>%
    mutate(MatchesPlayed = Wins_a + Wins_h + Defeats_a + Defeats_h + Ties_h + Ties_a) %>%
    mutate(PtPct = Points / 3 * MatchesPlayed, PPM = Points/MatchesPlayed, GS = Goals_made_a + Goals_made_h) %>%
    mutate(GSM = GS / MatchesPlayed, GA = Goals_against_a + Goals_against_h, GAM = GA / MatchesPlayed) %>%
    mutate(wins = Wins_h + Wins_a, loses = Defeats_a + Defeats_h, ties = Ties_h + Ties_a) %>%
    mutate(HomeRec = paste0(Wins_h,"-", Defeats_h, "-",Ties_h), AwayRec = paste0(Wins_a,"-", Defeats_a, "-",Ties_a), Record = paste0(wins,"-", loses, "-", ties))
  
  
  Home1 <- select(season_2018_2019,Date, HomeTeam, FTHG, FTAG, FTR)
    Home10<- Home1 %>% rename(TeamName = HomeTeam)
   
   Away1 <- select(season_2018_2019,Date, AwayTeam, FTHG, FTAG, FTR)
    Away10 <- Away1 %>% rename(TeamName = AwayTeam)
    
 
  Home_and_Away_10 <- rbind(Home10, Away10)
  class(Home_and_Away_10$Date)
  Home_and_Away_10f<- Home_and_Away_10[order(Home_and_Away_10$Date, decreasing = TRUE),]
  Home_and_Away_10ff<-top_n(Home_and_Away_10f, 10, wt = Date)
  Home_and_Away_10ff_Stats<- Home_and_Away_10ff %>% group_by(TeamName) %>% 
      summarize(Wins_10 = sum(FTR == "A" | FTR == 'H'), Ties_10 = sum(FTR == "D"))
  HA10_record <- Home_and_Away_10ff_Stats %>% mutate(Last10_Wins_Ties = paste0(Wins_10, "-", Ties_10))%>%
    select(TeamName, Last10_Wins_Ties)
  
  Home_and_Away_stats_F <- Home_and_Away_stats[order(-Home_and_Away_stats$PPM,-Home_and_Away_stats$wins,-Home_and_Away_stats$GSM, Home_and_Away_stats$GAM),]
  Home_and_Away_stats_final <- left_join(Home_and_Away_stats_F, HA10_record, by = "TeamName")
  
  EPL <- Home_and_Away_stats_final %>% select(TeamName, Record, HomeRec, AwayRec, MatchesPlayed, Points, PPM, PtPct, GS, GSM, GA, GAM, Last10_Wins_Ties)
 
  
  return(data.frame(EPL))
  } else if (Seasonf == "2017/18") {
    
    
    
    #Get information 
    Datef <- mdy(Datef)
    season_2017_2018 <- read.csv(url("http://www.football-data.co.uk/mmz4281/1718/E0.csv"))
    season_2017_2018 <- select(season_2017_2018, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
    season_2017_2018$Date <- dmy(season_2017_2018$Date)
    
    #Points
    
    season_2017_2018 <- mutate( season_2017_2018, Home_Points = ifelse(FTR == "H", 3, ifelse(FTR == "D", 1, 0)))%>%
      mutate(Away_Points = ifelse(FTR == "A", 3, ifelse(FTR == "D", 1, 0))) %>%
      filter(Date<=Datef)
    #Home
    Home0 <- select(season_2017_2018,Date, HomeTeam, FTHG, FTAG, FTR, Home_Points)
    Home<- Home0 %>% rename(TeamName = HomeTeam, FTHG_h = FTHG, FTAG_h = FTAG, FTR_h = FTR)
    Home_stats <- Home %>% group_by(TeamName) %>% 
      summarize(Points_h = sum(Home_Points), Wins_h = sum(FTR_h == "H"), Defeats_h = sum(FTR_h == "A"), Ties_h = sum(FTR_h == "D"), Goals_made_h = sum(FTHG_h), Goals_against_h = sum(FTAG_h))
    
    #Away
    Away0 <- select(season_2017_2018,Date, AwayTeam, FTHG, FTAG, FTR, Away_Points)
    Away <- Away0 %>% rename(TeamName = AwayTeam, FTHG_a = FTHG, FTAG_a = FTAG, FTR_a = FTR)
    Away_stats <- Away %>% group_by(TeamName) %>% 
      summarize(Points_a = sum(Away_Points), Wins_a = sum(FTR_a == "A"), Defeats_a = sum(FTR_a == "H"), Ties_a = sum(FTR_a == "D"), Goals_made_a = sum(FTHG_a), Goals_against_a = sum(FTAG_a))
    
    Home_and_Away <- full_join(Home_stats, Away_stats, by = "TeamName")
    Home_and_Away_stats <- Home_and_Away %>% mutate(Points = Points_h + Points_a) %>%
      mutate(MatchesPlayed = Wins_a + Wins_h + Defeats_a + Defeats_h + Ties_h + Ties_a) %>%
      mutate(PtPct = Points / 3 * MatchesPlayed, PPM = Points/MatchesPlayed, GS = Goals_made_a + Goals_made_h) %>%
      mutate(GSM = GS / MatchesPlayed, GA = Goals_against_a + Goals_against_h, GAM = GA / MatchesPlayed) %>%
      mutate(wins = Wins_h + Wins_a, loses = Defeats_a + Defeats_h, ties = Ties_h + Ties_a) %>%
      mutate(HomeRec = paste0(Wins_h,"-", Defeats_h, "-",Ties_h), AwayRec = paste0(Wins_a,"-", Defeats_a, "-",Ties_a), Record = paste0(wins,"-", loses, "-", ties))
    
    
    Home1 <- select(season_2017_2018,Date, HomeTeam, FTHG, FTAG, FTR)
    Home10<- Home1 %>% rename(TeamName = HomeTeam)
    
    Away1 <- select(season_2017_2018,Date, AwayTeam, FTHG, FTAG, FTR)
    Away10 <- Away1 %>% rename(TeamName = AwayTeam)
    
    
    Home_and_Away_10 <- rbind(Home10, Away10)
    class(Home_and_Away_10$Date)
    Home_and_Away_10f<- Home_and_Away_10[order(Home_and_Away_10$Date, decreasing = TRUE),]
    Home_and_Away_10ff<-top_n(Home_and_Away_10f, 10, wt = Date)
    Home_and_Away_10ff_Stats<- Home_and_Away_10ff %>% group_by(TeamName) %>% 
      summarize(Wins_10 = sum(FTR == "A" | FTR == 'H'), Ties_10 = sum(FTR == "D"))
    HA10_record <- Home_and_Away_10ff_Stats %>% mutate(Last10_Wins_Ties = paste0(Wins_10, "-", Ties_10))%>%
      select(TeamName, Last10_Wins_Ties)
    
    Home_and_Away_stats_F <- Home_and_Away_stats[order(-Home_and_Away_stats$PPM,-Home_and_Away_stats$wins,-Home_and_Away_stats$GSM, Home_and_Away_stats$GAM),]
    Home_and_Away_stats_final <- left_join(Home_and_Away_stats_F, HA10_record, by = "TeamName")
    
    EPL <- Home_and_Away_stats_final %>% select(TeamName, Record, HomeRec, AwayRec, MatchesPlayed, Points, PPM, PtPct, GS, GSM, GA, GAM, Last10_Wins_Ties)
    
    
    return(data.frame(EPL))

  } else {print("Not Included or wrong format")}
   
  }


#Test the Funtion 

EPL_Standings("10/18/2024", "2020/21")

