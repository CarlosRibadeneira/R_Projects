
#Project 1- Carlos Ribadeneira

#1- Temp categorizer 
temp_categorizer <- function(temperature) {
  if (temperature >= 90)
    temp <- "hot"
  else if(temperature > 60 & temperature < 90)
    temp <- "warm"
  else if(temperature > 32 & temperature < 60)
    temp <- "cold" 
  else if (temperature <= 32)
    temp <- "freezing"
  return(temp)
  
}
temp_categorizer(32)

#2-For Loop
sentence <- c("Learning", "loops", "in", "R", "is", "not", "that", "bad")
at_length <- function(value) {
  
  for(i in sentence){
    if (nchar(i) == value)
      print(paste(i, "has", value, "letters"))
    
  }
} 

#Test the function
at_length(8)



# joins
library(tidyverse)
library(nycflights13)

airlines
airports
planes
weather

# lets join the airline name into the flights table
flights2 <- flights %>% 
  select(year:day, hour, origin, dest, tailnum, carrier)

flights2 %>%
  select(-origin, -dest) %>% 
  left_join(airlines, by = "carrier")


# notice the default behavior of dplyr is a natural join
flights2 %>% 
  left_join(weather)

# Compute the average delay by destination, then join on the airports data frame so you can show the spatial distribution of delays. 
avg_delay_table <- flights %>% 
  group_by(dest) %>% 
  summarize(avg_delay = mean(dep_delay, na.rm = T)) %>% 
  arrange(-avg_delay)


# Here is an easy way to draw a map of the United States:
airports %>%
  right_join(avg_delay_table, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat, color = avg_delay, size = avg_delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap()


