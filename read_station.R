library(dplyr)
library(tidyverse)
library(data.table)
library(ggplot2)
getwd()
setwd("F:/Directory/")

coor_stations<-read.csv("Air_temp_stations.csv", sep=";")


Air_temp_list<-list.files(path="./Air_temp", pattern=".txt", all.files=FALSE,
           full.names=TRUE)

li <- purrr::map(Air_temp_list,
                 function(file){
                   df <- read.csv(file, sep ="" , header = F)
                   current_names <- names(df)
                   return(df)
                 }
)

output <- bind_rows(li)
View(output)
colnames(output)<-c("code", "year", "1", "2", "3", "4", "5", "6", "7", "8", "9","10","11","12")
#Stats stations per month. 
x<-output %>% 
  select(-year)%>% #eliminate years
  na.omit()%>%     #eliminate na
    group_by(code)%>% 
   summarise(across(everything(), mean))
names(output$code)
y<-output %>% 
  na.omit() %>% 
  filter(year=="2005") %>% 
  filter(code %in% c("20674", "21432", "23418", "23589")) %>% 
  group_by(code)

boxplot(y[3:14])

write.csv(x,"Temp_average.csv")

#Join code stations
stations<-coor_stations%>%dplyr::select(code,Lat,Lon)
stations[stations < 0] <- NA  #to evaoid any mistake in negative coord

x3<-inner_join(x, stations, by="code")  #evaluate if this stations are inside of our zone
#Y is data without average calculation
View(x3)
x3$Lat<-as.double(x3$Lat)
x3$Lon<-as.double(x3$Lon)

#plot stations
world <- map_data("world")

ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "black", fill = "lightgray", size = 0.1
  ) +
  geom_point(
    data = x3,
    aes(Lon, Lat, color = "Stations"),
    alpha = 0.7, colour="red"
    
  )+labs(title = "Air temperature stations") 

