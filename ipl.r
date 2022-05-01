                                              ###IPL DTA SET ANALYIS WITH R PROGRAMMING USING DPLYR PACKAGE###

#Load dplyr package 
library(dplyr)

#Read data sets
matches_data<-read.csv("C:/Users/Yaswanth Karri/Desktop/matches.csv")
deliveries_data <- read.csv("C:/Users/Yaswanth Karri/Desktop/deliveries.csv/deliveries.csv")

#Integrating both datasets
data <- bind_rows(matches_data,deliveries_data)

#Data Cleaning
data<- select(data, -c(umpire1,umpire2))

#Knowing the data
head(matches_data)
head(deliveries_data)
str(matches_data)
str(deliveries_data)

#Total number of matches in the dataset
length(data$id) 
##151216 matche

#Total number of seasons 
unique(data$season) 
##Total IPL seasons from the data is 13

#Total number of matches played in each season
matches_data %>% 
  group_by(season) %>%
  dplyr::summarize(number_of_matches = n())%>% arrange(desc(number_of_matches))

##season number_of_matches##
#   2013                76
#   2012                74
#   2011                73
#   2010                60
#   2014                60
#   2016                60
#   2018                60
#   2019                60
#   2015                59
#   2017                59
#   2008                58
#   2009                57

#Number of matches played at a particular venue in descending order
data%>% 
  group_by(venue) %>%
  dplyr::summarize(nmatches = n())%>%arrange(desc(nmatches))

##venue                                        nmatches
#1 NA                                         150460
#2 Eden Gardens                                   77
#3 M Chinnaswamy Stadium                          73
#4 Wankhede Stadium                               73
#5 Feroz Shah Kotla                               67
#6 Rajiv Gandhi International Stadium, Uppal      56
#7 MA Chidambaram Stadium, Chepauk                49
#8 Sawai Mansingh Stadium                         47
#9 Punjab Cricket Association Stadium, Mohali     35
#10 Maharashtra Cricket Association Stadium        21
# ... with 32 more rows

#IPL teams and their number of wins in descending order
matches_data %>% 
  group_by(winner) %>%
  dplyr::summarize(wins = n())%>% arrange(desc(wins))

##     winner                      wins
#1 "Mumbai Indians"                109
#2 "Chennai Super Kings"           100
#3 "Kolkata Knight Riders"          92
#4 "Royal Challengers Bangalore"    84
#5 "Kings XI Punjab"                82
#6 "Rajasthan Royals"               75
#7 "Delhi Daredevils"               67
#8 "Sunrisers Hyderabad"            58
#9 "Deccan Chargers"                29
#10 "Gujarat Lions"                 13
#11 "Pune Warriors"                 12
#12 "Delhi Capitals"                10
#13 "Rising Pune Supergiant"        10
#14 "Kochi Tuskers Kerala"           6
#15 "Rising Pune Supergiants"        5
#16 ""                               4


#Which team is dominating in a certain locations?
data%>% 
  filter(result != 'no result') %>% group_by(winner,city) %>% 
  dplyr::summarise(win = n()) %>% arrange(desc(win))

##winner                      city         win
#1 Mumbai Indians              Mumbai        53
#2 Kolkata Knight Riders       Kolkata       45
#3 Chennai Super Kings         Chennai       40
#4 Rajasthan Royals            Jaipur        32
#5 Royal Challengers Bangalore Bangalore     30
#6 Sunrisers Hyderabad         Hyderabad     30
#7 Delhi Daredevils            Delhi         27
#8 Kings XI Punjab             Chandigarh    22
#9 Chennai Super Kings         Mumbai        11
#10 Mumbai Indians              Kolkata       10
# ... with 206 more rows


#Which teams not able to perform well in non-home location??
data%>% 
  filter(result != 'no result') %>% group_by(winner,city) %>% 
  dplyr::summarise(win = n()) %>% arrange((win))

##winner              city             win
#1 Chennai Super Kings Abu Dhabi        1
#2 Chennai Super Kings Bengaluru        1
#3 Chennai Super Kings Dharamsala       1
#4 Chennai Super Kings Durban           1
#5 Chennai Super Kings East London      1
#6 Chennai Super Kings Johannesburg     1
#7 Chennai Super Kings Kimberley        1
#8 Chennai Super Kings Sharjah          1
#9 Deccan Chargers     Bangalore        1
#10 Deccan Chargers     Kimberley       1
# ... with 206 more rows

#Who are best IPL batsmen
data%>% 
  group_by(batsman) %>% 
  dplyr::summarise(runs =length(batsman_runs)) %>% arrange(desc(runs))
#   batsman     runs
#1 V Kohli     3494
#2 G Gambhir   3433
#3 SK Raina    3369
#4 RG Sharma   3274
#5 S Dhawan    3005
#6 RV Uthappa  2960
#7 DA Warner   2902
#8 MS Dhoni    2680
#9 AM Rahane   2602
#10 CH Gayle    2532
# ... with 452 more rows

#Who are best IPL bowlers
deliveries_data %>% group_by(bowler) %>% filter(player_dismissed!="") %>% 
  dplyr::summarise(wickets= n())%>% arrange(desc(wickets)) 

#bowler          wickets
#1 SL Malinga          170
#2 A Mishra            142
#3 DJ Bravo            137
#4 Harbhajan Singh     136
#5 PP Chawla           133
#6 R Vinay Kumar       125
#7 A Nehra             121
#8 Z Khan              119
#9 B Kumar             117
#10 R Ashwin            110
# ... with 304 more rows

#Most Man of the match awards
matches_data %>% 
  group_by(player_of_match) %>%
  dplyr::summarize(awards = n())%>% arrange(desc(awards))

#player_of_match awards
#1 CH Gayle            21
#2 AB de Villiers      20
#3 DA Warner           17
#4 MS Dhoni            17
#5 RG Sharma           17
#6 YK Pathan           16
#7 SR Watson           15
#8 SK Raina            14
#9 G Gambhir           13
#10 AM Rahane           12
# ... with 217 more rows

