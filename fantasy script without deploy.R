#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(baseballr)
library(tidyverse)
library(zoo)
library(rsconnect)
library(shiny)

team_ids <-load_ncaa_baseball_teams()
x<-ncaa_team_player_stats(694,2024,'pitching')
###checking rosters for correct names###
x<- ncaa_baseball_roster(193,2024)
y<-ncaa_baseball_roster(234,2024)
######Points for offensive stats###########
off_stats <- c(1, 2, 3, 4, 1, 1, 1, 1, -2)
pitch_stats<- c(1,1,5,2,-1,-1,-5)
####all team names for every player drafted
teams <- team_ids %>%
     filter(
          team_name %in% c('Wake Forest', 'Oregon St.', 'Old Dominion', 'California', 'Col. of Charleston', 'Air Force', 'Louisiana', 'Tennessee', 'Oklahoma St.', 'Northeastern', 'Maine', 'Davidson', 'South Alabama', 'Campbell', 'UC San Diego', 'Iowa', 'TCU', 'Vanderbilt', 'Virginia', 'Kansas', 'Florida', 'West Virginia', 'Troy', 'Texas A&M', 'Coastal Carolina', 'LSU', 'William & Mary', 'NC State', 'Delaware', 'Washington', 'Mercer', 'North Carolina', 'DBU', 'ETSU', 'VCU', 'Lipscomb', 'Oral Roberts', 'UC Santa Barbara', 'Texas', 'Duke', 'South Carolina', 'Texas Tech', 'Wright St.', 'Indiana', 'Mississippi St.', 'UC Irvine', 'Jacksonville', 'East Carolina', 'Stanford', 'Georgia', 'Fairleigh Dickinson', 'VMI', 'Arkansas', 'UNC Asheville', 'Ole Miss', 'Florida St.', 'Little Rock', 'Kansas State', 'Houston', 'Clemson', 'Oregon')
     ) %>%
     select(team_name, team_id)%>% dplyr::distinct()

################Garrett###############
players=c('Drew Beam','Brody Brecht','Peyton Tolle','Grayson Carter','Ethan Anderson','Sam Kulasingam','Kodey Shojinaga', 'Colby Shelton','JJ Wetherholt','Shane Lewis','Braden Montgomery','Seaver King','Derek Bender','Cam Maldonado','Rodney Green Jr','Dariyan Pendergrass','Gage Jump')

###Changing order of names#####
players <- str_extract_all(players, "\\b\\w+\\b") %>%
     sapply(function(x) paste(rev(x), collapse = ", ")) %>%
     unname()
players<- as.data.frame(players)
players$players[15]<- 'Green Jr., Rodney' ####players with special characters just had to change manually 
players$players[9]<- 'Wetherholt, J.J.'
#moving rodney green for jj wetherholt
players<-players[-c(1:4,9,14,16,17),]
players


##hitters##

#[1] "Anderson, Ethan"      "Kulasingam, Sam"     
#[3] "Shojinaga, Kodey"     "Shelton, Colby"      
#[5] "Whetherholt, JJ"      "Lewis, Shane"        
#[7] "Montgomery, Braden"   "King, Seaver"        
#[9] "Bender, Derek"        "Maldonaldo, Cam"     
#[11] "Jr, Green, Rodney"    "Pendergrass, Dariyan"

g_id=c(746,721,328,235,716,697,749,149,107)##list of team ids for hitters above 


results <- list()
##for loop that is connecting the id to the player and then pulling their stats out and storing in a list
for (i in seq_along(g_id)) {
     id <- g_id[i]
     player <- as.character(players[i])
     
     player_stats <- ncaa_team_player_stats(id, 2024, 'batting') %>%
          dplyr::filter(player_name == player) %>%
          dplyr::select(player_name,H,`2B`,`3B`, HR, BB,HBP,RBI, SB,CS)
     
     results[[i]] <- player_stats
}
#binding rows to make dataframe 
g_stats <- bind_rows(results)
g_stats<- na.fill(g_stats, 0)#i guess this messes with those columns 
g_stats<- as.data.frame(g_stats)
# Convert columns to numeric
g_stats[, c(2:10)] <- lapply(g_stats[, c(2:10)], base::as.numeric)

# Calculate 1B column
g_stats$`1B` <- g_stats$H - g_stats$`2B` - g_stats$`3B` - g_stats$HR
order<- c('player_name','1B','2B','3B','HR','BB','HBP','RBI','SB','CS')
g_stats<-g_stats[,order]
# Select columns from '1B' to 'CS' and multiply by off_stats
# Multiply each column from '1B' to 'CS' by the corresponding element in off_stats
g_stats[, c("1B", "2B", "3B", "HR", "BB", "HBP", "RBI", "SB", "CS")] <- 
     g_stats[, c("1B", "2B", "3B", "HR", "BB", "HBP", "RBI", "SB", "CS")] * rep(off_stats, each = nrow(g_stats))

g_stats$Total<-rowSums(g_stats[, c("1B",'2B','3B', "HR", "BB", "HBP", "RBI", "SB", "CS")])
sum<- sum(g_stats$Total)
total_row <- c("Total", NA, NA, NA,NA,NA,NA,NA,NA,NA, sum)
g_stats<- rbind(g_stats, total_row)

####Pitchers######
pitchers<-c('Drew Beam','Brody Brecht','Payton Tolle','Greysen Carter')
pitchers <- str_extract_all(pitchers, "\\b\\w+\\b") %>%
     sapply(function(x) paste(rev(x), collapse = ", ")) %>%
     unname()
g_id=c(694,312,698,736)##list of team ids for hitters above 


results <- list()

## For loop that is connecting the id to the player and then pulling their stats out and storing in a list
for (i in seq_along(g_id)) {
     id <- g_id[i]
     pitcher <- as.character(pitchers[i])
     
     player_stats <- ncaa_team_player_stats(id, 2024, 'pitching') %>%
          dplyr::filter(player_name == pitcher) %>%
          dplyr::select(player_name, IP, SO, W, SV, BB, HB, `HR-A`)
     
     results[[i]] <- player_stats
}
g_pitch<- bind_rows(results)
g_pitch<- na.fill(g_pitch, 0)#i guess this messes with those columns 
g_pitch<- as.data.frame(g_pitch)
# Convert columns to numeric
g_pitch[, c(2:8)] <- lapply(g_pitch[, c(2:8)], base::as.numeric)
g_pitch[, c('IP','SO','W','SV','BB','HB','HR-A')] <- 
     g_pitch[, c('IP','SO','W','SV','BB','HB','HR-A')] * rep(pitch_stats, each = nrow(g_pitch))

g_pitch$Total<-rowSums(g_pitch[, c('IP','SO','W','SV','BB','HB','HR-A')])
sum<- sum(g_pitch$Total)
total_row <- c("Total", NA, NA, NA,NA,NA,NA,NA, sum)
g_pitch<- rbind(g_pitch, total_row)
#######################Mason###################
players=c('Chase Burns',
          'Ayden May',
          'Michael Massey',
          'Holobetz',
          'Caleb Lomavita',
          'Cole Mathis',
          'Travis Bazzana',
          'Jay Thomason',
          'Kyle DeBarge',
          'Dylan Dreiling',
          'Carson Benge',
          'Mike Sirota',
          'Reed Stallman',
          'Jacob Friend',
          'Joseph Sullivan',
          'Jeremiah Jenkins',
          'Ryan Fortucci')

###Changing order of names#####
players <- str_extract_all(players, "\\b\\w+\\b") %>%
     sapply(function(x) paste(rev(x), collapse = ", ")) %>%
     unname()
players<- as.data.frame(players)

players<-players[-c(1:4,14:17),]
players


##hitters##
#[1] "Lomavita, Caleb"   "Mathis, Cole"      "Bazzana, Travis"   "Thomason, Jay"     "Debarge, Kyle"     "Dreling, Dylan"   
#[7] "Benge, Carson"     "Sirota, Mike"      "Jenkins, Jeremiah" "Friend, Jacob"     "Sullivan, Joseph"  "Stallman, Reed"


ma_id=c(107,1014,528,721,671,694,521,500,115)##list of team ids for hitters above 


results <- list()
##for loop that is connecting the id to the player and then pulling their stats out and storing in a list
for (i in seq_along(ma_id)) {
     id <- ma_id[i]
     player <- as.character(players[i])
     
     player_stats <- ncaa_team_player_stats(id, 2024, 'batting') %>%
          dplyr::filter(player_name == player) %>%
          dplyr::select(player_name,H,`2B`,`3B`, HR, BB,HBP,RBI, SB,CS)
     
     results[[i]] <- player_stats
}
#binding rows to make dataframe 
ma_stats <- bind_rows(results)
ma_stats<- na.fill(ma_stats, 0)#i guess this messes with those columns 
ma_stats<- as.data.frame(ma_stats)
# Convert columns to numeric
ma_stats[, c(2:10)] <- lapply(ma_stats[, c(2:10)], base::as.numeric)

# Calculate 1B column
ma_stats$`1B` <- ma_stats$H - ma_stats$`2B` - ma_stats$`3B` - ma_stats$HR
order<- c('player_name','1B','2B','3B','HR','BB','HBP','RBI','SB','CS')
ma_stats<-ma_stats[,order]
# Select columns from '1B' to 'CS' and multiply by off_stats
# Multiply each column from '1B' to 'CS' by the corresponding element in off_stats
ma_stats[, c("1B", "2B", "3B", "HR", "BB", "HBP", "RBI", "SB", "CS")] <- 
     ma_stats[, c("1B", "2B", "3B", "HR", "BB", "HBP", "RBI", "SB", "CS")] * rep(off_stats, each = nrow(ma_stats))
ma_stats$Total<-rowSums(ma_stats[, c("1B",'2B','3B', "HR", "BB", "HBP", "RBI", "SB", "CS")])
sum<- sum(ma_stats$Total)
total_row <- c("Total", NA, NA, NA,NA,NA,NA,NA,NA,NA, sum)
ma_stats<- rbind(ma_stats, total_row)

####Pitchers#####
pitchers<-c('Chase Burns',
            'Ryan Forcucci',
            'Michael Massey',
            'John Holobetz')
pitchers <- str_extract_all(pitchers, "\\b\\w+\\b") %>%
     sapply(function(x) paste(rev(x), collapse = ", ")) %>%
     unname()
ma_id=c(749,112,749,523)##list of team ids for hitters above 


results <- list()

## For loop that is connecting the id to the player and then pulling their stats out and storing in a list
for (i in seq_along(ma_id)) {
     id <- ma_id[i]
     pitcher <- as.character(pitchers[i])
     
     player_stats <- ncaa_team_player_stats(id, 2024, 'pitching') %>%
          dplyr::filter(player_name == pitcher) %>%
          dplyr::select(player_name, IP, SO, W, SV, BB, HB, `HR-A`)
     
     results[[i]] <- player_stats
}
ma_pitch<- bind_rows(results)
ma_pitch<- na.fill(ma_pitch, 0)#i guess this messes with those columns 
ma_pitch<- as.data.frame(ma_pitch)
# Convert columns to numeric
ma_pitch[, c(2:8)] <- lapply(ma_pitch[, c(2:8)], base::as.numeric)
ma_pitch[, c('IP','SO','W','SV','BB','HB','HR-A')] <- 
     ma_pitch[, c('IP','SO','W','SV','BB','HB','HR-A')] * rep(pitch_stats, each = nrow(ma_pitch))

ma_pitch$Total<-rowSums(ma_pitch[, c('IP','SO','W','SV','BB','HB','HR-A')])
sum<- sum(ma_pitch$Total)
total_row <- c("Total", NA, NA, NA,NA,NA,NA,NA, sum)
ma_pitch<- rbind(ma_pitch, total_row)
##############Micah##############
players=c('Thatcher Hurd',
          'Cole Klecker',
          'Aivan Cabral',
          'Carter Lovasz',
          'Jacob Cozart',
          'Nick Kurtz',
          'Davis Diaz',
          'Joey Loynd',
          'Cam Clayton',
          'Ty Dalley',
          'Vance Honeycutt',
          'Nathan Humphreys',
          'Cameron Sisneros',
          'Brandon Eike',
          'Gavin Turley',
          'Austin Kelly',
          'Brooks Fowler')

###Changing order of names#####
players <- str_extract_all(players, "\\b\\w+\\b") %>%
     sapply(function(x) paste(rev(x), collapse = ", ")) %>%
     unname()
players<- as.data.frame(players)

players<-players[-c(1:4,14:17),]
players


##hitters##
#"Kozart, Jacob"     "Kurtz, Nick"       "Diaz, Davis"       "Loynd, Joey"       "Clayton, Cam"      "Dalley, Ty"       
#[7] "Honeycutt, Vance"  "Humphreys, Nate"   "Sisneros, Cameron" "Eike, Brandon"     "Turley, Gavin"     "Kelly, Austin"


mi_id=c(490,749,736,180,756,406,457,1045,198)##list of team ids for hitters above 


results <- list()
##for loop that is connecting the id to the player and then pulling their stats out and storing in a list
for (i in seq_along(mi_id)) {
     id <- mi_id[i]
     player <- as.character(players[i])
     
     player_stats <- ncaa_team_player_stats(id, 2024, 'batting') %>%
          dplyr::filter(player_name == player) %>%
          dplyr::select(player_name,H,`2B`,`3B`, HR, BB,HBP,RBI, SB,CS)
     
     results[[i]] <- player_stats
}
#binding rows to make dataframe 
mi_stats <- bind_rows(results)
mi_stats<- na.fill(mi_stats, 0)#i guess this messes with those columns 
mi_stats<- as.data.frame(mi_stats)
# Convert columns to numeric
mi_stats[, c(2:10)] <- lapply(mi_stats[, c(2:10)], base::as.numeric)

# Calculate 1B column
mi_stats$`1B` <- mi_stats$H - mi_stats$`2B` - mi_stats$`3B` - mi_stats$HR
order<- c('player_name','1B','2B','3B','HR','BB','HBP','RBI','SB','CS')
mi_stats<-mi_stats[,order]
# Select columns from '1B' to 'CS' and multiply by off_stats
# Multiply each column from '1B' to 'CS' by the corresponding element in off_stats
mi_stats[, c("1B", "2B", "3B", "HR", "BB", "HBP", "RBI", "SB", "CS")] <- 
     mi_stats[, c("1B", "2B", "3B", "HR", "BB", "HBP", "RBI", "SB", "CS")] * rep(off_stats, each = nrow(mi_stats))
mi_stats$Total<-rowSums(mi_stats[, c("1B",'2B','3B', "HR", "BB", "HBP", "RBI", "SB", "CS")])
sum<- sum(mi_stats$Total)
total_row <- c("Total", NA, NA, NA,NA,NA,NA,NA,NA,NA, sum)
mi_stats<- rbind(mi_stats, total_row)
####Pitchers#####
pitchers<-c('Thatcher Hurd',
            'Kole Klecker',
            'Aiven Cabral',
            'Carter Lovasz')
pitchers <- str_extract_all(pitchers, "\\b\\w+\\b") %>%
     sapply(function(x) paste(rev(x), collapse = ", ")) %>%
     unname()
mi_id=c(365,698,500,786)##list of team ids for hitters above 


results <- list()

## For loop that is connecting the id to the player and then pulling their stats out and storing in a list
for (i in seq_along(mi_id)) {
     id <- mi_id[i]
     pitcher <- as.character(pitchers[i])
     
     player_stats <- ncaa_team_player_stats(id, 2024, 'pitching') %>%
          dplyr::filter(player_name == pitcher) %>%
          dplyr::select(player_name, IP, SO, W, SV, BB, HB, `HR-A`)
     
     results[[i]] <- player_stats
}
mi_pitch<- bind_rows(results)
mi_pitch<- na.fill(mi_pitch, 0)#i guess this messes with those columns 
mi_pitch<- as.data.frame(mi_pitch)
# Convert columns to numeric
mi_pitch[, c(2:8)] <- lapply(mi_pitch[, c(2:8)], base::as.numeric)
mi_pitch[, c('IP','SO','W','SV','BB','HB','HR-A')] <- 
     mi_pitch[, c('IP','SO','W','SV','BB','HB','HR-A')] * rep(pitch_stats, each = nrow(mi_pitch))

mi_pitch$Total<-rowSums(mi_pitch[, c('IP','SO','W','SV','BB','HB','HR-A')])
sum<- sum(mi_pitch$Total)
total_row <- c("Total", NA, NA, NA,NA,NA,NA,NA, sum)
mi_pitch<- rbind(mi_pitch, total_row)
###############Luke##############
players=c('Matt Ager',
          'Lebarron Johnson',
          'Luke Holman',
          'Fran Oschell',
          'Cole Messina',
          'Gavin Kash',
          'Christian Moore',
          'Tommy White',
          'Anthony Silva',
          'Andrew Patrick',
          'Jace Laviolette',
          'Nolan Schubart',
          'Trace Willhoite',
          'Devin Taylor',
          'Hunter Hines',
          'Anthony Martinez',
          'Evan Chrest')

###Changing order of names#####
players <- str_extract_all(players, "\\b\\w+\\b") %>%
     sapply(function(x) paste(rev(x), collapse = ", ")) %>%
     unname()
players<- as.data.frame(players)

players<-players[-c(1:4,14:17),]
players


##hitters##
#"Messina, Cole"     "Kash, Gavin"       "Moore, Christian"  "White, Tommy"      "Silva, Anthony"    "Patrick, Andrew"  
#[7] "Lavoilette, Jace"  "Schubert, Nolan"   "Wilhoit, Trace"    "Taylor, Devin"     "Hines, Hunter"     "Martinez, Anthony"


l_id=c(648,700,694,365,698,810,697,521,28600)##list of team ids for hitters above 


results <- list()
##for loop that is connecting the id to the player and then pulling their stats out and storing in a list
for (i in seq_along(l_id)) {
     id <- l_id[i]
     player <- as.character(players[i])
     
     player_stats <- ncaa_team_player_stats(id, 2024, 'batting') %>%
          dplyr::filter(player_name == player) %>%
          dplyr::select(player_name,H,`2B`,`3B`, HR, BB,HBP,RBI, SB,CS)
     
     results[[i]] <- player_stats
}
#binding rows to make dataframe 
l_stats <- bind_rows(results)
l_stats<- na.fill(l_stats, 0)#i guess this messes with those columns 
l_stats<- as.data.frame(l_stats)
# Convert columns to numeric
l_stats[, c(2:10)] <- lapply(l_stats[, c(2:10)], base::as.numeric)

# Calculate 1B column
l_stats$`1B` <- l_stats$H - l_stats$`2B` - l_stats$`3B` - l_stats$HR
order<- c('player_name','1B','2B','3B','HR','BB','HBP','RBI','SB','CS')
l_stats<-l_stats[,order]
# Select columns from '1B' to 'CS' and multiply by off_stats
# Multiply each column from '1B' to 'CS' by the corresponding element in off_stats
l_stats[, c("1B", "2B", "3B", "HR", "BB", "HBP", "RBI", "SB", "CS")] <- 
     l_stats[, c("1B", "2B", "3B", "HR", "BB", "HBP", "RBI", "SB", "CS")] * rep(off_stats, each = nrow(l_stats))
l_stats$Total<-rowSums(l_stats[, c("1B",'2B','3B', "HR", "BB", "HBP", "RBI", "SB", "CS")])
sum<- sum(l_stats$Total)
total_row <- c("Total", NA, NA, NA,NA,NA,NA,NA,NA,NA, sum)
l_stats<- rbind(l_stats, total_row)

####Pitchers#####
pitchers<-c('Matt Ager',
            'Lebarron Johnson',
            'Luke Holman',
            'Fran Oschell')
pitchers <- str_extract_all(pitchers, "\\b\\w+\\b") %>%
     sapply(function(x) paste(rev(x), collapse = ", ")) %>%
     unname()
pitchers[2]<-'Johnson Jr., Lebarron'
pitchers[4]<-'Oschell III, Fran'
l_id=c(104,703,365,193)##list of team ids for hitters above 


results <- list()

## For loop that is connecting the id to the player and then pulling their stats out and storing in a list
for (i in seq_along(l_id)) {
     id <- l_id[i]
     pitcher <- as.character(pitchers[i])
     
     player_stats <- ncaa_team_player_stats(id, 2024, 'pitching') %>%
          dplyr::filter(player_name == pitcher) %>%
          dplyr::select(player_name, IP, SO, W, SV, BB, HB, `HR-A`)
     
     results[[i]] <- player_stats
}
l_pitch<- bind_rows(results)
l_pitch<- na.fill(l_pitch, 0)#i guess this messes with those columns 
l_pitch<- as.data.frame(l_pitch)
# Convert columns to numeric
l_pitch[, c(2:8)] <- lapply(l_pitch[, c(2:8)], base::as.numeric)
l_pitch[, c('IP','SO','W','SV','BB','HB','HR-A')] <- 
     l_pitch[, c('IP','SO','W','SV','BB','HB','HR-A')] * rep(pitch_stats, each = nrow(l_pitch))

l_pitch$Total<-rowSums(l_pitch[, c('IP','SO','W','SV','BB','HB','HR-A')])
sum<- sum(l_pitch$Total)
total_row <- c("Total", NA, NA, NA,NA,NA,NA,NA, sum)
l_pitch<- rbind(l_pitch, total_row)
################Colby##############
players=c('Josh Hartle',
          'Trey Yesavage',
          'Ryan Johnson',
          'Brandon Neely',
          'Malcolm Moore',
          'Charlie Condon',
          'Hunter Demato',
          'Justin Starke',
          'Wehiwa Aloy',
          'Robbie Burnett',
          'Dakota Jordan',
          'Lawson Harrill',
          'Tyler MacGregor',
          'Treyson Hughes',
          'Cannon Peebles',
          "Griff O'Ferrell",
          'Jacob Hall')

###Changing order of names#####
players <- str_extract_all(players, "\\b\\w+\\b") %>%
     sapply(function(x) paste(rev(x), collapse = ", ")) %>%
     unname()
players[7]<-"D'Amato, Hunter"
players[16]<-"O'Ferrall, Griff"
players<- as.data.frame(players)

players<-players[-c(1:4,14:17),]
players


##hitters##
#[1] "Moore, Malcolm"    "Condon, Charlie"   "Demato, Hunter"    "Starke, Justin"    "Aloy, Wehiwa"      "Burnett, Robbie"  
#[7] "Jordan, Dakota"    "Harrill, Lawson"   "MacGregor, Tyler"  "Hughes, Treyson"   "Peebles, Cannon"   "Ferrell, O, Griff"


c_id=c(674,257,222,741,31,456,430,115,500)##list of team ids for hitters above 


results <- list()
##for loop that is connecting the id to the player and then pulling their stats out and storing in a list
for (i in seq_along(c_id)) {
     id <- c_id[i]
     player <- as.character(players[i])
     
     player_stats <- ncaa_team_player_stats(id, 2024, 'batting') %>%
          dplyr::filter(player_name == player) %>%
          dplyr::select(player_name,H,`2B`,`3B`, HR, BB,HBP,RBI, SB,CS)
     
     results[[i]] <- player_stats
}
#binding rows to make dataframe 
c_stats <- bind_rows(results)
c_stats<- na.fill(c_stats, 0)#i guess this messes with those columns 
c_stats<- as.data.frame(c_stats)
# Convert columns to numeric
c_stats[, c(2:10)] <- lapply(c_stats[, c(2:10)], base::as.numeric)

# Calculate 1B column
c_stats$`1B` <- c_stats$H - c_stats$`2B` - c_stats$`3B` - c_stats$HR
order<- c('player_name','1B','2B','3B','HR','BB','HBP','RBI','SB','CS')
c_stats<-c_stats[,order]
# Select columns from '1B' to 'CS' and multiply by off_stats
# Multiply each column from '1B' to 'CS' by the corresponding element in off_stats
c_stats[, c("1B", "2B", "3B", "HR", "BB", "HBP", "RBI", "SB", "CS")] <- 
     c_stats[, c("1B", "2B", "3B", "HR", "BB", "HBP", "RBI", "SB", "CS")] * rep(off_stats, each = nrow(c_stats))
c_stats$Total<-rowSums(c_stats[, c("1B",'2B','3B', "HR", "BB", "HBP", "RBI", "SB", "CS")])
sum<- sum(c_stats$Total)
total_row <- c("Total", NA, NA, NA,NA,NA,NA,NA,NA,NA, sum)
c_stats<- rbind(c_stats, total_row)

####Pitchers#####
pitchers<-c('Josh Hartle',
            'Trey Yesavage',
            'Ryan Johnson',
            'Brandon Neely')
pitchers <- str_extract_all(pitchers, "\\b\\w+\\b") %>%
     sapply(function(x) paste(rev(x), collapse = ", ")) %>%
     unname()
c_id=c(749,196,1045,235)##list of team ids for hitters above 


results <- list()

## For loop that is connecting the id to the player and then pulling their stats out and storing in a list
for (i in seq_along(c_id)) {
     id <- c_id[i]
     pitcher <- as.character(pitchers[i])
     
     player_stats <- ncaa_team_player_stats(id, 2024, 'pitching') %>%
          dplyr::filter(player_name == pitcher) %>%
          dplyr::select(player_name, IP, SO, W, SV, BB, HB, `HR-A`)
     
     results[[i]] <- player_stats
}
c_pitch<- bind_rows(results)
c_pitch<- na.fill(c_pitch, 0)#i guess this messes with those columns 
c_pitch<- as.data.frame(c_pitch)
# Convert columns to numeric
c_pitch[, c(2:8)] <- lapply(c_pitch[, c(2:8)], base::as.numeric)
c_pitch[, c('IP','SO','W','SV','BB','HB','HR-A')] <- 
     c_pitch[, c('IP','SO','W','SV','BB','HB','HR-A')] * rep(pitch_stats, each = nrow(c_pitch))

c_pitch$Total<-rowSums(c_pitch[, c('IP','SO','W','SV','BB','HB','HR-A')])
sum<- sum(c_pitch$Total)
total_row <- c("Total", NA, NA, NA,NA,NA,NA,NA, sum)
c_pitch<- rbind(c_pitch, total_row)
##############Sweeney############
players=c('Hagen Smith',
          #'Cam Leiter',
          'Jamie Arnold',
          'Jackson Wells',
          'Charlie Beilenson',
          'Caden Bodine',
          'Jac Caglianone',
          'Miguel Santos',
          'Billy Amick',
          'Jake Rainess',
          'Cam Cannarella',
          'James Tibbs III',
          'Ethan Petry',
          'Cade Kurland',
          'Justin Cassella',
          'Joichiro Oyama',
          'Ben Parker',
          'Brady Tygart')
#'Tyson Neighbors')

###Changing order of names#####
players <- str_extract_all(players, "\\b\\w+\\b") %>%
     sapply(function(x) paste(rev(x), collapse = ", ")) %>%
     unname()
players[11]<-'Tibbs III, James'
players<- as.data.frame(players)

players<-players[-c(1:4,14:18),]
players


##hitters##
#[1] "Bodine, Caden"    "Caglianone, Jac"  "Santos, Miguel"   "Amick, Billy"     "Rainess, Jake"    "Cannerella, Cam"  "Tibbs, James"    
#[8] "Petry, Ethan"     "Kurland, Cade"    "Cassella, Justin" "Oyama, Joichiro"  "Parker, Ben" 


d_id=c(149,235,1045,694,288,147,234,648,235)##list of team ids for hitters above 


results <- list()
##for loop that is connecting the id to the player and then pulling their stats out and storing in a list
for (i in seq_along(d_id)) {
     id <- d_id[i]
     player <- as.character(players[i])
     
     player_stats <- ncaa_team_player_stats(id, 2024, 'batting') %>%
          dplyr::filter(player_name == player) %>%
          dplyr::select(player_name,H,`2B`,`3B`, HR, BB,HBP,RBI, SB,CS)
     
     results[[i]] <- player_stats
}
#binding rows to make dataframe 
d_stats <- bind_rows(results)
d_stats<- na.fill(d_stats, 0)#i guess this messes with those columns 
d_stats<- as.data.frame(d_stats)
# Convert columns to numeric
d_stats[, c(2:10)] <- lapply(d_stats[, c(2:10)], base::as.numeric)

# Calculate 1B column
d_stats$`1B` <- d_stats$H - d_stats$`2B` - d_stats$`3B` - d_stats$HR
order<- c('player_name','1B','2B','3B','HR','BB','HBP','RBI','SB','CS')
d_stats<-d_stats[,order]
# Select columns from '1B' to 'CS' and multiply by off_stats
# Multiply each column from '1B' to 'CS' by the corresponding element in off_stats
d_stats[, c("1B", "2B", "3B", "HR", "BB", "HBP", "RBI", "SB", "CS")] <- 
     d_stats[, c("1B", "2B", "3B", "HR", "BB", "HBP", "RBI", "SB", "CS")] * rep(off_stats, each = nrow(d_stats))
d_stats$Total<-rowSums(d_stats[, c("1B",'2B','3B', "HR", "BB", "HBP", "RBI", "SB", "CS")])
sum<- sum(d_stats$Total)
total_row <- c("Total", NA, NA, NA,NA,NA,NA,NA,NA,NA, sum)
d_stats<- rbind(d_stats, total_row)

####Pitchers#####
pitchers<-c('Hagen Smith',
            #'Cam Leiter',
            'Jamie Arnold',
            'Jackson Wells',
            'Charlie Beilenson')
pitchers <- str_extract_all(pitchers, "\\b\\w+\\b") %>%
     sapply(function(x) paste(rev(x), collapse = ", ")) %>%
     unname()
d_id=c(31,234,32,193)##list of team ids for hitters above 


results <- list()

## For loop that is connecting the id to the player and then pulling their stats out and storing in a list
for (i in seq_along(d_id)) {
     id <- d_id[i]
     pitcher <- as.character(pitchers[i])
     
     player_stats <- ncaa_team_player_stats(id, 2024, 'pitching') %>%
          dplyr::filter(player_name == pitcher) %>%
          dplyr::select(player_name, IP, SO, W, SV, BB, HB, `HR-A`)
     
     results[[i]] <- player_stats
}
d_pitch<- bind_rows(results)
d_pitch<- na.fill(d_pitch, 0)#i guess this messes with those columns 
d_pitch<- as.data.frame(d_pitch)
# Convert columns to numeric
d_pitch[, c(2:8)] <- lapply(d_pitch[, c(2:8)], base::as.numeric)
d_pitch[, c('IP','SO','W','SV','BB','HB','HR-A')] <- 
     d_pitch[, c('IP','SO','W','SV','BB','HB','HR-A')] * rep(pitch_stats, each = nrow(d_pitch))

d_pitch$Total<-rowSums(d_pitch[, c('IP','SO','W','SV','BB','HB','HR-A')])
sum<- sum(d_pitch$Total)
total_row <- c("Total", NA, NA, NA,NA,NA,NA,NA, sum)
d_pitch<- rbind(d_pitch, total_row)

############Total DataFrame###########
# Create a new data frame named Total
Total <- data.frame(Garrett=NA,
                    Mason=NA,
                    Micah=NA,
                    Colby=NA,
                    Luke=NA,
                    Daniel=NA)
Total$Garrett <- base::as.numeric(g_stats$Total[10]) + base::as.numeric(g_pitch$Total[5])
Total$Mason <- base::as.numeric(ma_stats$Total[10]) + base::as.numeric(ma_pitch$Total[5])
Total$Micah <- base::as.numeric(mi_stats$Total[10]) + base::as.numeric(mi_pitch$Total[5])
Total$Colby <- base::as.numeric(c_stats$Total[10]) + base::as.numeric(c_pitch$Total[5])
Total$Luke <- base::as.numeric(l_stats$Total[10]) + base::as.numeric(l_pitch$Total[5])
Total$Daniel <- base::as.numeric(d_stats$Total[10]) + base::as.numeric(d_pitch$Total[5])






# Define UI for the application
ui <- fluidPage(
     titlePanel("College Fantasy Points"),
     sidebarLayout(
          sidebarPanel(
               selectInput("df_dropdown", label = "Select a DataFrame:",
                           choices = c('Total',"g_stats",'g_pitch', "ma_stats",'ma_pitch', "mi_stats",'mi_pitch', "d_stats",'d_pitch', "c_stats",'c_pitch', "l_stats",'l_pitch'),
                           selected = "Total")
          ),
          mainPanel(
               tableOutput("selected_df")
          )
     )
)

# Define server logic
server <- function(input, output) {
     # Function to return the selected DataFrame
     selected_df <- reactive({
          print(input$df_dropdown)
          switch(input$df_dropdown,
                 'Total'= Total,
                 "g_stats" = g_stats,
                 'g_pitch'= g_pitch,
                 "ma_stats" = ma_stats,
                 'ma_pitch'= ma_pitch,
                 "mi_stats" = mi_stats,
                 'mi_pitch'= mi_pitch,
                 "d_stats" = d_stats,
                 'd_pitch'= d_pitch,
                 "c_stats" = c_stats,
                 'c_pitch'= c_pitch,
                 "l_stats" = l_stats,
                 'l_pitch'= l_pitch)
     })
     # Output to display the selected DataFrame
     output$selected_df <- renderTable({
          selected_df()
     })
}

# Run the application
shinyApp(ui = ui, server = server)
