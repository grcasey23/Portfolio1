rm(list=ls())
library(tidyverse)
library(dplyr)
df<- read_csv('William and Mary 2023-24 Pitch Stat File - 2024 Printable Stats.csv')
fip_constant=3.2
df<-df[,-31]
#df['fip']=((df['HR'] * 13)+ (3 *(df['BB']+df['HBP']))-(2*df['K']))/df['IP']+fip_constant 
#df['K/9']=(df['K']/df['IP'])*9
#df['H/9']=(df['H']/df['IP'])*9

########Moving ab column#####

# Extract the column to be moved
AB <- df$AB

# Remove the column from the data frame
df <- df[, !(names(df) == "AB"), drop = FALSE]

# Insert the column at the desired position
df <- cbind(df[, 1:(6 - 1), drop = FALSE], AB, df[, 6:(ncol(df)), drop = FALSE])

glimpse(df)

#########################

x=df[,c('Name','FIP')]
x=x[order(x$FIP),]
library(gt)

#############################################
#install.packages('stringr')
library(stringr)

######taking percent off and making numeric###########
df$`Strike%` <- str_sub(df$`Strike%`, end = -2)
df$`Wiff%` <- str_sub(df$`Wiff%`, end = -2)
df$`K%` <- str_sub(df$`K%`, end = -2)
df$`BB%` <- str_sub(df$`BB%`, end = -2)
df$`OA3%` <- str_sub(df$`OA3%`, end = -2)
df$`1stP K%` <- str_sub(df$`1stP K%`, end = -2)
##########turning the columns i took percent off of into numeric######
df[, c(10,11,22,23,24,25,26)] <- lapply(df[, c(10,11,22,23,24,25,26)], as.numeric)
df[, c(1, 2, 3, 4,5,6)] <- lapply(df[, c(1, 2, 3, 4,5,6)], as.character)
numeric_columns <- df %>%
     select_if(is.numeric) %>%
     colnames()
###########need to somehow change strike, wiff, k, k%, 1st p k%, OA3&, velo to go higher=better########
high_better_columns <- c( 'BB', 'K', 'OA3%', 'FB velo','1stP K%','K%','Wiff%','Strike%','K/9')


low_better_columns <- c('FIP','HR','BB','ERA','WHIP','wOBA','H','2B','3B','Avg','Slg','HBP','BB%','H/9','Obp','BB&HBP/9')
##rows that need 3 after decimal
three_dec<-c('wOBA','Avg','Slg','Obp')
#rows that are 2 after decimal
two_dec<- c('BB', 'K', 'OA3%', 'FB velo','1stP K%','K%','Wiff%','Strike%','K/9','FIP','HR','BB','ERA','WHIP','H','2B','3B','HBP','BB%','H/9','BB&HBP/9')
####for bolding ov averages#####
num_rows=nrow(df)
#row_to_bold=21
# Applying color scales separately
library(scales)

# Define the number of colors for the gradient
num_colors <- 100

# Create a color palette from red to green
low_is_bad <- colorRampPalette(c("red",'white', "green"))(num_colors)

# Use the color palette in col_numeric
colors <- col_numeric(palette = low_is_bad, domain = NULL)

low_is_good<-colorRampPalette(c("green",'white', "red"))(num_colors)
colors1 <- col_numeric(palette = low_is_good, domain = NULL)
# Now you can use 'colors' in the data_color() function to create the heatmap

###ordering by anything ###########
#excluded_from_order<- df[21,]
#df<-df[-21,]
df<- df[order(df$WHIP,decreasing = FALSE),]
#df<- rbind(df, excluded_from_order)
library(gt)
stats <- gt(df) %>%
     tab_header(title = md('**2024 Tribe Pitchers Stat Sheet(2/16/2024 - 3/16/2024 (Ordered by WHIP))**')) %>%
     data_color(
          columns = high_better_columns,
          colors = scales::col_numeric(palette = colors,domain=NULL),
          rows = 1:(num_rows-1)
     ) %>%
     data_color(
          columns = low_better_columns,
          colors = scales::col_numeric(palette = colors1,domain=NULL),
          rows = 1:(num_rows)) %>%fmt_number(columns=three_dec, decimals=3)%>% fmt_number(columns =two_dec, decimals=2)

stats
#locations = cells_body(rows = row_to_bold, columns = everything()))
#########fip table ##########
fiptbl<-gt(x)%>% tab_header(title=md('**2024 Tribe Pitchers FIP Leaderboard**'))%>% data_color(columns='FIP',fn=scales::col_numeric( palette=c('green','white','red'),  domain=NULL))

fiptbl



