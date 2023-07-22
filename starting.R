remotes::install_github("wiscostret/fplscrapR")

library(fplscrapR)
library(dplyr)

# 22 not on yet!
df <- get_player_info(season = 21) %>%
  filter(total_points >= 30) %>% # filtering for players with more than 30 points scored
  mutate(pp90 = total_points / minutes * 90) %>% # creating the 'pp90' variable
  select(playername,pp90) %>% # selecting player name and pp90 for our table
  arrange(-pp90) %>% # ordering (arranging) by pp90
  slice(1:20) # showing just the top20


# Heatmap of difficulty - might inform player selection -------------------
gamelist <- get_game_list()
fdr <- get_fdr()

fdrfixtures <- rbind(
  gamelist %>% mutate(team=home,oppo=away,homeaway="home"),
  gamelist %>% mutate(team=away,oppo=tolower(home),homeaway="away"))

for (i in 1:nrow(fdrfixtures)){
  ifelse(fdrfixtures$homeaway[i]=="home",
         fdrfixtures$fdr[i] <- fdr$strength_overall_away[which(fdr$short_name==toupper(fdrfixtures$oppo[i]))],
         fdrfixtures$fdr[i] <- fdr$strength_overall_home[which(fdr$short_name==toupper(fdrfixtures$oppo[i]))])
}

# Make the heatmap
library(ggplot2)
fdrfixtures %>%
  filter(GW %in% 1:5) %>%  # filtering for the gameweeks we are interested in
  ggplot() +
  geom_tile(aes(x=GW,y=team,fill=fdr),colour="lightgrey") +
  geom_text(aes(x=GW,y=team,label=oppo),size=2) +
  theme_void() +
  theme(axis.text = element_text(face = "bold")) +
  theme(axis.text.y = element_text(margin=margin(0,-20,0,0))) + # fixing the margins
  scale_x_continuous(position="top",breaks=1:15) +
  labs(caption=paste("Data from fplscrapR | ",Sys.Date(),sep="")) +
  scale_fill_gradient2(guide=F,low="#00FF87",mid="#D6DCD8",high="#7F002D",midpoint=median(fdrfixtures$fdr)) # creating a gradient colour-coding that spans from toughest FDR ranking (coloured red) to easiest (coloured green)


# Looking at player info --------------------------------------------------
pl <- get_player_info() # start of this season
pl22 <- read.csv("~/Downloads/cleaned_players.csv") # End of last season (from https://github.com/vaastav/Fantasy-Premier-League/blob/master/data/2022-23/cleaned_players.csv)

pl22 %>% glimpse()
pl22 %>% arrange(-selected_by_percent) %>% slice(1:10)
