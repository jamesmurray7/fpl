# Looking at player info --------------------------------------------------
pl <- get_player_info() # start of this season
tm <- get_fdr() %>% select(id, name)
# Wittle down quite heavily
pl2 <- pl %>%
  dplyr::filter(minutes > 800, news == "") %>%  # Players playing ~half time and no news relating to them
  select(
  playername, first_name, second_name, team, team_code,
  pos = element_type, cost = now_cost, ppg = points_per_game,
  points = total_points, mins = minutes
  ) %>% 
  mutate(
    cost = cost / 10,
    ppg = as.numeric(ppg),
    ppm = points/mins,
    ppmpc = ppm/cost,
    postxt = case_when(
      pos == 1 ~ "GK",
      pos == 2 ~ "DEF",
      pos == 3 ~ "MID",
      pos == 4 ~ "FWD",
      T ~ "999"
    )) %>% 
  left_join(., tm %>% rename(teamname = name), by = c("team" = "id"))
  
# save("pl2", file = "./plredx.RData")


# Make 100 random teams ---------------------------------------------------
num.teams <- 100; budget <- 100
num.gks <- 2; num.def <- 5; num.mid <- 5; num.fwd <- 3; (num.gks + num.def + num.mid + num.fwd)

GKs <- pl2 %>% dplyr::filter(postxt == "GK")
nGK <- nrow(GKs)
DEF <- pl2 %>% dplyr::filter(postxt == "DEF")
nDEF <- nrow(DEF)
MID <- pl2 %>% dplyr::filter(postxt == "MID")
nMID <- nrow(MID)
FWD <- pl2 %>% dplyr::filter(postxt == "FWD")
nFWD <- nrow(FWD)

randomteam <- function(){
  valid.team <- FALSE
  iter <- 0
  while(!valid.team){
    gk.rows <- sample(1:nGK,  num.gks, replace = FALSE)
    df.rows <- sample(1:nDEF, num.def, replace = FALSE)
    md.rows <- sample(1:nMID, num.mid, replace = FALSE)
    fd.rows <- sample(1:nFWD, num.fwd, replace = FALSE)
    this.team <- rbind(
      GKs[gk.rows, ], DEF[df.rows, ], MID[md.rows, ], FWD[fd.rows, ]
    )
    cost <- sum(this.team$cost)
    max.of.one.team <- max(table(this.team$teamname)) <= 3
    valid.team <- (cost <= budget) & max.of.one.team
    iter <- iter + 1
  }
  
  this.team %>% arrange(
    pos, playername
  )
  
  return(list(
    team = this.team, tries = iter,
    cost = cost,
    ppg = sum(this.team$ppg),
    total.points = sum(this.team$points),
    mean.points = mean(this.team$points), # ie points per player.
    total.ppm = sum(this.team$ppm),
    mean.ppm = mean(this.team$ppm)
  ))
}

# Populate list of teams --------------------------------------------------
teams <- setNames(
  replicate(num.teams, randomteam(), simplify = FALSE),
  paste0("team ", 1:num.teams)
)

save(teams, file = "./randomteams.RData")









