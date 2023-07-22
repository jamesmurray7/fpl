# Looking at player info --------------------------------------------------
pl <- get_player_info() # start of this season
tm <- get_fdr() %>% select(id, name)
# Wittle down quite heavily
pl2 <- pl %>%
  filter(minutes > 1000, news == "") %>%  # Players playing ~half time and no news relating to them
  select(
  playername, first_name, second_name, team, team_code,
  pos = element_type, cost = now_cost, ppg = points_per_game,
  points = total_points, mins = minutes
  ) %>% 
  mutate(ppm = points/mins,
         ppmpc = ppm/cost,
         postxt = case_when(
           pos == 1 ~ "GK",
           pos == 2 ~ "DEF",
           pos == 3 ~ "MID",
           pos == 4 ~ "FWD",
           T ~ "999"
         )) %>% 
  left_join(., tm %>% rename(teamname = name), by = c("team" = "id"))
  

write.csv(pl2, file = "optimise/data.csv",
          row.names = FALSE)
