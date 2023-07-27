load("./randomteams.RData")
NROW(teams)
# Every single possible match-up.
combos <- t(combn(names(teams), 2))
# Mimic necessary data structure

make.df <- function(metric=c("ppg", "total.points", "mean.points", "total.ppm",
                              "mean.ppm")){
  metric <- match.arg(metric)
  dfs <- do.call(rbind, apply(combos, 1, function(x){
    home.score <- teams[[x[1]]][[metric]]
    away.score <- teams[[x[2]]][[metric]]
    home <- x[1]; away <- x[2]
    data.frame(home = home, away = away,
               neutral.site = 1, home.score = home.score,
               away.score = away.score)
  }))
  dfs$home.response <- dfs$home.score
  dfs$away.response <- dfs$away.score
  dfs
}

game.data <- make.df("total.points")
sapply(c("ppg", "total.points", "mean.points", "total.ppm", "mean.ppm"),
       function(m){
         df <- make.df(m)
         df[which.max(df$home.score), "home"]
       })

result <- mvglmmRank(game.data, max.iter.EM = 10, 
                     home.field = FALSE)
