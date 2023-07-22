data <- read.csv("optimise/data.csv")
model <- model_fpl(data, budget = 100, teamsize = 15, gk = 2)
