# Taken directly from https://github.com/wiscostret/optimize_fpl/blob/master/optimize_fpl.md

library(ompr)

# this defines how the constraint function (below) will identify the player costs in the data
cost <- function(i) data[["cost"]][i] 

# this adds the constraint to our optimizer model that the total cost of our selected players (expressed in the variable 'x'), out of the total number of players ('n'), cannot exceed the budget (a variable we specify in the model function further below)
add_budget_contraint <- function(model, n, cost, budget) {
  add_constraint(model, sum_expr(colwise(cost(i)) * x[i], i = 1:n) <= budget)
} 

# this adds the constraint to our optimizer model that the total number of selected players (x) cannot exceed the teamsize (a variable we specify in the model function further below, but that will default to '11')
add_teamsize_constraint <- function(model, n, teamsize) {
  add_constraint(model, sum_expr(x[i], i = 1:n) == teamsize)
} 

# this adds the constraint to our optimizer model that the unique identifier (player_id) can only feature once in the modelled solution
add_unique_id_constraint <- function(model, data) {
  n <- nrow(data)
  ids <- unique(data[["player_id"]])
  
  has_id <- function(i, id) as.integer(data[["player_id"]][i] == id)
  
  # only need to apply this constraint to players that appear more than once
  multi_players <- names(which(table(data[["player_id"]]) > 1))
  
  for (id in multi_players) {
    model <- add_constraint(model, sum_expr(colwise(has_id(i, id)) * x[i], i = 1:n) <= 1)
  }
  
  model
}

# this sets the basic objective of the optimizer model - that we have to maximize the expected points (xpts)
set_generic_objective <- function(model, n, xpts) {
  set_objective(model, sum_expr(colwise(xpts(i)) * x[i], i = 1:n))
}

# this functions defines how the constraint function (below) will identify the player teams in the data
is_team <- function(j, i) as.integer(data$team[i] == j)

# this adds the constraint to our optimizer model that the total number of selected players from a given team cannot exceed the max_from_team variable (a variable we specify in the model function further below, but that will default to '3')
add_max_from_team_constraint <- function(model, n, max_from_team, is_team) {
  for (j in 1:length(unique(data$team))) {
    model <- add_constraint(model, sum_expr(colwise(is_team(j, i))*x[i], i = 1:n) <= max_from_team)
  }
  model
}

# these three first functions specify constraints for our optimizer model that the number of goalkeepers in the solution have to be a) the exact number we specify, b) at least 1, c) no more than 2. In the model function further below, we'll specify that unless we manually supply a desired number in our solution (a), the model will apply constraints (b) and (c). The fourth function defines how the constraint functions will identify the goalkeepers in the data.

add_gk_constraint <- function(model, n, gk, is_gk){
  model <- add_constraint(model, sum_expr(colwise(is_gk(i)) * x[i], i = 1:n) == gk)
  model
}

add_min_gk_constraint <- function(model, n, is_gk){
  model <- add_constraint(model, sum_expr(colwise(is_gk(i)) * x[i], i = 1:n) >= 1)
  model
}

add_max_gk_constraint <- function(model, n, is_gk){
  model <- add_constraint(model, sum_expr(colwise(is_gk(i)) * x[i], i = 1:n) <= 2)
  model
}

is_gk <- function(i) as.integer(data$pos[i] == "G")

# these three first functions specify constraints for our optimizer model that the number of defenders in the solution have to be a) the exact number we specify, b) at least 3, c) no more than 5. In the model function further below, we'll specify that unless we manually supply a desired number in our solution (a), the model will apply constraints (b) and (c). The fourth function defines how the constraint functions will identify the defenders in the data.

add_def_constraint <- function(model, n, def, is_def){
  model <- add_constraint(model, sum_expr(colwise(is_def(i)) * x[i], i = 1:n) == def)
  model
}

add_min_def_constraint <- function(model, n, is_def){
  model <- add_constraint(model, sum_expr(colwise(is_def(i)) * x[i], i = 1:n) >= 3)
  model
}

add_max_def_constraint <- function(model, n, is_def){
  model <- add_constraint(model, sum_expr(colwise(is_def(i)) * x[i], i = 1:n) <= 5)
  model
}

is_def <- function(i) as.integer(data$pos[i] == "D")

# these three first functions specify constraints for our optimizer model that the number of midfielders in the solution have to be a) the exact number we specify, b) at least 2, c) no more than 5. In the model function further below, we'll specify that unless we manually supply a desired number in our solution (a), the model will apply constraints (b) and (c). The fourth function defines how the constraint functions will identify the midfielders in the data.

add_mid_constraint <- function(model, n, mid, is_mid){
  model <- add_constraint(model, sum_expr(colwise(is_mid(i)) * x[i], i = 1:n) == mid)
  model
}

add_min_mid_constraint <- function(model, n, is_mid){
  model <- add_constraint(model, sum_expr(colwise(is_mid(i)) * x[i], i = 1:n) >= 2)
  model
}

add_max_mid_constraint <- function(model, n, is_mid){
  model <- add_constraint(model, sum_expr(colwise(is_mid(i)) * x[i], i = 1:n) <= 5)
  model
}

is_mid <- function(i) as.integer(data$pos[i] == "M")

# these three first functions specify constraints for our optimizer model that the number of forwards in the solution have to be a) the exact number we specify, b) at least 1, c) no more than 3. In the model function further below, we'll specify that unless we manually supply a desired number in our solution (a), the model will apply constraints (b) and (c). The fourth function defines how the constraint functions will identify the forwards in the data.

add_fwd_constraint <- function(model, n, fwd, is_fwd){
  model <- add_constraint(model, sum_expr(colwise(is_fwd(i)) * x[i], i = 1:n) == fwd)
  model
}

add_min_fwd_constraint <- function(model, n, is_fwd){
  model <- add_constraint(model, sum_expr(colwise(is_fwd(i)) * x[i], i = 1:n) >= 1)
  model
}

add_max_fwd_constraint <- function(model, n, is_fwd){
  model <- add_constraint(model, sum_expr(colwise(is_fwd(i)) * x[i], i = 1:n) <= 3)
  model
}

is_fwd <- function(i) as.integer(data$pos[i] == "F")

# the first function specifies the constraint that a 'must'-have player, to be specified in the model function below, HAS to be part of the solution. The second function defines how the constraint functions will identify the 'must' player in the data.

add_must_constraint <- function(model, n, must, is_must){
  model <- add_constraint(model, sum_expr(colwise(is_must(i, must)) * x[i], i = 1:n) == length(must))
  model
}

is_must <- function(i, must) as.integer(data$name[i] %in% must)

# the first function specifies the constraint that a 'nope' player, to be specified in the model function below, WILL NOT be part of the solution. The second function defines how the constraint functions will identify the 'nope' player in the data.

add_nope_constraint <- function(model, n, nope, is_nope){
  model <- add_constraint(model, sum_expr(colwise(is_nope(i, nope)) * x[i], i = 1:n) == 0)
  model
}

is_nope <- function(i, nope) as.integer(data$name[i] %in% nope)


# Model functions ---------------------------------------------------------

# first we define the function, specifying the variables to be supplied (per above helper functions) and their default values:

model_fpl <- function(data, budget, teamsize = 11, max_from_team = 3, gk = NULL, def = NULL, mid = NULL, fwd = NULL, must = NULL, nope = NULL, type = "binary"){
  
  # first we define the base model, applying the constraints that HAVE to be a part of the solution:
  
  my_model <- MILPModel() %>%
    add_variable(x[i], i = 1:n, type = "binary") %>% 
    add_budget_contraint(n, cost, budget) %>% 
    add_teamsize_constraint(n,teamsize) %>%
    add_max_from_team_constraint(n, max_from_team, is_team) %>% 
    add_max_gk_constraint(n, is_gk) %>% 
    add_max_def_constraint(n, is_def) %>% 
    add_max_mid_constraint(n, is_mid) %>% 
    add_max_fwd_constraint(n, is_fwd)
  
  # next we apply the position-specific constraints that depend on whether we've supplied the function with targeted numbers of GK/DEF/MID/FWD players and MUSTs/NOPEs for our solution.
  
  ifelse(is.null(gk), 
         my_model <- my_model %>% add_min_gk_constraint(n, is_gk), 
         my_model <- my_model %>% add_gk_constraint(n, gk, is_gk))
  
  ifelse(is.null(def), 
         my_model <- my_model %>% add_min_def_constraint(n, is_def), 
         my_model <- my_model %>% add_def_constraint(n, def, is_def))
  
  ifelse(is.null(mid), 
         my_model <- my_model %>% add_min_mid_constraint(n, is_mid), 
         my_model <- my_model %>% add_mid_constraint(n, mid, is_mid))
  
  ifelse(is.null(fwd), 
         my_model <- my_model %>% add_min_fwd_constraint(n, is_fwd), 
         my_model <- my_model %>% add_fwd_constraint(n, fwd, is_fwd))
  
  ifelse(is.null(must),"",
         my_model <- my_model %>% add_must_constraint(n, must, is_must))
  
  ifelse(is.null(nope),"",
         my_model <- my_model %>% add_nope_constraint(n, nope, is_nope))
  
  # finally we add the 'unique id' constraint
  
  my_model %>% add_unique_id_constraint(data)
}


# Optimisation  -----------------------------------------------------------

library(ompr.roi)
library(ROI.plugin.glpk)


# first we define the function and its basic properties, incl. the data, model and a selection of 'solvers'
optimize_fpl <- function(data, model, solver = c("glpk","symphony","cbc")){
  
  # defining helpers for the solver to identify in the data 
  
  n <- nrow(data)
  xpts <- function(i) data[["xpts"]][i]
  
  # using our 'generic objective' function above to set the optimizer objective 
  
  model <- set_generic_objective(model, n, xpts)
  
  # drawing out the selected 'solver' 
  
  solver <- match.arg(solver)
  
  # developing the model solution using our solver
  
  result <- solve_model(model, with_ROI(solver=solver))
  
  # grabbing the result solution for our variable 'x' (the players)
  
  solution <- get_solution(result, x[i])
  
  # filtering out or final line-up (those player with 'x' marked in the solution)
  
  matches <- (solution %>% filter(value==1))$i
  
  # structuring the line-up output nicely
  
  lineup <- data
  lineup$x <- solution$value
  return(lineup %>% filter(x==1) %>% select(player_id,pos,name,cost,xpts,team) %>% arrange(factor(pos,levels=c("G","D","M","F")),-desc(name),-desc(xpts)))
}
