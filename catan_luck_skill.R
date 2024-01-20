library(tidyverse)
# Set the working directory
directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(directory)

players <- c("Player 1", "Player 2", "Player 3", "Player 4")

skills <- c(0.1, 0.2, 0.3, 0.4) 

############################ SIM A

n_games <- 10
n_reps <- 10000
game <- 1:n_games
winner <- vector(length = n_games)
rep <- vector(length = n_reps)
results_A <- matrix(data = NA, nrow = 0, ncol = 3)

set.seed(2023)
for(i in 1:n_reps){
  rep <- rep(i, times = n_games)
  
  for(j in 1:n_games){
    game[j] <- j
    winner[j] <- sample(players, size = 1, prob = skills)
  }
  
  result <- cbind(rep, game, winner)
  results_A <- rbind(results_A, result)
  
}

results_A <- as_tibble(results_A)

############################ SIM B

n_games <- 50
n_reps <- 10000
game <- 1:n_games
winner <- vector(length = n_games)
rep <- vector(length = n_reps)
results_B <- matrix(data = NA, nrow = 0, ncol = 3)

set.seed(2023)
for(i in 1:n_reps){
  rep <- rep(i, times = n_games)
  
  for(j in 1:n_games){
    game[j] <- j
    winner[j] <- sample(players, size = 1, prob = skills)
  }
  
  result <- cbind(rep, game, winner)
  results_B <- rbind(results_B, result)
  
}

results_B <- as_tibble(results_B)

# write_csv(results_A, "results_A.csv")
# write_csv(results_B, "results_B.csv")
results_A <- read_csv(file = "results_A.csv")

############################ Analysis of Sims
results_A %>%
  group_by(rep) %>%
  count(winner) %>%
  pivot_wider(names_from = winner, values_from = n, values_fill = 0) %>%
  transmute(
    rep = as.integer(rep),
    player_1 = `Player 1`, player_2 = `Player 2`, player_3 = `Player 3`, player_4 = `Player 4`, 
  ) %>%
  arrange(rep) %>%
  pivot_longer(cols = !rep, names_to = "player", values_to = "n_wins")

results_A %>%
  group_by(rep) %>%
  count(winner) %>%
  pivot_wider(names_from = winner, values_from = n, values_fill = 0) %>%
  transmute(
    rep = as.integer(rep),
    player_1 = `Player 1`, player_2 = `Player 2`, player_3 = `Player 3`, player_4 = `Player 4`, 
  ) %>%
  arrange(rep) %>%
  mutate(
    prob = (factorial(10) / (factorial(player_1)*factorial(player_2)*factorial(player_3)*factorial(player_4))) * 
      (skills[1]^player_1)*(skills[2]^player_2)*(skills[3]^player_3)*(skills[4]^player_4)
  )

### Cumulative simulation plot

sim_A <- results_A %>%
  group_by(rep) %>%
  mutate(
    rep = as.integer(rep),
    game = as.integer(game),
    p1 = ifelse(winner == "Player 1", 1, 0),
    p2 = ifelse(winner == "Player 2", 1, 0),
    p3 = ifelse(winner == "Player 3", 1, 0),
    p4 = ifelse(winner == "Player 4", 1, 0),
    p1_cw = cumsum(p1),
    p2_cw = cumsum(p2),
    p3_cw = cumsum(p3),
    p4_cw = cumsum(p4)
  ) %>%
  pivot_longer(cols = contains("cw"), names_to = "player", values_to = "cumulative_wins") %>%
  transmute(
    rep = as.integer(rep),
    game = as.integer(game),
    player = factor(x = player, levels = c("p1_cw", "p2_cw","p3_cw","p4_cw"), 
                    labels = c("Player 1", "Player 2", "Player 3", "Player 4")),
    cumulative_wins
  )

group_id <- sim_A %>%
  group_by(rep, player) %>%
  group_indices()

sim_A_plot <- sim_A %>%
  mutate(group_id = group_id) %>%
  arrange(group_id, rep, player, game) %>%
  select(group_id, rep, game, player,  cumulative_wins)


  filter(rep <= 1) %>%
  ggplot(aes(x = game, y = cumulative_wins, color = player, fill = player, group = rep)) +
  geom_line(size = 2, alpha = 0.1) +
  theme_classic() +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(breaks = 1:10)


test <- results_A %>%
  group_by(rep) %>%
  mutate(
    rep = as.integer(rep),
    game = as.integer(game),
    p1 = ifelse(winner == "Player 1", 1, 0),
    p2 = ifelse(winner == "Player 2", 1, 0),
    p3 = ifelse(winner == "Player 3", 1, 0),
    p4 = ifelse(winner == "Player 4", 1, 0),
    p1_cw = cumsum(p1),
    p2_cw = cumsum(p2),
    p3_cw = cumsum(p3),
    p4_cw = cumsum(p4)
  ) %>%
  pivot_longer(cols = contains("cw"), names_to = "player", values_to = "cumulative_wins") %>%
  filter(rep <= 1)
  

