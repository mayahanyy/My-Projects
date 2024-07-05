N <- 1000  # number of samples (simulation runs)
n <- 1000  # size of the sample
all_samples_X <- c()  # The Percentage of X won compared to n
all_samples_O <- c()  # The Percentage of O won compared to n
all_samples_tie <- c()  # The Percentage of tie that happened compared to n

for (k in 1:N) {
  sample <- c()  # The actual sample used
  sample_games <- list()
  
  for (i in 1:n) {
    board <- matrix(' ', 3, 3)  # creating a board
    
    winning_combinations <- list(
      # Rows
      c(1, 2, 3),
      c(4, 5, 6),
      c(7, 8, 9),
      
      # Columns
      c(1, 4, 7),
      c(2, 5, 8),
      c(3, 6, 9),
      
      # Diagonals
      c(1, 5, 9),
      c(3, 5, 7)
    )
    
    game_over <- FALSE
    
    # When this becomes true the game ends
    value <- "tie"
    
    for (j in 1:9) {
      space <- which(board == ' ')  # looking for an empty space in the board
      
      if (length(space) == 0) {
        value <- "tie"
        game_over <- TRUE  # game ends
        break
      }
      
      if (j %% 2 == 1) {
        mark <- 'X'
        opponent_mark <- 'O'
      } else {
        mark <- 'O'
        opponent_mark <- 'X'
      }
      
      # Check for winning moves for the current player and the opponent
      winning_move <- NULL
      opponent_winning_move <- NULL
      
      for (combination in winning_combinations) {
        symbols <- board[combination]
        
        if (sum(symbols == mark) == 2 && sum(symbols == ' ') == 1) {
          winning_move <- combination[which(symbols == ' ')]
        }
        
        if (sum(symbols == opponent_mark) == 2 && sum(symbols == ' ') == 1) {
          opponent_winning_move <- combination[which(symbols == ' ')]
        }
      }
      
      if (!is.null(winning_move)) {
        board[winning_move] <- mark  # Place the mark in the winning move position
      } else if (!is.null(opponent_winning_move)) {
        board[opponent_winning_move] <- mark  # Block the opponent's winning move
      } else {
        random_position_index <- sample(length(space), 1)
        random_position <- space[random_position_index]
        board[random_position] <- mark
      }
      
      # Check for a winning condition
      if (j >= 5) {
        for (combination in winning_combinations) {
          if (all(board[combination] == mark)) {
            value <- mark
            game_over <- TRUE  # game ends
            break
          }
        }
      }
      
      if (game_over) {
        break
      }
    }
    
    sample[i] <- c(value)
    sample_games[i] <- list(board)
  }
  
  all_samples_X <- c(all_samples_X, sum(sample == 'X') /n )
  all_samples_O <- c(all_samples_O, sum(sample == 'O') /n )
  all_samples_tie <- c(all_samples_tie, sum(sample == 'tie') /n )
}

means <- c(mean(all_samples_X), mean(all_samples_O), mean(all_samples_tie))
barplot(means, names.arg = c("X", "O", "Tie"), main = "Mean Scores (X: Smart, O: Smart)",
        ylab = "Percentage won", ylim = c(0, max(means) * 1.2))
text(x = 1:length(means), y = means, labels = means, pos = 3, col = "black")
quartz();

if (!require("grDevices")) install.packages("grDevices")
library(grDevices)
quartz();
#Bar plot comparing the average Percentage of X, O and tie appearing in a sample.

#histograms 
hist(all_samples_X*n, xlab = "all_samples_X", main = "Histogram of all_samples_X")
quartz();

hist(all_samples_O*n, xlab = "all_samples_O", main = "Histogram of all_samples_O")
quartz();

hist(all_samples_tie*n, xlab = "all_samples_tie", main = "Histogram of all_samples_tie")
quartz();
#computing the mean
mean(all_samples_X)
mean(all_samples_O)
mean(all_samples_tie)
#computing the variance
var(all_samples_X)
var(all_samples_O)
var(all_samples_tie)

curve(dnorm(x, mean = mean(all_samples_X)*n, sd = sqrt(var(all_samples_X))*n), from = 0, to = 1000,
      xlab = "Games", ylab = "Density", main = "All curves", col = "blue", ylim = c(0, 0.04))
curve(dnorm(x, mean = mean(all_samples_O)*n, sd = sqrt(var(all_samples_O))*n), from = 0, to = 1000,
      xlab = "Games", ylab = "Density", main = "All curves", col = "red", add = TRUE)
curve(dnorm(x, mean = mean(all_samples_tie)*n, sd = sqrt(var(all_samples_tie))*n), from = 0, to = 1000,
      xlab = "Games", ylab = "Density", main = "All curves", col = "green", add = TRUE)

legend("topright", legend = c("X", "O", "Tie"), col = c("blue", "red", "green"), lwd = 1)
quartz();