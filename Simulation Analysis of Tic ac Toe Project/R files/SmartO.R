N <- 1000  # number of samples (simulation runs)
n <- 1000  # size of the sample
all_samples_X <- c()  # The Percentage of X won compared to n
all_samples_O <- c()  # The Percentage of O won compared to n
all_samples_tie <- c()  # The Percentage of tie that happened compared to n

for (k in 1:N) {
  sample <- c()  # The actual sample used
  sample_games = list()
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
    value <- "tie"
    
    for (j in 1:9) {
      space <- which(board == ' ')
      
      if (j %% 2 == 1) {
        mark <- 'X'
        if (length(space) == 0) {
          value <- "tie"
          game_over <- TRUE
          break
        }
        
        random_position_index <- sample(length(space), 1)
        random_position <- space[random_position_index]
        board[random_position] <- mark
        space <- space[-random_position_index]
        
        # Check for a winning condition for Player X
        if (j >= 5) {
          for (combination in winning_combinations) {
            if (all(board[combination] == mark)) {
              value <- mark
              game_over <- TRUE
              break
            }
          }
        }
      } else {
        mark <- 'O'
        if (length(space) == 0) {
          value <- "tie"
          game_over <- TRUE
          break
        }
        
        # Smart move for Player O
        # Check if there is any winning move for Player O
        for (combination in winning_combinations) {
          if (sum(board[combination] == 'O') == 2 && sum(board[combination] == ' ') == 1) {
            winning_move <- combination[which(board[combination] == ' ')]
            board[winning_move] <- mark
            value <- mark
            game_over <- TRUE
            break
          }
        }
        
        if (!game_over) {
          # Check if there is any winning move for Player X
          for (combination in winning_combinations) {
            if (sum(board[combination] == 'X') == 2 && sum(board[combination] == ' ') == 1) {
              winning_move <- combination[which(board[combination] == ' ')]
              board[winning_move] <- mark
              break
            }
          }
        }
        
        # If no winning move is available, make a random move
        if (!game_over) {
          random_position_index <- sample(length(space), 1)
          random_position <- space[random_position_index]
          board[random_position] <- mark
        }
        
        space <- space[-random_position_index]
      }
      
      if (game_over) {
        break
      }
    }
    
    sample[i] <- c(value) 
    sample_games[i] <- list(board)
    
  }
  
  all_samples_X <- c(all_samples_X, sum(sample == 'X') /n )
  all_samples_O <- c(all_samples_O, sum(sample == 'O') /n)
  all_samples_tie <- c(all_samples_tie, sum(sample == 'tie') /n)
}

means <- c(mean(all_samples_X), mean(all_samples_O), mean(all_samples_tie))
barplot(
  means, names.arg = c("X", "O", "Tie"), main = "Mean Scores (X: Dumb, O: Smart)",
  ylab = "Percentage won", ylim = c(0, max(means) * 1.2)
)
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

curve(dnorm(x, mean = mean(all_samples_X)*n, sd = sqrt(var(all_samples_X))*n), from =0, to = 800, 
      xlab = "Games", ylab = "Density", main = "All curves ",col ="blue")
curve(dnorm(x, mean = mean(all_samples_O)*n, sd = sqrt(var(all_samples_O))*n), from =0, to = 800, 
      xlab = "Games", ylab = "Density", main = "All curves ",col ="red",add=TRUE) 
curve(dnorm(x, mean = mean(all_samples_tie)*n, sd = sqrt(var(all_samples_tie))*n), from =0, to = 800, 
      xlab = "Games", ylab = "Density", main = "All curves ",col ="green",add=TRUE) 

legend("topright", legend = c("X","O","tie"), col = c("blue","red","green"), lwd = 1)
quartz();
