#Simulation Project R Script 
# Tic Tace Toe Game Simulation 
## strategy 1
##Smart X 
N <- 1000  # number of samples (simulation runs)
n <- 1000  # size of the sample
all_samples_X <- c()  # The Percentage of X won compared to n
all_samples_O <- c()  # The Percentage of O won compared to n
all_samples_tie <- c()  # The Percentage of ties compared to n

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
        
        # Smart move for Player X
        # Check if there is any winning move for Player X
        for (combination in winning_combinations) {
          if (sum(board[combination] == 'X') == 2 && sum(board[combination] == ' ') == 1) {
            winning_move <- combination[which(board[combination] == ' ')]
            board[winning_move] <- mark
            value <- mark
            game_over <- TRUE
            break
          }
        }
        
        if (!game_over) {
          # Check if there is any winning move for Player O to block
          for (combination in winning_combinations) {
            if (sum(board[combination] == 'O') == 2 && sum(board[combination] == ' ') == 1) {
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
        
      } else {
        mark <- 'O'
        if (length(space) == 0) {
          value <- "tie"
          game_over <- TRUE
          break
        }
        
        # Random move for Player O
        random_position_index <- sample(length(space), 1)
        random_position <- space[random_position_index]
        board[random_position] <- mark
        space <- space[-random_position_index]
        
        # Check for a winning condition for Player O
        if (j >= 5) {
          for (combination in winning_combinations) {
            if (all(board[combination] == mark)) {
              value <- mark
              game_over <- TRUE
              break
            }
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
  
  all_samples_X <- c(all_samples_X, sum(sample == 'X')/n )
  all_samples_O <- c(all_samples_O, sum(sample == 'O') /n)
  all_samples_tie <- c(all_samples_tie, sum(sample == 'tie')/n)
}

means <- c(mean(all_samples_X), mean(all_samples_O), mean(all_samples_tie))

# Plotting the mean scores
barplot(
  means, names.arg = c("X", "O", "Tie"), main = "Mean Scores (X: Smart, O: Random)",
  ylab = "Percentage won", ylim = c(0, max(means) * 1.2)
)
text(x = 1:length(means), y = means, labels = means, pos = 3, col = "black") 

curve(dnorm(x, mean = mean(all_samples_X)*n, sd = sqrt(var(all_samples_X))*n), from = -100, to = 1000, 
      xlab = "Games", ylab = "Density", main = "All curves", col = "blue", ylim = c(0, 0.13))
curve(dnorm(x, mean = mean(all_samples_O)*n, sd = sqrt(var(all_samples_O))*n), from = -100, to = 1000, 
      xlab = "Games", ylab = "Density", main = "All curves", col = "red", add = TRUE)
curve(dnorm(x, mean = mean(all_samples_tie)*n, sd = sqrt(var(all_samples_tie))*n), from = 0, to = 1200, 
      xlab = "Games", ylab = "Density", main = "All curves", col = "green", add = TRUE)

legend("topright", legend = c("X", "O", "tie"), col = c("blue", "red", "green"), lwd = 1)

## strategy 2 
## Smart O
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

sample 

curve(dnorm(x, mean = mean(all_samples_X)*n, sd = sqrt(var(all_samples_X))*n), from =0, to = 800, 
      xlab = "Games", ylab = "Density", main = "All curves ",col ="blue")
curve(dnorm(x, mean = mean(all_samples_O)*n, sd = sqrt(var(all_samples_O))*n), from =0, to = 800, 
      xlab = "Games", ylab = "Density", main = "All curves ",col ="red",add=TRUE) 
curve(dnorm(x, mean = mean(all_samples_tie)*n, sd = sqrt(var(all_samples_tie))*n), from =0, to = 800, 
      xlab = "Games", ylab = "Density", main = "All curves ",col ="green",add=TRUE) 

legend("topright", legend = c("X","O","tie"), col = c("blue","red","green"), lwd = 1)

## strategy 3
##Smart XO

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
barplot(means, names.arg = c("X", "O", "Tie"), main = "Mean Scores (updated strategy)",
        ylab = "Percentage won", ylim = c(0, max(means) * 1.2))
text(x = 1:length(means), y = means, labels = means, pos = 3, col = "black")

curve(dnorm(x, mean = mean(all_samples_X)*n, sd = sqrt(var(all_samples_X))*n), from = 0, to = 1000,
      xlab = "Games", ylab = "Density", main = "All curves", col = "blue", ylim = c(0, 0.04))
curve(dnorm(x, mean = mean(all_samples_O)*n, sd = sqrt(var(all_samples_O))*n), from = 0, to = 1000,
      xlab = "Games", ylab = "Density", main = "All curves", col = "red", add = TRUE)
curve(dnorm(x, mean = mean(all_samples_tie)*n, sd = sqrt(var(all_samples_tie))*n), from = 0, to = 1000,
      xlab = "Games", ylab = "Density", main = "All curves", col = "green", add = TRUE)

legend("topright", legend = c("X", "O", "Tie"), col = c("blue", "red", "green"), lwd = 1)

## strategy 4 
#XO random
#This strategy involves two players, Player X and Player O, 
#who place their symbols randomly on the board (X and O)]
# X goes first

N= 1000 # number of samples ( simulation runs) 
n= 1000 # size of the sample
all_samples_X <- c() # The Percentage of X won compared to n 
all_samples_O <- c() # The Percentage of O won compared to n 
all_samples_tie <- c() # The Percentage of tie that happened compared to n
for (k in 1:N) {
  sample <- c() # The actual sample used 
  sample_games = list()
  
  
  for (i in 1:n) {
    board <- matrix(' ', 3, 3)  #creating a board
    
    winning_combinations <- list( #creating the winning condition 
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
      space <- which(board == ' ') #looking for an empty space in the board
      
      #and then randomly placing an X or an O
      
      if (j %% 2 == 1) {
        mark <- 'X'
      } else {
        mark <- 'O' 
      } 
      
      # We place an X first then an O. In the game, each player
      # is given a turn to place their symbol so this code
      # makes sure that happens. 
      
      if (length(space) == 0) {
        value <- "tie"
        game_over <- TRUE #game ends
        break
      } 
      
      #If there are no spaces left to place the symbols and no winning 
      #conditions activated, We declare the game ending in a draw (tie)
      
      random_position_index <- sample(length(space), 1)
      random_position <- space[random_position_index]
      board[random_position] <- mark 
      
      #The random number generation is done here using the code sample()
      # we sample from the space, which gets smaller each turn in order 
      # to avoid placing symbols on the same spot twice 
      #after we get an index we actual find a position relative to 
      # a place on the board 
      # After that with the position we found, we then assign the position
      # with either an X or O depending on which turn it is.
      
      
      space <- space[-random_position_index] 
      
      # We then update space to remove the position that we currently 
      #chose above. 
      
      # Check for a winning condition
      if (j >= 5) {
        for (combination in winning_combinations) {
          if (all(board[combination] == mark)) {
            value <- mark
            game_over <- TRUE #game ends
            break
          }
        }
      } 
      # This code checks whether a winning combination exists or not.
      # The reason that is for 5 moves and after is because at least 5 moves 
      # are required for either player to potentially form a 
      # winning combination. 
      # The code checks if all the positions in the current
      # combination of the board have been filled with 
      # the current player's symbol 
      
      
      
      
      if (game_over) {
        break
      }
    }
    # Thie code only executes when game over = true and stops the loop
    # if a winning condition is found or a tie happens. 

    sample[i] <- c(value) 
    sample_games[i] <- list(board)  # we make a sample with all the X's
    # O's and tie's that happened, it's size is n 
  }
  
  all_samples_X <- c(all_samples_X, sum(sample == 'X')/n) 
  all_samples_O <- c(all_samples_O, sum(sample == 'O')/n)
  all_samples_tie <- c(all_samples_tie, sum(sample == 'tie')/n) 
  
  # Each all_samples here calculate the amount of times X appears, 
  # O appears and tie appears, then we divide them by n and get percentage 
  # for the amount of times they appear in a sample
  #Then we get many samples. 
  
} 



means= c(mean(all_samples_X),mean(all_samples_O),mean(all_samples_tie))
barplot(means, names.arg = c("X", "O", "Tie"), main = "Mean Scores (random strategy)", 
        ylab = "Percentage won", ylim= c(0,max(means)*1.2))
text(x = 1:length(means), y = means, labels = means, pos = 3, col = "black")
#Bar plot comparing the average Percentage of X, O and tie appearing in a sample.






curve(dnorm(x, mean = mean(all_samples_X)*n, sd = sqrt(var(all_samples_X))*n), from = 0, to = 1000, 
      xlab = "Games", ylab = "Density", main = "All curves", col = "blue", ylim = c(0, 0.04))
curve(dnorm(x, mean = mean(all_samples_O)*n, sd = sqrt(var(all_samples_O))*n), from = 0, to = 1000, 
      xlab = "Games", ylab = "Density", main = "All curves", col = "red", add = TRUE)
curve(dnorm(x, mean = mean(all_samples_tie)*n, sd = sqrt(var(all_samples_tie))*n), from = 0, to = 1000, 
      xlab = "Games", ylab = "Density", main = "All curves", col = "green", add = TRUE)

legend("topright", legend = c("X", "O", "tie"), col = c("blue", "red", "green"), lwd = 1)

