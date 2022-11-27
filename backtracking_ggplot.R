# Include required packages
packages <- c("tidyverse", "ggplot2")

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

lapply(packages, library, character.only = TRUE) # Load the packages


# Initial sudoku state - with 0 representing empty cells
#   This single matrix will be treated as a global object accessed by all
#   functions, and reassigned values via the "<<-" operator in solveSudoku()
stateMatrix <- t(matrix(c(
  3, 0, 6,   5, 0, 8,   4, 0, 0   ,
  5, 2, 0,   0, 0, 0,   0, 0, 0   ,
  0, 8, 7,   0, 0, 0,   0, 3, 1   ,
  
  0, 0, 3,   0, 1, 0,   0, 8, 0   ,
  9, 0, 0,   8, 6, 3,   0, 0, 5   ,
  0, 5, 0,   0, 9, 0,   6, 0, 0   ,
  
  1, 3, 0,   0, 0, 0,   2, 5, 0   ,
  0, 0, 0,   0, 0, 0,   0, 7, 4   ,
  0, 0, 5,   2, 0, 6,   3, 0, 0
), nrow = 9))


# A function to check if safe to assign a number to given position in the matrix
isSafe <- function(y, x, testNum) {
  
  # Extract values of selected cell's corresponding row, column & 3x3 box
  rowVals <- stateMatrix[y, 1:9]
  colVals <- stateMatrix[1:9, x]
  
  yEnd <- 3 * floor((y+2)/3) # either 3, 6 or 9
  xEnd <- 3 * floor((x+2)/3) # either 3, 6 or 9
  boxVals <- stateMatrix[yEnd - 0:2, xEnd - 0:2]
  
  # Return FALSE if at least one of the values match testNum; TRUE otherwise
  return(!any(c(rowVals, colVals, boxVals) == testNum))
}

# A recursive function to iterate through each cell, assigning the first valid
# number to each current position and moving on to the next (i.e. depth first),
# and "backtracks" to change previously set numbers whenever a cell cannot be
# assigned any number from 1 to 9
solveSudoku <- function(y = 1, x = 1) {
  
  if (x == 10) { # End column has been reached
    
    if (y == 9) { # If end row has been reached
      # All cells have valid numbers i.e. puzzle is solved
      return(TRUE)
    } # Otherwise:
    
    # Reset column; Advance to next row
    y <- y + 1
    x <- 1
  }
  
  # check if current position already contains a nonzero,
  #   and advance if so i.e., by recursively calling itself
  if (stateMatrix[y, x] > 0) {
    return(solveSudoku(y, x + 1))
  }
  
  # Test the current position for all possible numbers one by one
  # This part cannot be vectorized at the function must be called recursively
  for (testNum in 1:9) {
    # Check if placing testNum is safe in current position
    if (isSafe(y, x, testNum)) {
      
      # Set the cell at current position to the testNum
      stateMatrix[y, x] <<- testNum
      
      # Advance to next iteration
      if (solveSudoku(y, x + 1)) {
        return(TRUE)
      }
    }
    
    # Otherwise (if not safe) reset the current position to have the value 0
    stateMatrix[y, x] <<- 0
  }
  
  # All possibilities exhausted, i.e., current starting state has no solution
  return(FALSE)
}

# IMPORTANT: Be sure to include required packages at top of script

if (solveSudoku()) {
  # Solution has been found; Display it!
  
  # Convert the matrix into a tibble that includes the cell elements' locations
  solution_df <- tibble(
    elem = c(t(stateMatrix))
  ) %>% mutate(
    x = (row_number()-1)%%9,
    y = 8 - floor((row_number()-1)/9)
  )
  
  # Skeleton of the Sudoku puzzle
  thin_frame_df <- tibble(
    x = c(
      rep(0, 6),
      1, 2, 4, 5, 7, 8
    ) - 0.5,
    xend = c(
      rep(9, 6),
      1, 2, 4, 5, 7, 8
    ) - 0.5,
    y = c(
      1, 2, 4, 5, 7, 8,
      rep(0, 6)
    ) - 0.5,
    yend = c(
      1, 2, 4, 5, 7, 8,
      rep(9, 6)
    ) - 0.5,
  )
  
  # Continuation - Skeleton of the Sudoku puzzle
  thick_frame_df <- tibble(
    x = c(
      rep(0, 4),
      0, 3, 6, 9
    ) - 0.5,
    xend = c(
      rep(9, 4),
      0, 3, 6, 9
    ) - 0.5,
    y = c(
      0, 3, 6, 9,
      rep(0, 4)
    ) - 0.5,
    yend = c(
      0, 3, 6, 9,
      rep(9, 4)
    ) - 0.5,
  )
  
  # Apply void theme as this is easily customizable
  theme_set(theme_void())
  
  # Plot the final state
  ggplot() +
    geom_segment(data = thin_frame_df,
                 aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_segment(data = thick_frame_df,
                 aes(x = x, y = y, xend = xend, yend = yend), size=1) +
    geom_text(data = solution_df, aes(x = x, y = y, label=elem)) +
    coord_equal()
  
} else {
  cat("The provided initial state has no solution")
}