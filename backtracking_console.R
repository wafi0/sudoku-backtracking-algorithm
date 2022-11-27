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


if (solveSudoku()) {
  # Solution has been found; Print it!
  
  # Separators between each cell, including newlines (\n) between rows
  xSep <- c(" ", " ", " | ", " ", " ", " | ", " ", " ")
  ySepA <- "\n"
  ySepB <- "\n------+-------+------\n"
  
  cat("Results:\n")
  cat(t(stateMatrix), sep = 
        rep(
          c(
            rep(c(xSep, ySepA),2), # Every 1st and 2nd of 3 rows
            xSep, ySepB), # Every 3rd row i.e. after row 3 and 6
          3))
  
} else {
  cat("The provided initial state has no solution")
}