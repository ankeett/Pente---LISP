(load "HumanPlayer.lsp")

;; /* *********************************************************************
;; Function Name: make-2d-board
;; Purpose: Creates a 2D board consisting of 'O' (representing empty cells).
;;          The board has the specified number of rows and columns.

;; Parameters:
;; - rows (integer): The number of rows in the 2D board.
;; - cols (integer): The number of columns in the 2D board.

;; Return Value: A 2D board represented as a nested list, where 'O' denotes an empty cell.

;; Algorithm: 
;; 1. The make-2d-board function is a recursive function that creates a 2D board with the specified number of rows and columns.
;; 2. It uses a helper function make-row to create a single row of the board.
;; 3. The make-row function takes the number of columns (cols) as a parameter and returns a row of 'O's represented as a list.
;; 4. The make-2d-board function recursively builds the 2D board by creating rows and adding them to the board until it reaches the specified number of rows.
;; 5. The base case is when rows is zero, in which case an empty list '()' is returned.
;; 6. In the recursive case, a row is created using make-row, and the result is a nested list representing the 2D board.

;; Assistance Received: None
;; ********************************************************************* */
(defun make-2d-board (rows cols)
  (labels ((make-row (cols)
             (cond
               ((zerop cols) '())
               (t (cons 'O (make-row (- cols 1)))))))
    (cond
      ((zerop rows) '())
      (t (cons (make-row cols)
               (make-2d-board (- rows 1) cols))))))


;; /* *********************************************************************
;; Function Name: print-1d-row
;; Purpose: Prints the elements of a 1D row represented as a list.
;; Parameters:
;; - row (list): A list representing a 1D row.
;; Return Value: None (nil). This function primarily performs output operations.
;; Algorithm:
;; 1. Recursively prints each element of the 'row' list using the 'format' function.
;; 2. The recursion continues until the 'row' is empty (null).
;; 3. In the base case, when 'row' is null, the function returns nil.

;; Assistance Received: None
;; ********************************************************************* */
(defun print-1d-row (row)
  (cond
    ((null row) nil)
    (t
      (format t "~a " (first row))
      (print-1d-row (rest row)))))


;; /* *********************************************************************
;; Function Name: print-2d-board
;; Purpose: Prints a 2D board represented as a list of rows, along with column and row labels.
;; Parameters:
;; - board (list): A 2D board represented as a list of rows.
;; Return Value: None (nil). This function primarily performs output operations.
;; Algorithm:
;; 1. Defines column labels and prints a space as the top-left corner.
;; 2. Prints the column labels.
;; 3. Calls the function 'print-board-rows' to print the board with row labels.

;; Assistance Received: None
;; ********************************************************************* */
(defun print-2d-board (board)
  (let ((column-labels '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S)))
    (format t "~3a " '#\space) ; Print a space as the top-left corner
    (print-column-labels column-labels) ; Print the column labels
    (format t "~%")
    (print-board-rows board 19 1))) ; Print the board with row labels


;; /* *********************************************************************
;; Function Name: print-column-labels
;; Purpose: Prints column labels for a 2D board.
;; Parameters:
;; - cols (list of characters): A list of characters representing column labels.
;; Return Value: None (nil). This function primarily performs output operations.
;; Algorithm:
;; 1. Recursively prints each column label from the 'cols' list.
;; 2. The recursion continues until the 'cols' list is empty (null).

;; Assistance Received: None
;; ********************************************************************* */
(defun print-column-labels (cols)
  (cond
    ((null cols) nil)
    (t
      (format t "~3a  " (first cols))
      (print-column-labels (rest cols)))))


;; /* *********************************************************************
;; Function Name: print-board-rows
;; Purpose: Prints the rows of a 2D board, including row labels.
;; Parameters:
;; - board (list of lists): A 2D board represented as a list of rows.
;; - row-label (integer): The label of the current row being printed.
;; - start-row (integer): The starting row label. Used to calculate row labels.
;; Return Value: None (nil). This function primarily performs output operations.
;; Algorithm:
;; 1. Recursively prints each row of the 'board' list along with the row label.
;; 2. Calls the 'print-board-row-contents' function to print the contents of each row.
;; 3. The recursion continues until the 'board' list is empty (null).

;; Assistance Received: None
;; ********************************************************************* */
(defun print-board-rows (board row-label start-row)
  (cond
    ((null board) nil)
    (t
      (format t "~2a  " row-label)
      (print-board-row-contents (first board) start-row)
      (format t "~%")
      (print-board-rows (rest board) (1- row-label) (1+ start-row)))))


;; /* *********************************************************************
;; Function Name: print-board-row-contents
;; Purpose: Prints the contents of a single row of a 2D board.
;; Parameters:
;; - row (list): A list representing a row of a 2D board.
;; - row-label (integer): The label of the row to which this contents belongs.
;; Return Value: None (nil). This function primarily performs output operations.
;; Algorithm:
;; 1. Recursively prints the contents of each cell in the 'row'.
;; 2. Uses the 'format' function to print each cell with proper spacing.
;; 3. The recursion continues until the 'row' list is empty (null).

;; Assistance Received: None
;; ********************************************************************* */
(defun print-board-row-contents (row row-label)
  (cond
    ((null row) nil)
    (t
      (let ((cell (first row)))
        (cond
          ((string= cell 'O) (format t "~3a  " #\.))
          (t (format t "~3a  " cell)))
      )
      (print-board-row-contents (rest row) row-label))))

;; /* *********************************************************************
;; Function Name: set-board-value
;; Purpose: Sets the value of a cell in a 2D board at the specified row and column.
;; Parameters:
;; - board (list of lists): A 2D board represented as a list of rows.
;; - row (integer): The row index at which to set the value.
;; - col (integer): The column index at which to set the value.
;; - new-value (any): The new value to be assigned to the specified cell.
;; Return Value: The updated 2D board with the value set at the specified row and column.
;; Algorithm:
;; 1. Checks if the row and col indices are within bounds (0 to 18).
;; 2. If they are within bounds, calls the 'update-board' function to set the 'new-value' at the specified row and column.
;; 3. Returns the updated 'board'.
;; 4. If the indices are out of bounds, prints an error message.
;; Assistance Received: None
;; ********************************************************************* */
(defun set-board-value (board row col new-value)
  (cond
    ((and (<= 0 row 18) (<= 0 col 18))
      (let ((board (update-board board row col new-value)))
        board)
    )
    (
      t 
      (format t "Indices out of bounds.(from set-board-value)~%")
    )
  )
)

;; /* *********************************************************************
;; Function Name: place-stone
;; Purpose: Places a stone symbol on a 2D board at the specified row and column.
;; Parameters:
;; - board (list of lists): A 2D board represented as a list of rows.
;; - row (integer): The row index at which to place the stone.
;; - col (integer): The column index at which to place the stone.
;; - symbol (any): The symbol to be placed on the board, typically 'W or 'B.
;; Return Value: The updated 2D board with the stone placed at the specified row and column.
;; Algorithm:
;; 1. Checks if the row and col indices are within bounds (0 to 18).
;; 2. If they are within bounds, checks the current value at the specified cell.
;; 3. If the cell is empty (0) or contains an 'O', calls the 'update-board' function to place the 'symbol' at the specified cell.
;; 4. Returns the updated 'board'.
;; 5. If the cell is already occupied, prints an error message, requests a new move, and calls 'getUserMove'.
;; 6. If the indices are out of bounds, prints an error message.
;; Assistance Received: None
;; ********************************************************************* */
(defun place-stone(board row col symbol)
  (cond
    ((and (<= 0 row 18) (<= 0 col 18))
      (cond
        ((equal (get-board-value board row col) 0)
          (let ((board (update-board board row col symbol)))
            board)
          )
        ((string= (get-board-value board row col) 'O)
          (let ((board (update-board board row col symbol)))
            board)
          )
        (t
          (princ "Invalid move.")
          (terpri)
          (princ "Please try again.")
          ()
          (getUserMove)
          )
        )
      )
    (t 
      (format t "Indices out of bounds.(from place stone)~%")
      )
    )
  )

      
;; /* *********************************************************************
;; Function Name: update-board
;; Purpose: Updates a cell in a 2D board at the specified row and column with a new value.
;; Parameters:
;; - board (list of lists): A 2D board represented as a list of rows.
;; - row (integer): The row index of the cell to be updated.
;; - col (integer): The column index of the cell to be updated.
;; - new-value (any): The new value to be assigned to the specified cell.
;; Return Value: The updated 2D board with the value set at the specified row and column.
;; Algorithm:
;; 1. Recursively updates a cell in the 'board' list by traversing rows and columns.
;; 2. When the row index matches the specified 'row', it calls the 'update-row' function to update the cell at the specified column.
;; 3. Returns the updated 'board'.
;; Assistance Received: None
;; ********************************************************************* */      
(defun update-board (board row col new-value)
    (cond
        ((null board) nil)
        ((= row 0) (cons (update-row (first board) col new-value) (rest board)))
        (t (cons (first board) (update-board (rest board) (- row 1) col new-value))))
)


;; /* *********************************************************************
;; Function Name: update-row
;; Purpose: Updates a cell in a row of a 2D board at the specified column with a new value.
;; Parameters:
;; - row (list): A list representing a row in a 2D board.
;; - col (integer): The column index of the cell to be updated.
;; - new-value (any): The new value to be assigned to the specified cell.
;; Return Value: The updated row with the value set at the specified column.
;; Algorithm:
;; 1. Recursively updates a cell in the 'row' list by traversing columns.
;; 2. When the column index matches the specified 'col', it replaces the value at that column with the 'new-value'.
;; 3. Returns the updated 'row'.
;; Assistance Received: None
;; ********************************************************************* */
(defun update-row (row col new-value)
    (cond
        ((null row) nil)
        ((= col 0) (cons new-value (rest row)))
        (t (cons (first row) (update-row (rest row) (- col 1) new-value)))))


;; /* *********************************************************************
;; Function Name: get-board-value
;; Purpose: Retrieves the value of a cell in a 2D board at the specified row and column.
;; Parameters:
;; - board (list of lists): A 2D board represented as a list of rows.
;; - row (integer): The row index of the cell to be retrieved.
;; - col (integer): The column index of the cell to be retrieved.
;; Return Value: The value of the cell at the specified row and column, or nil if indices are out of bounds.
;; Algorithm:
;; 1. Checks if the row and col indices are within bounds (0 to 18).
;; 2. If they are within bounds, calls the 'get-row-value' function to retrieve the value at the specified cell.
;; 3. Returns the retrieved value.
;; 4. If the indices are out of bounds, prints an error message and returns nil.

;; Assistance Received: None
;; ********************************************************************* */
(defun get-board-value (board row col)
  (cond
    ((and (<= 0 row 18) (<= 0 col 18))
     (let ((value (get-row-value board row col)))
       value)) ; Return the value
     (t
      (format t "Indices out of bounds.(from get-board-value)~%")))
)

;; /* *********************************************************************
;; Function Name: get-row-value
;; Purpose: Retrieves the value of a cell in a specific row of a 2D board at the specified column.
;; Parameters:
;; - board (list of lists): A 2D board represented as a list of rows.
;; - row (integer): The row index from which to retrieve the value.
;; - col (integer): The column index of the cell to be retrieved.
;; Return Value: The value of the cell in the specified row and column, or nil if indices are out of bounds.
;; Algorithm:
;; 1. Recursively retrieves the value of a cell in a specific row of the 'board' list by traversing columns.
;; 2. When the row index matches the specified 'row', it calls the 'get-column-value' function to retrieve the value at the specified column.
;; 3. Returns the retrieved value.
;; 4. If the indices are out of bounds, returns nil.

;; Assistance Received: None
;; ********************************************************************* */
(defun get-row-value (board row col)
    (cond
        ((null board) nil)
        ((= row 0) (get-column-value (first board) col))
        (t (get-row-value (rest board) (- row 1) col))))


;; /* *********************************************************************
;; Function Name: get-column-value
;; Purpose: Retrieves the value of a cell in a specific column of a row.
;; Parameters:
;; - row (list): A list representing a row in a 2D board. Passed by reference.
;; - col (integer): The column index from which to retrieve the value.
;; Return Value: The value of the cell in the specified column, or nil if the index is out of bounds.
;; Algorithm:
;; 1. Recursively retrieves the value of a cell in a specific column of the 'row' list by traversing columns.
;; 2. When the column index matches the specified 'col', it returns the value at the specified column.
;; 3. If the index is out of bounds, returns nil.

;; Assistance Received: None
;; ********************************************************************* */
(defun get-column-value (row col)
    (cond
        ((null row) nil)
        ((= col 0) (first row))
        (t (get-column-value (rest row) (- col 1)))))


;; /* *********************************************************************
;; Function Name: check-consecutive
;; Purpose: Checks if there are a specified number of consecutive symbols in a certain direction from a given position.
;; Parameters:
;; - board (list of lists): A 2D board represented as a list of rows.
;; - row (integer): The row index of the starting position.
;; - col (integer): The column index of the starting position.
;; - symbol (any): The symbol to be checked for consecutiveness.
;; - num-consecutive (integer): The desired number of consecutive symbols.
;; Return Value: 
;; - true if there are at least 'num-consecutive' consecutive symbols in any direction from the given position, otherwise false.
;; Algorithm:
;; 1. Calculates the sum of consecutive symbols in four directions: vertical-up, horizontal, diagonal-left-up, and diagonal-right-up.
;; 2. Compares each sum to 'num-consecutive + 1' to check for consecutive symbols.
;; 3. Returns true if any of the sums are greater than or equal to 'num-consecutive + 1', indicating consecutiveness.
;; 4. Returns false if no direction has the required number of consecutive symbols.

;; Assistance Received: None
;; ********************************************************************* */
(defun check-consecutive (board row col symbol num-consecutive)
  (let* ((vertical-up-sum
          (+ (check-direction board row col symbol 0 -1 num-consecutive)
             (check-direction board row col symbol 0 1 num-consecutive)))
         (horizontal-sum
          (+ (check-direction board row col symbol -1 0 num-consecutive)
             (check-direction board row col symbol 1 0 num-consecutive)))
         (diagonal-left-up-sum
          (+ (check-direction board row col symbol -1 -1 num-consecutive)
             (check-direction board row col symbol 1 1 num-consecutive)))
         (diagonal-right-up-sum
          (+ (check-direction board row col symbol -1 1 num-consecutive)
             (check-direction board row col symbol 1 -1 num-consecutive))))
    
    (cond
      ((>= vertical-up-sum (+ num-consecutive 1))
       t)
      ((>= horizontal-sum (+ num-consecutive 1))
       t)
      ((>= diagonal-left-up-sum (+ num-consecutive 1))
       t)
      ((>= diagonal-right-up-sum (+  num-consecutive 1))
       t)
      (t
       nil))))


;; /* *********************************************************************
;; Function Name: check-five
;; Purpose: Checks if there are five consecutive symbols of the specified type from a given position.
;; Parameters:
;; - board (list of lists): A 2D board represented as a list of rows.
;; - row (integer): The row index of the starting position.
;; - col (integer): The column index of the starting position.
;; - symbol (any): The symbol to be checked for consecutiveness
;; Return Value: 
;; - true if there are exactly five consecutive symbols of the specified type in any direction from the given position, otherwise false.
;; Algorithm:
;; - Calls the 'check-consecutive' function with 'num-consecutive' set to 5 to check for five consecutive symbols.
;; Assistance Received: None
;; ********************************************************************* */
(defun check-five (board row col symbol)
  (check-consecutive board row col symbol 5)
)

;; /* *********************************************************************
;; Function Name: check-four
;; Purpose: Checks if there are four consecutive symbols of the specified type from a given position.
;; Parameters:
;; - board (list of lists): A 2D board represented as a list of rows. 
;; - row (integer): The row index of the starting position.
;; - col (integer): The column index of the starting position.
;; - symbol (any): The symbol to be checked for consecutiveness 
;; Return Value: 
;; - true if there are exactly four consecutive symbols of the specified type in any direction from the given position, otherwise false.
;; Algorithm:
;; - Calls the 'check-consecutive' function with 'num-consecutive' set to 4 to check for four consecutive symbols.
;; Assistance Received: None
;; ********************************************************************* */
(defun check-four (board row col symbol)
  (check-consecutive board row col symbol 4)
)


;; /* *********************************************************************
;; Function Name: check-direction
;; Purpose: Checks the number of consecutive symbols of a specific type in a certain direction from a given position.
;; Parameters:
;; - board (list of lists): A 2D board represented as a list of rows.
;; - row (integer): The row index of the starting position.
;; - col (integer): The column index of the starting position.
;; - symbol (any): The symbol to be checked for consecutiveness
;; - delta-row (integer): The change in the row index for the direction being checked.
;; - delta-col (integer): The change in the column index for the direction being checked.
;; - count (integer): The desired number of consecutive symbols.
;; Return Value: The number of consecutive symbols in the specified direction from the given position.
;; Algorithm:
;; - Uses a recursive function 'check-direction-rec' to traverse in the specified direction.
;; - Checks for consecutiveness of symbols of the specified type.
;; - Returns the count of consecutive symbols.
;; Assistance Received: None
;; ********************************************************************* */
(defun check-direction (board row col symbol delta-row delta-col count)
  (labels ((check-direction-rec (r c consecutive-stones)
             (cond
               ((>= consecutive-stones count) consecutive-stones)
               ((or (< r 0) (>= r 19) (< c 0) (>= c 19) (not(equal (get-board-value board r c) symbol)))
                consecutive-stones)
               (t (check-direction-rec (+ r delta-row) (+ c delta-col) (+ consecutive-stones 1))))))
    (check-direction-rec row  col 0)))


;; /* *********************************************************************
;; Function Name: check-capture
;; Purpose: Checks if a stone can capture opponent stones in any direction from a given position.
;; Parameters:
;; - board (list of lists): A 2D board represented as a list of rows.
;; - row (integer): The row index of the starting position.
;; - col (integer): The column index of the starting position.
;; - symbol (any): The symbol of the stone making the capture attempt
;; Return Value: 
;; - A modified board with captured opponent stones, or nil if no capture is possible.
;; Algorithm:
;; 1. Determines the opponent's symbol based on the symbol of the capturing stone.
;; 2. Calls 'capture-pair' function for each possible direction to attempt capturing.
;; 3. Returns the modified board with captured stones, if capture is possible.
;; 4. Returns nil if no capture is possible in any direction.

;; Assistance Received: None
;; ********************************************************************* */
(defun check-capture(board row col symbol)
  (let* ((opponent-symbol (cond 
      
        ((string= symbol 'W)
          'B
        )
         (t
             'W
         )
       )
      
     ))
        (cond
            ((let ((capture-board (capture-pair board row col 0 1 symbol opponent-symbol 2)))
              capture-board)
            )
            ((let ((capture-board (capture-pair board row col 0 -1 symbol opponent-symbol 2)))
              capture-board)
            )
            ((let ((capture-board (capture-pair board row col 1 0 symbol opponent-symbol 2)))
              capture-board)
            )
            ((let ((capture-board (capture-pair board row col -1 0 symbol opponent-symbol 2)))
              capture-board)
            )
            ((let ((capture-board (capture-pair board row col 1 1 symbol opponent-symbol 2)))
              capture-board)
            )
            ((let ((capture-board (capture-pair board row col -1 -1 symbol opponent-symbol 2)))
              capture-board)
            )
            ((let ((capture-board (capture-pair board row col -1 1 symbol opponent-symbol 2)))
              capture-board)
            )
            ((let ((capture-board (capture-pair board row col 1 -1 symbol opponent-symbol 2)))
              capture-board)
            )

            (t
              nil)) 
  )
)

;; /* *********************************************************************
;; Function Name: capture-pair
;; Purpose: Attempts to capture a pair of stones in a specific direction from a given position.
;; Parameters:
;; - board (list of lists): A 2D board represented as a list of rows. Passed by reference.
;; - x (integer): The row index of the starting position.
;; - y (integer): The column index of the starting position.
;; - dx (integer): The change in the row index for the direction being checked.
;; - dy (integer): The change in the column index for the direction being checked.
;; - oColor (any): The symbol representing our stone.
;; - eColor (any): The symbol representing the enemy's stone.
;; - count (integer): The number of consecutive opponent stones required for a capture.
;; Return Value: 
;; - A modified board with captured opponent stones, or nil if no capture is possible in the specified direction.
;; Algorithm:
;; 1. Calls 'check-capture-direction' to verify if a capture is possible in the specified direction.
;; 2. If capture is possible, calls 'remove-captured' to remove captured stones and returns the modified board.
;; 3. Returns nil if no capture is possible in the specified direction.

;; Assistance Received: None
;; ********************************************************************* */
(defun capture-pair (board x y dx dy oColor eColor count)
  (cond
    ((check-capture-direction board (+ x dx) (+ y dy) dx dy oColor eColor count)
      (remove-captured board (+ x dx) (+ y dy) dx dy 2)   
    )
  (t
    nil)
  )
)

;; /* *********************************************************************
;; Function Name: check-capture-direction
;; Purpose: Checks if a capture of opponent stones is possible in a specific direction from a given position.
;; Parameters:
;; - board (list of lists): A 2D board represented as a list of rows. Passed by reference.
;; - x (integer): The row index of the starting position.
;; - y (integer): The column index of the starting position.
;; - dx (integer): The change in the row index for the direction being checked.
;; - dy (integer): The change in the column index for the direction being checked.
;; - oColor (any): The symbol representing our stone.
;; - eColor (any): The symbol representing the enemy's stone.
;; - count (integer): The number of consecutive opponent stones required for a capture.
;; Return Value: 
;; - true if a capture is possible in the specified direction, otherwise false.
;; Algorithm:
;; 1. Checks if the indices are within bounds. If not, returns an empty list.
;; 2. Checks if the count is 0 and the current cell contains our symbol.
;; 3. Returns true if the above condition is met.
;; 4. Checks if the current cell doesn't contain the opponent's stone symbol. If so, returns an empty list.
;; 5. Recursively calls itself with updated indices and decreased count.
;; 6. Returns true if the count reaches 0 and all the opponent's stones are consecutively aligned in the specified direction.
;; 7. Returns false otherwise.

;; Assistance Received: None
;; ********************************************************************* */
(defun check-capture-direction (board x y dx dy oColor eColor count)

  (cond 
      ((not(and (<= 0 x 18) (<= 0 y 18)))
        ()
      )
    ((and (equal count 0) (string= (get-board-value board x y) oColor))
      t
    )
    ((not (string= (get-board-value board x y ) eColor))
      ()
    )
    (t
      (check-capture-direction board (+ x dx) (+ y dy) dx dy oColor eColor (- count 1))
    )
  )
)

;; /* *********************************************************************
;; Function Name: remove-captured
;; Purpose: Removes captured opponent stones from the board in a specific direction from a given position.
;; Parameters:
;; - board (list of lists): A 2D board represented as a list of rows.
;; - x (integer): The row index of the starting position.
;; - y (integer): The column index of the starting position.
;; - dx (integer): The change in the row index for the direction being checked.
;; - dy (integer): The change in the column index for the direction being checked.
;; - count (integer): The number of opponent stones to be removed.
;; Return Value: 
;; - A modified board with captured opponent stones removed.
;; Algorithm:
;; 1. Checks if the count is 0, indicating that all opponent stones have been removed.
;; 2. Returns the board if the count reaches 0.
;; 3. Updates the board by setting the current cell to 'O'.
;; 4. Recursively calls itself with updated indices and decreased count.
;; 5. Returns the modified board after removing all the captured opponent stones.

;; Assistance Received: None
;; ********************************************************************* */
(defun remove-captured(board x y dx dy count)
  (cond
    ((equal count 0)
      board)
    (t
      (let* ((new-board (set-board-value board x y 'O))
        (x-next (+ x dx))
        (y-next (+ y dy)))
        (remove-captured new-board x-next y-next dx dy (- count 1)))

    )
  )
)