(load "Board.lsp")

;; /* *********************************************************************
;; Function Name: recursively-check-capture
;; Purpose: Recursively check and capture connected stones in the game board.
;; Parameters:
;;     - board: A 2D game board represented as a matrix.
;;     - row: The row coordinate of the stone to check for capture.
;;     - col: The column coordinate of the stone to check for capture.
;;     - playerColor: The color of the player whose stones are being checked for capture.
;;     - playerCaptures: The count of captures by the player.
;; Return Value:
;;     - A list containing two elements:
;;         - (first) the game board after checking and capturing stones.
;;         - (second) the updated count of captures by the player.
;; Algorithm:
;;     1. Check if the stone at the specified row and column (row, col) is capturable.
;;     2. If the stone is capturable, recursively capture connected stones and update the board.
;;     3. Repeat the capture process until no more connected stones can be captured.
;;     4. Return a list containing the updated game board and the count of player captures.
;; Assistance Received: None
;; ********************************************************************* */
(defun recursively-check-capture (board row col playerColor playerCaptures)
  (let* ((captured-board (check-capture board row col playerColor)))
    (cond
      (captured-board
       (recursively-check-capture captured-board row col playerColor (+ 1 playerCaptures)))
      (t
       (list board playerCaptures)))))


;; /* *********************************************************************
;; Function Name: is-three-points-away
;; Purpose: Determine if the next position is at least three points away from the initial position.
;; Parameters:
;;     - initial-pos: A string representing the initial position (e.g., "J10").
;;     - next-pos: A string representing the next position to be checked.
;; Return Value:
;;     - Returns true if the next position is at least three points away from the initial position; otherwise, returns false.
;; Algorithm:
;;     1. Convert the initial and next positions to row and column coordinates.
;;     2. Calculate the absolute differences in rows and columns.
;;     3. Check if at least one of the differences is greater than or equal to 3, indicating that the positions are three points away.
;;     4. Return true if the condition is met, otherwise return false.
;; Assistance Received: None
;; ********************************************************************* */
(defun is-three-points-away (initial-pos next-pos)
  (let* ((next-pos-upper (string-upcase next-pos)) ; Create a new variable with uppercase next-pos
         (initial-pos-row (parse-integer (subseq initial-pos 1)))
         (initial-pos-col (- (char-code (char initial-pos 0)) (char-code #\A)))
         (next-pos-row (parse-integer (subseq next-pos-upper 1))) ; Use next-pos-upper
         (next-pos-col (- (char-code (char next-pos-upper 0)) (char-code #\A))) ; Use next-pos-upper
         (row-difference (abs (- initial-pos-row next-pos-row)))
         (col-difference (abs (- initial-pos-col next-pos-col)))
         (at-least-three-points-away (or (>= row-difference 3)
                                        (>= col-difference 3)
                                        (and (>= row-difference 3) (>= col-difference 3)))))
    (cond (at-least-three-points-away t)
          (t nil))))



;; /* *********************************************************************
;; Function Name: is-empty-cell
;; Purpose: Check if a cell on the game board is empty (contains 'O').
;; Parameters:
;;     - board: The game board represented as a two-dimensional array.
;;     - row: The row index of the cell to check.
;;     - col: The column index of the cell to check.
;; Return Value:
;;     - Returns true if the cell at the specified row and column is empty ('O').
;;       Otherwise, returns false.
;; Algorithm:
;;     1. Get the value of the cell at the specified row and column from the game board.
;;     2. Check if the cell value is equal to 'O', indicating an empty cell.
;;     3. Return true if the cell is empty; otherwise, return false.
;; Assistance Received: None
;; ********************************************************************* */
(defun is-empty-cell (board row col)
  (string= (get-board-value board row col) 'O)
  )


;; /* *********************************************************************
;; Function Name: second-move
;; Purpose: Generate a random valid move for the second turn of the game.
;; Parameters:
;;     - board: The game board represented as a two-dimensional array.
;; Return Value:
;;     - A list containing the generated row, column, and move (in "XY" format).
;;     - If a valid move is generated, it will satisfy the conditions defined below.
;;       Otherwise, the function will recursively attempt to generate a valid move.
;; Algorithm:
;;     1. Generate a random row and column within the board's dimensions (0-18).
;;     2. Convert the row and column into a move in the "A1" format.
;;     3. Check if the generated move is at least three points away from "J10" (the center).
;;     4. Check if the cell at the specified row and column is empty.
;;     5. If both conditions are met, return the generated row, column, and move.
;;     6. If not, recursively call the function to generate another move.
;; Assistance Received: None
;; ********************************************************************* */
(defun second-move (board)
  (let* ((row (random 19))
        (col (random 19)))
    (let ((move (convert-to-move row col)))
      (cond
        ((and (is-three-points-away "J10" move) (empty-cell-p board row col))
         (list row col move))
        (t
         (second-move board))))))




;; /* *********************************************************************
;; Function Name: check-stones
;; Purpose: Count the number of stones on the game board.
;; Parameters:
;;     - board: The game board represented as a two-dimensional array.
;; Return Value:
;;     - The total count of stones (occupied cells) on the game board.
;; Algorithm:
;;     1. Initialize a count to 0.
;;     2. Iterate through each cell in the two-dimensional board:
;;        a. If the cell is empty, continue to the next cell.
;;        b. If the cell is occupied (contains a stone), increment the count by 1.
;;     3. Return the final count of stones on the board.
;; Assistance Received: None
;; ********************************************************************* */
(defun check-stones(board)
  (defun check-next-cell (current-board current-row current-col count)
    (cond
      ((>= current-row 18)
        ;;this count gives the moveCount
        (+ 1 count))
      ((>= current-col 18)
       (check-cell current-board (+ 1 current-row) 0 count))
      (t
       (check-cell current-board current-row (+ 1 current-col) count))))

  (defun check-cell (current-board row col count)
    (cond
      ((empty-cell-p current-board row col)
       (check-next-cell current-board row col count))
      (t
       (check-next-cell current-board row col (+ count 1)))))

  (check-cell board 0 0 0)
)

;; /* *********************************************************************
;; Function Name: count-four
;; Purpose: Count the number of open four-in-a-row positions for a given player symbol on the game board.
;; Parameters:
;;     - board: The game board represented as a two-dimensional array.
;;     - symbol: The player's symbol ('W' for White or 'B' for Black).
;; Return Value:
;;     - The count of open four-in-a-row positions for the specified player symbol.
;; Algorithm:
;;     1. Define an internal function count-in-direction to count consecutive symbols in a given direction.
;;     2. Define an internal function count-at-position to count open four-in-a-row positions at a specific cell.
;;     3. Define an internal function count-at-coordinates to iterate through all cells and count open four-in-a-row positions.
;;     4. Initialize the count by calling count-at-coordinates starting from the top-left corner of the board.
;;     5. Return the final count of open four-in-a-row positions for the player symbol.
;; Assistance Received: None
;; ********************************************************************* */
(defun count-four (board symbol)
  (defun count-in-direction (row col dx dy count)
    (cond
      ((and (<= 0 row 18) (<= 0 col 18))
       (let* ((value (get-board-value board row col)))
         (cond
           ((string= value symbol)
            (count-in-direction (+ row dx) (+ col dy) dx dy (+ count 1)))
           (t count))))
       (t 0)))

  (defun count-at-position (row col direction)
    (let* ((dx (first direction))
           (dy (second direction))
           (count (count-in-direction row col dx dy 0)))
      (cond
        ((= count 4) 1)
        (t 0))))

  (defun count-at-coordinates (row col)
    (cond
      ((<= row 18)
       (cond
         ((<= col 18)
          (+ (count-at-position row col '(0 1))
             (count-at-position row col '(1 0))
             (count-at-position row col '(1 1))
             (count-at-position row col '(1 -1))
             (count-at-coordinates row (+ col 1))))
         (t (count-at-coordinates (+ row 1) 0))))
      (t 0)))

  (count-at-coordinates 0 0))