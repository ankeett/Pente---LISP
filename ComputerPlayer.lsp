(load "Board.lsp")

(defun computerMove()
  (let* ((row (random 19))
         (col (random 19))
         (adjusted-row (- 19 row)) ;suspected error
         (move (concatenate 'string (string (code-char (+ 65 col))) (write-to-string adjusted-row)))
         )
    (list row col move)))


;;IN PROGRESS
(defun empty-cell-p (board row col)
  (equal (get-board-value board row col) 0))






(defun find-winning-move (board player-symbol)
  (defun check-next-cell (current-board current-row current-col)
    (cond
      ((>= current-row 18)
       (list nil nil)) ; Return nil values if no winning move is found
      ((>= current-col 18)
       (check-cell current-board (+ 1 current-row) 0))
      (t
       (check-cell current-board current-row (+ 1 current-col)))))

  (defun check-cell (current-board row col)
    (cond
      ((empty-cell-p current-board row col)
       (let* ((new-board (set-board-value current-board row col player-symbol)))
         (cond
           ((check-five new-board row col player-symbol)
            (list row col)) ; Return the row and col of the winning move
           (t
            (check-next-cell current-board row col))
            )))
      (t
       (check-next-cell current-board row col))))

  (check-cell board 0 0)
)

(defun defend-winning-move(board player-symbol)
  (let* ((opponent-symbol (cond 
      
        ((string= player-symbol "W")
          "B"
        )
         (t
             "W"
         )
       )

  ))
    (find-winning-move board opponent-symbol)
  )
)

(defun find-capture-position (board player-symbol)
  (defun check-next-cell (current-board current-row current-col)
    (cond
      ((>= current-row 18)
       (list nil nil)) ; Return nil values if no winning move is found
      ((>= current-col 18)
       (check-cell current-board (+ 1 current-row) 0))
      (t
       (check-cell current-board current-row (+ 1 current-col)))))

  (defun check-cell (current-board row col)
    (cond
      ((empty-cell-p current-board row col)
       (let* ((new-board (set-board-value current-board row col player-symbol)))
         (cond
            ((let* ((captured-board (check-capture new-board row col player-symbol)))
                  
              (cond
                (captured-board
                  (list row col))
                (t
                  (check-next-cell current-board row col)
                )
              ))
            ))
            
            ))
      (t
       (check-next-cell current-board row col))))

  (check-cell board 0 0)

)

(defun defend-capture-position (board player-symbol)
  (let* ((opponent-symbol (cond 
      
        ((string= player-symbol "W")
          "B"
        )
         (t
             "W"
         )
       )

  ))
    (find-capture-position board opponent-symbol)
  )

)


(defun search-empty-cell (board center-row center-col dr dc)
  (cond ((or (< dr (- center-row 4)) (> dr (+ center-row 4))
             (< dc (- center-col 4)) (> dc (+ center-col 4)))
         nil) ; Out of bounds, return nil
        ((and (>= dr 0) (< dr 19) (>= dc 0) (< dc 19) (empty-cell-p board dr dc))
         (list dr dc)
         ) ; If the cell is empty and within bounds, return its coordinates as a list
        (t
         (search-empty-cell board center-row center-col (+ dr 1) dc)
        )
  )) ; Recursively search the next position

(defun find-empty-cell (board center-row center-col player-symbol radius)
  (cond ((= radius 5)
         (list -1 -1)
        ) ; If radius exceeds 4, no move is available
        (t
         (let* ((result (search-empty-cell board center-row center-col (- radius) (- radius)))
                (next-radius (+ radius 1)))
           (cond ((result)
                  result)
                 (t
                  (find-empty-cell board center-row center-col player-symbol next-radius)
                  )
            ))
        ))
)

(defun control-center (board player-symbol)
  (let* ((center-row 9)
         (center-col 9))
    (cond ((empty-cell-p board center-row center-col)
           (list center-row center-col)) ; Return center as a list if it's empty
          (t
           (find-empty-cell board center-row center-col player-symbol 1)))))
;; pair<int, int> Strategy::controlCenter(Board B, int playerSymbol) {
;; 	// Define the coordinates of the center of the board
;; 	int centerRow = 9;
;; 	int centerCol = 9;

;; 	// Check if the center is empty and return it if so
;; 	if (B.isEmptyCell(centerRow, centerCol)) {
;; 		return make_pair(centerRow, centerCol);
;; 	}

;; 	// If the center is not empty, search for the nearest empty cell around the center
;; 	for (int radius = 1; radius < 5; radius++) {
;; 		for (int dr = -radius; dr <= radius; dr++) {
;; 			for (int dc = -radius; dc <= radius; dc++) {
;; 				int newRow = centerRow + dr;
;; 				int newCol = centerCol + dc;

;; 				// Check if the new position is within bounds and empty
;; 				if (newRow >= 0 && newRow < 19 && newCol >= 0 && newCol < 19 &&
;; 					B.isEmptyCell(newRow, newCol)) {
;; 					return make_pair(newRow, newCol);
;; 				}
;; 			}
;; 		}
;; 	}

;; 	// If no suitable position is found, return {-1, -1} to indicate no move is available
;; 	return make_pair(-1, -1);
;; }


(let* ((my-2d-board
      '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 "W" "W" "B" "B" 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
        ))
  (print-2d-board my-2d-board)
  ;;(print (get-board-value my-2d-board 2 0))
  (print (defend-capture-position my-2d-board "B"))


  (print (empty-cell-p my-2d-board 4 0))
)
