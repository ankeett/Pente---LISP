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




(let* ((my-2d-board
      '((0 0 0 0 0 0 0 "W" "W" "B" "B" 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        ("B" 0 "B" 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        ("B" "B" 0 "B" 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        ( 0 "W" 0 0 "B" 0 0  0 "W" 0 0 0 0 0 0 0 0 0 0)
        (0 "W" 0 0 0 0 0 0 "B" "B" 0 0 0 0 0 0 0 0 0)
        (0 "B" 0 0 0 0 0 0 "B" 0 "B" 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 "W" 0 0 "W" 0 0 0 0 0 0 0)
        (0 0 "W" "W" "W" 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 "W" 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 "W" 0 0 0 0 0 0 0 0 0 0)
        ("B" 0 0 0 0 0 0 0 0  "W" 0  0 0 0 0 0 0 0 0)
        ("B" 0 0 0 0 0 0 0 0 0 "W" 0 0 0 0 0 0 0 0)
        ("B" 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        ("B" 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
        ))
  (print-2d-board my-2d-board)
  ;;(print (get-board-value my-2d-board 2 0))
  (print (defend-capture-position my-2d-board "W"))


  (print (empty-cell-p my-2d-board 4 0))
)


;; pair<int, int> Strategy::findWinningMove(Board B, int playerSymbol) {
;;     // Iterate through the entire game board
;;     for (int row = 1; row <= 19; row++) {
;;         for (int col = 0; col < 19; col++) {
;;             // Check if the current cell is empty
;;             if (B.isEmptyCell(row,col)) {
;;                 // Simulate placing the player's stone in the current empty cell
;;                 B.setBoard(row, col, playerSymbol);

;;                 // Check for five-in-a-row with the simulated stone
;;                 if (B.checkFive(row,col,playerSymbol)) {
;;                     // If winning move is found, return the position
;;                     B.setBoard(row, col, 0); 
;;                     return make_pair(row, col);
;;                 }

;;                 // Undo the simulation by resetting the cell to empty
;;                 B.setBoard(row, col, 0);
;;             }
;;             else {
;; 				continue;
;;             }
;;         }
;;     }

;;     // No immediate winning move found
;;     return make_pair(-1, -1);
;; }

;; (defun find-winning-move (board player-symbol)

;;   (cond
;;     ((equal (get-board-value board row col) 0)
;;       ()
;;     )
;;     (t

    
;;     )
;;   )

;; )

;; (defun check-cell (board row col playerSymbol)


;;)