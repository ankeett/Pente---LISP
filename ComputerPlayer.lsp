(load "Board.lsp")

(defun computerMove(board player-symbol)
  (let* ((evaluation-result (evaluate-all-cases board player-symbol))
         (random-value (random-move))
         (row (cond ((equal evaluation-result '(nil nil)) (first random-value))
                    (t (first evaluation-result))))
         (col (cond ((equal evaluation-result '(nil nil)) (second random-value))
                    (t (second evaluation-result))))
         (adjusted-row (- 19 row))
         (move (concatenate 'string (string (code-char (+ 65 col))) (write-to-string adjusted-row))))
    (list row col move)))

(defun random-move()
  (list (random 19) (random 19))
)




;;IN PROGRESS
(defun empty-cell-p (board row col)
  (string= (get-board-value board row col) 'O))


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
      
        ((string= player-symbol 'W)
          'B
        )
         (t
            'W
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
      
        ((string= player-symbol 'W)
          'B
        )
         (t
            'W
         )
       )

  ))
    (find-capture-position board opponent-symbol)
  )

)

(defun make-four-move (board player-symbol)
  (defun check-next-cell (current-board current-row current-col)
    (cond
      ((>= current-row 18)
       (list nil nil)) 
      ((>= current-col 18)
       (check-cell current-board (+ 1 current-row) 0))
      (t
       (check-cell current-board current-row (+ 1 current-col)))))

  (defun check-cell (current-board row col)
    (cond
      ((empty-cell-p current-board row col)
       (let* ((new-board (set-board-value current-board row col player-symbol)))
         (cond
           ((check-four new-board row col player-symbol)
            (list row col)) ; Return the row and col of the winning move
           (t
            (check-next-cell current-board row col))
            )))
      (t
       (check-next-cell current-board row col))))

  (check-cell board 0 0)
)


(defun defend-four-move(board player-symbol)
  (let* ((opponent-symbol (cond 
      
        ((string= player-symbol 'W)
          'B
        )
         (t
            'W
         )
       )

  ))
    (make-four-move board opponent-symbol)
  )
)

(defun make-consecutive-move (board player-symbol consecutive)
  (defun check-next-cell (current-board current-row current-col)
    (cond
      ((>= current-row 18)
       (list nil nil)) 
      ((>= current-col 18)
       (check-cell current-board (+ 1 current-row) 0))
      (t
       (check-cell current-board current-row (+ 1 current-col)))))

  (defun check-cell (current-board row col)
    (cond
      ((empty-cell-p current-board row col)
       (let* ((new-board (set-board-value current-board row col player-symbol)))
         (cond
           ((check-consecutive new-board row col player-symbol consecutive)
            (list row col)) ; Return the row and col for consecutive
           (t
            (check-next-cell current-board row col))
            )))
      (t
       (check-next-cell current-board row col))))

  (check-cell board 0 0)
)

(defun evaluate-all-cases (board player-symbol)
  (labels ((run-functions (functions)
             (cond
               ((null functions) '(nil nil)) ; Base case: No more functions to evaluate, return (nil nil)
               ((equal (car functions) '(nil nil)) (run-functions (cdr functions))) ; If the result is null, move on to the next function
               (t (car functions)))) ; Return the non-nil result
           )
    (run-functions
     (list (find-winning-move board player-symbol)
           (defend-winning-move board player-symbol)
           (make-four-move board player-symbol)
           (defend-four-move board player-symbol)
           (find-capture-position board player-symbol)
           (defend-capture-position board player-symbol)
           (make-consecutive-move board player-symbol 3)
           (make-consecutive-move board player-symbol 2)))))


;; Usage example:
;; To find the first cell with three consecutive symbols:
;; (make-consecutive-move board 'X 3)

;; To find the first cell with two consecutive symbols:
;; (make-consecutive-move board 'O 2)


;;for making consecutive


;; Usage example:
;; To check for three consecutive symbols:
;; (check-consecutive board 1 1 'X 3)

;; To check for two consecutive symbols:
;; (check-consecutive board 2 2 'O 2)




;; (let* ((my-2d-board
;;       '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;         (0 0 0 0 0 0 0 0 0 "W" "W" "B" "B" 0 0 0 0 0 0)
;;         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;;         ))
;;   (print-2d-board my-2d-board)
;;   ;;(print (get-board-value my-2d-board 2 0))
;;   (print (defend-capture-position my-2d-board "B"))


;;   (print (empty-cell-p my-2d-board 4 0))
;; )
