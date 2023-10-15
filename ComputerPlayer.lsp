(load "Board.lsp")
(load "test.lsp")

(defun computerMove(board player-symbol moveCount)
  (cond
  
    ((equal moveCount 1)
      (format t "Reason: First stone always at the center of the board~%")
      (list 9 9 "J10")
    )

    (
      (equal moveCount 3)
      (format t "Reason: 3 intersection away from the center of the board~%")
      (second-move board)
    ) 
    (t
      (let* ((evaluation-result (evaluate-all-cases board player-symbol))
            (random-value (random-move))
            (row (cond ((equal evaluation-result '(nil nil)) (first random-value))
                        (t (first evaluation-result))))
            (col (cond ((equal evaluation-result '(nil nil)) (second random-value))
                        (t (second evaluation-result))))
            (adjusted-row (- 19 row))
            (move (concatenate 'string (string (code-char (+ 65 col))) (write-to-string adjusted-row))))
        (list row col move)))
  )
  )

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

;; (defun find-capture-position (board player-symbol)
;;   (defun check-next-cell (current-board current-row current-col)
;;     (cond
;;       ((>= current-row 18)
;;        (list nil nil)) ; Return nil values if no winning move is found
;;       ((>= current-col 18)
;;        (check-cell current-board (+ 1 current-row) 0))
;;       (t
;;        (check-cell current-board current-row (+ 1 current-col)))))

;;   (defun check-cell (current-board row col)
;;     (cond
;;       ((empty-cell-p current-board row col)
;;        (let* ((new-board (set-board-value current-board row col player-symbol)))
;;          (cond
;;             ((let* ((captured-board (check-capture new-board row col player-symbol)))
                  
;;               (cond
;;                 (captured-board
;;                   (list row col))
;;                 (t
;;                   (check-next-cell current-board row col)
;;                 )
;;               ))
;;             ))
            
;;             ))
;;       (t
;;        (check-next-cell current-board row col))))

;;   (check-cell board 0 0)

;; )

(defun find-capture-position (board player-symbol)

  (defun check-next-cell (current-board current-row current-col maxCaptures maxRow maxCol)
    (cond
      ((>= current-row 18)
       (list maxRow maxCol))
      ((>= current-col 18)
       (check-cell current-board (+ 1 current-row) 0 maxCaptures maxRow maxCol))
      (t
       (check-cell current-board current-row (+ 1 current-col) maxCaptures maxRow maxCol))))

  (defun check-cell (current-board row col maxCaptures maxRow maxCol)
    (cond
      ((empty-cell-p current-board row col)
       (let* ((new-board (set-board-value current-board row col player-symbol)))
         (cond
            ((let* ((captured-board (recursively-check-capture new-board row col player-symbol 0)))
                  
              (cond
                ((first captured-board)
                  (cond (
                     (> (second captured-board) maxCaptures)
                     (check-next-cell current-board row col (second captured-board) row col)
                  )
                  (t
                    (check-next-cell current-board row col maxCaptures maxRow maxCol)
                  )
                  
                  )
                )
                (t
                  (check-next-cell current-board row col maxCaptures maxRow maxCol)
                )
              ))
            ))
            
            ))
      (t
       (check-next-cell current-board row col maxCaptures maxRow maxCol))))

  (check-cell board 0 0 0 nil nil)

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

;; (defun evaluate-all-cases (board player-symbol)
;;   (labels ((run-functions (functions)
;;              (cond
;;                ((null functions) '(nil nil)) ; Base case: No more functions to evaluate, return (nil nil)
;;                ((equal (car functions) '(nil nil)) (run-functions (cdr functions))) ; If the result is null, move on to the next function
;;                (t (car functions)))) ; Return the non-nil result
;;            )
;;     (run-functions
;;      (list (find-winning-move board player-symbol)
;;            (defend-winning-move board player-symbol)
;;            (make-four-move board player-symbol)
;;            (defend-four-move board player-symbol)
;;            (find-capture-position board player-symbol)
;;            (defend-capture-position board player-symbol)
;;            (make-consecutive-move board player-symbol 3)
;;            (make-consecutive-move board player-symbol 2)))))

(defun evaluate-all-cases (board player-symbol)
  ;; Define the evaluation order
  (let* ((winning-move (find-winning-move board player-symbol))
         (defending-win (defend-winning-move board player-symbol))
         (make-four (make-four-move board player-symbol))
         (defending-four (defend-four-move board player-symbol))
         (capturing-opponent (find-capture-position board player-symbol))
         (defending-capture (defend-capture-position board player-symbol))
         (max-consecutive-pos3 (make-consecutive-move board player-symbol 3))
         (max-consecutive-pos2 (make-consecutive-move board player-symbol 2))
         (random-move (random-move)))
    ;; Check each condition in order
    (cond
      ((not (equal (car winning-move) nil))
       (format t "Reason: Winning Move~%")
       winning-move)
      ((not (equal (car defending-win) nil))
       (format t "Reason: Defending Win~%")
       defending-win)
      ((not (equal (car defending-four) nil))
       (format t "Reason: Defending Four~%")
       defending-four)
      ((not (equal (car capturing-opponent) nil))
       (format t "Reason: Capturing Opponent~%")
       capturing-opponent)
      ((not (equal (car defending-capture) nil))
       (format t "Reason: Defending Capture~%")
       defending-capture)
      ((not (equal (car make-four) nil))
       (format t "Reason: Making Four~%")
       make-four)
      ((not (equal (car max-consecutive-pos3) nil))
       (format t "Reason: Max Consecutive of 3~%")
       max-consecutive-pos3)
      ((not (equal (car max-consecutive-pos2) nil))
       (format t "Reason: Max Consecutive of 2~%")
       max-consecutive-pos2)
      (t
       (format t "Reason: Random Move~%")
       random-move))))
