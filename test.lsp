(load "Board.lsp")

(defun recursively-check-capture (board row col playerColor playerCaptures)
  (let* ((captured-board (check-capture board row col playerColor)))
    (cond
      (captured-board
       (recursively-check-capture captured-board row col playerColor (+ 1 playerCaptures)))
      (t
       (list board playerCaptures)))))

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



;randomize row and col
;convert into move
;check if the move is three intersection away form "J10"
;if yes, return, if not randomize again

(defun is-empty-cell (board row col)
  (string= (get-board-value board row col) 'O)
  )

(defun random-move()
  (list (random 19) (random 19))
)


(defun second-move (board)
  (let* ((row (random 19))
        (col (random 19)))
    (let ((move (convert-to-move row col)))
      (cond
        ((and (is-three-points-away "J10" move) (empty-cell-p board row col))
         (list row col move))
        (t
         (second-move board))))))

(defun empty-cell-p (board row col)
  (string= (get-board-value board row col) 'O))

(defun convert-to-move (row col)
  (let* ((move (concatenate 'string (string (code-char (+ 65 col))) (write-to-string (- 19 row)))))
    move))

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




(defun check-stones(board)
  (defun check-next-cell (current-board current-row current-col count)
    (cond
      ((>= current-row 18)
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




;; (let* ((my-2d-board
;;   '((O O O O O O O O O O O O O O B B B B O)
;;     (O O O O O O O O O O O O O O O O O O O)
;;     (O O O O O O O O O O O O O O O O O O O)
;;     (O O O O O O O O O O O O O O O O O O O)
;;     (O O O O O O O O O O O O O O O O O O O)
;;     (O O O O O O O O O O O O O O O O O O O)
;;     (O O O O O O O O O O O O O O O O O O O)
;;     (O O O O O O O O O O O O O O O O O O O)
;;     (O O O O O O O O O O O O O O O O O O O)
;;     (O O O O O O O W B B B O O O O O O O O)
;;     (O O O O O O O B B O O O O O O O O O O)
;;     (O O O O O O O B O B O O O O O O O O O)
;;     (O O O O O O O B O O B O O O O O O O O)
;;     (O O O O O O O O O O O B O O O O O O O)
;;     (O O O O O O O O O O O O B B O O O O O)
;;     (O O O O O O O O O O O O O O O O O O O)
;;     (O O O O O O O O O O O O O O O O O O O)
;;     (O O O O O O O O O O O O O O O O O O O)
;;     (O O O O O O O O O O O O O O O O O O O))
;;     ))
;;     (print-2d-board my-2d-board)
;;     (print (count-four my-2d-board 'B))


;; )


