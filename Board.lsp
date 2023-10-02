(defun make-2d-board (rows cols)
  (labels ((make-row (cols)
             (cond
               ((zerop cols) '())
               (t (cons 0 (make-row (1- cols)))))))
    (cond
      ((zerop rows) '())
      (t (cons (make-row cols)
               (make-2d-board (1- rows) cols))))))

(defun print-1d-row (row)
  (cond
    ((null row) nil)
    (t
      (format t "~a " (first row))
      (print-1d-row (rest row)))))

(defun print-2d-board (board)
  (let ((column-labels '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S)))
    (format t "~3a " '#\space) ; Print a space as the top-left corner
    (print-column-labels column-labels) ; Print the column labels
    (format t "~%")
    (print-board-rows board 19 1))) ; Print the board with row labels

(defun print-column-labels (cols)
  (cond
    ((null cols) nil)
    (t
      (format t "~3a  " (first cols))
      (print-column-labels (rest cols)))))

(defun print-board-rows (board row-label start-row)
  (cond
    ((null board) nil)
    (t
      (format t "~2a  " row-label)
      (print-board-row-contents (first board) start-row)
      (format t "~%")
      (print-board-rows (rest board) (1- row-label) (1+ start-row)))))

(defun print-board-row-contents (row row-label)
  (cond
    ((null row) nil)
    (t
      (format t "~3a  " (first row))
      (print-board-row-contents (rest row) row-label))))



(defun set-board-value (board row col new-value)
  (if (and (<= 0 row 18) (<= 0 col 18)) ; Check if the indices are within bounds
      (let ((board (update-board board row col new-value))) ; Create a new board with the updated value
       board) ; Return the new board
      (format t "Indices out of bounds.~%"))
)      
      
(defun update-board (board row col new-value)
    (cond
        ((null board) nil)
        ((= row 0) (cons (update-row (first board) col new-value) (rest board)))
        (t (cons (first board) (update-board (rest board) (1- row) col new-value))))
)
(defun update-row (row col new-value)
    (cond
        ((null row) nil)
        ((= col 0) (cons new-value (rest row)))
        (t (cons (first row) (update-row (rest row) (1- col) new-value)))))

(defun get-board-value (board row col)
    (if (and (<= 0 row 18) (<= 0 col 18)) ; Check if the indices are within bounds
        (let ((value (get-row-value board row col))) ; Get the value at the specified row and column
            value) ; Return the value
        (format t "Indices out of bounds.~%")))

(defun get-row-value (board row col)
    (cond
        ((null board) nil)
        ((= row 0) (get-column-value (first board) col))
        (t (get-row-value (rest board) (1- row) col))))

(defun get-column-value (row col)
    (cond
        ((null row) nil)
        ((= col 0) (first row))
        (t (get-column-value (rest row) (1- col)))))



;;check five function
(defun check-five (board row col symbol)
  (let* ((vertical-up-sum
          (+ (check-direction board row col symbol 0 -1 5)
             (check-direction board row col symbol 0 1 5)))
         (horizontal-sum
          (+ (check-direction board row col symbol -1 0 5)
             (check-direction board row col symbol 1 0 5)))
         (diagonal-left-up-sum
          (+ (check-direction board row col symbol -1 -1 5)
             (check-direction board row col symbol 1 1 5)))
         (diagonal-right-up-sum
          (+ (check-direction board row col symbol -1 1 5)
             (check-direction board row col symbol 1 -1 5))))
    

    (cond
      ((>= vertical-up-sum 6)
       
       t)
      ((>= horizontal-sum 6)
       
       t)
      ((>= diagonal-left-up-sum 6)
       
       t)
      ((>= diagonal-right-up-sum 6)
       
       t)
      (t nil))))


(defun check-direction (board row col symbol delta-row delta-col count)
  (labels ((check-direction-rec (r c consecutive-stones)
             (cond
               ((>= consecutive-stones count) consecutive-stones)
               ((or (< r 0) (>= r 18) (< c 0) (>= c 18) (/= (get-board-value board r c) symbol))
                consecutive-stones)
               (t (check-direction-rec (+ r delta-row) (+ c delta-col) (+ consecutive-stones 1))))))
    (check-direction-rec row  col 0)))

;;capture function
;;capture function
(defun check-capture(board row col symbol)
  (let* ((opponent-symbol (cond 
      
        ((= symbol 1)
          2
        )
        (t
            1
        )
      )
      
    ))
  (cond
    ( (or (capture-pair board row col 0 1 symbol opponent-symbol 2)
         (capture-pair board row col 0 -1 symbol opponent-symbol 2)
         (capture-pair board row col 1 0 symbol opponent-symbol 2)
         (capture-pair board row col -1 0 symbol opponent-symbol 2)
         (capture-pair board row col 1 1 symbol opponent-symbol 2)
        (capture-pair board row col -1 -1 symbol opponent-symbol 2)
         (capture-pair board row col -1 1 symbol opponent-symbol 2)
         (capture-pair board row col 1 -1 symbol opponent-symbol 2))
    t)
    (t nil)))

)

(defun capture-pair (board x y dx dy oColor eColor count)
    (check-capture-direction board (+ x dx) (+ y dy) dx dy oColor eColor count)
)


(defun check-capture-direction (board x y dx dy oColor eColor count)

  (cond 
    ((and (equal count 0) (equal (get-board-value board x y) oColor))
      t
    )
    ((not (equal (get-board-value board x y ) eColor))
      ()
    )
    (t
      (check-capture-direction board (+ x dx) (+ y dy) dx dy oColor eColor (- count 1))
    )
  )
)

(let* ((my-2d-board
      '((1 1 1 1 1 1 0 0 0 0 0 1 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 2 0 2 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 1 1 1 1 2 2 2 1 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
  (print-2d-board my-2d-board)
  ;; (print (get-board-value my-2d-board 0 0))
  (cond ((check-five my-2d-board 3 9 1)
         (format t "Five in a row.~%"))
        (t (format t "No five in a row.~%")))
  (cond ((check-capture my-2d-board 3 8 1)
         (format t "Captured.~%"))
        (t (format t "Not captured.~%")))
    
  ;; (print (check-for-capture my-2d-board 4 8 1 0 1 2 2))
             
  )
