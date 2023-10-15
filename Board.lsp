(load "HumanPlayer.lsp")

(defun make-2d-board (rows cols)
  (labels ((make-row (cols)
             (cond
               ((zerop cols) '())
               (t (cons 'O (make-row (- cols 1)))))))
    (cond
      ((zerop rows) '())
      (t (cons (make-row cols)
               (make-2d-board (- rows 1) cols))))))

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
      (let ((cell (first row)))
        (cond
          ((string= cell 'O) (format t "~3a  " #\.))
          (t (format t "~3a  " cell)))
      )
      (print-board-row-contents (rest row) row-label))))


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
  (cond
    ((and (<= 0 row 18) (<= 0 col 18))
     (let ((value (get-row-value board row col)))
       value)) ; Return the value
     (t
      (format t "Indices out of bounds.(from get-board-value)~%")))
)

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
      ((>= vertical-up-sum (1+ num-consecutive))
       t)
      ((>= horizontal-sum (1+ num-consecutive))
       t)
      ((>= diagonal-left-up-sum (1+ num-consecutive))
       t)
      ((>= diagonal-right-up-sum (1+ num-consecutive))
       t)
      (t
       nil))))

;;check five function
(defun check-five (board row col symbol)
  (check-consecutive board row col symbol 5)
)

(defun check-four (board row col symbol)
  (check-consecutive board row col symbol 4)
)

;; (defun check-five (board row col symbol)
;;   (let* ((vertical-up-sum
;;           (+ (check-direction board row col symbol 0 -1 5)
;;              (check-direction board row col symbol 0 1 5)))
;;          (horizontal-sum
;;           (+ (check-direction board row col symbol -1 0 5)
;;              (check-direction board row col symbol 1 0 5)))
;;          (diagonal-left-up-sum
;;           (+ (check-direction board row col symbol -1 -1 5)
;;              (check-direction board row col symbol 1 1 5)))
;;          (diagonal-right-up-sum
;;           (+ (check-direction board row col symbol -1 1 5)
;;              (check-direction board row col symbol 1 -1 5))))
    

;;     (cond
;;       ((>= vertical-up-sum 6)

;;        t)
;;       ((>= horizontal-sum 6)
       
;;        t)
;;       ((>= diagonal-left-up-sum 6)
       
;;        t)
;;       ((>= diagonal-right-up-sum 6)
       
;;        t)
;;       (t 
;;       nil
;;       ))))


(defun check-direction (board row col symbol delta-row delta-col count)
  (labels ((check-direction-rec (r c consecutive-stones)
             (cond
               ((>= consecutive-stones count) consecutive-stones)
               ((or (< r 0) (>= r 19) (< c 0) (>= c 19) (not(equal (get-board-value board r c) symbol)))
                consecutive-stones)
               (t (check-direction-rec (+ r delta-row) (+ c delta-col) (+ consecutive-stones 1))))))
    (check-direction-rec row  col 0)))

;;check-four
;; (defun check-four (board row col symbol)
;;   (let* ((vertical-up-sum
;;           (+ (check-direction board row col symbol 0 -1 4)
;;              (check-direction board row col symbol 0 1 4)))
;;          (horizontal-sum
;;           (+ (check-direction board row col symbol -1 0 4)
;;              (check-direction board row col symbol 1 0 4)))
;;          (diagonal-left-up-sum
;;           (+ (check-direction board row col symbol -1 -1 4)
;;              (check-direction board row col symbol 1 1 4)))
;;          (diagonal-right-up-sum
;;           (+ (check-direction board row col symbol -1 1 4)
;;              (check-direction board row col symbol 1 -1 4))))
    

;;     (cond
;;       ((>= vertical-up-sum 5)

;;        t)
;;       ((>= horizontal-sum 5)
       
;;        t)
;;       ((>= diagonal-left-up-sum 5)
       
;;        t)
;;       ((>= diagonal-right-up-sum 5)
       
;;        t)
;;       (t 
;;       nil
;;       ))))

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

(defun capture-pair (board x y dx dy oColor eColor count)
  (cond
    ((check-capture-direction board (+ x dx) (+ y dy) dx dy oColor eColor count)
      (remove-captured board (+ x dx) (+ y dy) dx dy 2)   
    )
  (t
    nil)
  )
)

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