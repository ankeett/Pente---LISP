(load "Board.lsp")
(load "ComputerPlayer.lsp")

;recursively check capture here
(defun recursively-check-capture (board row col playerColor playerCaptures)
  (let* ((captured-board (check-capture board row col playerColor)))
    (cond
      (captured-board
       (recursively-check-capture captured-board row col playerColor (+ 1 playerCaptures)))
      (t
       (list board playerCaptures)))))


;;(recursively-check-capture new-board row col playerColor playerCaptures opponentColor opponentType playerType opponentCaptures)



(let* ((my-2d-board
      '((O O O O O O O O O O O O O O O O O O O)
        (O O O O O O O O O O O O O O O O O O O)
        (O O O O O O O O O O O O O O O O O O O)
        (O O O O O O O O O O O O O O O O O O O)
        (O O O O O O O O O O O O O O O O O O O)
        (O O O O O O O O O O O O O O O O O O O)
        (O O O O O O O O O O O O O O O O O O O)
        (O O O O O O O O O O O O O O O O O O O)
        (O O O O O O O O O O O O O O O O O O O)
        (O O O O O O O O O O O O O O O O O O O)
        (O O O O O O O O O O O O O O O O O O O)
        (O O O O O O O O O O O O O O O O O O O)
        (O O O O O O O O O O O O O O O O O O O)
        (O O O O O O O O O O O O O O O O O O O)
        (O O O O O O O O O O O O O O O O O O O)
        (O O O O O O O O O O O O O O O O O O O)
        (O O O O O O O O O O O O O O O O O O O)
        (O O O O O O O O O O O O O O O O O O O)
        (O O O O O O O O O O O O O O O O O O O))
        ))
  (print-2d-board my-2d-board)
  ;;(print (get-board-value my-2d-board 2 O))
  (cond ((print (computerMove my-2d-board 'B))
         )
        (t (format t "No four in a row.~%")))

        ;; (cond
        ;;     ((let* ((captured-board (check-capture my-2d-board 2 1 'B))
        ;;           )
        ;;       (cond
        ;;         (captured-board
        ;;         (print-2d-board captured-board)
        ;;         (format t "Capture~%"))
        ;;         (t
        ;;         (print-2d-board my-2d-board)
        ;;         (format t "Not Capture~%")
        ;;         )))
        ;;     ))
       ;;(print (recursively-check-capture my-2d-board 3 1 'B 0))
)



