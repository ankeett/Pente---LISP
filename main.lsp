(load "Round.lsp")
(load "Board.lsp")
(load "HumanPlayer.lsp")
(load "ComputerPlayer.lsp")

(welcome)
(get-welcome-input)




(defun play-game (board)
  (print-2d-board board)
  (format t "Player's turn.~%")
  (let* ((user-move (getUserMove))
         (row (first user-move))
         (col (second user-move)))
    
       (let* ((new-board (set-board-value board row col 1)))
         (print-2d-board new-board)
         (cond
           ((check-five new-board row col 1)
            (format t "Player wins!~%"))
           (t
            (format t "Computer's turn.~%")
            (multiple-value-bind (computer-row computer-col computer-move) (computerMove)
              (format t "Computer move: ~a ~a ~a~%" computer-row computer-col computer-move)
              (let* ((updated-board (set-board-value new-board computer-row computer-col 2)))
                (print-2d-board updated-board)
                (cond
                  ((check-five updated-board computer-row computer-col 2)
                   (format t "Computer wins!~%"))
                  (t
                   (unless (check-game-over updated-board)
                     (play-game updated-board)))))
              (unless (check-game-over new-board)
                (play-game new-board)))))
     
      )))


(defun check-game-over (board)
  ; Implement game-over condition logic here
  nil)

(defun start-game ()
  (let* ((my-2d-board (make-2d-board 19 19)))
    (play-game my-2d-board)))

(start-game)


