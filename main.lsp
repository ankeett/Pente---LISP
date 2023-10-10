(load "Round.lsp")
(load "Board.lsp")
(load "HumanPlayer.lsp")
(load "ComputerPlayer.lsp")


(defun play-game(board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures)
  (print-2d-board board)
  (format t "~a's turn.~%" playerType)

  (cond
    (
      ;if someone wins
      ;(check-five board row col playerColor)
      ;(format t "~a wins!~%" playerType)
      ()
    )
    (
      ;if the board is full
      ()
      
    )
    
    (
      t 
      
      (cond
        (
          ;if computer's turn
          (equal playerType "Computer")
          (let* ((computer-move (computerMove))
                 (row (first computer-move))
                 (col (second computer-move))
                 (new-board (place-stone board row col playerColor)))
            (captures "Computer" playerCaptures "Human"  opponentCaptures)
            (format t "Computer move: ~a~%" (third computer-move))
              (cond
                ((check-five new-board row col playerColor)
                (format t "Five in a row.~%")
                ;;return the board and everything
                )
                ;;(calculate-score playerCaptures opponentCaptures "Computer"))
                (t
                    (cond
                        ((let* ((captured-board (check-capture new-board row col playerColor))
                              (next-playerCaptures (+ 1 playerCaptures)))
                          (cond
                            (captured-board
                            (play-game captured-board opponentColor opponentType playerColor playerType opponentCaptures next-playerCaptures))
                            (t
                            (play-game new-board opponentColor opponentType playerColor playerType opponentCaptures playerCaptures))))
                        ))

                    
                    ))
              ;;(play-game new-board opponentColor opponentType playerColor playerType opponentCaptures playerCaptures)
          ))
      (
        t 
        (equal playerType "Human")
        ;get user move
        (let* ((user-move (getUserMove board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures))
              (row (first user-move))
              (col (second user-move)))
          (let* ((new-board (place-stone board row col playerColor)))
              (captures "Human" playerCaptures "Computer"  opponentCaptures)
            (cond
                ((check-five new-board row col playerColor)
                (format t "Five in a row.~%")
                ;;return the board and everything here
                (list new-board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures 5))
                ;send 5 for 5 in a row to add to score
                ;;(calculate-score playerCaptures opponentCaptures "Human"))
                (t
                    (format t "Row: ~d~%" row)
                    (format t "Col: ~d~%" col)

                    (cond
                        ((let* ((captured-board (check-capture new-board row col playerColor))
                              (next-playerCaptures (+ 1 playerCaptures)
                              ))
                              
                          (cond
                            (captured-board
                              (play-game captured-board opponentColor opponentType playerColor playerType opponentCaptures next-playerCaptures))
                            (t
                              (play-game new-board opponentColor opponentType playerColor playerType opponentCaptures playerCaptures))))
                        ))
                    ))
          )
        )
      )

    )

  )
))

;;;recursively check capture here
;; (defun recursively-check-capture (board row col playerColor playerCaptures opponentColor opponentType playerType opponentCaptures)
;;   (let* ((captured-board (check-capture board row col playerColor))
;;          (next-playerCaptures (+ 1 playerCaptures)))
;;     (print captured-board)
;;     (if captured-board
;;         (recursively-check-capture captured-board row col playerColor next-playerCaptures opponentColor opponentType playerType opponentCaptures)
;;         (play-game board opponentColor opponentType playerColor playerType opponentCaptures next-playerCaptures))))

;; (recursively-check-capture new-board row col playerColor playerCaptures opponentColor opponentType playerType opponentCaptures)



(defun captures(playerType playerCapture opponentType opponentCapture)
  (format t "--------------------------------~%")
  (format t "~a Captures: ~a~%" playerType playerCapture)
  (format t "~a Captures: ~a~%" opponentType opponentCapture)
  (format t "--------------------------------~%"))


;;(list new-board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures)
(defun calculate-score (result)
  (let* ((board (first result))
         (winnerColor (second result))
         (winnerType (third result))
         (opponentColor (fourth result))
         (opponentType (fifth result))
         (winnerCapture (sixth result))
         (opponentCapture (seventh result))
         (isFiveinarow (eighth result )))



    (cond 
      ((equal winnerType "Human")
        (format t "Human Scores: ~a~%" (+ winnerCapture isFiveinarow))
        (format t "Computer Scores: ~a~%" opponentCapture)
        (list (+ winnerCapture isFiveinarow) opponentCapture))
      (t
        (format t "Human Scores: ~a~%" opponentCapture)
        (format t "Computer Scores: ~a~%" (+ winnerCapture isFiveinarow))
        (list opponentCapture (+ winnerCapture isFiveinarow))))
    ))


(defun ask-for-next-game(winner loser)
  (format t "Do you want to play one more round?(y/n) ~%")
  (finish-output)
  (let* ((user-input (read-line)))
    (format t "You entered: ~a~%" user-input)
    (cond 
      ((equal user-input "y")
       (start-game))
      ((equal user-input "n")
       (quit-game))
      (t (format t "Invalid input.~%"))))
)


(defun quit-game()
  (format t "Do you want to Serialize (y/n)")

)

;; (defun start-round(firstPlayerType secondPlayerType )
;;   (let* ((my-2d-board (make-2d-board 19 19)))
;;     (play-game my-2d-board "W" firstPlayerType "B" secondPlayerType 0 0)))

(defun start-round (firstPlayerType secondPlayerType)
  (let* ((my-2d-board (make-2d-board 19 19))
         (game-result (play-game my-2d-board "W" firstPlayerType "B" secondPlayerType 0 0)))
    (values game-result)))


;; (let* ((playerList (get-welcome-input)))
;;   (start-round (first playerList) (second playerList))
;; )



(defun tournament(humanScore computerScore)

  ;;checks human and computer score
  ;;if equal toss the coin and determine first and second playerType


  ;;start-round(firstPlayerType, secondPlayerType)
      ;;play-round()
      ;;play-round should return everything  like board winnerColor winnerType opponentColor opponentType winnerCapture opponentCapture
      ;;this again returned by start-round
  
  ;;calls calculateScore
    ;;takes board for 4 in a row
    ;;capture winnerType for 5 in a row
    ;;winnerCapture and opponentCapture with winnerType and opponentType to calculate captureScore

  ;;calculate score returns humanScore and computerScore
  ;;recursion tournament again


  ;; (cond
  ;;   ((> humanScore computerScore)
  ;;   (let* ((result (start-round "Human" "Computer")))
  ;;     (let* ((scores (calculate-score result)))
  ;;           (format t "-----Tournament Scores-----~%")
  ;;       (format t "Human Scores: ~a~%" (+ (first scores) humanScore))
  ;;       (format t "Computer Scores: ~a~%" (+ (second scores) computerScore))
  ;;       (tournament (+ (first scores) humanScore)  (+ (second scores) computerScore))))
  ;;   )
    
  ;;   ((< humanScore computerScore)
  ;;   (let* ((result (start-round "Computer" "Human")))
  ;;     (let* ((scores (calculate-score result)))
  ;;           (format t "-----Tournament Scores-----~%")
  ;;       (format t "Human Scores: ~a~%" (+ (first scores) humanScore))
  ;;       (format t "Computer Scores: ~a~%" (+ (second scores) computerScore))
  ;;       (tournament (+ (first scores) humanScore)  (+ (second scores) computerScore)))
  ;;   ))
    
  ;;   (t
  ;;   (let* ((playerList (start-game))
  ;;           (result (start-round (first playerList) (second playerList)))
  ;;           (scores (calculate-score result)))
  ;;       (format t "-----Tournament Scores-----~%")
  ;;     (format t "Human Scores: ~a~%" (+ (first scores) humanScore))
  ;;       (format t "Computer Scores: ~a~%" (+ (second scores) computerScore))
  ;;       (tournament (+ (first scores) humanScore)  (+ (second scores) computerScore)))
  ;;     )
  ;; )


 (cond
  ((> humanScore computerScore)
   (let* ((result (start-round "Human" "Computer")))
     (let* ((scores (calculate-score result)))
       (format t "-----Tournament Scores-----~%")
       (format t "Human Scores: ~a~%" (+ (first scores) humanScore))
       (format t "Computer Scores: ~a~%" (+ (second scores) computerScore))
       (format t "Continue the tournament? (yes/no): ~%")
       (let ((response (read-line)))
         (cond
           ((string= response "yes")
            (tournament (+ (first scores) humanScore) (+ (second scores) computerScore)))
           (t
            (format t "Tournament ended.~%")))))
   )
  )

  ((< humanScore computerScore)
   (let* ((result (start-round "Computer" "Human")))
     (let* ((scores (calculate-score result)))
       (format t "-----Tournament Scores-----~%")
       (format t "Human Scores: ~a~%" (+ (first scores) humanScore))
       (format t "Computer Scores: ~a~%" (+ (second scores) computerScore))
       (format t "Continue the tournament? (yes/no): ~%")
       (let ((response (read-line)))
         (cond
           ((string= response "yes")
            (tournament (+ (first scores) humanScore) (+ (second scores) computerScore)))
           (t
            (format t "Tournament ended.~%")))))
   )
  )

  (t
   (let* ((playerList (start-game))
          (result (start-round (first playerList) (second playerList)))
          (scores (calculate-score result)))
     (format t "-----Tournament Scores-----~%")
     (format t "Human Scores: ~a~%" (+ (first scores) humanScore))
     (format t "Computer Scores: ~a~%" (+ (second scores) computerScore))
     (format t "Continue the tournament? (yes/no): ~%")
     (let ((response (read-line)))
       (cond
         ((string= response "yes")
          (tournament (+ (first scores) humanScore) (+ (second scores) computerScore)))
         (t
          (format t "Tournament ended.~%")))))
  )
  )
)

;;(tournament 0 0)

;;load game state
;;call play-game
;; call round score and call tournament with the updated score
(let* ((response (welcome)))
  (cond
    ((not(equal response 1))
      ;;(load-game)
        ;;returns
      ;;(list board playerColor playerType opponentColor opponentType playerCapture opponentCapture playerScore opponentScore)
      ;;play-game(board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures)
      ;;returns
      ;;(list new-board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures 5))

    ;;   (scores (calculate-score result)))
    ;;  (format t "-----Tournament Scores-----~%")
    ;;  (format t "Human Scores: ~a~%" (+ (first scores) humanScore))
    ;;  (format t "Computer Scores: ~a~%" (+ (second scores) computerScore))
    ;;  (format t "Continue the tournament? (yes/no): ~%")
    ;;  (let ((response (read-line)))
    ;;    (cond
    ;;      ((string= response "yes")
    ;;       (tournament (+ (first scores) humanScore) (+ (second scores) computerScore)))
    ;;      (t
    ;;       (format t "Tournament ended.~%")))))
      (let ((game-state (load-game)))
          (format t "Loaded game state: ~A~%" game-state)
          (let ((result (play-game (first game-state) (second game-state) (third game-state) (fourth game-state) (fifth game-state) (sixth game-state) (seventh game-state))))
            (format t "Game result: ~A~%" result)
            (let* ((scores (calculate-score result)))
              (format t "-----Tournament Scores-----~%")
              (format t "Human Scores: ~a~%" (+ (first scores) (eighth game-state)))
              (format t "Computer Scores: ~a~%" (+ (second scores) (ninth game-state)))
              (format t "Continue the tournament? (yes/no): ~%")
              (let ((response (read-line)))
                (cond
                  ((string= response "yes")
                   (tournament (+ (first scores) (eighth game-state)) (+ (second scores) (ninth game-state))))
                  (t
                   (format t "Tournament ended.~%")))))
          ))
    )
    (t
      (tournament 0 0))))


