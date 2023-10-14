(defun welcome()
    (format t "Welcome to Pente!~%")
    (format t "You will be playing against the computer.~%")
    (format t "Choose:~%")
    (format t "1. Start a new game~%")
    (format t "2. Load a saved game~%")
    (get-welcome-input)
)

;; (defun get-welcome-input ()
;;   (format t "Enter something: ")
;;   (finish-output)
;;   (let ((user-input (read-line)))
;;     (format t "You entered: ~a~%" user-input)

;;     (cond ((equal user-input "1")
;;            (let ((result (start-game)))
;;              result))
;;           ((equal user-input "2") (load-game))
;;           (t (format t "Invalid input.~%")
;;              (get-welcome-input)))    
;;   )
;; )
(defun get-welcome-input ()
  (format t "Enter 1 to start a new game, 2 to load a game: ~%")
  (let ((user-input (read-line)))
    (cond
      ((equal user-input "1")
       "1")
      ((equal user-input "2")
       "2")
      (t
       (format t "Invalid input.~%")
       (get-welcome-input)))))



(defun toss-coin()
    (print (+ 1 (random 2)))
)

(defun start-game()
    (format t "Starting a new game~%")
    (format t "Tossing a coin to decide who plays first~%")
    (princ "Enter 1 for heads or 2 for tails: ")
    (terpri)
    (finish-output)
    (let* ((tossed (toss-coin)))
        (let* ((user-input (read-line)))
            (format t "You entered: ~a~%" user-input)

            (cond ((equal (parse-integer user-input) tossed)
                (princ "You won the toss.")
                (terpri)
                (princ "You will play: White")
                (terpri)
                (list 'Human 'Computer))
                
                (t
                    (princ "Computer won the toss.")
                    (terpri)
                    (princ "You will play: Black")
                    (terpri)
                    (list 'Computer 'Human)
                )    
            )
    
        )
    )
)

(defun load-game()
    (format t "Loading a saved game:~%")
    ;;(list board playerCapture playerScore opponentCapture opponentScore playerType color-string)

    ;;defun play-game(board playerColor playerType opponentColor opponentType playerCaptures opponentCaptures)

    (let* ((values (deserialize))
       (board (first values))
       (playerCapture (second values))
       (playerScore (third values))
       (opponentCapture (fourth values))
       (opponentScore (fifth values))
       (playerType (sixth values))
       (color-string (seventh values))
       (playerColor
        (cond ((string= color-string 'Black) 'B)
              (t 
                (string= color-string 'White) 'W
              )))
       (opponentColor
        (cond ((string= playerColor 'B) 'W)
              
              (t (string= playerColor 'W) 'B)))
        (opponentType
        (cond ((string= playerType 'Human) 'Computer)
              (t 
                (string= playerType 'Computer) 'Human
              )))
       )

        
        (list board playerColor playerType opponentColor opponentType playerCapture opponentCapture playerScore opponentScore)
    )
)



(defun deserialize()
  (format t "Enter filename to deserialize: ~%")
  (finish-output)
  
  (let* ((filename (read-line)))
    (with-open-file (stream filename
                        :direction :input
                        :if-does-not-exist :error)
      (let ((data (read stream)))
       
        ;; Process the deserialized data as needed
        ;; For example, you can access elements of the data list
        (let* ((board (first data))
                (playerCapture (second data))
                (playerScore (third data))
                (opponentCapture (fourth data))
                (opponentScore (fifth data))
                (playerType (sixth data))
                (color-string (seventh data))
              )
          ; Continue processing the data
        

        (cond 
            (
                (string= playerType "Human")
                (list board playerCapture playerScore opponentCapture opponentScore playerType color-string)
            )
            (t
                (list board opponentCapture opponentScore playerCapture playerScore  playerType color-string)
            )
        ))
        
      ))
    )
)
