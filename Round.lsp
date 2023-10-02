(defun welcome()
    (format t "Welcome to Pente!~%")
    (format t "You will be playing against the computer.~%")
    (format t "Choose:~%")
    (format t "1. Start a new game~%")
    (format t "2. Load a saved game~%"))


(defun get-welcome-input ()
  (format t "Enter something: ")
  (finish-output)
  (let ((user-input (read-line)))
    (format t "You entered: ~a~%" user-input)

    (cond ((equal user-input "1") (start-game))
          ((equal user-input "2") (load-game))
          (t (format t "Invalid input.~%")
             (get-welcome-input)))    
  )
)


(defun start-game()
    (format t "Starting a new game~%")
    (let ((toss (toss-coin)))
        (cond ((= toss 1) (format t "You go first!~%"))
              ((= toss 2) (format t "Computer goes first!~%"))
        )) 
)


(defun load-game()
    (print "Loading a saved game")
)

(defun toss-coin()
    (format t "Tossing a coin~%")
    (+ 1 (random 2))
)

