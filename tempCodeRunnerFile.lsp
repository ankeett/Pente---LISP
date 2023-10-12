;recursively check capture here
;; (defun recursively-check-capture (board row col playerColor playerCaptures opponentColor opponentType playerType opponentCaptures)
;;   (let* ((captured-board (check-capture board row col playerColor))
;;          (next-playerCaptures (+ 1 playerCaptures)))
;;     (print captured-board)
;;     (if captured-board
;;         (recursively-check-capture captured-board row col playerColor next-playerCaptures opponentColor opponentType playerType opponentCaptures)
;;         (play-game board opponentColor opponentType playerColor playerType opponentCaptures next-playerCaptures))))

;; (recursively-check-capture new-board row col playerColor playerCaptures opponentColor opponentType playerType opponentCaptures)
