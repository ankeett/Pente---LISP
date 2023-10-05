(defun computerMove()
  (let* ((row (random 19))
         (col (random 19))
         (adjusted-row (- 19 row)) ;suspected error
         (move (concatenate 'string (string (code-char (+ 65 col))) (write-to-string adjusted-row)))
         )
    (list row col move)))


;;IN PROGRESS
;; (defun find-winning-move (board player-symbol)
;;   (labels ((check-cell (row col)
;;              (cond
;;                ((empty-cell-p board row col)
;;                 (let* ((new-board (set-board board row col player-symbol)))
;;                   (cond
;;                     ((check-five new-board row col player-symbol)
;;                      (values row col))
;;                     (t
;;                      (check-next-cell new-board row col))))
;;                 (values nil nil))
;;                (t
;;                 (check-next-cell board row col))))

;;            (check-next-cell (current-board current-row current-col)
;;              (cond
;;                ((>= current-row 19)
;;                 (values -1 -1))
;;                ((>= current-col 19)
;;                 (check-cell current-row (1+ current-row) 0))
;;                (t
;;                 (check-cell current-row (1+ current-col)))))))
;;     (check-cell board 0 0))

;; (defun empty-cell-p (board row col)
;;   (eq (aref board row col) 0))

;;   (defvar *my-2d-board*
;;   '((B B B B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;     (B 0 0 0 B B B B B 0 0 0 0 0 0 0 0 0 0)
;;     (B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;     (B W 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;     (0 B 0 0 0 0 0 0 W 0 0 0 0 0 0 0 0 0 0)
;;     (0 B 0 0 0 0 0 0 B B 0 0 0 0 0 0 0 0 0)
;;     (0 W 0 0 0 0 0 0 B 0 B 0 0 0 0 0 0 0 0)
;;     (0 W 0 0 0 0 0 0 W 0 0 W 0 0 0 0 0 0 0)
;;     (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;     (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;     (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;     (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;     (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;     (B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;     (B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;     (B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;     (B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;     (B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;     (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
;; (let ((result (find-winning-move *my-2d-board* 'B)))
;;   (format t "Winning move: ~A~%" result))