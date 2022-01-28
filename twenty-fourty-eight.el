;;; twenty-fourty-eight.el --- the game of twenty fourty eight using simple textual buffer
;;; Commentary:
;;; Nothing in particular
;;; Code:

(defconst 2048-size 4)
(defconst 2048-cell-size 15)
(defvar-local 2048-state [[0 0 0 0] [0 0 0 0]  [0 0 0 0] [0 0 0 0]])
(defvar-local 2048-game-over nil)
(defvar-local 2048-score 0)

(defvar twenty-fourty-eight-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left>") '2048-left)
    (define-key map (kbd "<right>") '2048-right)
    (define-key map (kbd "<up>") '2048-up)
    (define-key map (kbd "<down>") '2048-down)
    (define-key map (kbd "r") '2048-reset-game)
    map))


;; repeats a single vector for all rows
;; (make-vector 4 (make-vector 4 0))

(defun 2048-cell (i j)
  "Return a cell of the game state.

  I: index of the row.
  J: index of the column."
  (elt (elt 2048-state i) j))

(defun 2048-other-line ()
  "Create string for every other horizontal line except for the top line."
  (let ((str-acc '()))
    (dotimes (i 2048-size)
      (setq str-acc (cons (concat (make-string (- 2048-cell-size (if (= i 0) 2 1)) ?_) (string ?|)) str-acc)))
    (setq str-acc (cons "|" str-acc))
    (eval (cons 'concat str-acc))))

(defun 2048-val-to-string (val)
  "Convert a value into a string by adding space margin to left and right.

  VAL: integer in a cel of the state"
  (let (str-val margin)
    (setq str-val (if (= val 0) " " (number-to-string val))
          margin (/ (- 2048-cell-size (length str-val)) 2))
    (concat (make-string margin ?\s) str-val (make-string (- margin 1) ?\s))))

(defun 2048-move-empty-cells-row (dir)
  "Remove all the empty cells in the matrix along the direction of all rows.

   DIR: one of the four directions."
  (let ((change nil) i k)
    (dotimes (r 2048-size)
      (setq i (if (eq dir :right) (1- 2048-size) 0))
      (dotimes (j 2048-size)
        (setq k (if (eq dir :right) (- (1- 2048-size) j) j))
        (unless (= (2048-cell r k) 0)
          (unless (= i k)
            (aset (elt 2048-state r) i (2048-cell r k))
            (aset (elt 2048-state r) k 0)
            (setq change t))
          (setq i (if (eq dir :right) (1- i) (1+ i))))))
    change))


(defun 2048-move-empty-cells-col (dir)
  "Remove all empty cells in the matrix alogn the direction in all columns.

   DIR: either :up or :down."
  (let ((change nil) i r)
    (dotimes (c 2048-size)
      (setq i (if (eq dir :down) (1- 2048-size) 0))
      (dotimes (k 2048-size)
        (setq r (if (eq dir :down) (- (1- 2048-size) k) k))
        (unless (= (2048-cell r c) 0)
          (unless (= i r)
            (aset (elt 2048-state i) c (2048-cell r c))
            (aset (elt 2048-state r) c 0)
            (setq change t))
          (setq i (if (eq dir :down) (1- i) (1+ i))))))
    change))

(defun 2048-merge-identicals-row (dir)
  "Merge identical cells at the end of each direction.

  DIR: one of the four directions."
  (let ((change nil) k next)
    (dotimes (r 2048-size)
      (dotimes (j (1- 2048-size))
        (setq k (if (eq dir :right) (- (1- 2048-size) j) j)
              next (if (eq dir :right) (1- k) (1+ k)))
        (when (and (not (= (2048-cell r k) 0)) (= (2048-cell r k) (2048-cell r next)))
          (aset (elt 2048-state r) k (* (elt (elt 2048-state r) k) 2))
          (aset (elt 2048-state r) next 0)
          (setq 2048-score (+ 2048-score (2048-cell r k))
                change t))))
    change))

(defun 2048-merge-identicals-col (dir)
  "Merge identical cells at the end of each direction.

  DIR: one of the four directions."
  (let ((change nil) k next)
    (dotimes (c 2048-size)
      (dotimes (j (1- 2048-size))
        (setq k (if (eq dir :down) (- (1- 2048-size) j) j)
              next (if (eq dir :down) (1- k) (1+ k)))
        (when (and (not (= (2048-cell k c) 0)) (= (2048-cell k c) (2048-cell next c)))
          (aset (elt 2048-state k) c (* (2048-cell k c) 2))
          (aset (elt 2048-state next) c 0)
          (setq 2048-score (+ 2048-score (2048-cell k c))
                change t))))
    change))

(defun 2048-new-horizontal-dashed-line ()
  "Insert a dashed line on the next new line."
  (newline)
  (beginning-of-line)
  (dotimes (i (window-width)) (insert "-")))


(defun 2048-insert-score ()
  "Insert the score in the buffer."
  (newline)
  (move-to-column 20 t)
  (insert (format "Score: %d" 2048-score))
  (dotimes (i 40) (insert " "))
  (insert "Press 'r' to reset game")
  (2048-new-horizontal-dashed-line))


(defun 2048-insert-header ()
  "Insert header of the game."
  (goto-char (window-start))
  (let ((next-line-add-newlines t))
    (dotimes (i 2)
      (forward-line 1)))
  (beginning-of-line)
  (move-to-column (floor (/ (- (window-width) 9) 2)) t)
  (insert "2048 Game")
  (2048-new-horizontal-dashed-line))

(defun 2048-insert-matrix ()
  "Insert the game state as a matrix."
  (newline)
  (beginning-of-line)
  (move-to-column 15 t)
  (insert (make-string (* 2048-size 2048-cell-size) ?_))
  (newline)
  (dotimes (i 2048-size)
    (beginning-of-line)
    (move-to-column 15 t)
    (insert (concat "|" (mapconcat '2048-val-to-string (elt 2048-state i) "|") "|"))
    (newline)
    (beginning-of-line)
    (move-to-column 15 t)
    (insert (2048-other-line))
    (newline)))

(defun 2048-move-empty-cells (dir)
  "Move empty cells to the end of a matrix.

  DIR: one of the main four directions."
  (cond
   ((or (eq dir :left) (eq dir :right)) (2048-move-empty-cells-row dir))
   ((or (eq dir :up) (eq dir :down)) (2048-move-empty-cells-col dir))))

(defun 2048-merge-identicals (dir)
  "Merge identical cells along the given direction.

  DIR: one of the four directions."
  (cond
   ((or (eq dir :left) (eq dir :right)) (2048-merge-identicals-row dir))
   ((or (eq dir :up) (eq dir :down)) (2048-merge-identicals-col dir))))

(defun 2048-add-new-cell ()
  "Check to sea of there is any zero cells left."
  (let (positions pos i)
    (setq positions '())
    (dotimes (r 2048-size)
      (dotimes (c 2048-size)
        (when (= (2048-cell r c) 0)
          (setq positions (cons (cons r c) positions)))))
    (setq 2048-game-over (= (length positions) 0))
    (unless 2048-game-over
      (setq i (random (length positions)))
      (setq pos (elt positions i))
      (aset (elt 2048-state (car pos)) (cdr pos) (if (> (random) 0) 2 4)))
    2048-game-over))

(defun 2048-render ()
  "Create a text representation of the game matrix."
  (let ((inhibit-read-only t) str-acc str-final)
    (erase-buffer)
    (2048-insert-header)
    (2048-insert-score)
    (forward-line 2)
    (2048-insert-matrix)))

(defun 2048-reset-game ()
  "Reset game state and score also draw a new board."
  (interactive)
  (setq 2048-score 0
        2048-state [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]]
        2048-game-over nil)
  (2048-add-new-cell)
  (2048-render)
  )


(defun 2048-up ()
  "Command for moving up."
  (interactive)
  (2048-step :up))

(defun 2048-down ()
  "Command for moving down."
  (interactive)
  (2048-step :down))

(defun 2048-left ()
  "Command for moving left."
  (interactive)
  (2048-step :left))

(defun 2048-right ()
  "Command for moving right."
  (interactive)
  (2048-step :right))


(defun 2048-step (dir)
  "Move the numbers.

  DIR: one of the four directions"
  (let ((change nil))
    (unless 2048-game-over
      (setq change (or (2048-move-empty-cells dir) change)
            change (or (2048-merge-identicals dir) change)
            change (or (2048-move-empty-cells dir) change))
      (when change
        (2048-add-new-cell))
      (2048-render))))

(define-derived-mode twenty-fourty-eight-mode special-mode "2048"
  ""
  (let (buf)
    (setq buf (get-buffer-create "*2048*"))
    (with-current-buffer buf
      (read-only-mode)
      (2048-reset-game))
    (switch-to-buffer buf))
  (use-local-map twenty-fourty-eight-mode-map))


(provide 'twenty-fourty-eight)
;;; twenty-fourty-eight.el ends here
