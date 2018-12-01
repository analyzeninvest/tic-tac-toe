;;; tic-tac-toe.el --- play tic-tack-toe in Emacs

;; Copyright 2018 Aritra Bhattacharjee

;; Author: Aritra Bhattacharjee <analyzeninvest@protonmail.com>
;; Version: 2014.01.12
;; URL: https://github.com/analyzeninvest/tic-tac-toe

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This program is an implementation of tic-tac-toe for Emacs.
;; To begin playing, call `M-x tic-tac-toe`, then use the arrow keys,
;; to move within the board.

;; Press SPACE-BAR to mark the square in the board. 'X' or 'O' is
;; marked alternatively.

;; When either of the player 'X' or 'O' makes 3 consecutive lines,
;; either in a row/column/diagonal, same player is won. Else if all
;; the squares are filled but no consecutive 3 row/col/diagonal
;; line has same player, the match is a draw.

;;; Code:

(defun tic-tac-toe ()
  "Start a new game of tic tac toe"
  (interactive)
  (switch-to-buffer "tic-tac-toe")
  (tic-tac-toe-mode)
  (text-scale-increase 5)
  (tic-tac-toe-init)
  (linum-mode -1)
  (hl-line-mode -1)
  )


(define-derived-mode tic-tac-toe-mode special-mode "tic-tac-toe"
  (define-key tic-tac-toe-mode-map (kbd "SPC") 'tic-tac-toe-mark)
  (define-key tic-tac-toe-mode-map (kbd "<left>") 'tic-tac-toe-left-move)
  (define-key tic-tac-toe-mode-map (kbd "<right>") 'tic-tac-toe-right-move)
  (define-key tic-tac-toe-mode-map (kbd "<up>") 'tic-tac-toe-up-move)
  (define-key tic-tac-toe-mode-map (kbd "<down>") 'tic-tac-toe-down-move)
  )


(defun tic-tac-toe-left-move ()
  "move left"
  (interactive)
  (if (equal (tic-tac-toe-adjust-col) 0)
      (left-char 8)
    (left-char 2)
    )
  )


(defun tic-tac-toe-right-move ()
  "move right"
  (interactive)
  (if (equal (tic-tac-toe-adjust-col) 2)
      (right-char 8)
    (right-char 2)
    )
  )


(defun tic-tac-toe-up-move ()
  "move up"
  (interactive)
  (previous-line 2)
  )


(defun tic-tac-toe-down-move ()
  "move down"
  (interactive)
  (next-line 2)
  )


(defun tic-tac-toe-init ()
  "Init game"
  (defconst tic-tac-toe-player-1 "X")
  (defconst tic-tac-toe-player-2 "O")
  (defvar tic-tac-toe-board nil "Board itself.")
  (defvar tic-tac-toe-current-player tic-tac-toe-player-1 "tic-tac-toe get/set current player")
  (defconst tic-tac-toe-row-or-col-size 3 "bord row & col size")
  (defconst tic-tac-toe-before-move ".")
  (defconst tic-tac-toe-seperating-char-row "|")
  (defconst tic-tac-toe-seperating-char-newline-odd "-")
  (defconst tic-tac-toe-seperating-char-newline-even "+")
  (defconst tic-tac-toe-game-over-status nil)
  (setq tic-tac-toe-board (make-vector (* tic-tac-toe-row-or-col-size tic-tac-toe-row-or-col-size) tic-tac-toe-before-move))
  (tic-tac-toe-print-board)
  )


(defun tic-tac-toe-print-board ()
  "Print the board of the tic-tac-toe game."
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dotimes (row tic-tac-toe-row-or-col-size)
	(dotimes (col tic-tac-toe-row-or-col-size)
	  (insert (tic-tac-toe-get-square row col))
	  (if (not (equal col (- tic-tac-toe-row-or-col-size 1))) (insert tic-tac-toe-seperating-char-row))
	  )
	(insert "\n")
	(if (not (equal row (- tic-tac-toe-row-or-col-size 1)))
	    (dotimes (each-square (+ tic-tac-toe-row-or-col-size (- tic-tac-toe-row-or-col-size 1)))
	      (if (not (equal each-square (+ tic-tac-toe-row-or-col-size (- tic-tac-toe-row-or-col-size 1))))
		  (if (equal 0 (mod each-square 2))
		      (insert tic-tac-toe-seperating-char-newline-odd)
		    (insert tic-tac-toe-seperating-char-newline-even)))
	      )
	  )
	(insert "\n")
	)
      )
    (beginning-of-buffer)
    )


(defun tic-tac-toe-get-square (row col)
  "get the values of the board from row & column in the square"
  (elt tic-tac-toe-board
       (+ col
	  (* row
	     tic-tac-toe-row-or-col-size))
       )
  )


(defun tic-tac-toe-set-square (row col value)
  "set the values of the board from row & column in the square"
  (aset tic-tac-toe-board
       (+ col
	  (* row
	     tic-tac-toe-row-or-col-size))
       value)
  )


(defun tic-tac-toe-mark ()
  "Mark square of the board."
  (interactive)
  (let (row col)
    (setq row (tic-tac-toe-adjust-row))
    (setq col (tic-tac-toe-adjust-col))
    (if (equal (tic-tac-toe-get-square row col) tic-tac-toe-before-move)
	(tic-tac-toe-set-square row col tic-tac-toe-current-player)
      (tic-tac-toe-toggle-player))
    (tic-tac-toe-print-board)
    (when (tic-tac-toe-game-over)
      (tic-tac-toe-game-end-sequence)
      )
    (tic-tac-toe-toggle-player)
    )
  )


(defun tic-tac-toe-toggle-player ()
  "swap or toggle player"
  (setq tic-tac-toe-current-player
	(if (equal tic-tac-toe-current-player tic-tac-toe-player-1)
	    tic-tac-toe-player-2
	  tic-tac-toe-player-1
	  )
	)
  )


(defun tic-tac-toe-adjust-row ()
  "this is for adjustig the rows from line"
  (/ (1- (line-number-at-pos)) 2)
  )


(defun tic-tac-toe-adjust-col ()
  "this is for adjustig the cols from line"
  (/ (current-column) 2)
  )


(defun tic-tac-toe-game-over ()
  "Check if game is over?"
    (if (or (tic-tac-toe-if-draw) (tic-tac-toe-if-win))
	(setq tic-tac-toe-game-over-status t)
      )
    tic-tac-toe-game-over-status
  )


(defun tic-tac-toe-if-win ()
  "tic tack toe win?"
  (if (or (tic-tac-toe-diagonal-win)
	  (tic-tac-toe-row-win)
	  (tic-tac-toe-col-win)
	  )
      (progn (setq tic-tac-toe-end-message (concat "Congratulations!!! " tic-tac-toe-current-player " have won the game!!! Play again? " ))
	     t
	     )
    nil
    )
  )


(defun tic-tac-toe-if-draw ()
  "Check if game is draw?"
  (unless (tic-tac-toe-if-win)
    (let ((tic-tac-toe-game-ongoing t) (tic-tac-toe-count-init 0))
      (dotimes (row tic-tac-toe-row-or-col-size)
	(dotimes (col tic-tac-toe-row-or-col-size)
	  (if (equal (tic-tac-toe-get-square row col) tic-tac-toe-before-move)
	      (setq tic-tac-toe-count-init (+ 1 tic-tac-toe-count-init))
	    )
	  )
	)
      (if (equal tic-tac-toe-count-init 0)
	  (setq tic-tac-toe-game-ongoing nil)
	(setq tic-tac-toe-game-ongoing t)
	)
      (if (equal tic-tac-toe-game-ongoing t)
	  nil
	(progn (setq tic-tac-toe-end-message "Match is a Draw!!! Play again? ")
	       t)
	)
      )
    )
  )

(defun tic-tac-toe-compare-3-char (row1 col1 row2 col2 row3 col3)
  "Compare 3 squares in the board."
  (if (and (equal tic-tac-toe-current-player (tic-tac-toe-get-square row1 col1))
	   (equal (tic-tac-toe-get-square row2 col2) (tic-tac-toe-get-square row1 col1))
	   (equal (tic-tac-toe-get-square row3 col3) (tic-tac-toe-get-square row1 col1))
	   )
      t
    nil
    )
  )


(defun tic-tac-toe-diagonal-win ()
  "Check if diagonal win?"
  (if (or (tic-tac-toe-compare-3-char 0 0 1 1 2 2)
	  (tic-tac-toe-compare-3-char 0 2 1 1 2 0)
	  )
      (progn (message "diagonal win")
	     t)
    nil
    )
  )


(defun tic-tac-toe-row-win ()
  "Check if row win?"
  (if (or (tic-tac-toe-compare-3-char 0 0 0 1 0 2)
	  (tic-tac-toe-compare-3-char 1 0 1 1 1 2)
	  (tic-tac-toe-compare-3-char 2 0 2 1 2 2)
	  )
      (progn (message "row win")
	     t)
    nil
      )
  )


(defun tic-tac-toe-col-win ()
  "Check if col win?"
  (if (or (tic-tac-toe-compare-3-char 0 0 1 0 2 0)
	  (tic-tac-toe-compare-3-char 0 1 1 1 2 1)
	  (tic-tac-toe-compare-3-char 0 2 1 2 2 2)
	  )
      (progn (message "col win")
	     t)
    nil
      )
  )


(defun tic-tac-toe-game-end-sequence ()
  "this is the sequence for end game."
  (if (y-or-n-p tic-tac-toe-end-message)
      (tic-tac-toe-init)
    (kill-buffer "tic-tac-toe")
      )
  )


(provide 'tic-tac-toe-game)
