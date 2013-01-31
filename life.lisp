;;; My implementation of the Conway's Game of Life.
;;; http://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
;;; Author: Viacheslav Chimishuk <voice@root.ua>
;;; License: GPLv3 or later.
;;; Date: 28.01.2013

(defpackage :life
  (:use :common-lisp))

(in-package :life)

(require 'asdf)
(require 'uffi)

(asdf:operate 'asdf:load-op :uffi)
(asdf:oos 'asdf:load-op 'cl-ncurses)

(cl-ncurses:initscr)

; One generation lifetime.
(defparameter *generation-lifetime* 1)
; Map can't be smaller than this limit size.
(defparameter *min-map-size* 10)
; Dead cell state.
(defparameter *dead* 0)
; Alive cell state.
(defparameter *alive* 1)
; Live cell character for visualization.
(defparameter *alive-char* "o")
; Dead cell character for visualization.
(defparameter *dead-char* " ")

(defun create-map (size)
  "Creates new map object with given size."
  (make-array (list size size) :initial-element *dead*))

(defun map-setf (map y x val)
  "Sets map's element with x:y coordinates to the specified value."
  (setf (aref map y x) val)
  map)

(defun alivep (map y x)
  "Return T if cell is alive."
  (let ((max-y (array-dimension map 0))
        (max-x (array-dimension map 1)))
    (and (>= y 0) (< y max-y)
         (>= x 0) (< x max-x)
         (equal (aref map y x) *alive*))))

(defun deadp (map y x)
  "Returns T if cell is dead and is not breathing."
  (not (alivep map y x)))

(defun born (map y x)
  "Make the cell alive. Now I'm your father, cell!"
  (map-setf map y x *alive*))

(defun kill (map y x)
  "Kill this child of mine."
  (map-setf map y x *dead*))

(defun init-map (map)
  "Initialize empty (all cells are dead) map with some
first generation. For now it creates Glider in the upper
left corner of the map."
  (let* ((map (born map 0 1))
         (map (born map 1 2))
         (map (born map 2 0))
         (map (born map 2 1))
         (map (born map 2 2)))
    map))

(defun neighbors-number (map y x)
  "Returns number of alive neighbors for the given cell."
  (let ((n 0))
    ; Row above cell.
    (and (alivep map (- y 1) (- x 1)) (setf n (+ n 1)))
    (and (alivep map (- y 1) (+ x 0)) (setf n (+ n 1)))
    (and (alivep map (- y 1) (+ x 1)) (setf n (+ n 1)))
    ; Row wich the cell belongs.
    (and (alivep map y (- x 1)) (setf n (+ n 1)))
    (and (alivep map y (+ x 1)) (setf n (+ n 1)))
    ; Row under the cell.
    (and (alivep map (+ y 1) (- x 1)) (setf n (+ n 1)))
    (and (alivep map (+ y 1) (+ x 0)) (setf n (+ n 1)))
    (and (alivep map (+ y 1) (+ x 1)) (setf n (+ n 1)))
    n))

(defun next-generation (map)
  "Generate next population generation. During this process some cells will die,
others will be born, so population will be completly different."
  (let* ((size (array-dimension map 0))
         (max-index (- size 1))
         (new-map (create-map size)))
    (loop for y from 0 to max-index do
         (loop for x from 0 to max-index do
              (let ((neighbors (neighbors-number map y x)))
                (if (equal neighbors 3)
                    (born new-map y x)
                    (if (and (equal neighbors 2) (alivep map y x))
                        (born new-map y x)
                        (kill new-map y x))))))
    new-map))

(defun print-map (map)
  "Print map on the screen."
  (let ((max-index (1- (array-dimension map 0))))
    (loop for y from 0 to max-index do
         (loop for x from 0 to max-index do
              (cl-ncurses:mvprintw y x (if (alivep map y x)
                                           *alive-char*
                                           *dead-char*)))))
  (cl-ncurses:refresh))

(defun live-one-generation (map)
  "Live one generation."
  (print-map map)
  (sleep *generation-lifetime*)
  (live-one-generation (next-generation map)))

(defun life (size)
  "Let the game begins!"
  (let ((size (max size *min-map-size*)))
    (live-one-generation (init-map (create-map size)))))

(life 10)
