;---------------------------
; Global variables
;---------------------------
(defvar *nodes-expanded* 0)

(defvar *goal-state* '(1 2 3 4 5 6 7 8 0))


;---------------------------
; Creating initial state
;---------------------------
; generate random initial state
(defun create-initial (&optional (num-of-moves 40) (state *goal-state*))
  (if (> num-of-moves 0)
      (create-initial (- num-of-moves 1) (move-blank-randomly state))
      (print state)))

; move blank randomly once
(defun move-blank-randomly (state)
  (move-tile state (tile-location 0 state) (nth (random (length (possible-moves (tile-location 0 state)))) (possible-moves (tile-location 0 state)))))


;---------------------------
; Misc function definitions
;---------------------------
; test to see if state is the goal state
(defun goal-test (state) (equalp *goal-state* state))

; test to see if the two states are the same
(defun state-test (state-A state-B) (equalp state-A state-B))

; checks location of a specific tile and returns the index
(defun tile-location (target-tile state &optional (puzzle-index 0))
  (cond ((atom state) puzzle-index)
	((equalp target-tile (car state)) puzzle-index)
	(t (tile-location target-tile (cdr state) (+ puzzle-index 1))))) 

; check for where a tile can move at each index
(defun possible-moves (puzzle-index)
     (svref '#((1 3) (0 2 4) (1 5) (0 4 6) (1 3 5 7) (2 4 8) (3 7) (4 6 8) (7 5)) puzzle-index)) 

; check for where a tile can move at each index, labeled with moves s move sequence can be printed out at the end of program run.
(defun labeled-possible-moves (puzzle-index) 
  (svref '#(((right 1)(down 3)) ((left 0)(right 2)(down 4)) ((left 1)(down 5)) ((up 0)(right 4)(down 6)) ((up 1)(left 3)(right 5)(down 7)) ((up 2)(left 4)(down 8)) ((up 3)(right 7)) ((up 4)(left 6)(right 8)) ((up 5)(left 7))) puzzle-index))

; moves a tile at location specified by "from" to "to"
(defun move-tile (state from to) ;; copies contents of tile to swap blank space with (this tile is set to 0) into the current blank space
  (let ((copy-state (copy-list state))) (setf (nth from copy-state) (nth to copy-state)) (setf (nth to copy-state) (nth from state)) copy-state))

; function for comparing the evaluation function (f of n) of two nodes
(defun compare-func (node-A node-B)
  (< (+ (node-path-cost node-A) (node-heuristic-value node-A)) (+ (node-path-cost node-B) (node-heuristic-value node-B))))


;----------------------------
; Misplaced tiles algorithm
;----------------------------
(defun misplaced-tiles (state &optional (goal-state-iterator *goal-state*))
  (cond ((null state) 0)
	((equalp (car state) (car goal-state-iterator)) (misplaced-tiles (cdr state) (cdr goal-state-iterator)))
	(t (+ (misplaced-tiles (cdr state) (cdr goal-state-iterator)) 1))))


;-------------------------------
; Manhattan distance algorithm
;-------------------------------
(defun manhattan-dist (state &optional (puzzle-index 0))
  (cond ((null state) 0)
	(t (+ (manhattan-dist (cdr state) (+ puzzle-index 1)) (coord-distance (coord puzzle-index) (coord (tile-location (car state) *goal-state*)))))))

; coordinatess for calculating manhattan distances
(defun coord (puzzle-index)
     (svref '#((0 0) (1 0) (2 0) (0 1) (1 1) (2 1) (0 2) (1 2) (2 2)) puzzle-index)) 

; calculates distance between two coordinates
(defun coord-distance (coord-A coord-B)
  (+ (car (mapcar #'abs (mapcar #'- coord-A coord-B))) (cadr (mapcar #'abs (mapcar #'- coord-A coord-B)))))


;---------------
; Graph search
;---------------
;takes a node and makes a list out of it
(defun make-queue (node) (list node))

;takes a queue and returns the first item
(defun queue-front (q) (car q))

;takes a queue, pops the first item and returns the new queue
(defun queue-pop (q) (cdr q)) 

; node structure: contains a node's state, parent action, path-cost and its heuristic value.
(defstruct node state parent action (path-cost 0) heuristic-value)

; general search: provided by Professor Pasik
(defun general-search (initial-state successorf goalp samep heuristicf &optional (enqueuef #'append))
  (graph-search (make-queue (make-node :state initial-state)) ; fringe
		nil successorf goalp samep heuristicf enqueuef))

; action sequence records the sequence of moves a tile takes: provided by Professor Pasik
(defun action-sequence (node &optional (so-far nil))
  (if (node-parent node)
    (action-sequence (node-parent node) (cons (node-action node) so-far)) so-far))

; graph search: provided by Professor Pasik
(defun graph-search (fringe closed successorf goalp samep heuristicf enqueuef)
  (when fringe
    (let ((node (queue-front fringe)) (fringe (queue-pop fringe)))
      (cond ((funcall goalp (node-state node)) (let ((actions (action-sequence node))) (format t "is the initial state. Nodes expanded is ~S. Below is the sequence of moves by blank tile:" *nodes-expanded*) (setf *nodes-expanded* 0) actions))
            ((member node closed :test (lambda (x y) (funcall samep (node-state x) (node-state y)))) (graph-search fringe closed successorf goalp samep heuristicf enqueuef))
            (t (graph-search (funcall enqueuef fringe (expand successorf node heuristicf)) (cons node closed) successorf goalp samep heuristicf enqueuef)) ;insert-all(expand...)
            ))))

; expand function for expanding a node to find its successors: provided by Professor Pasik
(defun expand (successorf node heuristicf)
  (setf *nodes-expanded* (1+ *nodes-expanded*)) ;incrementing *nodes-expanded* to be printed out later
  (mapcar (lambda (action-state-cost)	    
              (let ((action (car action-state-cost))
                    (state (cadr action-state-cost))
                    (cost (caddr action-state-cost)))
                (make-node :state state :parent node
                           :action action :path-cost (+ (node-path-cost node) cost)
                           :heuristic-value cost ; check to see if heuristic-value is really cost <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
			   )) ;:depth (1+ (node-depth node))
	  )
	  (funcall successorf (node-state node) heuristicf)))

; successor function for calculating sucessor nodes
(defun successor-func (state heuristicf)
  (mapcar #'(lambda (tile-move) (let ((new-state (move-tile state (tile-location 0 state) (cadr tile-move)))) (list (car tile-move) new-state (funcall heuristicf new-state))))
	    (labeled-possible-moves (tile-location 0 state))))

; enqueue function for enqueuing successor nodes into fringe according
; to the nodes' evaluation function (f(n)=g(n) + h(n))
(defun enqueue-func (fringe successors)
  (sort (append fringe successors) #'compare-func))

;----------------------------------------
; Solving the 8-puzzle (Main function)
;----------------------------------------
(defun solve (heuristic-func &optional (start-state (create-initial)))
  (general-search start-state #'successor-func #'goal-test #'state-test heuristic-func #'enqueue-func))
