;; dfs.scm --- depth first searching of graphs

;; adjacency list, sedgewick p. 421

;; graphs can be represented with matrices or alists

(define *the-graph* '((LEN . 13)
		      (A  (F C B G))
		      (B  (A))
		      (C (A))
		      (D  (F E))
		      (E  (G F D))
		      (F  (A E D))
		      (G  (E A))
		      (H  (I))
		      (I  (H))
		      (J  (K L M))
		      (K  (J))
		      (L  (J M))
		      (M  (J L))))

;; dfs is a natural way to visit every node and check every edge in
;; the graph systematically.

'(define (dfs nodes start-id visited)
   (let loop ((nodes nodes)
              (id start-id)
              (visited visited)
              (sum 0))
     (if (null? nodes)
         sum
         (let ((neighbour (car nodes)))
           (begin (vector-set! visited id #t)
                  (format #t "At ~A, visiting ~A next~%"
                          id (route-dest neighbour))
                  (loop (cdr nodes)
                        (route-dest neighbour)
                        visited
                        (+ sum (route-cost neighbour))))))))

'(define (depth-first-search the-graph)	;Ignore.
  (let* ((len (cdr (assoc 'LEN the-graph)))
	 (the-graph* (cdr the-graph))
	 (val (make-vector len #f)))
    (let loop ((k 0)))))



;;; This code came from longest-path.scm.  Work will be needed to
;;; merge it into graphs.scm.

;; ,open srfi-8 srfi-9 srfi-13 pp

(define *graph-file*
  (expand-file-name
   "~/Documents/personal/intro-algos-with-scheme/data/smallgraph.dat"))

(define-record-type :route		;SRFI-9
  (make-route dest cost)
  route?
  (dest route-dest)
  (cost route-cost))

(define-record-type :node
  (make-node id neighbour distance)
  node?
  (id node-id)
  (neighbour node-neighbour)
  (distance node-distance))

(define (parse-line line)
  ;; Str -> Node
  (if (eof-object? line) '()
      (let* ((pieces (map
		      (lambda (s) (string->number s))
		      (string-tokenize line)))
             (id (first pieces))
	     (neighbour (second pieces))
	     (distance (third pieces)))
	(make-node id neighbour distance))))

(define (read-places)
  ;; -> List of Nodes, Int
  (with-input-from-file *graph-file*
    (lambda ()
      (let ((num-lines (string->number (read-line))))
	(let loop ((line #f)
		   (retval '()))
	  (if (null? line)
	      (values retval num-lines)
	      (let ((line (parse-line (read-line))))
		(loop line (cons (parse-line (read-line)) (cons line retval))))))))))

(define (parse-places)
  (receive (place-data num-nodes)	;srfi-8
      (read-places)
    (let ((nodes (make-vector num-nodes #f)))
      (let loop ((place-data place-data))
	(cond ((null? place-data) nodes)
	      ((node? (car place-data))
	       (let* ((node (car place-data))
		      (id (node-id node))
		      (neighbour (node-neighbour node))
		      (distance (node-distance node))
		      (route (make-route neighbour distance)))
		 (vector-set! nodes id route)
		 (loop (cdr place-data))))
	      (else (loop (cdr place-data))))))))

(define (get-longest-path nodes id visited)
  ;; List Int Vec -> Int
  (let ((node-id id))
    (vector-set! visited node-id #t)
    (let ((max 0))
      (map (lambda (neighbour)
             (if (not (vector-ref visited (route-dest neighbour)))
                 (let ((distance (+ (route-cost neighbour)
                                    (get-longest-path nodes
                                                      (route-dest neighbour)
                                                      visited))))
                   (if (> distance max)
                       (set! max distance)))))
           nodes)
      (vector-set! visited node-id #f)
      max)))

;; works for DFS (depth-first search), but not longest path; gets 8929
;; but correct value is 8981

(define (dfs nodes start-id visited)
  (let loop ((nodes nodes)
             (id start-id)
             (visited visited)
             (sum 0))
    (if (null? nodes)
        sum
        (let ((neighbour (car nodes)))
          (begin (vector-set! visited id #t)
                 (format #t "At ~A, visiting ~A next~%"
                         id (route-dest neighbour))
                 (loop (cdr nodes)
                       (route-dest neighbour)
                       visited
                       (+ sum (route-cost neighbour))))))))

(define (run-it proc)
  (let* ((nodes (parse-places))
         (nodes* (vector->list nodes))
         (visited (make-vector (vector-length nodes) #f))
         (the-answer (proc nodes* 0 visited)))
    (display nodes*) (newline)
    (display the-answer) (newline)))

(define (show-data-structures)          ;For debugging.
  (let* ((nodes (parse-places))
	 (nodes* (vector->list nodes))
	 (visited (make-vector (vector-length nodes) #f)))
    (for-each (lambda (x) (map (lambda (y) (p y)) x)) (list nodes*))))
