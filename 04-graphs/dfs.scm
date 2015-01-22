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

'(define (depth-first-search the-graph)	;Ignore.
  (let* ((len (cdr (assoc 'LEN the-graph)))
	 (the-graph* (cdr the-graph))
	 (val (make-vector len #f)))
    (let loop ((k 0)))))
