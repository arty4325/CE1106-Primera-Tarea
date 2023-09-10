#lang racket

(provide find_path
         find_all_paths
         find_node
         extend
         neighbors
         path_distance
         nodes_distance
         find_ordered_paths
         reverse_all)


(define (reverse_all list)
  (reverse_all_aux list '())
)

(define (reverse_all_aux list result)
  (cond ((null? list) (reverse result) )
        (else (reverse_all_aux (cdr list)
                               (cons (reverse (car list))
                                     result) ))
  )
)

;; Finds the shorter path between two points
;; Uses Deep First Search
;; Returns a pair with the path as a list and the total weight

(define (find_path start end graph)
  (find_path_aux (list (list start)) end graph)        
)

(define (find_path_aux paths end graph)
  (define (helper current-path)
    (cond
      ((null? current-path) '())
      ((equal? end (car current-path)) (reverse current-path))
      (else
       (helper
        (append-map
         (lambda (neighbor)
           (unless (member neighbor current-path)
             (list neighbor (car current-path))))
         (neighbors (car current-path) graph))
        current-path))))

  (helper (list end))
)


;; Finds all paths between two points
;; Uses Width First Search
(define (find_all_paths start end graph)
  (cond((not(null? graph))
        (find_all_paths_aux (list (list start)) end graph '()))
       (else
        '())
       )
)

(define (find_all_paths_aux paths end graph result)
  (cond ((null? paths) (reverse_all result))
        ((equal? end (caar paths)) (find_all_paths_aux (cdr paths)
                                                 end
                                                 graph
                                                 (cons (car paths) result)) )
        (else (find_all_paths_aux (append  
                             (extend (car paths) graph)
                             (cdr paths))
                            end
                            graph
                            result))
   )
)

    
;; Finds new paths following the given one
(define (extend path graph)
  (extend_aux (neighbors (car path) graph) '() path)
)

(define (extend_aux neighbors result path)
  (define (helper neighbors result path acc)
    (cond
      ((null? neighbors) (append result acc))
      ((member (car neighbors) path)
       (helper (cdr neighbors) result path acc))
      (else
       (helper (cdr neighbors) result path
               (append acc (list (cons (car neighbors) path)))))))

  (helper neighbors result path '())
)
  

;; Searchs for a given node in the graph
;; Retunrs the node and neighbors
(define (find_node node graph)
  (cond ((null? graph) '())
        ((equal? node (caar graph)) (car graph))
        (else (find_node node (cdr graph)))
   )
)


;; Return the neighbors from a node as a list
;; The node must exist
(define (neighbors node graph)
  (neighbors_aux (last (find_node node graph)) '())
)

(define (neighbors_aux pairs result)
  (define (helper pairs acc)
    (cond
      ((null? pairs) (reverse acc))
      (else (helper (cdr pairs) (cons (caar pairs) acc)))))

  (helper pairs '())
)



;; Finds the distance for the path in the graph
(define (path_distance path graph)
  (define (helper path acc)
    (cond
      ((null? path) -1) ; Si la lista de caminos está vacía, retorna -1
      ((null? (cdr path)) 0) ; Si la lista de caminos tiene un solo elemento, retorna 0
      (else (helper (cdr path) (+ acc (nodes_distance (car path) (cadr path) graph))))))

  (helper path 0) ; Inicia la llamada a la función auxiliar con un acumulador en 0

)


;; Finds the distance between two nodes in a graph
(define (nodes_distance start end graph)
  (cond ((member end (neighbors start graph))
         (nodes_distance_aux end (last (find_node start graph))) )
        (else +inf.0)
  )
)

(define (nodes_distance_aux end neighbors)
  (cond ((equal? end (caar neighbors)) (last (car neighbors)))
        (else (nodes_distance_aux end (cdr neighbors)))
  )
)

(define (find_ordered_paths start end graph)
  (define (insert-sorted path paths)
    (if (null? paths)
        (list path)
        (if (< (path_distance path graph)
               (path_distance (car paths) graph))
            (cons path paths)
            (cons (car paths) (insert-sorted path (cdr paths))))))
  
  (define (insertion-sort paths)
    (if (null? paths)
        '()
        (insert-sorted (car paths) (insertion-sort (cdr paths)))))
  
  (if (null? (find_all_paths start end graph))
      '()
      (insertion-sort (find_all_paths start end graph))))
