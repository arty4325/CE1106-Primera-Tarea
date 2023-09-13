#lang racket

;; Provide functions for external use
(provide find_path
         find_all_paths
         find_node
         extend
         neighbors
         path_distance
         nodes_distance
         find_ordered_paths
         reverse_all)

;; Auxiliary function to reverse all lists in a list of lists (graph)
(define (reverse_all list)
  (reverse_all_aux list '())
)

(define (reverse_all_aux list result)
  (cond
    ((null? list) (reverse result)) ; If the input list is empty, return the accumulated reversed list in 'result'
    (else
     (reverse_all_aux (cdr list)
                      (cons (reverse (car list))
                            result)) ; Reverse the first list and add it to 'result'
     )
  )
)

;; Find the shortest path between two points in a graph using Depth First Search (DFS)
(define (find_path start end graph)
  (find_path_aux (list (list start)) end graph)        
)

(define (find_path_aux paths end graph)
  (define (helper current-path)
    (cond
      ((null? current-path) '()) ; If the current path is empty, no path was found
      ((equal? end (car current-path)) (reverse current-path)) ; If the first node in the current path is equal to the end node, return the reversed path
      (else
       (helper
        (append-map
         (lambda (neighbor)
           (unless (member neighbor current-path) ; Avoid cycles by checking if the neighbor is already in the current path
             (list neighbor (car current-path)))) ; Add unvisited neighbors to the current path
         (neighbors (car current-path) graph))
        current-path))))

  (helper (list end)) ; Call the auxiliary function with a list containing the end node
)

;; Find all possible paths between two points using Breadth First Search (BFS)
(define (find_all_paths start end graph)
  (cond
    ((not (null? graph)) ; If the graph is not empty
     (find_all_aux (list (list start)) end graph '())) ; Call the auxiliary function with a list containing the start node and an empty result list
    (else
     '()) ; If the graph is empty, return an empty list
    )
)

(define (find_all_aux paths end graph result)
  (cond
    ((null? paths) (reverse_all result)) ; If there are no more paths to explore, return the reversed result list
    ((equal? end (caar paths)) ; If the first node in the current path is equal to the end node
     (find_all_aux (cdr paths) end graph (cons (car paths) result))) ; Add the current path to the results and continue exploring
    (else
     (find_all_aux (append  
                          (extend (car paths) graph) ; Extend the current path
                          (cdr paths))
                         end
                         graph
                         result))
   )
)

;; Extend a given path in the graph, finding new paths
(define (extend path graph)
  (extend_aux (neighbors (car path) graph) '() path)
)

(define (extend_aux neighbors result path)
  (define (helper neighbors result path acc)
    (cond
      ((null? neighbors) (append result acc)) ; When there are no more neighbors, add the extended paths to the results
      ((member (car neighbors) path) ; If the neighbor is already in the path, skip it to avoid cycles
       (helper (cdr neighbors) result path acc))
      (else
       (helper (cdr neighbors) result path
               (append acc (list (cons (car neighbors) path))))))) ; Add new extended paths to the result

  (helper neighbors result path '()) ; Call the auxiliary function with neighbors, result, current path, and an empty accumulator
)

;; Search for a given node in the graph and return the node along with its neighbors
(define (find_node node graph)
  (cond
    ((null? graph) '()) ; If the graph is empty, return an empty list
    ((equal? node (caar graph)) (car graph)) ; If the node is found, return the list containing the node and its neighbors
    (else (find_node node (cdr graph))) ; If not found, continue searching in the rest of the graph
   )
)

;; Return a list of neighbors of a given node in the graph
(define (neighbors node graph)
  (neighbors_aux (last (find_node node graph)) '())
)

(define (neighbors_aux pairs result)
  (define (helper pairs acc)
    (cond
      ((null? pairs) (reverse acc)) ; When there are no more pairs, return the reversed list of neighbors
      (else (helper (cdr pairs) (cons (caar pairs) acc))))) ; Add the current neighbor to the result list

  (helper pairs '()) ; Call the auxiliary function with pairs of nodes and an empty result list
)

;; Calculate the distance of a path in the graph by summing the distances between successive nodes
(define (path_distance path graph)
  (define (helper path acc)
    (cond
      ((null? path) -1) ; If the list of paths is empty, return -1
      ((null? (cdr path)) 0) ; If the list of paths has only one element, return 0
      (else (helper (cdr path) (+ acc (nodes_distance (car path) (cadr path) graph)))))) ; Calculate the accumulated distance

  (helper path 0) ; Start the call to the auxiliary function with an accumulator at 0
)

;; Calculate the distance between two nodes in a graph
(define (nodes_distance start end graph)
  (cond
    ((member end (neighbors start graph)) ; If the end node is a neighbor of the start node
     (nodes_distance_aux end (last (find_node start graph))) ) ; Call the auxiliary function to find the distance
    (else +inf.0) ; If they are not neighbors, return positive infinity (+inf.0)
  )
)

(define (nodes_distance_aux end neighbors)
  (cond
    ((equal? end (caar neighbors)) (last (car neighbors))) ; When the end node is found, return the distance
    (else (nodes_distance_aux end (cdr neighbors))) ; If not found, continue searching in the remaining neighbors
  )
)

;; Find all paths between two points in the graph and sort them by distance from least to greatest
(define (find_ordered_paths start end graph)
  ;; Auxiliary function to insert a path into a list of sorted paths
  (define (insert-sorted path paths)
    (if (null? paths)
        (list path)
        (if (< (path_distance path graph)
               (path_distance (car paths) graph)) ; Compare the distances of the paths
            (cons path paths) ; Insert the path in the correct position
            (cons (car paths) (insert-sorted path (cdr paths)))))) ; Recursively call to insert in order

  ;; Insertion sort function
  (define (insertion-sort paths)
    (if (null? paths)
        '() ; When there are no more paths, return an empty list
        (insert-sorted (car paths) (insertion-sort (cdr paths))))) ; Call the insertion function to build the sorted list
  
  (if (null? (find_all_paths start end graph)) ; If no paths are found
      '() ; Return an empty list
      (insertion-sort (find_all_paths start end graph)))) ; Sort the found paths and return them in order of distance
