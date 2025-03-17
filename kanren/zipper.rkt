#lang racket
(require
  (only-in racket/match match)
  data/queue
  first-order-miniKanren/microk-fo
  first-order-miniKanren/tools)

(define (zipper-breadth-search initial-query max-depth)
  ;; Initialize exploration with the query
  (define initial-loc (init-explore initial-query))

  ;; Use a queue to manage locations to explore
  (define queue (make-queue))
  (enqueue! queue (cons initial-loc '())) ;; (location . path)

  (let loop ((depth 0))
    (if (or (>= depth max-depth) (queue-empty? queue))
        (values #f '()) ;; No solution found within depth limit
        (let* ((current (dequeue! queue))
               (loc (car current))
               (path (cdr current))
               (tree (explore-loc-tree loc))
               (choices (explore-node-choices tree))
               (results (takef choices state?))
               (remaining-choices (dropf choices state?)))

          ;; If we found a result, return it with path
          (if (not (null? results))
              (values (car results) (reverse path))

              ;; Otherwise enqueue all choices with updated paths
              (begin
                (for ([i (in-range (length remaining-choices))])
                  (let ((next-loc (explore-choice loc step i))
                        (next-path (cons i path)))
                    (enqueue! queue (cons next-loc next-path))))
                (loop (add1 depth))))))))


(define (zipper-biased-interleaving-v2 initial-query max-steps)
  ;; Initialize exploration with the query
  (define initial-loc (init-explore initial-query))

  ;; Keep track of path to each location and the number of results found in that branch
  ;; Format: (loc path results-found depth)
  (define location-queue '())

  ;; Add a location to the queue
  (define (enqueue-location loc path results-found depth)
    (set! location-queue
          (append location-queue (list (list loc path results-found depth)))))

  ;; Add initial location
  (enqueue-location initial-loc '() 0 0)

  ;; Main search loop with step counter
  (let loop ((steps 0))
    (if (or (>= steps max-steps) (null? location-queue))
        (values #f '()) ;; No solution found within step limit or queue empty

        (let* (;; Extract next location to explore from queue
               (current (car location-queue))
               (rest-queue (cdr location-queue))
               (loc (first current))
               (path (second current))
               (results-found (third current))
               (depth (fourth current))

               ;; Get information about the current location
               (tree (explore-loc-tree loc))
               (choices (explore-node-choices tree))
               (new-results (takef choices state?))
               (remaining-choices (dropf choices state?)))

          ;; Update queue for next iteration
          (set! location-queue rest-queue)

          ;; If we found results, return the first one with its path
          (if (not (null? new-results))
              (values (car `(,new-results . ,loc)) (reverse path))

              ;; Otherwise process the remaining choices
              (if (null? remaining-choices)
                  ;; No choices left, continue to next location in queue
                  (loop (add1 steps))

                  ;; Process the first choice immediately (depth-first bias)
                  ;; and add the rest to the queue (breadth-first interleaving)
                  (let ((first-idx 0)
                        (rest-indices (cdr (range (length remaining-choices)))))

                    ;; Queue the rest of the choices after the current frontier
                    (for ([i rest-indices])
                      (enqueue-location
                       (explore-choice loc step i)
                       (cons i path)
                       results-found
                       (add1 depth)))

                    ;; Process the first choice immediately
                    (let* ((next-loc (explore-choice loc step first-idx))
                           (next-path (cons first-idx path))
                           (next-results-found (+ results-found (length new-results))))

                      ;; Alternate between continuing in this branch and processing the queue
                      ;; This is the key to biased interleaving - prioritize branches with results
                      (if (> next-results-found 0)
                          ;; Branch has produced results, process immediately
                          (begin
                            (set! location-queue
                                  (cons (list next-loc next-path next-results-found (add1 depth))
                                        location-queue))
                            (loop (add1 steps)))
                          ;; No results yet, add to end of queue for interleaving
                          (begin
                            (enqueue-location next-loc next-path next-results-found (add1 depth))
                            (loop (add1 steps))))))))))))

(define-relation (conso head tail result) (== `(,head . ,tail) result))

(define-relation (appendo ab c abc)
                 (conde ((== '() ab) (== c abc))
                        ((fresh (a b bc) (conso a b ab) (conso a bc abc) (appendo b c bc)))))

(define-values (result path)
  (zipper-biased-interleaving-v2 (query (a b) (appendo a b '(1 2 3 4))) 100))


(define (zipper-biased-interleaving-multi initial-query max-steps n-results)
  ;; Initialize exploration with the query
  (define initial-loc (init-explore initial-query))

  ;; Track found results and their locations
  (define results '())  ;; List of (result . explore-loc) pairs

  ;; Keep track of location and the number of results found in that branch
  ;; Format: (loc results-found depth)
  (define location-queue '())

  ;; Add a location to the queue
  (define (enqueue-location loc results-found depth)
    (set! location-queue
          (append location-queue (list (list loc results-found depth)))))

  ;; Add initial location
  (enqueue-location initial-loc 0 0)

  ;; Main search loop with step counter
  (let loop ((steps 0))
    (cond
      ;; Stop conditions
      [(>= steps max-steps)
       results] ;; Reached max steps
      [(null? location-queue)
       results] ;; Search space exhausted
      [(>= (length results) n-results)
       (take results n-results)] ;; Found enough results

      [else
       (let* (;; Extract next location to explore from queue
              (current (car location-queue))
              (rest-queue (cdr location-queue))
              (loc (first current))
              (results-found (second current))
              (depth (third current))

              ;; Get information about the current location
              (tree (explore-loc-tree loc))
              (choices (explore-node-choices tree))
              (new-results (takef choices state?))
              (remaining-choices (dropf choices state?)))

         ;; Update queue for next iteration
         (set! location-queue rest-queue)

         ;; Process any new results
         (when (not (null? new-results))
           (for ([result new-results])
             (set! results
                   (append results
                           (list (cons result loc))))))

         ;; If we've found enough results, return them
         (if (>= (length results) n-results)
             (take results n-results)

             ;; Otherwise process the remaining choices
             (if (null? remaining-choices)
                 ;; No choices left, continue to next location in queue
                 (loop (add1 steps))

                 ;; Process the first choice immediately (depth-first bias)
                 ;; and add the rest to the queue (breadth-first interleaving)
                 (let ((first-idx 0)
                       (rest-indices (cdr (range (length remaining-choices)))))

                   ;; Queue the rest of the choices after the current frontier
                   (for ([i rest-indices])
                     (enqueue-location
                      (explore-choice loc step i)
                      results-found
                      (add1 depth)))

                   ;; Process the first choice immediately
                   (let* ((next-loc (explore-choice loc step first-idx))
                          (next-results-found (+ results-found (length new-results))))

                     ;; Alternate between continuing in this branch and processing the queue
                     ;; This is the key to biased interleaving - prioritize branches with results
                     (if (> next-results-found 0)
                         ;; Branch has produced results, process immediately
                         (begin
                           (set! location-queue
                                 (cons (list next-loc next-results-found (add1 depth))
                                       location-queue))
                           (loop (add1 steps)))
                         ;; No results yet, add to end of queue for interleaving
                         (begin
                           (enqueue-location next-loc next-results-found (add1 depth))
                           (loop (add1 steps)))))))))])))


(define results
  (zipper-biased-interleaving-multi (query (a b) (appendo a b '(1 2 3 4))) 1000 5))

;; Access the first result and its explore-loc
(define first-result-state (caar results))
(define first-result-loc (cdar results))

;; You can reconstruct the complete path if needed using the explore-loc structure
(define (extract-path loc)
  (let loop ((current-loc loc)
             (path '()))
    (match current-loc
      [(explore-loc _ 'X-TOP) (reverse path)]
      [(explore-loc _ (explore-context index _ _ parent))
       (loop (explore-undo current-loc) (cons index path))])))

(define first-result-path (extract-path first-result-loc))

(walk* initial-var (state-sub (car (list-ref results 4))))
first-result-path
(extract-path (cdr (list-ref results 4)))
