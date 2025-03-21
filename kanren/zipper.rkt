#lang racket
(require
  (only-in racket/match match)
  first-order-miniKanren/microk-fo
  first-order-miniKanren/tools)

(define-relation (conso head tail result) (== `(,head . ,tail) result))

(define-relation (appendo ab c abc)
                 (conde ((== '() ab) (== c abc))
                        ((fresh (a b bc) (conso a b ab) (conso a bc abc) (appendo b c bc)))))

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
  (zipper-biased-interleaving-multi (query (a b) (appendo a b '(1 2 3 4))) 6 5))

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
