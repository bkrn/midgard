(define test-random (lambda () (let* ((point-1 (rand-pos))
    (point-2 (rand-pos))
    (point-3 (rand-pos))
    (point-4 (rand-pos))

    (cart-1 (to-cartesian point-1))
    (cart-2 (to-cartesian point-2))
    (cart-3 (to-cartesian point-3))
    (cart-4 (to-cartesian point-4))

    (plane-1 (to-plane cart-1 cart-2))
    (plane-2 (to-plane cart-3 cart-4))

    (unit-vector-1 (to-unit-vector plane-1))
    (unit-vector-2 (to-unit-vector plane-2))

    (director (to-director unit-vector-1 unit-vector-2))
    (intersects (to-intersects director)))
    (have-intersect intersects (cons point-1 point-2) (cons point-3 point-4)))))


(define rand-tests (lambda (count) 
    (define append-tests 
        (lambda (count lst) 
        (if (equal? count 0) lst (append-tests (- count 1) (cons (test-random) lst))))) 
    (append-tests (- count 1) (cons (test-random) ()))))


(define cross-count 
    (lambda (count) (foldr + 0 (map (lambda (b) (if b 1 0)) (rand-tests count)))))


(define portland (cons 45.5122 -122.6587))
(define fort-pierce (cons 27.4467 -80.3256))
(define cape-cod (cons 41.7898 -69.9897))
(define san-diego (cons 32.7157 -117.1611))

(define test-arcs (lambda (arc-1 arc-2) (let* ((cart-1 (to-cartesian (car arc-1)))
    (cart-2 (to-cartesian (cdr arc-1)))
    (cart-3 (to-cartesian (car arc-2)))
    (cart-4 (to-cartesian (cdr arc-2)))

    (plane-1 (to-plane cart-1 cart-2))
    (plane-2 (to-plane cart-3 cart-4))

    (unit-vector-1 (to-unit-vector plane-1))
    (unit-vector-2 (to-unit-vector plane-2))

    (director (to-director unit-vector-1 unit-vector-2))
    (intersects (to-intersects director)))
    (have-intersect intersects arc-1 arc-2))))

(define should-true (test-arcs (cons portland fort-pierce) (cons cape-cod san-diego)))
(if (not should-true) (throw "Portland to Florida did not intersect Cape Cod to San Diego") (display "Test 1 Pass\n"))


(define fake-bbox (list 
    (cons "max-lat" 100) 
    (cons "min-lat" 0)
    (cons "min-lon" 0)
    (cons "max-lon" 50)))

(define normalizer (create-normalizer fake-bbox))
(define should-true (equal? (normalizer (cons 50 25)) (cons 50 100)))
(if (not should-true) (throw "Normalizer failed") (display "Test 2 Pass"))