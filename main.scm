;;;;;;;;;
; About
;
;
;
;
;
;
;
;
;;;;;;;;;






;;;;;;;;;
; Generic Utilities
;;;;;;;;;

; get the value associated with `key` in assoc list `data`
(define (of key data) (cdr (assoc key data)))

; create a list of n results of calling fn
(define (n-results n fn) 
    (define (append-result n lst) 
    (if (<= n 0) lst (append-result (- n 1) (cons (fn) lst))))
    (append-result (- n 1) (cons (fn) ())))

; return a new list of members of lst where (fn members) returns #t
(define (filter lst fn) 
    (define (filter-result accum alst) 
        (if (equal? 0 (length alst)) accum  
        (if (fn (car alst)) 
            (filter-result (cons (car alst) accum) (cdr alst)) 
            (filter-result accum (cdr alst)))))
    (filter-result () lst))

(define pi (* 4 (atan 1.0)))

(define urandom (open-input-file "/dev/urandom"))

(define (as-radian v) (/ (* v pi) 180.0))
(define (as-degree v) (/ (* v 180.0) pi))

;;;;;;;;;
; Generate random (lat . lon) pairs
;;;;;;;;;

; assuming we're pointed at /dev/unrandom return a random byte
; rand-bytes sets the input so call 
(define (rand-byte) (char->integer (read-char)))

; get `count` random bytes, sets input port as /dev/urandom
; TODO: return to initial input port at end of call
(define (rand-bytes count) (let ((port (set-input-port urandom)))
    (n-results count rand-byte)))

; multiplying 2 ^ k to a number is the same as shifting it by
; k, tinyscheme doesn't have a native bitshift function so we
; use this
(define (left-shift value amount) (* value (expt 2 amount)))

; Given a list of bytes return an (* 8 (length bytes)) bit integer where the 
; bytes are stacked on top of each other
(define (stack-bytes bytes) 
    (define (stack value bytes) 
        (if (equal? (length bytes) 0) value 
        (stack (+ (left-shift value 8) (car bytes)) (cdr bytes))))
    (stack 0 bytes))

(define (rand-u32) (stack-bytes (rand-bytes 4)))

(define (rand-float) (/ (rand-u32) (expt 2 32)))

; create a function that returns a random float between `min` and `(+width min)`
(define (rand-range width min) (lambda () (+(* width (rand-float)) min)))

(define rand-lat (rand-range 180.0 -90.0))
(define rand-lon (rand-range 360.0 -180.0)) 

(define (rand-pos) (cons (rand-lat) (rand-lon)))
; aliases for clarity on access to items in (lat . lon) pairs
(define lat car)
(define lon cdr)


;;;;;;;;;
; Test if two arcs along great circles interect
; Lifted from:
; http://www.boeing-727.com/Data/fly%20odds/distance.html
;;;;;;;;;

; Aliases for treating a 3 length list as 
; x y z coordinates explicitly
(define x car)
(define y cadr)
(define z caddr)

; Convert a (lat . lon) pair to (x y z) cartesian coordinates
(define (pos-x pos) 
    (*(cos (as-radian (lat pos))) (cos (as-radian (lon pos)))))
(define (pos-y pos) 
    (*(cos (as-radian (lat pos))) (sin (as-radian (lon pos)))))
(define (pos-z pos) 
    (sin (as-radian (lat pos))))
(define (to-cartesian pos) 
    (list 
        (pos-x pos)
        (pos-y pos)
        (pos-z pos)))


; Matrix multiplication of two length 3 lists (like cartesian x y z
; coordinates)
(define to-v1 (lambda (cart-one cart-two) 
    (- (* (y cart-one) (z cart-two)) (* (z cart-one) (y cart-two)))))
(define to-v2 (lambda (cart-one cart-two) 
    (- (* (z cart-one) (x cart-two)) (* (x cart-one) (z cart-two)))))
(define to-v3 (lambda (cart-one cart-two) 
    (- (* (x cart-one) (y cart-two)) (* (y cart-one) (x cart-two)))))
(define m3-mult 
    (lambda (cart-one cart-two) 
        (list 
        (to-v1 cart-one cart-two) 
        (to-v2 cart-one cart-two) 
        (to-v3 cart-one cart-two))))
   
; Aliases of m3 mult to make typing more explicit        
(define to-plane m3-mult)
(define to-director m3-mult)

; Calculate the distance along an arc from its plane
(define (arc-length plane) 
    (sqrt (foldr + 0.0 (map (lambda (v) (expt v 2)) plane))))

; Convert a plane to its unit vector
(define (to-unit-vector plane) 
    (map (lambda (v) (/ v (arc-length plane))) plane))
 
; Convert cartesian (x y z) to a (lat . lon) pair
(define (to-polar director) (let* ((lat (asin (z director)))
    (tmp (cos lat))
    (sign (if (> (asin (/ (y director) tmp)) 0) 1 -1))
    (lon (* (acos (/ (x director) tmp)) sign)))
    (cons (as-degree lat) (as-degree lon))))


(define (flip-sign lst) (map (lambda (v) (* -1.0 v)) lst))

; Return the two points at which great circles intersect
(define (to-intersects director) 
    (cons (to-polar (to-unit-vector director)) 
          (to-polar (to-unit-vector (flip-sign director)))))

; Calculate the distance between two points on a sphere 
; along the shortest great circle path
(define (haversine point-1 point-2) (let* ((l1 (as-radian (lat point-1)))
    (l2 (as-radian (lat point-2)))
    (d-lat (as-radian (- (lat point-2) (lat point-1))))
    (d-lon (as-radian (- (lon point-2) (lon point-1))))
    (a1 (expt (sin (/ d-lat 2)) 2))
    (a2 (* (cos l1) (cos l2)))
    (a3 (expt (sin (/ d-lon 2)) 2))
    (a (+ a1 (* a2 a3))))
    (* 2 (asin (min 1 (sqrt a))))))

; distance from point-1 to point-2 is the distance along out arc
; point three is an intersection point so the distance between 
; p1 -> p3 + p2 -> p3 will equal p1 -> p2 if p3 is on the arc p1 -> p2
(define (distances point-1 point-2 point-3) (list (haversine point-1 point-2)
    (haversine point-1 point-3)
    (haversine point-2 point-3)))


; #t if point (lat . lon) is on an arc 
; described by ((lat . lon) . (lat . lon))
(define (on-arc arc point) 
    (let* ((d (distances (car arc) (cdr arc) point))) 
    (< (expt (foldr - (car d) (cdr d)) 2) (expt 10 -10))))

; Is point on the arcs described arc-1 and arc-2
; where an arc is ((lat . lon) . (lat . lon))
(define (is-crossing arc-1 arc-2 point) 
    (and (on-arc arc-1 point) (on-arc arc-2 point)))

; returns #t if either of the two great circle instersects on both arcs
(define (have-intersect intersects arc-1 arc-2) 
    (or (is-crossing arc-1 arc-2 (car intersects)) 
        (is-crossing arc-1 arc-2 (cdr intersects))))

;;;;;;;;;
; Import a description a polygon in the shape of lon,lat,elev and
; create a bounding box and a random point generator
;;;;;;;;;

; Open a file and set it as the input
(define (open-data file-name) 
    (set-input-port (open-input-file file-name)))

; read a lon,lat,elev line from a file and convert it from symbol to string
(define (read-row) (let ((row (read))) 
    (if (symbol? row) (symbol->string row) row)))

; convert a list of chars into a number
(define (chars->number chars) (string->number (list->string chars)))

; convert a string of "lon,lat,elev" to a (lat . lon) pair
(define (read-point row) 
    (let* ((lon-lat (cdr (member #\, (reverse (string->list row)))))
        (lon-chars (reverse (cdr (member #\, lon-lat))))
        (lat-chars (cdr (member #\, (reverse lon-lat)))))
    (cons (chars->number lat-chars) (chars->number lon-chars))))

; Open a lon,lat,elev csv and read it to (list (lat . lon))
(define (read-points file-name)
    (define (inner lst) 
        (let ((row (read-row))) 
        (if (string? row) (inner (cons (read-point row) lst)) lst))) 
    (let ((port (open-data file-name))) (inner ())))

; bounding box dimension getters
(define (lat-width bbox) (- (of "max-lat" bbox) (of "min-lat" bbox)))
(define (lon-width bbox) (- (of "max-lon" bbox) (of "min-lon" bbox)))

; return a function that returns random points within a bounding box
(define (rand-boxed-points bbox) 
    (let ((rand-lat (rand-range (lat-width bbox) (of "min-lat" bbox)))
    (rand-lon (rand-range (lon-width bbox) (of "min-lon" bbox))))
    (lambda () (cons (rand-lat) (rand-lon)))))

; A bbox holds the bounding dimensions of a (list (lat . lon)) and the
; list itself
(define (new-bbox points) (let* ((lats (map car points))
    (lons (map cdr points))) 
    (list (cons "max-lon" (foldr max -180.0 lons))
    (cons "min-lon" (foldr min 180.0 lons)) 
    (cons "max-lat" (foldr max -90.0 lats)) 
    (cons "min-lat" (foldr min 90.0 lats))
    (cons "points" points))))

; find whether a point (lat . lon) is within bounding box `bb`
(define (is-external-lat bb point) 
    (or (< (lat point) (cdr (assoc "min-lat" bb))) 
        (> (lat point) (cdr (assoc "max-lat" bb)))))
(define (is-external-lon bb point) 
    (or (< (lon point) (cdr (assoc "min-lon" bb))) 
        (> (lon point) (cdr (assoc "max-lon" bb)))))
(define (is-external bb point) 
    (and (is-external-lat bb point) (is-external-lon bb point)))

; return a point that is external to a boundning box
; recursively calls rand-pos until a mathcing point is found
(define (get-external-point bb) 
    (define (inner bb point) 
        (if (is-external bb point) point (inner bb (rand-pos))))
    (inner bb (rand-pos)))

; open and read a polygon csv lon,lat,elev convert to a list of (lat . lon)
; create a bounding box and find a point external to that 
(define (import-data file-name) (let* ((points (read-points file-name))
    (bbox (new-bbox points))
    (external-point (get-external-point bbox))) 
    (cons (cons "ext-point" external-point) bbox)))

; Create an arc ((lat . lon) . (lat .lon)) from the first two points in  a list
; of points
(define (top-arc polygon) (cons (car polygon) (cadr polygon)))

; return count + 1 if arc and (top-arc polygon) intersect otherwise count
(define (add-intersect count arc polygon) 
    (if (test-arcs arc (top-arc polygon)) (+ 1 count) count))

; count the number of intersections between an arc and the arcs on a polygon
(define (poly-walk-compare arc polygon count) 
    (if (> 2 (length polygon)) count 
    (poly-walk-compare arc (cdr polygon) (add-intersect count arc polygon))))

; #t if an arc has an end in the polygon
; assuming one end of the arc is outside of the polygon
; if an arc has an odd number of intersections with a polygons boundary
; then it has one end inside of the polygon
; and even number and both ends are outside
(define (end-in-poly arc polygon) 
    (let ((count (poly-walk-compare arc polygon 0))) 
    (equal? 1 (modulo count 2))))

; pretty print a ((lat . lon)) point
; useful fro copy paste into google maps...
(define (display-point point) 
    (write (caar point))(display ", ")
    (write (cdar point))(display "\n"))


; scale v as in 0 - scale from min - max
(define (scale v min max scale) 
    (inexact->exact (round (* (/(- v min) (- max min)) scale))))

;create a point normalizer from a bbox
;that sets the minimum as zero and the maximum as 100 for the width and scaling
;the height against that from 0
;also flips from lat . lon to col . row
(define (create-normalizer bbox) (lambda (point) 
    (let* ((latscale (* 100 (/ (lat-width bbox) (lon-width bbox))))
    (row (scale (lat point) (of "min-lat" bbox) (of "max-lat" bbox) latscale))
    (col (scale (lon point) (of "min-lon" bbox) (of "max-lon" bbox) 100)))
    (cons col row))))

; translate a list of points into an assoc list of file (x,y)
; uses the bbox to determine the bounds
(define (normalize-points bbox points) 
    (let * ((normalizer create-normalizer bbox))
    (map (lambda (point) normalizer point) points)))

; write a set of interior points and their middle to a file in a way that
; hopefully describes them visually
;(define write-map (lambda (points center)))

; estimate the midpoint of polygon described in the lon,lat,elev csv 
; at `file-name` using `count` random samples
(define (get-midpoint count file-name) (let* ((data (import-data file-name))
    (rand-point (rand-boxed-points data))
    (rand-arc (lambda () (cons (rand-point) (of "ext-point" data))))
    (arcs (n-results count rand-arc))
    (interior-points (map car (filter arcs 
        (lambda (arc) (end-in-poly arc (of "points" data))))))
    (lat (/ (foldr + 0 (map car interior-points)) (length interior-points)))
    (lon (/ (foldr + 0 (map cdr interior-points)) (length interior-points))))
    (cons lat lon)))

(define (t) (get-midpoint 50 "fifty-states.csv"))




