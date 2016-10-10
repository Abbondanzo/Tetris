;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Tetris-Dev) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; World Settings
(define BLOCKSIZE 20)
(define GRID/ROWS 20)
(define GRID/COLS 10)
(define EMPTYWORLD (empty-scene (* GRID/COLS BLOCKSIZE) (* GRID/ROWS BLOCKSIZE)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions

;; A Block is a (make-block Number Number Color)
(define-struct block (x y color))

;; A Tetra is a (make-tetra Posn BSet)
;; The center point is the point around which the tetra rotates
;; when it spins.
(define-struct tetra (center blocks))
 
;; A Set of Blocks (BSet) is one of:
;; - empty
;; - (cons Block BSet)
;; Order does not matter.
 
;; A World is a (make-world Tetra BSet Score)
;; The BSet represents the pile of blocks at the bottom of the screen.
;; The Score represents the number of blocks that become part of Bset
(define-struct world (tetra pile score))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tetras

(define O (make-tetra (make-posn (- (/ (* GRID/COLS BLOCKSIZE) 2)
                                    (/ BLOCKSIZE 2)) (/ (* -3 BLOCKSIZE) 2))
                      (list (make-block 0 0 "green") (make-block 0 -1 "green")
                            (make-block -1 -1 "green")
                            (make-block -1 0 "green"))))
(define I (make-tetra (make-posn (- (/ (* GRID/COLS BLOCKSIZE) 2)
                                    (/ BLOCKSIZE 2)) (/ (* -1 BLOCKSIZE) 2))
                      (list (make-block -1 0 "blue") (make-block 0 0 "blue")
                            (make-block 1 0 "blue")
                            (make-block 2 0 "blue"))))
(define L (make-tetra (make-posn (- (/ (* GRID/COLS BLOCKSIZE) 2)
                                    (/ BLOCKSIZE 2)) (/ (* -3 BLOCKSIZE) 2))
                      (list (make-block -1 0 "purple") (make-block 0 0 "purple")
                            (make-block 1 -1 "purple")
                            (make-block 1 0 "purple"))))
(define J (make-tetra (make-posn (- (/ (* GRID/COLS BLOCKSIZE) 2)
                                    (/ BLOCKSIZE 2)) (/ (* -3 BLOCKSIZE) 2))
                      (list (make-block -1 -1 "cyan") (make-block -1 0 "cyan")
                            (make-block 0 0 "cyan") (make-block 1 0 "cyan"))))
(define T (make-tetra (make-posn (- (/ (* GRID/COLS BLOCKSIZE) 2)
                                    (/ BLOCKSIZE 2)) (/ (* -3 BLOCKSIZE) 2))
                      (list (make-block -1 0 "orange") (make-block 0 0 "orange")
                            (make-block 0 -1 "orange")
                            (make-block 1 0 "orange"))))
(define Z (make-tetra (make-posn (- (/ (* GRID/COLS BLOCKSIZE) 2)
                                    (/ BLOCKSIZE 2)) (/ (* -3 BLOCKSIZE) 2))
                      (list (make-block -1 -1 "pink") (make-block 0 -1 "pink")
                            (make-block 0 0 "pink") (make-block 1 0 "pink"))))
(define S (make-tetra (make-posn (- (/ (* GRID/COLS BLOCKSIZE) 2)
                                    (/ BLOCKSIZE 2)) (/ (* -3 BLOCKSIZE) 2))
                      (list (make-block -1 0 "red") (make-block 0 -1 "red")
                            (make-block 0 0 "red") (make-block 1 -1 "red"))))

;;; num->tetra : Tetra-Number -> Tetra
;;; A Tetra-Number is a Number 0 to 6 inclusive
(define (num-tetra num)
  (cond [(= num 0) O]
        [(= num 1) I]
        [(= num 2) L]
        [(= num 3) J]
        [(= num 4) T]
        [(= num 5) Z]
        [else S]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collision

;;; touch-bottom? : Tetra -> Boolean
(define (touch-bottom? tetra)
  (cond [(empty? (tetra-blocks tetra)) #false]
        [else (or (> (+ (posn-y (tetra-center tetra))
                        (* BLOCKSIZE (block-y (first (tetra-blocks tetra)))))
                     (- (* GRID/ROWS BLOCKSIZE) (/ (* 3 BLOCKSIZE) 2)))
                  (touch-bottom? (make-tetra (tetra-center tetra)
                                             (rest (tetra-blocks tetra)))))]))

;;; block-here? : World -> Boolean
(define (block-here? w)
  (cond [(empty? (tetra-blocks (world-tetra w))) #false]
        [else (or (block-parse (first (tetra-blocks (world-tetra w)))
                               (world-pile w) (posn-x (tetra-center
                                                       (world-tetra w)))
                               (posn-y (tetra-center (world-tetra w))))
                   (block-here? (make-world (make-tetra (tetra-center
                                                         (world-tetra w))
                                                        (rest (tetra-blocks
                                                              (world-tetra w))))
                                            (world-pile w) (world-score w))))]))

;;; block-parse : Block Bset Number Number -> Boolean
(define (block-parse block bset offset-x offset-y)
  (cond [(empty? bset) #false]
        [else (or (and (= (+ offset-x (* BLOCKSIZE (block-x block)))
                          (block-x (first bset))) (= (+ BLOCKSIZE offset-y
                                                        (* BLOCKSIZE
                                                           (block-y block)))
                                                     (block-y (first bset))))
                  (block-parse block (rest bset) offset-x offset-y))]))

;;; side-here? : World +/-Blocksize -> Boolean
;;; Returns true if World's Tetra's Bset's future position left/right comes in contact with World's Bset
(define (side-here? w bsize)
  (cond [(empty? (tetra-blocks (world-tetra w))) #false]
        [else (or (side-parse (first (tetra-blocks (world-tetra w)))
                              (world-pile w) (posn-x (tetra-center
                                                      (world-tetra w)))
                              (posn-y (tetra-center (world-tetra w))) bsize)
                  (< (+ bsize (posn-x (tetra-center (world-tetra w)))
                        (* (block-x (first (tetra-blocks (world-tetra w))))
                           BLOCKSIZE)) 0)
                  (> (+ bsize (posn-x (tetra-center (world-tetra w)))
                        (* (block-x (first (tetra-blocks (world-tetra w))))
                           BLOCKSIZE)) (* GRID/COLS BLOCKSIZE)) 
                  (side-here? (make-world (make-tetra (tetra-center
                                                       (world-tetra w))
                                                      (rest (tetra-blocks
                                                             (world-tetra w))))
                                          (world-pile w) (world-score w))
                              bsize))]))

;;; side-parse : Block Bset Number Number +/-Blocksize -> Boolean
;;; Checks if there are any blocks in the bset that the tetra would collide with
(define (side-parse block bset offset-x offset-y bsize)
  (cond [(empty? bset) #false]
        [else (or (and (= (+ bsize offset-x (* BLOCKSIZE (block-x block)))
                          (block-x (first bset))) (= (+ offset-y (* BLOCKSIZE
                                                               (block-y block)))
                                                     (block-y (first bset))))
                  (side-parse block (rest bset) offset-x offset-y bsize))]))

;;; rotate-here? : World -> Boolean
;;; Returns true if World's Tetra's Bset's future position rotation comes in contact with World's Bset.
(define (rotate-here? w)
  (cond [(empty? (tetra-blocks (world-tetra w))) #false]
        [else (or (rotate-parse (first (tetra-blocks (world-tetra w)))
                                (world-pile w)
                                (posn-x (tetra-center (world-tetra w)))
                                (posn-y (tetra-center (world-tetra w))))
                  (< (+ (* BLOCKSIZE (- 0 (block-y (first (tetra-blocks
                                                           (world-tetra w))))))
                        (posn-x (tetra-center (world-tetra w)))) 0) ; Check if future rotated blocks go off left wall
                  (> (+ (* BLOCKSIZE (- 0 (block-y (first (tetra-blocks
                                                           (world-tetra w))))))
                        (posn-x (tetra-center (world-tetra w))))
                     (* GRID/COLS BLOCKSIZE)) ; Check if future rotated blocks go off right wall
                  (> (+ (* BLOCKSIZE (block-x (first (tetra-blocks
                                                      (world-tetra w)))))
                        (posn-y (tetra-center (world-tetra w))))
                     (* GRID/ROWS BLOCKSIZE)) ; Check if future rotated blocks go thru floor
                  (rotate-here? (make-world (make-tetra (tetra-center
                                                         (world-tetra w))
                                                        (rest (tetra-blocks
                                                              (world-tetra w))))
                                            (world-pile w)
                                            (world-score w))))]))

;;; rotate-parse : Block Bset Number Number -> Boolean
;;; Checks if a specifc Tetra's Block future rotation would collide with any of the blocks in World's Bset.
(define (rotate-parse block bset offset-x offset-y)
  (cond [(empty? bset) #false]
        [else (or (and (= (+ offset-x (* BLOCKSIZE (- 0 (block-y block))))
                          (block-x (first bset))) (= (+ offset-y (* BLOCKSIZE
                                                               (block-x block)))
                                                     (block-y (first bset))))
                  (rotate-parse block (rest bset) offset-x offset-y))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; World
;;; next-world : World -> World
;;; Progresses world state from given (or lack of) user input
(define (next-world w)
  (cond [(block-here? w) (make-world (num-tetra (random 7))
                                     (append (world-pile w) (modify-pile w))
                                     (+ 4 (world-score w)))]
        [(touch-bottom? (world-tetra w)) (make-world (num-tetra (random 7))
                                                     (append (world-pile w)
                                                             (modify-pile w))
                                                     (+ 4 (world-score w)))]
        [(full-row? (- (* GRID/ROWS BLOCKSIZE) (/ BLOCKSIZE 2)) w)
         (shift-pile-down (- (* GRID/ROWS BLOCKSIZE) (/ BLOCKSIZE 2)) w)]
        [else (make-world (make-tetra (make-posn (posn-x (tetra-center
                                                          (world-tetra w)))
                                                 (+ BLOCKSIZE (posn-y
                                                           (tetra-center
                                                             (world-tetra w)))))
                                      (tetra-blocks (world-tetra w)))
                          (world-pile w)
                          (world-score w))]))

;;; full-row? : Number World -> Boolean
;;; Consumes world and checks every row if it is full
(define (full-row? y w)
  (cond [(< y 0) #false]
        [(break-apart-row 1 y w) #true]
        [else (full-row? (- y BLOCKSIZE) w)]))

;;; break-apart-row : Number Number World -> Boolean
;;; Breaks apart row at specific Y coordinate to see if row is full
(define (break-apart-row count y w)
  (cond [(empty? (world-pile w)) #false]
        [(and (= (block-y (first (world-pile w))) y) (= count GRID/COLS)) #true]
        [(and (= (block-y (first (world-pile w))) y) (< count GRID/COLS))
         (break-apart-row (+ 1 count) y (make-world (world-tetra w)
                                                    (rest (world-pile w))
                                                    (world-score w)))]
        [else (break-apart-row count y (make-world (world-tetra w)
                                                   (rest (world-pile w))
                                                   (world-score w)))]))

;;; shift-pile-down : World -> World
(define (shift-pile-down y w)
  (cond [(< y 0) w]
        [(break-apart-row 1 y w)
         (make-world (world-tetra w)
                     (drop-after-remove y
                                       (remove-and-drop y (world-pile w)))
                     (world-score w))]
        [else (shift-pile-down (- y BLOCKSIZE) w)]))

;;; remove-and-drop : Number Bset -> Bset
;;; Breaks apart row at specific Y coordinate and removes it 
(define (remove-and-drop y bset)
  (cond [(empty? bset) bset]
        [(= (block-y (first bset)) y) (remove-and-drop y (rest bset))]
        [else (cons (first bset) (remove-and-drop y (rest bset)))]))

;;; drop-after-remove : Number Bset -> Bset
;;; After a row has been removed, drops all blocks above by one
(define (drop-after-remove y bset)
  (cond [(empty? bset) bset]
        [(< (block-y (first bset)) y) (cons (make-block (block-x (first bset))
                                                        (+ BLOCKSIZE (block-y
                                                                  (first bset)))
                                                     (block-color (first bset)))
                                            (drop-after-remove y (rest bset)))]
        [else (cons (make-block (block-x (first bset))
                                (block-y (first bset))
                                (block-color (first bset)))
                    (drop-after-remove y (rest bset)))]))


;;; modify-pile : World -> List
;;; Consumes tetra in World and adds blocks to World Bset
(define (modify-pile world)
  (correct-position-pile (tetra-blocks (world-tetra world)) (posn-x
                                                          (tetra-center
                                                           (world-tetra world)))
                         (posn-y (tetra-center (world-tetra world)))))

;;; correct-position-pile : Bset Number Number -> List
;;; Consumes World's Bset and the x/y coordinates of center of Tetra and adds them to World Bset
(define (correct-position-pile pile-list x-offset y-offset)
  (cond [(empty? pile-list) '()]
        [else (cons (make-block (+ x-offset (* BLOCKSIZE (block-x
                                                          (first pile-list))))
                                (+ y-offset (* BLOCKSIZE (block-y
                                                          (first pile-list))))
                                (block-color (first pile-list)))
                    (correct-position-pile (rest pile-list)
                                           x-offset y-offset))]))

;;; key-handler : World Key-Event -> World
;;; Gets key pressed and returns a world state.
(define (key-handler w a-key)
  (cond [(key=? a-key "left") (key-collision w a-key)]
        [(key=? a-key "right") (key-collision w a-key)]
        [(key=? a-key "s") (rotate-collision w)]
        [(key=? a-key "a") (rotate-collision (rotate-collision
                                              (rotate-collision w)))]
        [else w]))

;;; rotate-collision : World -> World
;;; Consumes world state and returns new world if rotating the tetra does not collide with walls or other blocks.
(define (rotate-collision w)
  (cond [(rotate-here? w) w]
        [else (make-world (make-tetra (tetra-center (world-tetra w))
                                      (rotate-recur w)) (world-pile w)
                                                        (world-score w))]))

;;; rotate-recur : World -> Bset
;;; Consumes world state and returns a new block set for Tetra.
(define (rotate-recur w)
  (cond [(empty? (tetra-blocks (world-tetra w))) '()]
        [else (cons (block-rotate-cw (tetra-center (world-tetra w))
                                     (first (tetra-blocks (world-tetra w))))
                    (rotate-recur (make-world (make-tetra (tetra-center
                                                           (world-tetra w))
                                                          (rest (tetra-blocks
                                                              (world-tetra w))))
                                            (world-pile w) (world-score w))))]))

;; block-rotate-cw : Posn Block -> Block
;; Rotates the given block 90 counterclockwise around the posn.
(define (block-rotate-cw c b)
  (make-block (- 0 (block-y b))
              (block-x b)
              (block-color b)))

;;; key-collision : World Key-event -> World
;;; Detects if left or right key and returns world state.
(define (key-collision w a-key)
  (cond [(key=? a-key "left") (move-left/right w (- 0 BLOCKSIZE))]
        [else (move-left/right w (+ 0 BLOCKSIZE))]))

;;; move-left/right : World FutureBlock -> World
;;; Moves tetra left or right unless collision is detected, consumes world and positive/negative future position offset; returns a world
(define (move-left/right w bsize)
  (cond [(side-here? w bsize) w]
        [else (make-world (make-tetra (make-posn (+ (posn-x (tetra-center
                                                             (world-tetra w)))
                                                    bsize)
                                                 (posn-y (tetra-center
                                                          (world-tetra w))))
                                      (tetra-blocks (world-tetra w)))
                          (world-pile w)
                          (world-score w))]))

;;; block + image : Block -> Image
;;; Takes a Block structure and returns a block with specified color and size
(define (block+image block)
  (overlay (rectangle (- BLOCKSIZE 1) (- BLOCKSIZE 1) "solid"
                      (block-color block)) (rectangle BLOCKSIZE BLOCKSIZE
                                                      "solid" "black"))) 

;;; draw-world : World -> Tetra
;;; Consumes World State and passes the tetra to draw-tetra function.
(define (draw-world w)
  (place-image (text (string-append "Score: " (number->string (world-score w)))
                     12 "blue" )
               (+ 8 (/ (image-width (text (string-append "Score: "
                                                         (number->string
                                                          (world-score w)))
                                          12 "blue" )) 2))
               (image-height (text (string-append "Score: " (number->string
                                                             (world-score w)))
                                   12 "blue" ))
               (draw-pile w)))

;;; draw-pile : World -> Image
(define (draw-pile w)
  (cond [(empty? (world-pile w)) (draw-tetra (world-tetra w))]
        [else (place-image (block+image (first (world-pile w)))
                           (block-x
                            (first
                             (world-pile w))) (block-y (first (world-pile w)))
                                              (draw-pile (make-world
                                                          (world-tetra w)
                                                          (rest (world-pile w))
                                                          (world-score w))))])) 
   
;;; draw-tetra : Tetra -> Image
;;; Consumes Tetra center position and draws blocks using BSet
(define (draw-tetra tetra)
  (cond [(empty? (tetra-blocks tetra)) EMPTYWORLD]
        [else (place-image (block+image (first (tetra-blocks tetra)))
                           (+ (* (block-x (first (tetra-blocks tetra)))
                                 BLOCKSIZE) (posn-x (tetra-center tetra)))
                           (+ (* (block-y (first (tetra-blocks tetra)))
                                 BLOCKSIZE) (posn-y (tetra-center tetra)))
                           (draw-tetra (make-tetra (tetra-center tetra)
                                               (rest (tetra-blocks tetra)))))]))
;;; check-overflow : World -> Boolean
;;; Checks if any blocks in Bset are above the world and returns true
(define (check-overflow w)
  (cond [(empty? (world-pile w)) #false]
        [(< (block-y (first (world-pile w))) 0) #true]
        [else (check-overflow (make-world (world-tetra w) (rest (world-pile w))
                                          (world-score w)))]))

;;; end-game : World -> Image
;;; Draws the final end-game screen
(define (end-game w)
  (place-image (text (string-append "Score: " (number->string
                                               (- (world-score w) 4)))
                     24 "blue" ) (/ (* GRID/COLS BLOCKSIZE) 2)
                                 (/ (* GRID/ROWS BLOCKSIZE) 2)
                                 (place-image
                                  (text "Game Over" 24 "blue")
                                  (/ (* GRID/COLS BLOCKSIZE) 2)
                                  (- (/(* GRID/ROWS BLOCKSIZE) 2) 24)
                                                                EMPTYWORLD)))
   
(big-bang (make-world (num-tetra (random 7)) '() 0)
     (on-tick next-world 0.2)
     (on-key key-handler)
     (to-draw draw-world)
     (stop-when check-overflow end-game))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(check-expect (touch-bottom? (make-tetra (make-posn 90 210) '())) #false)
(check-expect (touch-bottom? (make-tetra (make-posn 90 210)
                                         (list (make-block -1 0 "purple")
                                               (make-block 0 0 "purple")
                                               (make-block 1 -1 "purple")
                                               (make-block 1 0 "purple"))))
              #false)
(check-expect (touch-bottom? (make-tetra (make-posn 90 390)
                                         (list (make-block -1 0 "purple")
                                               (make-block 0 0 "purple")
                                               (make-block 1 -1 "purple")
                                               (make-block 1 0 "purple"))))
              #true)
(check-expect (block-here? (make-world (make-tetra (make-posn 90 70)
                                                   (list
                                                    (make-block -1 0 "orange")
                                                    (make-block 0 0 "orange")
                                                    (make-block 0 -1 "orange")
                                                    (make-block 1 0 "orange")))
                                       '() 0)) #false)
(check-expect (block-here? (make-world (make-tetra (make-posn 30 370)
                                                   (list
                                                    (make-block -1 0 "red")
                                                    (make-block 0 -1 "red")
                                                    (make-block 0 0 "red")
                                                    (make-block 1 -1 "red")))
                                       (list (make-block 10 390 "orange")
                                             (make-block 30 390 "orange")
                                             (make-block 30 370 "orange")
                                             (make-block 50 390 "orange")) 4))
              #true)
(check-expect (block-here? (make-world (make-tetra (make-posn 30 370)
                                                   empty)
                                       (list (make-block 10 390 "orange")
                                             (make-block 30 390 "orange")
                                             (make-block 30 370 "orange")
                                             (make-block 50 390 "orange")) 4))
              #false)
(check-expect (block-parse (make-block -1 0 "purple")
                           (list (make-block 70 390 "blue")
                                 (make-block 90 390 "blue")
                                 (make-block 110 390 "blue")
                                 (make-block 130 390 "blue")
                                 (make-block 90 370 "green")
                                 (make-block 90 350 "green")
                                 (make-block 70 350 "green")
                                 (make-block 70 370 "green"))
                           (posn-x (make-posn 90 -30))
                           (posn-y (make-posn 90 -30))) #false)
(check-expect (block-parse (make-block -1 0 "purple")
                           empty
                           (posn-x (make-posn 90 -30))
                           (posn-y (make-posn 90 -30))) #false)
(check-expect (block-parse (make-block -1 0 "purple")
                           (list (make-block 70 390 "blue")
                                 (make-block 90 390 "blue")
                                 (make-block 110 390 "blue")
                                 (make-block 130 390 "blue")
                                 (make-block 90 370 "green")
                                 (make-block 90 350 "green")
                                 (make-block 70 350 "green")
                                 (make-block 70 370 "green"))
                           (posn-x (make-posn 90 330))
                           (posn-y (make-posn 90 330))) #true)
(check-expect (side-here? (make-world
 (make-tetra (make-posn 90 50) (list (make-block -1 -1 "pink")
                                     (make-block 0 -1 "pink")
                                     (make-block 0 0 "pink")
                                     (make-block 1 0 "pink")))
 (list (make-block 90 390 "green")
       (make-block 90 370 "green")
       (make-block 70 370 "green")
       (make-block 70 390 "green"))
 4) BLOCKSIZE) #false)
(check-expect (side-here? (make-world
 (make-tetra (make-posn 50 370) (list (make-block -1 -1 "pink")
                                     (make-block 0 -1 "pink")
                                     (make-block 0 0 "pink")
                                     (make-block 1 0 "pink")))
 (list (make-block 90 390 "green")
       (make-block 90 370 "green")
       (make-block 70 370 "green")
       (make-block 70 390 "green"))
 4) BLOCKSIZE) #true)
(check-expect (side-here? (make-world
 (make-tetra (make-posn 90 50) empty)
 (list (make-block 90 390 "green")
       (make-block 90 370 "green")
       (make-block 70 370 "green")
       (make-block 70 390 "green"))
 4) BLOCKSIZE) #false)

(check-expect (side-parse (make-block -1 0 "purple")
                           (list (make-block 70 390 "blue")
                                 (make-block 90 390 "blue")
                                 (make-block 110 390 "blue")
                                 (make-block 130 390 "blue")
                                 (make-block 90 370 "green")
                                 (make-block 90 350 "green")
                                 (make-block 70 350 "green")
                                 (make-block 70 370 "green"))
                           (posn-x (make-posn 90 -30))
                           (posn-y (make-posn 90 -30)) BLOCKSIZE) #false)
(check-expect (side-parse (make-block -1 0 "purple")
                           empty
                           (posn-x (make-posn 90 -30))
                           (posn-y (make-posn 90 -30)) BLOCKSIZE) #false)
(check-expect (side-parse (make-block -1 0 "purple")
                           (list (make-block 70 390 "blue")
                                 (make-block 90 390 "blue")
                                 (make-block 110 390 "blue")
                                 (make-block 130 390 "blue")
                                 (make-block 90 370 "green")
                                 (make-block 90 350 "green")
                                 (make-block 70 350 "green")
                                 (make-block 70 370 "green"))
                           (posn-x (make-posn 90 350))
                           (posn-y (make-posn 90 350)) BLOCKSIZE) #true)

(check-expect (rotate-here? (make-world (make-tetra
                                         (make-posn 90 110)
                                         (list (make-block -1 -1 "pink")
                                               (make-block 0 -1 "pink")
                                               (make-block 0 0 "pink")
                                               (make-block 1 0 "pink")))
                                        '() 0)) #false)
(check-expect (rotate-here? (make-world (make-tetra
                                         (make-posn 190 330)
                                         (list (make-block 0 1 "purple")
                                               (make-block 0 0 "purple")
                                               (make-block -1 -1 "purple")
                                               (make-block 0 -1 "purple")))
                                        '() 0)) #true)

(check-expect (rotate-here? (make-world (make-tetra
                                         (make-posn 90 110)
                                         empty)
                                        '() 0)) #false)

(check-expect (rotate-parse (make-block -1 0 "red")  (list
                                 (make-block 70 390 "blue")
                                 (make-block 90 390 "blue")
                                 (make-block 110 390 "blue")
                                 (make-block 130 390 "blue")
                                 (make-block 90 370 "green")
                                 (make-block 90 350 "green")
                                 (make-block 70 350 "green")
                                 (make-block 70 370 "green"))
                            (posn-x (make-posn 90 350))
                           (posn-y (make-posn 90 350))) #false)
(check-expect (rotate-parse (make-block -1 0 "red")  empty
                            (posn-x (make-posn 90 350))
                           (posn-y (make-posn 90 350))) #false)

(check-expect (next-world (make-world (make-tetra (make-posn 90 170)
                                                 (list (make-block -1 0 "blue")
                                                       (make-block 0 0 "blue")
                                                       (make-block 1 0 "blue")
                                                       (make-block 2 0 "blue")))
                                      '() 0))
              (make-world (make-tetra (make-posn 90 190)
                                      (list (make-block -1 0 "blue")
                                            (make-block 0 0 "blue")
                                            (make-block 1 0 "blue")
                                            (make-block 2 0 "blue"))) '() 0))
(check-random (next-world (make-world (make-tetra
                                       (make-posn 90 310)
                                       (list (make-block -1 0 "purple")
                                             (make-block 0 0 "purple")
                                             (make-block 1 -1 "purple")
                                             (make-block 1 0 "purple")))
                                      (list (make-block 90 330 "blue")
                                            (make-block 90 350 "blue")
                                            (make-block 90 370 "blue")
                                            (make-block 90 390 "blue"))
                                      4)) (make-world (num-tetra (random 7))
                                     (list (make-block 90 330 "blue")
                                           (make-block 90 350 "blue")
                                           (make-block 90 370 "blue")
                                           (make-block 90 390 "blue")
                                           (make-block 70 310 "purple")
                                           (make-block 90 310 "purple")
                                           (make-block 110 290 "purple")
                                           (make-block 110 310 "purple")) 8))
(check-random (next-world (make-world (make-tetra
                                       (make-posn 90 390)
                                       (list (make-block -1 -1 "pink")
                                             (make-block 0 -1 "pink")
                                             (make-block 0 0 "pink")
                                             (make-block 1 0 "pink")))
                                      '() 0)) (make-world (num-tetra (random 7))
                                     (list (make-block 70 370 "pink")
                                           (make-block 90 370 "pink")
                                           (make-block 90 390 "pink")
                                           (make-block 110 390 "pink")) 4))
(check-expect (next-world (make-world (make-tetra (make-posn 90 150)
             (list (make-block 0 0 "green")
                   (make-block 0 -1 "green")
                   (make-block -1 -1 "green")
                   (make-block -1 0 "green")))
 (list
  (make-block 10 370 "cyan")
  (make-block 10 390 "cyan")
  (make-block 30 390 "cyan")
  (make-block 30 370 "cyan")
  (make-block 50 390 "cyan")
  (make-block 50 370 "pink")
  (make-block 70 370 "pink")
  (make-block 70 390 "pink")
  (make-block 90 390 "pink")
  (make-block 90 370 "pink")
  (make-block 110 370 "cyan")
  (make-block 110 390 "cyan")
  (make-block 130 390 "cyan")
  (make-block 150 390 "cyan")
  (make-block 170 390 "blue")
  (make-block 190 350 "blue")
  (make-block 190 370 "blue")
  (make-block 190 390 "blue"))
 16)) (make-world
 (make-tetra
  (make-posn 90 150)
  (list
   (make-block 0 0 "green")
   (make-block 0 -1 "green")
   (make-block -1 -1 "green")
   (make-block -1 0 "green")))
 (list
  (make-block 10 390 "cyan")
  (make-block 30 390 "cyan")
  (make-block 50 390 "pink")
  (make-block 70 390 "pink")
  (make-block 90 390 "pink")
  (make-block 110 390 "cyan")
  (make-block 190 370 "blue")
  (make-block 190 390 "blue"))
 16))
(check-expect (full-row? 370 (make-world
                              (make-tetra (make-posn 90 190)
                                          (list (make-block -1 0 "blue")
                                                (make-block 0 0 "blue")
                                                (make-block 1 0 "blue")
                                                (make-block 2 0 "blue")))
                                         '() 0)) #false)
(check-expect (full-row? 390 (make-world
 (make-tetra (make-posn 90 150) (list (make-block 0 0 "green")
                                      (make-block 0 -1 "green")
                                      (make-block -1 -1 "green")
                                      (make-block -1 0 "green")))
 (list
  (make-block 10 370 "cyan")
  (make-block 10 390 "cyan")
  (make-block 30 390 "cyan")
  (make-block 50 390 "cyan")
  (make-block 50 370 "pink")
  (make-block 70 370 "pink")
  (make-block 70 390 "pink")
  (make-block 90 390 "pink")
  (make-block 110 370 "cyan")
  (make-block 110 390 "cyan")
  (make-block 130 390 "cyan")
  (make-block 150 390 "cyan")
  (make-block 170 390 "blue")
  (make-block 190 350 "blue")
  (make-block 190 370 "blue")
  (make-block 190 390 "blue"))
 16)) #true)
(check-expect (full-row? -10 (make-world (make-tetra
                                          (make-posn 90 190)
                                          (list (make-block -1 0 "blue")
                                                (make-block 0 0 "blue")
                                                (make-block 1 0 "blue")
                                                (make-block 2 0 "blue")))
                                         '() 0))
              #false)

(check-expect (break-apart-row 9 390 (make-world
 (make-tetra (make-posn 90 150) (list
                                 (make-block 0 0 "green")
                                 (make-block 0 -1 "green")
                                 (make-block -1 -1 "green")
                                 (make-block -1 0 "green")))
 (list
  (make-block 10 370 "cyan")
  (make-block 10 390 "cyan")
  (make-block 30 390 "cyan")
  (make-block 50 390 "cyan")
  (make-block 50 370 "pink")
  (make-block 70 370 "pink")
  (make-block 70 390 "pink")
  (make-block 90 390 "pink")
  (make-block 110 370 "cyan")
  (make-block 110 390 "cyan")
  (make-block 130 390 "cyan")
  (make-block 150 390 "cyan")
  (make-block 170 350 "blue")
  (make-block 190 350 "blue")
  (make-block 190 370 "blue")
  (make-block 190 390 "blue"))
 16)) #true)
(check-expect (break-apart-row 9 390 (make-world
 (make-tetra (make-posn 90 150) (list
                                 (make-block 0 0 "green")
                                 (make-block 0 -1 "green")
                                 (make-block -1 -1 "green")
                                 (make-block -1 0 "green")))
 empty
 16)) #false)

(check-expect (shift-pile-down 390 (make-world (make-tetra
                                                (make-posn 90 190)
                                                (list (make-block -1 0 "blue")
                                                      (make-block 0 0 "blue")
                                                      (make-block 1 0 "blue")
                                                      (make-block 2 0 "blue")))
                                               '() 0))
              (make-world (make-tetra (make-posn 90 190)
                                      (list (make-block -1 0 "blue")
                                            (make-block 0 0 "blue")
                                            (make-block 1 0 "blue")
                                            (make-block 2 0 "blue")))
                          '() 0))
(check-expect (shift-pile-down -10 (make-world (make-tetra
                                                (make-posn 90 190)
                                                (list (make-block -1 0 "blue")
                                                      (make-block 0 0 "blue")
                                                      (make-block 1 0 "blue")
                                                      (make-block 2 0 "blue")))
                                               '() 0))
              (make-world (make-tetra (make-posn 90 190)
                                      (list (make-block -1 0 "blue")
                                            (make-block 0 0 "blue")
                                            (make-block 1 0 "blue")
                                            (make-block 2 0 "blue")))
                          '() 0))
(check-expect (shift-pile-down 390 (make-world
 (make-tetra (make-posn 90 150)
             (list (make-block 0 0 "green")
                   (make-block 0 -1 "green")
                   (make-block -1 -1 "green")
                   (make-block -1 0 "green")))
 (list
  (make-block 10 370 "cyan")
  (make-block 10 390 "cyan")
  (make-block 30 390 "cyan")
  (make-block 30 370 "cyan")
  (make-block 50 390 "cyan")
  (make-block 50 370 "pink")
  (make-block 70 370 "pink")
  (make-block 70 390 "pink")
  (make-block 90 390 "pink")
  (make-block 90 370 "pink")
  (make-block 110 370 "cyan")
  (make-block 110 390 "cyan")
  (make-block 130 390 "cyan")
  (make-block 150 390 "cyan")
  (make-block 170 390 "blue")
  (make-block 190 350 "blue")
  (make-block 190 370 "blue")
  (make-block 190 390 "blue"))
 16)) (make-world
 (make-tetra
  (make-posn 90 150)
  (list
   (make-block 0 0 "green")
   (make-block 0 -1 "green")
   (make-block -1 -1 "green")
   (make-block -1 0 "green")))
 (list
  (make-block 10 390 "cyan")
  (make-block 30 390 "cyan")
  (make-block 50 390 "pink")
  (make-block 70 390 "pink")
  (make-block 90 390 "pink")
  (make-block 110 390 "cyan")
  (make-block 190 370 "blue")
  (make-block 190 390 "blue"))
 16))

(check-expect (remove-and-drop 390 empty) empty)
(check-expect (remove-and-drop 390 (list
  (make-block 10 370 "cyan")
  (make-block 10 390 "cyan")
  (make-block 30 390 "cyan")
  (make-block 50 390 "cyan")
  (make-block 50 370 "pink")
  (make-block 70 370 "pink")
  (make-block 70 390 "pink")
  (make-block 90 390 "pink")
  (make-block 110 370 "cyan")
  (make-block 110 390 "cyan")
  (make-block 130 390 "cyan")
  (make-block 150 390 "cyan")
  (make-block 170 350 "blue")
  (make-block 190 350 "blue")
  (make-block 190 370 "blue")
  (make-block 190 390 "blue")))
  (list
  (make-block 10 370 "cyan")
  (make-block 50 370 "pink")
  (make-block 70 370 "pink")
  (make-block 110 370 "cyan")
  (make-block 170 350 "blue")
  (make-block 190 350 "blue")
  (make-block 190 370 "blue")))

(check-expect (drop-after-remove 390 empty) empty)
(check-expect (drop-after-remove 370 (list
  (make-block 10 350 "cyan")
  (make-block 170 350 "blue")
  (make-block 190 350 "blue")
  (make-block 190 390 "blue")))
              (list
  (make-block 10 370 "cyan")
  (make-block 170 370 "blue")
  (make-block 190 370 "blue")
  (make-block 190 390 "blue")))

(check-expect (modify-pile (make-world
 (make-tetra
  (make-posn 90 150)
  (list
   (make-block 0 0 "green")
   (make-block 0 -1 "green")
   (make-block -1 -1 "green")
   (make-block -1 0 "green")))
 (list
  (make-block 10 370 "cyan")
  (make-block 10 390 "cyan")
  (make-block 30 390 "cyan")
  (make-block 50 390 "cyan")
  (make-block 50 370 "pink")
  (make-block 70 370 "pink")
  (make-block 70 390 "pink")
  (make-block 90 390 "pink")
  (make-block 110 370 "cyan")
  (make-block 110 390 "cyan")
  (make-block 130 390 "cyan")
  (make-block 150 390 "cyan")
  (make-block 170 350 "blue")
  (make-block 190 350 "blue")
  (make-block 190 370 "blue")
  (make-block 190 390 "blue"))
 16))
  (list (make-block 90 150 "green")
        (make-block 90 130 "green")
        (make-block 70 130 "green")
        (make-block 70 150 "green")))

(check-expect (correct-position-pile (list (make-block -1 0 "blue")
                                           (make-block 0 0 "blue")
                                           (make-block 1 0 "blue")
                                           (make-block 2 0 "blue")) 30 290)
              (list (make-block 10 290 "blue")
                    (make-block 30 290 "blue")
                    (make-block 50 290 "blue")
                    (make-block 70 290 "blue")))

(check-expect (key-handler (make-world
 (make-tetra
  (make-posn 90 150)
  (list
   (make-block 0 0 "green")
   (make-block 0 -1 "green")
   (make-block -1 -1 "green")
   (make-block -1 0 "green")))
 (list
  (make-block 10 370 "cyan")
  (make-block 10 390 "cyan")
  (make-block 30 390 "cyan")
  (make-block 50 390 "cyan")
  (make-block 50 370 "pink")
  (make-block 70 370 "pink")
  (make-block 70 390 "pink")
  (make-block 90 390 "pink")
  (make-block 110 370 "cyan")
  (make-block 110 390 "cyan")
  (make-block 130 390 "cyan")
  (make-block 150 390 "cyan")
  (make-block 170 350 "blue")
  (make-block 190 350 "blue")
  (make-block 190 370 "blue")
  (make-block 190 390 "blue"))
 16) "up")
(make-world
 (make-tetra (make-posn 90 150) (list (make-block 0 0 "green")
                                      (make-block 0 -1 "green")
                                      (make-block -1 -1 "green")
                                      (make-block -1 0 "green")))
 (list
  (make-block 10 370 "cyan")
  (make-block 10 390 "cyan")
  (make-block 30 390 "cyan")
  (make-block 50 390 "cyan")
  (make-block 50 370 "pink")
  (make-block 70 370 "pink")
  (make-block 70 390 "pink")
  (make-block 90 390 "pink")
  (make-block 110 370 "cyan")
  (make-block 110 390 "cyan")
  (make-block 130 390 "cyan")
  (make-block 150 390 "cyan")
  (make-block 170 350 "blue")
  (make-block 190 350 "blue")
  (make-block 190 370 "blue")
  (make-block 190 390 "blue"))
 16))              
(check-expect (key-handler (make-world
 (make-tetra
  (make-posn 90 150)
  (list
   (make-block 0 0 "green")
   (make-block 0 -1 "green")
   (make-block -1 -1 "green")
   (make-block -1 0 "green")))
 (list
  (make-block 10 370 "cyan")
  (make-block 10 390 "cyan")
  (make-block 30 390 "cyan")
  (make-block 50 390 "cyan")
  (make-block 50 370 "pink")
  (make-block 70 370 "pink")
  (make-block 70 390 "pink")
  (make-block 90 390 "pink")
  (make-block 110 370 "cyan")
  (make-block 110 390 "cyan")
  (make-block 130 390 "cyan")
  (make-block 150 390 "cyan")
  (make-block 170 350 "blue")
  (make-block 190 350 "blue")
  (make-block 190 370 "blue")
  (make-block 190 390 "blue"))
 16) "left")
(make-world
 (make-tetra (make-posn 70 150) (list (make-block 0 0 "green")
                                      (make-block 0 -1 "green")
                                      (make-block -1 -1 "green")
                                      (make-block -1 0 "green")))
 (list
  (make-block 10 370 "cyan")
  (make-block 10 390 "cyan")
  (make-block 30 390 "cyan")
  (make-block 50 390 "cyan")
  (make-block 50 370 "pink")
  (make-block 70 370 "pink")
  (make-block 70 390 "pink")
  (make-block 90 390 "pink")
  (make-block 110 370 "cyan")
  (make-block 110 390 "cyan")
  (make-block 130 390 "cyan")
  (make-block 150 390 "cyan")
  (make-block 170 350 "blue")
  (make-block 190 350 "blue")
  (make-block 190 370 "blue")
  (make-block 190 390 "blue"))
 16))
(check-expect (key-handler (make-world
 (make-tetra
  (make-posn 90 150)
  (list
   (make-block 0 0 "green")
   (make-block 0 -1 "green")
   (make-block -1 -1 "green")
   (make-block -1 0 "green")))
 (list
  (make-block 10 370 "cyan")
  (make-block 10 390 "cyan")
  (make-block 30 390 "cyan")
  (make-block 50 390 "cyan")
  (make-block 50 370 "pink")
  (make-block 70 370 "pink")
  (make-block 70 390 "pink")
  (make-block 90 390 "pink")
  (make-block 110 370 "cyan")
  (make-block 110 390 "cyan")
  (make-block 130 390 "cyan")
  (make-block 150 390 "cyan")
  (make-block 170 350 "blue")
  (make-block 190 350 "blue")
  (make-block 190 370 "blue")
  (make-block 190 390 "blue"))
 16) "right")
(make-world
 (make-tetra (make-posn 110 150) (list (make-block 0 0 "green")
                                      (make-block 0 -1 "green")
                                      (make-block -1 -1 "green")
                                      (make-block -1 0 "green")))
 (list
  (make-block 10 370 "cyan")
  (make-block 10 390 "cyan")
  (make-block 30 390 "cyan")
  (make-block 50 390 "cyan")
  (make-block 50 370 "pink")
  (make-block 70 370 "pink")
  (make-block 70 390 "pink")
  (make-block 90 390 "pink")
  (make-block 110 370 "cyan")
  (make-block 110 390 "cyan")
  (make-block 130 390 "cyan")
  (make-block 150 390 "cyan")
  (make-block 170 350 "blue")
  (make-block 190 350 "blue")
  (make-block 190 370 "blue")
  (make-block 190 390 "blue"))
 16))
(check-expect (key-handler (make-world
 (make-tetra
  (make-posn 90 150)
  (list
   (make-block 0 0 "green")
   (make-block 0 -1 "green")
   (make-block -1 -1 "green")
   (make-block -1 0 "green")))
 (list
  (make-block 10 370 "cyan")
  (make-block 10 390 "cyan")
  (make-block 30 390 "cyan")
  (make-block 50 390 "cyan")
  (make-block 50 370 "pink")
  (make-block 70 370 "pink")
  (make-block 70 390 "pink")
  (make-block 90 390 "pink")
  (make-block 110 370 "cyan")
  (make-block 110 390 "cyan")
  (make-block 130 390 "cyan")
  (make-block 150 390 "cyan")
  (make-block 170 350 "blue")
  (make-block 190 350 "blue")
  (make-block 190 370 "blue")
  (make-block 190 390 "blue"))
 16) "s")
(make-world
 (make-tetra
  (make-posn 90 150)
  (list
   (make-block 0 0 "green")
   (make-block 1 0 "green")
   (make-block 1 -1 "green")
   (make-block 0 -1 "green")))
 (list
  (make-block 10 370 "cyan")
  (make-block 10 390 "cyan")
  (make-block 30 390 "cyan")
  (make-block 50 390 "cyan")
  (make-block 50 370 "pink")
  (make-block 70 370 "pink")
  (make-block 70 390 "pink")
  (make-block 90 390 "pink")
  (make-block 110 370 "cyan")
  (make-block 110 390 "cyan")
  (make-block 130 390 "cyan")
  (make-block 150 390 "cyan")
  (make-block 170 350 "blue")
  (make-block 190 350 "blue")
  (make-block 190 370 "blue")
  (make-block 190 390 "blue"))
 16))
(check-expect (key-handler (make-world
 (make-tetra
  (make-posn 90 150)
  (list
   (make-block 0 0 "green")
   (make-block 0 -1 "green")
   (make-block -1 -1 "green")
   (make-block -1 0 "green")))
 (list
  (make-block 10 370 "cyan")
  (make-block 10 390 "cyan")
  (make-block 30 390 "cyan")
  (make-block 50 390 "cyan")
  (make-block 50 370 "pink")
  (make-block 70 370 "pink")
  (make-block 70 390 "pink")
  (make-block 90 390 "pink")
  (make-block 110 370 "cyan")
  (make-block 110 390 "cyan")
  (make-block 130 390 "cyan")
  (make-block 150 390 "cyan")
  (make-block 170 350 "blue")
  (make-block 190 350 "blue")
  (make-block 190 370 "blue")
  (make-block 190 390 "blue"))
 16) "a")
(make-world
 (make-tetra
  (make-posn 90 150)
  (list
   (make-block 0 0 "green")
   (make-block -1 0 "green")
   (make-block -1 1 "green")
   (make-block 0 1 "green")))
 (list
  (make-block 10 370 "cyan")
  (make-block 10 390 "cyan")
  (make-block 30 390 "cyan")
  (make-block 50 390 "cyan")
  (make-block 50 370 "pink")
  (make-block 70 370 "pink")
  (make-block 70 390 "pink")
  (make-block 90 390 "pink")
  (make-block 110 370 "cyan")
  (make-block 110 390 "cyan")
  (make-block 130 390 "cyan")
  (make-block 150 390 "cyan")
  (make-block 170 350 "blue")
  (make-block 190 350 "blue")
  (make-block 190 370 "blue")
  (make-block 190 390 "blue"))
 16))
(check-expect (rotate-collision (make-world (make-tetra
                                         (make-posn 190 330)
                                         (list (make-block 0 1 "purple")
                                               (make-block 0 0 "purple")
                                               (make-block -1 -1 "purple")
                                               (make-block 0 -1 "purple")))
                                        '() 0)) (make-world (make-tetra
                                         (make-posn 190 330)
                                         (list (make-block 0 1 "purple")
                                               (make-block 0 0 "purple")
                                               (make-block -1 -1 "purple")
                                               (make-block 0 -1 "purple")))
                                        '() 0))
(check-expect (move-left/right (make-world
 (make-tetra (make-posn 50 370) (list (make-block -1 -1 "pink")
                                     (make-block 0 -1 "pink")
                                     (make-block 0 0 "pink")
                                     (make-block 1 0 "pink")))
 (list (make-block 90 390 "green")
       (make-block 90 370 "green")
       (make-block 70 370 "green")
       (make-block 70 390 "green"))
 4) BLOCKSIZE) (make-world
 (make-tetra (make-posn 50 370) (list (make-block -1 -1 "pink")
                                     (make-block 0 -1 "pink")
                                     (make-block 0 0 "pink")
                                     (make-block 1 0 "pink")))
 (list (make-block 90 390 "green")
       (make-block 90 370 "green")
       (make-block 70 370 "green")
       (make-block 70 390 "green"))
 4))
