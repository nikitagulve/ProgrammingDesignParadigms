;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rectangle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Draggable Rectangle 
;; A rectangle is displayed. The user can select the rectangle, drag it
;; and place it in a canvas

(require rackunit)
(require rackunit/text-ui)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")

;; run with (run 0)

(provide run)
(provide initial-world)
(provide world-to-center)
(provide world-selected?)
(provide world-after-mouse-event)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; RUN FUNCTION

;; run : Any -> World
;; GIVEN: any value
;; EFFECT: Ignores its argument and runs the world.
;; RETURNS: the final state of the world.
(define (run initial-pos)
  (big-bang (initial-world initial-pos)
            (on-draw world->scene)
            (on-mouse world-after-mouse-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

(define UNSELECTED-RECTANGLE (rectangle 100 60 "solid" "green"))
(define SELECTED-RECTANGLE  (rectangle 100 60 "outline" "green"))
(define MOUSE-POINT (circle 5 "solid" "red"))

;; dimensions of the canvas
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;; dimensions of the rectangle
(define HALF-RECTANGLE-WIDTH (/ 100 2))
(define HALF-RECTANGLE-HEIGHT (/ 60 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; DATA DEFINITIONS

(define-struct world (x y mx my is-selected?))
;; A World is a (make-world Number Number Number Number Boolean)
;; Interpretation: 
;; (x, y) gives the position of origin of the rectangle in pixels
;; (mx, my) is the position of mouse pointer
;; selected? determines whether the rectangle has been selected by the user
;; template:
;; world-fn : World -> ??
;(define (world-fn w)
;  (... (world-x w) (world-y w) (world-is-selcted? w)))

;;examples of worlds, for testing
(define unselected-world (make-world 50 50 0 0 false)) 
(define selected-world (make-world 50 50  25 25 true))

;; A DragRectangleMouseEvent is a partition of MouseEvent into the
;; following categories:
;; -- "button-down"   (interp: select the rectangle)
;; -- "drag"          (interp: drag the rectangle)
;; -- "button-up"     (interp: unselect the rectangle)
;; -- any other mouse event (interp: ignored)

;(define (mev-fn mev)
;  (cond
;    [(mouse=? mev "button-down") ...]
;    [(mouse=? mev "drag") ...]
;    [(mouse=? mev "button-up") ...]
;    [else ...]))


  
;;; END DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; in-rectangle?: World Number Number -> Boolean
;; GIVEN: x, y coordinates of the mouse pointer and coordinates 
;; of the origin of rectangle
;; RETURNS: a boolean value determing whether the clicked point 
;; is within the rectangle

;; EXAMPLES: 
;;  (in-rectangle? (make-world 200 200) 300 350) ->  false

;; STRATEGY: structural decomposition on w : World
(define (in-rectangle? w x y)
  (and
    (<= 
      (- (world-x w) HALF-RECTANGLE-WIDTH)
      x
      (+ (world-x w) HALF-RECTANGLE-WIDTH))
    (<= 
      (- (world-y w) HALF-RECTANGLE-HEIGHT)
      y
      (+ (world-y w) HALF-RECTANGLE-HEIGHT))))

(define-test-suite in-rectangle?-tests
  (check-equal? 
    (in-rectangle? (make-world 200 200 0 0 false) 
                   (+ (- 200 HALF-RECTANGLE-WIDTH) 2)
                   200)
     true
     "Should return true since mouse pointer is inside rectangle 
     left boundary")

  (check-equal? 
    (in-rectangle? (make-world 200 200 0 0 false) 
                   (- (+ 200 HALF-RECTANGLE-WIDTH) 2)
                   200)
     true
     "Should return true since mouse pointer is inside rectangle 
     right boundary")
  (check-equal? 
    (in-rectangle? (make-world 200 200 0 0 false)                    
                   200
                   (+ (- 200 HALF-RECTANGLE-HEIGHT) 2))
     true
     "Should return true since mouse pointer is inside rectangle 
     top boundary")
    
  (check-equal? 
    (in-rectangle? (make-world 200 200 0 0 false)                    
                   200
                   (- (+ 200 HALF-RECTANGLE-HEIGHT) 2))
     true
     "Should return true since mouse pointer is inside rectangle 
     bottom boundary")
    
    (check-equal? 
    (in-rectangle? (make-world 200 200 0 0 false) 
                   (- 200 HALF-RECTANGLE-WIDTH 2)
                   200)
     false
     "Should return false since mouse pointer is outside rectangle 
     left boundary")
    
    (check-equal? 
    (in-rectangle? (make-world 200 200 0 0 false) 
                   (+ 200 HALF-RECTANGLE-WIDTH 2)
                   200)
     false
     "Should return false since mouse pointer is outside rectangle 
     right boundary")
    (check-equal? 
      (in-rectangle? (make-world 200 200 0 0 false)                    
                     200
                     (- 200 HALF-RECTANGLE-HEIGHT 2))
     false
     "Should return false since mouse pointer is outside rectangle 
     top boundary")
    
  (check-equal? 
    (in-rectangle? (make-world 200 200 0 0 false)                    
                   200
                   (+ 200 HALF-RECTANGLE-HEIGHT 2))
     false
     "Should return false since mouse pointer is outside rectangle 
     bottom boundary"))
  
(run-tests in-rectangle?-tests)

;; world->scene : World -> Scene
;; RETURNS: a Scene that portrays the given world.

;; EXAMPLE: (world->scene (make-world 20 20 false))
;;          = (place-image UNSELECTED-RECTANGLE 20 20 EMPTY-CANVAS)

;; STRATEGY: Structural Decomposition [World]
(define (world->scene w)
  (if (world-is-selected? w)            
      (place-image MOUSE-POINT (world-mx w) (world-my w) 
                   (place-image SELECTED-RECTANGLE (world-x w) 
                                (world-y w) EMPTY-CANVAS))               
      (place-image UNSELECTED-RECTANGLE (world-x w) (world-y w) 
                   EMPTY-CANVAS)))  

;; tests

;; some constants
(define img-rect-selected 
  (place-image MOUSE-POINT
               60 
               50  
               (place-image SELECTED-RECTANGLE  50 50 EMPTY-CANVAS)))
(define img-rect-unselected 
  (place-image UNSELECTED-RECTANGLE 50 50 EMPTY-CANVAS))

(define-test-suite world->scene-tests
  (check-equal? 
    (world->scene (make-world 50 50 60 50 false))
    img-rect-unselected
    "Should return a solid rectangle at (50,50)")
  (check-equal? 
    (world->scene (make-world 50 50 60 50 true))
    img-rect-selected
    "Should return a outlined rectangle at (50,50)"))
(run-tests world->scene-tests)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; new-rec-x: Number Number Number -> Number
;; GIVEN: x coordinate of the origin of rectangle, x coordinate of
;; of the mouse pointer before drag event and x coordinate of the 
;; mouse pointer after drag event
;; RETURNS: a number which is the new x cordinate of the dragged
;; rectangle
  
;; EXAMPLES: (new-rec-x 200 250 255) -> 205
  
;; STRATEGY: Domain Knowledge
  
(define (new-rec-x x mx-old mx-new)
 (+ x (- mx-new mx-old)))

(define (new-rec-y y my-old my-new)
 (+ y (- my-new my-old)))
  
;; tests are covered in world-after-bottom

;; world-after-button-down : World Number Number -> World
;; RETURNS: the world following a button-down at the given location.
;; if the button-down is inside the rectangle, returns an outlined 
;; rectangle with a red circle at a point pointed by the mouse
  
;; STRATEGY: Structural Decomposition on w : World
  
(define (world-after-button-down w mx my)
  (if (in-rectangle? w mx my )
  (make-world (world-x w) 
              (world-y w)
               mx
               my
               true)
  w))                         

;; world-after-drag : World Number Number -> World
;; RETURNS: the world following a drag at the given location.
;; if the world is selected, then return a world just like the given
;; one, except that it is now centered on the mouse position.
  
;; STRATEGY: Structural Decomposition on w: World
  
(define (world-after-drag w mx my)
  (if (world-is-selected? w)
       (make-world (new-rec-x (world-x w) (world-mx w) mx)
                   (new-rec-y (world-y w) (world-my w) my)
                    mx
                    my
                    true)                    
      w))                         
     

;; world-after-button-up : World Number Number -> World
;; RETURNS: the world following a button-up at the given location.
;; if the cat is selected, return a cat just like the given one,
;; except that it is no longer selected.
  
;; STRATEGY: Structural Decomposition on w: World
  
(define (world-after-button-up w mx my)
  (if (world-is-selected? w)
      (make-world (world-x w) (world-y w) mx my false)
      w))

;; world-after-mouse-event : World Number Number 
;;                           DragRectangleMouseEvent -> World
;; RETURN: produce the world that should follow the given mouse event
;; EXAMPLES: 
  
;; STRATEGY: Structural Decomposition on mouse events
  
(define (world-after-mouse-event w mx my mev)
  (cond
    [(mouse=? mev "button-down") (world-after-button-down w mx my)]
    [(mouse=? mev "drag") (world-after-drag w mx my)]
    [(mouse=? mev "button-up")(world-after-button-up w mx my)]
    [else w]))

;; how many tests do we need here?
;; 3 mouse events (+ a test for the else clause)
;; cat selected or unselected  (do we need to worry about being
;; paused?)
;; event inside cat or not.

(define-test-suite world-after-mouse-event-tests

  ;; for button-down
  ;; mouse is inside the rectangle
  (check-equal?
    (world-after-mouse-event 
     (make-world 200 200 250 200 false)
     (+ 200 HALF-RECTANGLE-WIDTH)
     200     
     "button-down")
     (make-world 200 200 (+ 200 HALF-RECTANGLE-WIDTH) 200 true)
    "button down inside the rectangle should select it")
  ;; mouse outside rectangle
  (check-equal?
    (world-after-mouse-event 
     (make-world 200 200 250 200 false)
     (+ 200 HALF-RECTANGLE-WIDTH 3)
     200    
     "button-down")
     (make-world 200 200 (+ 200 HALF-RECTANGLE-WIDTH) 200 false)
    "button down outside the rectangle should not select it")

   ;; tests for drag
  
  (check-equal?
    (world-after-mouse-event 
     (make-world 200 200 210 200 true)
     220
     200     
     "drag")
     (make-world 210 200 220 200 true)
    "drag when the rectangle is selected")

  (check-equal?
    (world-after-mouse-event 
     (make-world 200 200 210 200 false)
     220
     200     
     "drag")
     (make-world 200 200 210 200 false)
    "drag unselected rectangle leave it unchanged")

  ;; tests for button-up
 
  (check-equal?
    (world-after-mouse-event 
     (make-world 200 200 210 200 true)
     220
     200     
     "button-up")
     (make-world 200 200 220 200 false)
    "button up the selected rectangle is unselected")

  (check-equal?
    (world-after-mouse-event 
     (make-world 200 200 210 200 false)
     220
     200     
     "button-up")
     (make-world 200 200 210 200 false)
    "button up on unselectected rectangle, no change")

  ;; tests for other mouse events

  (check-equal?
    (world-after-mouse-event 
     (make-world 200 200 210 200 false)
     220
     200     
     "move")
     (make-world 200 200 210 200 false)    
    "move when rectangle is unselected should leave it unchanged"))
 
(run-tests world-after-mouse-event-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : Any -> World
;; GIVEN: any value
;; RETURNS: the initial world.
;; Ignores its argument
;; EXAMPLES: (initial-world 1) -> (make-world 200 200 0 0 false)
;; STRATEGY: Domain Knowledge
(define (initial-world num)
  (make-world 200 200 0 0 false))

;; tests

(define-test-suite initial-world-tests
  (check-equal? (initial-world 3) (make-world 200 200 0 0 false))
  (check-equal? (initial-world "a") (make-world 200 200 0 0 false)))

(run-tests initial-world-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-center : World -> Posn
;; GIVEN: a world
;; RETURNS: the coordinates of the center of the rectangle as a Posn
;; EXAMPLES: (world-to-center (make-world 20 20 0 0 false)) -> 
;;                            (make-posn 20 20)
;; STRATEGY: Domain Knowledge
(define (world-to-center w)
  (make-posn (world-x w) (world-y w)))

;; tests

(define-test-suite world-to-center-tests
  (check-equal? (world-to-center (make-world 20 21 0 0 false)) 
                (make-posn 20 21))
  (check-equal? (world-to-center (make-world -190 20 0 0 false)) 
                (make-posn -190 20)))

(run-tests world-to-center-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-selected? : World -> Boolean
;; GIVEN a world
;; RETURNS: true iff the rectangle is selected.
;; EXAMPLES: (world-selected? (make-world 20 20 0 0 false)) -> false
;; STRATEGY: Domain Knowledge
(define (world-selected? w)
  (world-is-selected? w))

;; tests

(define-test-suite world-selected?-tests
  (check-equal? (world-selected? (make-world 20 21 0 0 false)) 
                false)
  (check-equal? (world-selected? (make-world -190 20 0 0 true)) 
                true))

(run-tests world-selected?-tests)
  








