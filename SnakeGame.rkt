(require 2htdp/image)
(require 2htdp/universe)

;Clone of snake - snake walking across the screen

;Constants

(define NCOL 25) ;Number of columns
(define NROW 25) ;number of rows
(define FIELDSIZE 30) ;size of the square in pixels
(define WIDTH (* FIELDSIZE NCOL)) ;WIDTH OF THE SCREEN
(define HEIGHT (* FIELDSIZE NROW)) ;HEIGHT OF THE SCREEN
(define SNAKECLR "grey") ;SNAKE COLOR
(define FOODCLR "white") ;FOOD COLOR
(define TEXTCLR "white")
(define TEXTSIZE FIELDSIZE)
(define SCREEN (rectangle WIDTH HEIGHT 'solid 'blue)) ;screen

;Data Definitions - making the core struct of the program

(define-struct field (col row))
;Col is the column number of the field
;Row is the row number of the field


;Defining worldstate
(define-struct WS (snake pausegame? direction trace food score))
;; Snake is the main object of the game controlled by the user
(define (tock ws)
  (if (WS-pausegame? ws)
      ws
      (changeWS ws)))

(define (changeWS ws)
  (if (foodeaten? (WS-snake ws)             ;Condition for eating food
                   (WS-food ws))
      (make-WS (grow-snake (WS-snake ws)
                           (WS-trace ws))   ;snake grows into the field trace
               (WS-pausegame? ws)           ;Pause the game
               (WS-direction ws)            ;Direction remains the same
               (WS-trace ws)                ;Trace also remain the same
               (new-food (WS-snake ws)         ;Generate food, new food must be different from snake and previous food.
                         (WS-food ws))
               (+ 1 (WS-score ws)))                  ;Add 1 to the score
     
      (make-WS (move-snake (WS-snake ws)
                           (WS-direction ws))
               (WS-pausegame? ws)
               (WS-direction ws)   ;Keep Direction
               (last (WS-snake ws))
               (WS-food ws)        ;keep the food
               (WS-score ws))))    ;keep the same score

;food-eaten?
;Snake food -> Boolean
;Produce true when food is eaten
(define (foodeaten? snake food)
  (fieldequal? (first snake)
                food))

(define (fieldequal? f1 f2)
  (and ( =  (field-col f1)
            (field-col f2))
       (= (field-row f1)
          (field-row f2))))

;Grow-snake
;Snake Field -> Snake
;Make snake longer after eating
(define (grow-snake snake trace)
  (cond
    [(empty? snake) (cons trace empty)]
    [else (cons (first snake)
                (grow-snake (rest snake)
                            trace))]))

;New food
;snake field-> field
;Produce random food
(define (new-food snake current-food)
  (if (or (contains-field? snake current-food)
          (adjacent? (first snake)
                     current-food)) ;snake contains current-food or current-food is adjacent to snake's head
          (new-food snake           ;recursively call new-food the same snake and random food
                (make-field (add1 (random NCOL))
                            (add1 (random NROW))))
      current-food))

;ListOfField Field-> Boolean
;Produce true if f is an element of lof
(define (contains-field? lof f)
  (cond
    [(empty? lof) false]
    [else (if (fieldequal? (first lof) f)
              true
              (contains-field? (rest lof) f))]))

;Field Field ->Boolean
;Produce true if f1 f2 is adjacent to f2
(define (adjacent? f1 f2)
  (cond
    [(and (= (field-col f1)
             (field-col f2))
          (= (abs (- (field-row f1)
                     (field-row f2)))
             1))
     true]
    [(and (= (field-row f1)
             (field-row f2))
          (= (abs (- (field-col f1)
                     (field-col f2)))
             1))
     true]
    [else false]))



;Move snake
;Snake Direction-> Snake
;Move the snake according to its direction

(define (move-snake snk dir)
  (cons (move-field (first snk)
                    dir)
        (follow (rest snk)
                (first snk))))

;Field Direction-> Field
(define (move-field f dir)
  (cond
    [(string=? "up" dir) (make-field (field-col f)
                                     (sub1 (field-row f)))]
    [(string=? "down" dir) (make-field (field-col f)
                                       (add1 (field-row f)))]
    [(string=? "left" dir)  (make-field (sub1 (field-col f))
                                        (field-row f))]
    [(string=? "right" dir)  (make-field (add1 (field-col f))
                                         (field-row f))]
    [else false]))


;;ListOfField Field -> ListOfField
;; Make lof follow
(define (follow lof f)
  (cond
    [(empty? lof) empty]
    [else (cons f
                (follow (rest lof)
                        (first lof)))]))


;Last
;Snake->Field
;Get the last field of the snake
(define (last snk)
  (cond
    [(empty? (rest snk)) (first snk)]
    [else (last (rest snk))]))


;;WorldState-> Image
;Render will return 3 images (snake, food, score)
(define (render ws)
  (render-snake (WS-snake ws)
                (render-field (WS-food ws)
                              FOODCLR
                              (render-score (WS-score ws)
                                            SCREEN))))

;Snake, Image -> Image
;Render snake into the scene
(define (render-snake snake scene)
  (cond
    [(empty? snake) (place-image empty-image
                                 0
                                 0
                                 scene)]
    [else (render-field (first snake)
                        SNAKECLR
                        (render-snake (rest snake)
                                      scene))]))

;Field, Image -> Image
;Render field into the scene
(define (render-field f clr scene)
  (place-image (square FIELDSIZE "solid" clr)
               (field-x f)
               (field-y f)
               scene))

;Produce x coordinate of field
(define (field-x field)
  (- (* (field-col field)
        FIELDSIZE)
     (/ FIELDSIZE 2)))

;Produce y coordinate of field
(define (field-y field)
  (- (* (field-row field)
        FIELDSIZE)
     (/ FIELDSIZE 2)))



;Natural, Image -> Image
;render score into the scene
(define (render-score score scene)
  (overlay/align "center"
                 "top"
                 (text (string-append "Score: "
                                      (number->string score))
                       TEXTSIZE
                       TEXTCLR)
                 scene))





;WorldState-> Boolean
;Produce true when game should terminate
(define (last-world? ws)
  (or (self-bite? (WS-snake ws))
      (edge-collision? (WS-snake ws))))

;Snake -> Boolean
;Produce true if it's bite itself
(define (self-bite?  snake)
  (contains-field? (rest snake)
                   (first snake)))


;Snake -> Boolean
;Produce true if it's hits the edge
(define(edge-collision? snk)
  (or (< (field-row (first snk))
         1)
      (> (field-row (first snk))
         NROW)
      (< (field-col (first snk))
         1)
      (> (field-col (first snk))
         NCOL)))

;;WorldState Keyevent -> WorldState
(define (is-arrow-key? key)
  (cond
    [(string=? "up" key) #true ]
    [(string=? "down" key)  #true ]
    [(string=? "left" key)  #true ]
    [(string=? "right" key)  #true ]
    [else #false]))

(define (handle-key ws key)
  (cond
    [(opposite? (WS-direction ws)
                key)
     ws]
    [(is-arrow-key? key) (make-WS (WS-snake ws)
                   (WS-pausegame? ws)
                   key
                   (WS-trace ws)
                   (WS-food ws)
                   (WS-score ws))]
    [(string=? " " key) (make-WS
                        (WS-snake ws)
                        (not (WS-pausegame? ws))
                        (WS-direction ws)
                        (WS-trace ws)
                        (WS-food ws)
                        (WS-score ws))]
    [else ws]))

;Distance Direction -> Boolean
;d1 and d2 are the opposite direction of each other
(define (opposite? d1 d2)
  (cond
    [(and (string=? "up" d1)
          (string=? "down" d2))
     true]
    [(and (string=? "down" d1)
          (string=? "up" d2))
     true]
    [(and (string=? "left" d1)
          (string=? "right" d2))
     true]
    [(and (string=? "right" d1)
          (string=? "left" d2))
     true]
    [else false]))

(define (last-picture ws)
  (place-image (above (text "GAME OVER!"
                            15
                            "white")
                      (text (string-append "Your score is "
                                           (number->string (WS-score ws)))
                            15
                            "white"))
               (/ WIDTH 2)
               (/ HEIGHT 2)
               SCREEN))



;;Defining snake--snake is the listoffield wtih at least 3 fields each is adjacent to atleast one another
;Example 2 -- straight, horizontal, 3-element snake
(define SNAKE (list (make-field 4 2)
                    (make-field 3 2)
                    (make-field 2 2)))


(define (INITIAL-WS ws)
  (big-bang ws
    (on-tick tock 0.2)
    (to-draw render)
    (stop-when last-world? last-picture)
    (on-key handle-key)
    ))
(INITIAL-WS (make-WS SNAKE
               #false
               "right"
               (make-field 2 2)
               (make-field 1 1)
               0))
