#lang racket/gui
(require racket/draw racket/draw/arrow)
(require "finder.rkt")

#|------------------------------------------------------GUI SET-UP----------------------------------------------------------|#

;Cities graph

(define citiesGraph (make-hash))

;Colors for rutes
(define blackDrawingPen (new pen% [color "black"][width 2]))
(define greenDrawingPen (new pen% [color (make-object color% 0 200 0)][width 2]))

;Interface Hash Variable Definition
(define xyposHash (make-hash))
(define nameNumberHash (make-hash))
(define numberNameHash (make-hash))
(define counter 0)
(define lineList '())

;Panels Definitions
(define window (new frame% [label "Waze"] [width 1200] [height 700] [style '(no-resize-border)]))
(define background (new horizontal-panel% [parent window] [style '(border)] [alignment '(center center)]))

(define mainPanel (new vertical-panel% [parent background] [alignment '(center center)]))

(define topRow (new horizontal-panel% [parent mainPanel] [style '(border)] [horiz-margin 10] [vert-margin 10] [min-width 300]))

(define topLeft (new vertical-panel% [parent topRow] [style '(border)] [min-height 100]))
(define topLeftTitle (new message% [parent topLeft] [label "Add City"]))
(define topLeftContent (new vertical-panel% [parent topLeft] [style '(border)] [min-height 100]))

(define topRight (new vertical-panel% [parent topRow] [style '(border)] [min-height 100]))
(define topRightTitle (new message% [parent topRight] [label "Add Route"]))
(define topRightContent (new vertical-panel% [parent topRight] [style '(border)] [min-height 100]))

(define bottomRow (new vertical-panel% [parent mainPanel] [style '(border)] [horiz-margin 10] [vert-margin 10] [min-width 700]))

(define topBottomRow (new horizontal-panel% [parent bottomRow][alignment '(center center)]
                                                   [horiz-margin 10][min-height 50]))

(define rightColumn (new vertical-panel% [parent background] [style '(border)] [horiz-margin 10] [vert-margin 10] [min-width 300]))

(define rightColumnTitle (new message% [parent rightColumn] [label "Available Routes"]))

;Map Definition
(define mapCanva (new canvas% [parent bottomRow] [min-height 300]))
(define dc (send mapCanva get-dc))


;-------------------------Add City Panel------------------------------

;Labels
(define nameField
  (new text-field% [parent topLeftContent] [label "City Name:"] [font (make-object font% 10 'default 'normal)]))
(define xField
  (new text-field% [parent topLeftContent] [label "       X Pos:"] [font (make-object font% 10 'default 'normal )]))
(define yField
  (new text-field% [parent topLeftContent] [label "       Y Pos:"] [font (make-object font% 10 'default 'normal )]))
(define colorField
  (new text-field% [parent topLeftContent] [label "       Color:"] [font (make-object font% 10 'default 'normal )]))

;Add City to Graph
(define btn_addNode 
  (new button%  
       [parent topLeftContent] 
       [label "Add City"] 
       [font (make-object font% 10 'default 'normal 'bold)]
       [callback (lambda (button event) 
                   (let ([ciudad (send nameField get-value)])
                     (addCitytoGraph ciudad)
                     (addNodetoGraph)))]))



;-----------------------------------Add Rute Panel--------------------

;Labels
(define fromField
  (new text-field% [parent topRightContent] [label "              From:"] [font (make-object font% 10 'default 'normal )]))
(define toField
  (new text-field% [parent topRightContent] [label "                  To:"] [font (make-object font% 10 'default 'normal )]))
(define weightField
  (new text-field% [parent topRightContent] [label " Distance (Km):"] [font (make-object font% 10 'default 'normal )]))


;Add Route between cities
(define btn_joinNodes (new button%  [parent topRightContent] [label "Add Route (both ways)"] [font (make-object font% 10 'default 'normal 'bold)]
                                   [callback (lambda (button event)
                                               (joinNodes blackDrawingPen
                                                          (car(hash-ref xyposHash (hash-ref nameNumberHash (send fromField get-value))))
                                                          (cadr(hash-ref xyposHash (hash-ref nameNumberHash (send fromField get-value))))
                                                          (car(hash-ref xyposHash (hash-ref nameNumberHash (send toField get-value))))
                                                          (cadr(hash-ref xyposHash (hash-ref nameNumberHash (send toField get-value)))))
                                               (drawWeight (send weightField get-value) (car(hash-ref xyposHash (hash-ref nameNumberHash (send fromField get-value))))
                                                          (cadr(hash-ref xyposHash (hash-ref nameNumberHash (send fromField get-value))))
                                                          (car(hash-ref xyposHash (hash-ref nameNumberHash (send toField get-value))))
                                                          (cadr(hash-ref xyposHash (hash-ref nameNumberHash (send toField get-value)))))
                                               (add-route (send fromField get-value) (send  toField get-value) (string->number(send weightField get-value)))
                                               (set! lineList (cons (list (hash-ref nameNumberHash (send fromField get-value))
                                                                          (hash-ref nameNumberHash (send  toField get-value)))
                                                                    lineList))
                                               )]))

;------------------------------------Draw in map Functions-----------------------------

;Add Route between cities
(define btn_joinNodes2 (new button%  [parent topRightContent] [label "Add Route (one way)"] [font (make-object font% 10 'default 'normal 'bold)]
                                   [callback (lambda (button event)
                                               (joinNodes blackDrawingPen
                                                          (car(hash-ref xyposHash (hash-ref nameNumberHash (send fromField get-value))))
                                                          (cadr(hash-ref xyposHash (hash-ref nameNumberHash (send fromField get-value))))
                                                          (car(hash-ref xyposHash (hash-ref nameNumberHash (send toField get-value))))
                                                          (cadr(hash-ref xyposHash (hash-ref nameNumberHash (send toField get-value)))))
                                               (drawWeight (send weightField get-value) (car(hash-ref xyposHash (hash-ref nameNumberHash (send fromField get-value))))
                                                          (cadr(hash-ref xyposHash (hash-ref nameNumberHash (send fromField get-value))))
                                                          (car(hash-ref xyposHash (hash-ref nameNumberHash (send toField get-value))))
                                                          (cadr(hash-ref xyposHash (hash-ref nameNumberHash (send toField get-value)))))
                                               (add-route2 (send fromField get-value) (send  toField get-value) (string->number(send weightField get-value)))
                                               (set! lineList (cons (list (hash-ref nameNumberHash (send fromField get-value))
                                                                          (hash-ref nameNumberHash (send  toField get-value)))
                                                                    lineList))
                                               )]))

;------------------------------------Draw in map Functions-----------------------------

 
#|
Post to Hash a new node
|# 
(define (addNodetoGraph)
  (drawNodes (string->number(send xField get-value))(string->number(send yField get-value))(send nameField get-value)(send colorField get-value))
  (addPostoHash (send nameField get-value) (list (+ (string->number(send xField get-value)) 5)
                                                  (+ (string->number(send yField get-value)) 5))))
#|
Add to graph a new node
|#
(define (addCitytoGraph ciudad)
  (hash-set! citiesGraph ciudad '()))

#|
Gives a node a position in hash
|#
(define (addPostoHash key coordlist)
  (hash-set! nameNumberHash key counter)
  (hash-set! numberNameHash counter key)
  (hash-set! xyposHash counter coordlist)
  (set! counter (+ counter 1)))

#|
Draw nodes in canvas with coordenades
|# 
(define (drawNodes x y name color)
  (cond((eq? color null)
        (send dc set-pen "black" 'solid)
        (send dc draw-ellipse x y 10 10))
       (else
         (send dc set-brush color 'solid)
         (send dc draw-ellipse x y 10 10)))
  (send dc set-text-foreground "black")
  (send dc draw-text name (- x 10) (- y 20))
)

#|
Join nodes in canvas with coordenades
|# 
(define (joinNodes color x1 y1 x2 y2)
  (send dc set-pen color)
  (draw-arrow dc x1 y1 x2 y2 0 0)
  (send dc set-pen blackDrawingPen)
  )

#|
Join nodes in Graph
|# 
(define (add-route city1 city2 weight)
  (define routes-city1 (hash-ref citiesGraph city1 '()))
  (define routes-city2 (hash-ref citiesGraph city2 '()))
  (set! routes-city1 (cons (cons city2 weight) routes-city1))
  (set! routes-city2 (cons (cons city1 weight) routes-city2))
  (hash-set! citiesGraph city1 routes-city1)
  (hash-set! citiesGraph city2 routes-city2)
  (showGraph))

(define (add-route2 city1 city2 weight)
  (define routes-city1 (hash-ref citiesGraph city1 '()))
  (set! routes-city1 (cons (cons city2 weight) routes-city1))
  (hash-set! citiesGraph city1 routes-city1)
  (showGraph))



#|
Draw routes weight between nodes
|# 
(define (drawWeight weight x1 y1 x2 y2)
  (define kmWeight (string-append weight " Km"))
  (define midX (+ x1 (/ (- x2 x1) 2)))
  (define midY (- (+ y1 (/ (- y2 y1) 2)) 10))


  (joinNodes blackDrawingPen x1 y1 x2 y2)

  (send dc draw-text kmWeight midX midY)
)

#|
Shows grph in console
|# 
(define (showGraph)
  (for ([ciudad (hash-keys citiesGraph)])
    (display ciudad)
    (display " ")
    (display (hash-ref citiesGraph ciudad '()))
    (newline))
  (define newGraph (transformGraph citiesGraph))
(display newGraph)
(display "\n"))

#|
Transforms graph into an aceptable format for finder
|# 
(define (transformGraph city-graph)
  (let ([cities (hash-keys city-graph)])
    (map
     (lambda (city)
       (list city
             (map
              (lambda (route)
                (list (car route) (cdr route)))
              (hash-ref city-graph city '()))))
     cities)))


#|------------------------------------------------------FIND ROUTES PANEL----------------------------------------------------------|#


;Text-fields and button declarations


(define srcField
  (new text-field% [parent topBottomRow] [label "From"]  [font (make-object font% 10 'default 'normal)]))
(define destField
  (new text-field% [parent topBottomRow] [label "To"]  [font (make-object font% 10 'default 'normal )]))


(define btn_searchRoutes (new button%  [parent topBottomRow] [label "Search"]
                                        [font (make-object font% 10 'default 'normal 'bold)]
                                        [callback (lambda (button event) (searchRoutes))]))

#|
Search all the ordered available routes calling finder and append them to list-box
|# 

(define (searchRoutes)
  (send allPathsList clear)

(define ordered_paths (find_ordered_paths (send srcField get-value) (send destField get-value) (transformGraph citiesGraph)))
  
(define listaDeCadenas
  (map
   (lambda (lista)
     (string-join (list (string-append "(" (string-join lista " ") ")")) " "))
   ordered_paths))

(for-each (lambda (cadena)
            (send allPathsList append cadena))
          listaDeCadenas)
)

#|------------------------------------------------------AVAILABLES ROUTES PANEL----------------------------------------------------------|# 


;list-box declaration 

(define allPathsList
  (new list-box% [parent rightColumn] [label ""] [font (make-object font% 10 'default 'normal )]
                 [choices '()]
                 [style (list 'single 'column-headers 'variable-columns)]
                 [columns (list "                                                                                                                     ")]
                 ))


(define btn_selectRoute (new button%  [parent rightColumn] [label "Show"]
                                      [font (make-object font% 10 'default 'normal 'bold)]
                                      [callback (lambda (button event)
                                                        (getSelectedPath (convertToList  (send allPathsList get-string-selection))))]))


#|
Converts to list a given selected route
|# 

(define (convertToList str)
  (with-input-from-string str read))

#|
Search and draw the selected path in list-box
|#

(define (getSelectedPath path)
  (unless (empty? path)
    (cond ((null? (rest path))
           (joinNodes greenDrawingPen
                      (car (hash-ref xyposHash (hash-ref nameNumberHash (symbol->string (first path)))))
                      (cadr (hash-ref xyposHash (hash-ref nameNumberHash (symbol->string (first path)))))
                      (car (hash-ref xyposHash (hash-ref nameNumberHash (symbol->string (first path)))))
                      (cadr (hash-ref xyposHash (hash-ref nameNumberHash (symbol->string (first path))))))
           (getSelectedPath (rest path)))
          (else
           (joinNodes greenDrawingPen
                      (car (hash-ref xyposHash (hash-ref nameNumberHash (symbol->string (first path)))))
                      (cadr (hash-ref xyposHash (hash-ref nameNumberHash (symbol->string (first path)))))
                      (car (hash-ref xyposHash (hash-ref nameNumberHash (symbol->string (car (rest path))))))
                      (cadr (hash-ref xyposHash (hash-ref nameNumberHash (symbol->string (car (rest path)))))))
           (getSelectedPath (rest path)))
          )
    )
  )


(send window show #t)
