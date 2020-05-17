#lang scheme
;2016400348

; HELPER FUNCTIONS

;This function takes 2 parameters. 
;First parameter will be a function and the second parameter will be a list. 
;This function would apply the given function to each element of the list in order. 
;If the given function returns something that is not false, 
;then this function should return the return value as well. 
;If the given func- tion returns false for each and every element of the list 
;then this function should return false as well.

(define (RETURN-FIRST-NOT-FALSE predicate list )
  (if (null? list)
  #f
  (if (eq? (predicate (car list)) #f )
    (RETURN-FIRST-NOT-FALSE predicate (cdr list))
    (predicate (car list))
  )))

;This function takes 2 parameters. 
;Both parameters will be lists with 2 integers representing a position on a grid. 
;This function should return true if these positions are adjacent, including diago- nals, or the same position. 
;Otherwise it should return false.

(define (ADJACENT p1 p2)
  (if (<= (+ (abs (- (car p1) (car p2))) (abs (- (cadr p1) (cadr p2)))) 2)
    #t
    ;else
    #f  
  )
)

;This function takes a single parameter. 
;It will be a list with 2 integers representing a position on a grid. 
;This function should return a list of neighboring positions, horizontally and vertically. 
;The function can return the neighbors in any order. 
;Even if a position is out of bounds in the context of the tents puzzle this function must put that position in the list that this function returns.

(define (NEIGHBOR-LIST point)
  (cons   
  ; UP
  (cons (- (car point) 1) (cons (cadr point) '()))     
  (cons
  ; LEFT 
  (cons (car point) (cons (- (cadr point) 1) '()))  
  (cons
  ; RIGHT
  (cons (car point) (cons (+ (cadr point) 1)  '()))   
  (cons
  ; DOWN
  (cons (+ (car point) 1) (cons (cadr point) '())) '()))))
)

;This function takes 2 parameters. 
;A list containing 2 integers and a list of lists that are containing 2 integers.
;The first parameter represents a position on a grid and each element on the second list also represents a position. 
;This function should return true if the position represented 
;by the first parameter is adjacent to at least one of the positions in the given list (using the same definition for adjacency from the ADJACENT function). 
;Otherwise it should return false


(define (ADJACENT-WITH-LIST list listOfList)
  (if (null? listOfList)
  #f
  (if (ADJACENT list (car listOfList))
  #t
  (ADJACENT-WITH-LIST list (cdr listOfList))
  ))
)

;This function takes 3 parameters. 
;A list, an integer indicating a position on this list and a value. 
;It should return a list that is identical to the given list, except the value in the given position should be replaced with the given value. 
;The given position will always be positive and not bigger than the length of the given list.


(define (REPLACE-NTH list pos val)
  (if (eq? pos 1)
  (cons val (cdr list))
  (cons (car list) (REPLACE-NTH (cdr list) (- pos 1) val ))
  )
)

;This function takes 2 parameters.
;A point of grid and size of puzzle (NxN)
;It returns true if given point is in bound of puzzle, otherwise false.



(define (MEM list val)
  (if (null? list)
    #f
    (if (eq? (car list) val)
      #t
      (or #f (MEM (cdr list) val))
    )
  )
)


(define (LISTMEMBEROFLIST list val)
  (if (null? list)
      #f
      (if (and (eq? (car (car list)) (car val)) (eq? (cadr (car list)) (cadr val)))
          #t
          (LISTMEMBEROFLIST (cdr list) val)
      )
  )  
)

(define (ISMEMBEROFLIST list otherList)
  (if (null? list)
    #f
    (if (LISTMEMBEROFLIST otherList (car list))
      #t
      (or #f (ISMEMBEROFLIST (cdr list) otherList))
    )
  )
)

(define (ALL-NEIGHBOR point)

  (cons
  ;UPLEFT
  (cons (- (car point) 1) (cons (- (cadr point)  1) '()))
  (cons   
  ; UP
  (cons (- (car point) 1) (cons (cadr point) '()))
  (cons
  ; UPRIGHT
  (cons (- (car point) 1) (cons (+ (cadr point) 1) '() ))     
  (cons
  ; LEFT 
  (cons (car point) (cons (- (cadr point) 1) '()))  
  (cons
  ; RIGHT
  (cons (car point) (cons (+ (cadr point) 1)  '()))
  (cons
  ;DOWNLEFT
  (cons (+ (car point) 1) (cons (- (cadr point) 1) '()))
  (cons
  ;DOWN
  (cons (+ (car point) 1) (cons  (cadr point) '()))
  (cons
  ; DOWNRIGHT
  (cons (+ (car point) 1) (cons (+ (cadr point) 1) '())) '()))))))))
)


(define (FIND-POSITION val list index)
  (if (null? list)
  '()
  (if (eq? (car list) val)
  (cons index (FIND-POSITION val (cdr list) (+ 1 index)))
  (FIND-POSITION val (cdr list) (+ 1 index))
  )
  )
)
(define (FIND-VALUE list index)
  (if (null? list)
    '()
    (if (eq? index 1)
      (car list)
      (FIND-VALUE (cdr list) (- index 1))
    )
  )
)

(define (POSSIBLE-NEIGHBOR treeNeighbors gridList)
  (if (null? treeNeighbors )
      '()
      (if (LISTMEMBEROFLIST gridList (car treeNeighbors))
          (cons (car treeNeighbors) (POSSIBLE-NEIGHBOR (cdr treeNeighbors) gridList))
          (POSSIBLE-NEIGHBOR (cdr treeNeighbors) gridList)
      )
           
      
  )
)

(define (DECREMENT-ELEMENT-OF-LIST list pos)
  (if (eq? pos 1)
    (cons (- (car list) 1) (cdr list))
    (cons (car list) (DECREMENT-ELEMENT-OF-LIST (cdr list) (- pos 1)))
  )
)

(define (FILTER-LIST-WITH-ROW-INDEX predicate list treeList rowIndex)
  (if (null? list)
    '()
    (if (or (not (predicate (car (car list)) rowIndex)) (LISTMEMBEROFLIST treeList (car list)))
      (cons (car list) (FILTER-LIST-WITH-ROW-INDEX predicate (cdr list) treeList rowIndex))
      (FILTER-LIST-WITH-ROW-INDEX predicate (cdr list) treeList rowIndex)
    )
  )
)

(define (FILTER-LIST-WITH-COL-INDEX predicate list treeList colIndex)
  (if (null? list)
    '()
    (if (or (not (predicate (cadr (car list)) colIndex)) (LISTMEMBEROFLIST treeList (car list)))
      (cons (car list) (FILTER-LIST-WITH-COL-INDEX predicate (cdr list) treeList colIndex))
      (FILTER-LIST-WITH-COL-INDEX predicate (cdr list) treeList colIndex)
    )
  )
)



(define CHECKINDEX (lambda (val index)
  (if (eq? val index)
    #t 
    #f
  )
))

(define (FILTER-GRID-ZEROS rowList colList gridList treeList)
  (if (null? rowList)
    (if (null? colList)
      gridList
      (FILTER-GRID-ZEROS  rowList (cdr colList) (FILTER-LIST-WITH-COL-INDEX CHECKINDEX gridList treeList (car colList)) treeList)

    )
    (FILTER-GRID-ZEROS  (cdr rowList) colList (FILTER-LIST-WITH-ROW-INDEX CHECKINDEX gridList treeList (car rowList)) treeList)
  )
)

(define (FILTER-ZERO-COL colIndex gridList)
  (if (null? gridList)
    '()
    (if (eq? (cadr (car gridList)) colIndex)
      (FILTER-ZERO-COL colIndex (cdr gridList))
      (cons (car gridList) (FILTER-ZERO-COL colIndex (cdr gridList)) )
    )  
  )
)

(define (FILTER-ZERO-ROW rowIndex gridList)
  (if (null? gridList)
    '()
    (if (eq? (car (car gridList)) rowIndex)
      (FILTER-ZERO-ROW rowIndex (cdr gridList))
      (cons (car gridList) (FILTER-ZERO-ROW rowIndex (cdr gridList)) )
    )  
  )
)

(define (FILTER-GRID-IMPOSSIBLE-POINTS gridList treeList)
  (if (null? gridList)
    '()
    (if (and (ISMEMBEROFLIST (NEIGHBOR-LIST (car gridList)) treeList)  (not (LISTMEMBEROFLIST treeList (car gridList))))
      (cons (car gridList) (FILTER-GRID-IMPOSSIBLE-POINTS (cdr gridList) treeList))
      (FILTER-GRID-IMPOSSIBLE-POINTS (cdr gridList) treeList)
    )
  )
)


(define (REMOVE-POINTS pointList list)
  (if (null? list)
    '()
    (if (LISTMEMBEROFLIST  pointList (car list))
      (append (REMOVE-POINTS pointList (cdr list)) '())
      (cons (car list) (REMOVE-POINTS pointList (cdr list)))
    )
  )
)

(define ZERO-INDEX (lambda (rowList) (if (MEM rowList 0) (FIND-POSITION 0 rowList 1) '())) )


(define (ROWALLZERO rowList)
  (if (null? rowList)
    #t
    (if (not (eq? (car rowList) 0))
      #f
      (ROWALLZERO (cdr rowList))
    )
  )
)

(define (sum elemList)
  (if
    (null? elemList)
    0
    (+ (car elemList) (sum (cdr elemList)))
  )
)

(define (COLALLZERO colList)
  (if (null? colList)
    #t
    (if (not (eq? (car colList) 0))
      #f
      (COLALLZERO (cdr colList))
    )
  )
)

(define (ERROR rowList)
  (if (null? rowList)
      #f
      (if (< (car rowList) 0)
          #t
          (ERROR (cdr rowList))
       )
  
  )
)


(define (CREATE-ROW row col initial)
  (if (eq? col initial)
    (cons (cons row (cons initial '())) '())
    (cons (cons row (cons initial '())) (CREATE-ROW row col (+ 1 initial)))
  )
)


(define (FULL-GRID row col initial) 
  (if (eq? row initial)
    (append (CREATE-ROW row col 1) '())
    (append (FULL-GRID (- row 1) col initial) (CREATE-ROW row col 1) )
  )
)

(define (FIND-PROPER-PLACE treeNeighbors gridList)
  (if (null? treeNeighbors)
      #f
      (if (LISTMEMBEROFLIST gridList (car treeNeighbors))
          (car treeNeighbors)
          (FIND-PROPER-PLACE (cdr treeNeighbors) gridList)
       )
   )
  
)



(define (POINT-ONLY rowList colList treeNeighbor gridList)
  (if (null? treeNeighbor)
      (cons #f (cons rowList (cons colList (cons gridList '()))))
      (if (LISTMEMBEROFLIST gridList (car treeNeighbor))
          (cons (car treeNeighbor) (cons (DECREMENT-ELEMENT-OF-LIST rowList (car (car treeNeighbor))) (cons (DECREMENT-ELEMENT-OF-LIST colList (cadr (car treeNeighbor))) (cons (REMOVE-POINTS (cons (car treeNeighbor) (ALL-NEIGHBOR (car treeNeighbor))) gridList) '()))))
          (cons #f (cons rowList (cons colList (cons gridList '()))))
         )
 )
)



(define (iterateTreeNeighbors POINT rowList colList treeNeighbors treeList gridList)


  (if (and (null? (cdr treeList)) (ISMEMBEROFLIST (NEIGHBOR-LIST (car treeList)) gridList))
      (cons (FIND-PROPER-PLACE (NEIGHBOR-LIST (car treeList)) gridList) '())
      (if (null? treeNeighbors)
          #f
          (if (eq? (car POINT) #f )
              (iterateTreeNeighbors (POINT-ONLY rowList colList (cdr treeNeighbors) gridList) rowList colList (cdr treeNeighbors) treeList gridList)
              (if (or (ERROR (cadr POINT)) (ERROR (caddr POINT)))
                  (iterateTreeNeighbors  (POINT-ONLY rowList colList (cdr treeNeighbors)  gridList) rowList colList (cdr treeNeighbors) treeList gridList)
                  (if (eq? (iterateTreeNeighbors (POINT-ONLY (cadr POINT) (caddr POINT) (POSSIBLE-NEIGHBOR (NEIGHBOR-LIST (car (cdr treeList))) (cadddr POINT)) (cadddr POINT)) (cadr POINT) (caddr POINT) (POSSIBLE-NEIGHBOR (NEIGHBOR-LIST (car (cdr treeList))) (cadddr POINT)) (cdr treeList) (cadddr POINT)) #f)
                    (iterateTreeNeighbors (POINT-ONLY rowList colList (cdr treeNeighbors) gridList) rowList colList (cdr treeNeighbors) treeList gridList)
                    (cons (car POINT) (iterateTreeNeighbors (POINT-ONLY (cadr POINT) (caddr POINT) (POSSIBLE-NEIGHBOR (NEIGHBOR-LIST(car (cdr treeList))) (cadddr POINT)) (cadddr POINT)) (cadr POINT) (caddr POINT) (POSSIBLE-NEIGHBOR (NEIGHBOR-LIST(car (cdr treeList))) (cadddr POINT)) (cdr treeList) (cadddr POINT)))
                  )
                  
              )
          )
  
      )
  )
  
)





(define (TENTS-SOLUTION list)
  
  


 
  (if (or (null? (caddr list)) (not (eq? (sum (car list)) (sum (cadr list)))))
    #f
    (iterateTreeNeighbors (POINT-ONLY (car list) (cadr list) (POSSIBLE-NEIGHBOR (NEIGHBOR-LIST (car (caddr list))) (FILTER-GRID-IMPOSSIBLE-POINTS (FILTER-GRID-ZEROS (ZERO-INDEX (car list)) (ZERO-INDEX (cadr list)) (FULL-GRID (length (car list)) (length (cadr list)) 1) (caddr list)) (caddr list))) (FILTER-GRID-IMPOSSIBLE-POINTS (FILTER-GRID-ZEROS (ZERO-INDEX (car list)) (ZERO-INDEX (cadr list)) (FULL-GRID (length (car list)) (length (cadr list)) 1) (caddr list)) (caddr list)) ) (car list) (cadr list) (POSSIBLE-NEIGHBOR (NEIGHBOR-LIST (car (caddr list))) (FILTER-GRID-IMPOSSIBLE-POINTS (FILTER-GRID-ZEROS (ZERO-INDEX (car list)) (ZERO-INDEX (cadr list)) (FULL-GRID (length (car list)) (length (cadr list)) 1) (caddr list)) (caddr list))) (caddr list) (FILTER-GRID-IMPOSSIBLE-POINTS (FILTER-GRID-ZEROS (ZERO-INDEX (car list)) (ZERO-INDEX (cadr list)) (FULL-GRID (length (car list)) (length (cadr list)) 1) (caddr list)) (caddr list)))
  
  )
)

(TENTS-SOLUTION '( (1 3 0 3 1 3 2 3 2 2) (2 3 2 2 1 2 2 2 2 2) ( (1 2) (2 6) (2 7) (2 10) (3 2) (3 4) (4 9) (5 6) (5 7) (6 2) (6 8) (6 10) (7 1) (7 2) (7 4)(7 6)(9 3) (9 9) (10 2) (10 8))))