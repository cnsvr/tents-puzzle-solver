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

(define (IS-IN-BOUND point SIZE ) 
  (if (and (and (>= (car point) 1) (<= (car point) SIZE)) (and (>= (cadr point) 1) (<= (cadr point) SIZE)))
  #t
  #f
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
  (or #f (LISTMEMBEROFLIST (cdr list) val))
  ))  
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

(define (FILTER-NEIGHBOR neigbourList list)
  (if (null? neigbourList)
    '()
    (if (LISTMEMBEROFLIST list (car neigbourList))
      (cons (car neigbourList) (FILTER-NEIGHBOR (cdr neigbourList) list))
      (FILTER-NEIGHBOR (cdr neigbourList) list)
    )

  )
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

;(define (HORIZONTAL-NEIGHBOR-CHECK parameters))

;(define (VERTICAL-NEIGHBOR-CHECK parameters))

(define (POSITION-FOR-COL-TENTS positionForTents)
  (if (null? positionForTents)
    '()
    (cons (cadr (car (car positionForTents))) (append (POSITION-FOR-COL-TENTS (cdr positionForTents)) '()))
  )
)

(define (POSITION-FOR-ROW-TENTS positionForTents)
  (if (null? positionForTents)
    '()
    (cons (car (car (car positionForTents))) (append (POSITION-FOR-ROW-TENTS (cdr positionForTents)) '()))
  )
)


(define (DECREMENT-ELEMENTS-OF-LIST list listPos index)
  (if (null? listPos)
    list
    (if (eq? (car listPos) index)
      (cons (- (car list) 1) (DECREMENT-ELEMENTS-OF-LIST (cdr list) (cdr listPos) (+ 1 index)))
      (cons (car list) (DECREMENT-ELEMENTS-OF-LIST (cdr list) listPos (+ 1 index)))
    )

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

(define (FILTER-GRID-IMPOSSIBLE-POINTS gridList treeList)
  (if (null? gridList)
    '()
    (if (ISMEMBEROFLIST (NEIGHBOR-LIST (car gridList)) treeList)
      (cons (car gridList) (FILTER-GRID-IMPOSSIBLE-POINTS (cdr gridList) treeList))
      (FILTER-GRID-IMPOSSIBLE-POINTS (cdr gridList) treeList)
    )
  )
)

(define (COMPARE-POINTS point1 point2)
  (if (and (eq? (car point1) (car point2)) (eq? (cadr point1) (cadr point2)))
    #t
    #f
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

(define (COL-NUMBER-MATCH list colPos index)
  (if (null? list)
    index
    (if (eq? (cadr (car list)) colPos)
      (COL-NUMBER-MATCH (cdr list) colPos (+ index 1))
      (COL-NUMBER-MATCH (cdr list) colPos index)
    )
  )
)

(define (ROW-NUMBER-MATCH list rowPos index)
  (if (null? list)
    index
    (if (eq? (car (car list)) rowPos)
      (ROW-NUMBER-MATCH (cdr list) rowPos (+ index 1))
      (ROW-NUMBER-MATCH (cdr list) rowPos index)
    )
  )
)

(define (RETURN-POINT-IF-COL-MATCH colList gridList index)
  (if (null? colList)
    '()
    (if (eq? (car colList) (COL-NUMBER-MATCH gridList index 0) )
      index
      (RETURN-POINT-IF-COL-MATCH (cdr colList) gridList (+ 1 index))
    )
  )  
)
(define (RETURN-POINT-IF-ROW-MATCH rowList gridList index)
  (if (null? rowList)
    '()
    (if (eq? (car rowList) (ROW-NUMBER-MATCH gridList index 0) )
      index
      (RETURN-POINT-IF-ROW-MATCH (cdr rowList) gridList (+ 1 index))
    )
  )  
)

(define (RETURN-FIRST-POSSIBLE-TREE-OPTION-FOR-TENTS possibleList treeList)
  (if (null? possibleList)
    '()
    (if (LISTMEMBEROFLIST treeList (car possibleList))
      (car possibleList)
      (RETURN-FIRST-POSSIBLE-TREE-OPTION-FOR-TENTS (cdr possibleList) treeList)
    )

  )
)


(define (FIND-TENTS-AND-TREE-POINTS-WITH-ROW-INDEX treeList gridList rowIndex)
  (if (null? gridList)
    '()
    (if (eq? (car (car gridList)) rowIndex)
      (append (cons (cons (car gridList) (cons (RETURN-FIRST-POSSIBLE-TREE-OPTION-FOR-TENTS (NEIGHBOR-LIST (car gridList)) treeList) '())) '()) (FIND-TENTS-AND-TREE-POINTS-WITH-ROW-INDEX treeList (cdr gridList) rowIndex))
      (FIND-TENTS-AND-TREE-POINTS-WITH-ROW-INDEX treeList (cdr gridList) rowIndex)
    )
  )
)


(define (FIND-TENTS-AND-TREE-POINTS-WITH-COL-INDEX treeList gridList colIndex)
  (if (null? gridList)
    '()
    (if (eq? (cadr (car gridList)) colIndex)
      (append (cons (cons (car gridList) (cons (RETURN-FIRST-POSSIBLE-TREE-OPTION-FOR-TENTS (NEIGHBOR-LIST (car gridList)) treeList) '())) '()) (FIND-TENTS-AND-TREE-POINTS-WITH-COL-INDEX treeList (cdr gridList) colIndex))
      (FIND-TENTS-AND-TREE-POINTS-WITH-COL-INDEX treeList (cdr gridList) colIndex)
    )
  )
)

(define (REMOVE-POINTS-IF-HAS-EQUALITY list gridList)
  (if (null? list)
    gridList
    (REMOVE-POINTS-IF-HAS-EQUALITY (cdr list) (REMOVE-POINTS (cons (car (car list)) (ALL-NEIGHBOR (car (car list)))) gridList))
  )
)

(define (REMOVE-TREES-IF-HAS-EQUALITY list treeList)
  (if (null? list)
    treeList
    (REMOVE-TREES-IF-HAS-EQUALITY (cdr list) (REMOVE-POINTS (cons (cadr (car list)) '()) treeList))
  )
)



(define (ADD-TENTS-TO-LIST positionForTents tentList)
  (if (null? positionForTents)
    tentList
    (ADD-TENTS-TO-LIST (cdr positionForTents) (cons (car (car positionForTents)) tentList))
  )
)






(define ZERO-INDEX (lambda (rowList) (if (MEM rowList 0) (FIND-POSITION 0 rowList 1) '())) )

(define (RETURN-POINT-IF-ANY-TREE-HAS-ONE-POSSIBILITY treeList gridList)
  (if (null? treeList)
    '()                        ;ALL NEIGBHOR OLMASI LAZIM AMA HATA VERIYOR
    (if (eq? (length (FILTER-NEIGHBOR (NEIGHBOR-LIST (car treeList)) gridList)) 1)
      (append (FILTER-NEIGHBOR (NEIGHBOR-LIST (car treeList)) gridList) (cons (car treeList) '()))
      (RETURN-POINT-IF-ANY-TREE-HAS-ONE-POSSIBILITY (cdr treeList) gridList)
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

(define (SOLUTION rowList colList treeList gridList returnList)

  ; SOLUTION LOOP
  ; (if (length treeList) 0)
      ;returnList
      ;CALL SOLUTION AGAIN WTIH NEW LISTS

     
  (define rowZeroList (ZERO-INDEX rowList))
  (define colZeroList (ZERO-INDEX colList))


  (define filterGrid (FILTER-GRID-ZEROS rowZeroList colZeroList gridList treeList))

; CHANGE 0 values as - 1 at rowList and oloumnList

  (define filteredRowList (DECREMENT-ELEMENTS-OF-LIST rowList rowZeroList 1))
  (define filteredColList (DECREMENT-ELEMENTS-OF-LIST colList colZeroList 1))
  ;(display filteredRowList)
  ;(display filteredColList)



  ;filterGrid
  (define impossibleGrid (FILTER-GRID-IMPOSSIBLE-POINTS filterGrid treeList))
  ;(display "\n")
  ;(display treeList)
  ;(display "\n")

  ; row sayısı ve col sayısı tam eşleşiyorsa doldur oraları ;;??????

  (define rowEquality (RETURN-POINT-IF-ROW-MATCH filteredRowList impossibleGrid 1))
  

  ; There is a row equality or colomn equality 

  (define positionsForTents1 (if (not (null? rowEquality)) (FIND-TENTS-AND-TREE-POINTS-WITH-ROW-INDEX treeList impossibleGrid rowEquality) '()))

  ; IF there is row equality remove tree from treeList and put tents in tentList.
  (define newGridList1 (if (not (null? positionsForTents1)) (REMOVE-POINTS-IF-HAS-EQUALITY positionsForTents1 impossibleGrid) impossibleGrid))
  (define newTreeList1 (if (not (null? positionsForTents1)) (REMOVE-TREES-IF-HAS-EQUALITY positionsForTents1 treeList) treeList))

  ;IF there is row equality row position number will be -1 and col positions will be decremented.

  (define rowList1 (if (not (null? positionsForTents1)) (REPLACE-NTH filteredRowList rowEquality -1) filteredRowList))
  (define colList1 (if (not (null? positionsForTents1)) (DECREMENT-ELEMENTS-OF-LIST filteredColList (POSITION-FOR-COL-TENTS positionsForTents1) 1) filteredColList))

  (define tent1 (if (not (null? positionsForTents1)) (ADD-TENTS-TO-LIST positionsForTents1 returnList) returnList))

  ;IF there is col equality col position number will be -1 and row positions will be decremented.

  (define colEquality (RETURN-POINT-IF-COL-MATCH colList1 newGridList1 1))

  (define positionsForTents2 (if (not (null? colEquality)) (FIND-TENTS-AND-TREE-POINTS-WITH-COL-INDEX newTreeList1 newGridList1 colEquality) '()))

  ;IF there is row equality remove tree from treeList and put tents in tentList.
  (define newGridList2 (if (not (null? positionsForTents2)) (REMOVE-POINTS-IF-HAS-EQUALITY positionsForTents2 newGridList1) newGridList1))
  (define newTreeList2 (if (not (null? positionsForTents2)) (REMOVE-TREES-IF-HAS-EQUALITY positionsForTents2 newTreeList1) newTreeList1))

  ; new row and column list
  (define colList2 (if (not (null? positionsForTents2)) (REPLACE-NTH colList1 colEquality -1) colList1))
  (define rowList2 (if (not (null? positionsForTents2)) (DECREMENT-ELEMENTS-OF-LIST rowList1 (POSITION-FOR-ROW-TENTS positionsForTents2) 1) rowList1))

  ; put tents to tentlist

  (define tent2 (if (not (null? positionsForTents2)) (ADD-TENTS-TO-LIST positionsForTents2 tent1) tent1))



  (define oneOptionForTree (RETURN-POINT-IF-ANY-TREE-HAS-ONE-POSSIBILITY newTreeList2 newGridList2))

  
  
  (define newGridList3 (if (not (null? oneOptionForTree)) (REMOVE-POINTS (cons (car oneOptionForTree) (ALL-NEIGHBOR (car oneOptionForTree))) newGridList2) newGridList2))
  (define newTreeList3 (if (not (null? oneOptionForTree)) (REMOVE-POINTS (cons (cadr oneOptionForTree) '()) newTreeList2) newTreeList2))
    
  (define rowList3 (if (not (null? oneOptionForTree)) (DECREMENT-ELEMENTS-OF-LIST rowList2 (cons (car (car oneOptionForTree)) '()) 1) rowList2))
  (define colList3 (if (not (null? oneOptionForTree)) (DECREMENT-ELEMENTS-OF-LIST colList2 (cons (cadr (car oneOptionForTree)) '()) 1) colList2))
  (define tent3   (if (not (null? oneOptionForTree)) (cons (car oneOptionForTree) tent2) tent2))


  ;(display newTreeList)
  ;(display newGridList)
  ;(display "\n")
  ;(display newTreeList)
  ;(display "\n")
  ;(display newRowList)
  ;(display "\n")
  ;(display newColList)

  ;; NO SOLUTION PROBLEM??????????????????????
  (if (eq? (length newTreeList3) 0)
    tent3
    (SOLUTION rowList3 colList3 newTreeList3 newGridList3 tent3)
  )


)

(define (TENTS-SOLUTION list)
  (define rowList (car list))
  (define colList (cadr list))
  (define treeList (caddr list))
  
  (define gridList (FULL-GRID (length rowList) (length colList) 1))
  (SOLUTION rowList colList treeList gridList '())
)



(TENTS-SOLUTION '( (2 1 3 1 3 1 1 0) (1 2 1 3 0 2 1 2) ( (1 3) (1 6) (2 4) (2 8) (3 2) (3 5) (4 3) (4 6) (5 7) (6 4) (7 1) (8 3) ) ) )








