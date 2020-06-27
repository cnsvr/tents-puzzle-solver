## TENTS PUZZLE SOLVER 

### This a simple Scheme Project.

### Introduction

  For this project,goal is implementing an automated solver for the Tents puzzle in Scheme. Here is [an online version](https://www.chiark.greenend.org.uk/~sgtatham/puzzles/js/tents.html) where you can play around. Even though the puzzle rules will be given here, it is highly recommended that you try solving a few yourself as well.
  
### Tents Rules

Place tents in the empty squares in such a way that:
  * No two tents are adjacent, even diagonally.
  * The number of tents in each row and column matches the numbers around the edge of the grid.
  * It is possible to match tents to trees so that each tree is horizontally or vertically adjacent to its own tent (but may also be adjacent to other tents that are matched with other trees).
  
### Input and Output Format

In Scheme the input puzzle will be in the following form:
  * ( (tent-count-row1 ... tent-count-rowN) (tent-count-column1 ... tent-count-columnM) ( (tree1-x tree1-y) ... (treeT-x treeT-y) ) )

For example the puzzle would be written down like this:
  * '( (2 1 3 1 3 1 1 0) (1 2 1 3 0 2 1 2) ( (1 3) (1 6) (2 4) (2 8) (3 2) (3 5) (4 3) (4 6) (5 7) (6 4) (7 1) (8 3) ) )
  
 In Scheme the answer for a puzzle will be in the following form:
  * ( (tents1-x tents1-y) ... (tentsT-x tentsT-y) )
  
 An example answer for this puzzle would be out- putted like this in Scheme:
 
  * ((7 3) (6 1) (5 4) (5 8) (5 6) (4 2) (3 6) (2 2) (3 8) (3 4) (1 7) (1 4))
