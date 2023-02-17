# What is the problem?
Our project will be implementing the game of Peg Solitaire in Haskell. During the game, a player is presented with a board that looks like this:
```
  1 2 3 4 5 6 7
a     · · ·    
b     · · ·
c · · · · · · · 
d · · · o · · · 
e · · · · · · · 
f     · · ·
g     · · ·
```
Where each dot represents a peg, and the circle in the middle represents an empty spot. The player can then move pegs according to the following rule:
A valid move is to jump a peg orthogonally over an adjacent peg into a hole two positions away and then to remove the jumped peg.
For example, the player could perform the following action: d6 to d4, resulting in the following board:  
```
  1 2 3 4 5 6 7
a     · · ·    
b     · · ·
c · · · · · · · 
d · · · · o o · 
e · · · · · · · 
f     · · ·
g     · · ·
```
The goal of the game is to remove all the pegs from the board.

# What is the something extra?
Our game will include a solver that, given the state of a Peg Solitaire game, will produce the steps needed to solve that game. Users can use this as a ‘hint’, to help them get unstuck, or just to complete the game for them.

