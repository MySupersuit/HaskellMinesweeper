# minesweeper

Main.hs implements the GUI

minesweeper.hs has the game logic

AIPlayer.hs has the AI logic

Currently implemented
- Pretty much a complete minesweeper including:
- Opening mines with left click, flagging mines with right click
- Opening a space with no surrounding mines propagates as expected.
- Clicking on an already opened space will attempt to open all mines around it.
- Losing reveals all mines.

- AI player makes most (I think it's all but not sure yet) obvious moves including moves which require looking at multiple squares (using Matrices + Reduced Row Echelon Form).
- Although it does not take number of remaining mines into account yet so doesn't solve some obvious end game scenarios yet.
- Best move when no obvious move (based on probability) also not implemented yet.

Not implemeneted
- First click guaranteed not to be a mine - the random mine coordinates are generated before the player clicks so not possible currently.
- Changing board size / #mines / difficulty not yet implemented.

Video attached: https://streamja.com/qVKQG
