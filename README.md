# cse230-project

Team members: Yuhan Yang, Fangqi Yuan, Enze Ma, Hanfei He

## Milestone: Proposal

### Introduction

This project involves developing a grid-based adventure game in ``Haskell`` using the ``brick`` library. In this game, players control a *character*, represented by an asterisk `*`, navigating through a series of `15x15` grids. Each grid consists of *paths* (`white spaces`), *walls* (`black spaces`), and three types of collectible items: *bronze*, *silver*, and *gold*, valued at `1`, `2`, and `5` points respectively.

The game is structured into 5 levels, with increasing complexity and point requirements for completion. In each level, the player must gather items to accumulate points within a set number of steps or a time limit. Once the player moves over a path, it turns into a wall, preventing backtracking.

The objective is to collect enough points to pass the level before running out of steps or time. Failing to achieve the required points in a level results in a game over. Successfully completing all 5 levels signifies winning the game.

### Goals

- **Game Logic**: Create game mechanics with `Haskell`, ensuring the character's movement, item collection, and path-to-wall transformation.
- **Two Game Modes**: Develop a time-limited mode and a step-limited mode, challenging players to maximize their score under these constraints.
- **Level Design**: Design 5 increasingly challenging levels. Progress to the next level is based on achieving a certain score. Failure to achieve this score results in game over.
- **User Interface**: Use the `brick` library to create a visually appealing and intuitive interface for the game.
- **Scoring System**: Design a scoring system that evaluates players based on item collection and move efficiency.
- **User Experience**: Ensure clear instructions, real-time score display, and feedback on progress.
- **Development**: Utilize GitHub for version control, with all team members contributing equally and effectively. The project will be able to installed via `cabal` and `stack`. Unit tests will be written for key functions.

### Technologies

- **Programming Language**: `Haskell`
- **Library**: `brick` (for UI development)

### Updates
Currently we've implemented the basic module of the project, using blocks of different colors to represent items of different values. There are obstacles in the map that cannot be reached.

### To-do List
- Add timer. 
- Make all places the player has reached become obstacles. 
- Add more types of maps. 
- Add more levels and clearance settings. 
- Optimize item and player icons (optional).
- Add bomb area which should not be reached or the game will be terminated immediately (optional).