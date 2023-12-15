# cse230-project

Team members: Yuhan Yang, Fangqi Yuan, Enze Ma, Hanfei He

## Milestone 1: Proposal

### Introduction

This project involves developing a grid-based adventure game in ``Haskell`` using the ``brick`` library. In this game, players control a *character*, navigating through a series of grids. Each grid consists of *paths*, *walls*, and three types of collectible items: *Fries*, *Hotdog*, and *Burger*, valued at `1`, `2`, and `5` points respectively.

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

## Milestone 2: Updates
We've implemented the whole module of the project, using different icons to represent items of different values. There are obstacles(walls and bombs) in the map that cannot be reached.

### Check List
- Add timer. 
- Make all places the player has reached become obstacles. 
- Add more types of maps. 
- Add more levels and clearance settings. 
- Optimize item and player icons.
- Add bomb area which should not be reached or the game will be terminated immediately.

### Unit Test
We have developed unit tests for the `movePlayer` function. These tests will cover various scenarios such as moving the player in different directions, handling walls, bombs, and the use of wall breakers. 
- `Mock Game State`: The `createMockGame` function creates a mock Game state.
- `Testing Scenarios`: The tests cover four scenarios: moving without obstacles, hitting a wall, hitting a bomb, and using a wall breaker.
- `Running Tests`: Compile and run the Spec.hs to execute the tests. You can directly use `stack build` and `stack test` to run the tests.
