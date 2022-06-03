# Assignment 2: Advanced Functional Programming, Spring 2022

This codebase contains a minimal implementation of the game [Minesweeper](https://en.wikipedia.org/wiki/Minesweeper_%28video_game%29), which exercises several sophisticated features of Haskell.

In particular:

- The game board state types have their dimensions tracked in the type system, to guarantee that we're never indexing out of bounds at runtime.
- The algorithms to uncover cells on the game board and to populate the board with a random arrangement of mines both use state monads, and the mine population algorithm uses a monad transformer.
- The UI code uses the [Brick](https://github.com/jtdaugherty/brick/blob/master/README.md) library to provide an interactive terminal interface with text graphics.

For assignment 2, your job is to **extend** this codebase with additional features or implementation techniques. See the "Projects" section of this README for more details.

Check `TIPS.md` for a couple cheat sheets to help you through this assignment.

Let me know if you catch any mistakes in the code or documentation!


# Minesweeper

Take a few minutes to familiarize yourself with the rules of Minesweeper on Wikipedia or another source - the rules are pretty simple, but somewhat non-obvious.

Our code is divided into three files, which I recommend approaching in this order:

- `src/Grid.hs` defines the general `Grid` data structure that we use to represent various aspects of the Minesweeper game grid.
- `src/Minesweeper.hs` defines the rules of Minesweeper using the `Grid` type.
- `src/Main.hs` defines an interactive console UI for playing Minesweeper.

Each module is commented, but it will still probably take serious effort to decipher much of the code.

We'll spend time going through this code in week 8 of lecture, and as usual please don't hesitate to reach out over email/Zulip/Slack or in office hours if you have any more questions. I'm not really expecting you to do this assignment all on your own - I'm expecting that I'll help you out a bit!


# Building and running

This project comes with configuration for Cabal and Stack, so you should be able to compile and run it like any other Haskell project.

- Enter the REPL: `cabal repl`
- Compile the project: `cabal build`
- Run the compiled Minesweeper program: `cabal exec minesweeper`

In the game UI:

- Use the arrow keys to move around the board.
- Press the Enter key to select a cell.
- Press the Escape key to exit the game.


# Projects

For this assignment, your job is to **extend** this codebase with additional features or implementation techniques.

This README section lists ideas for small projects on this topic that you might take on. You're also welcome and encouraged to propose your own small project ideas to me.

To consider this assignment complete, undergraduate students are expected to complete at least 3 points worth of projects, and graduate students are expected to complete at least 6 points worth.

The points for each small project are "all or nothing" - you won't get partial points. You will get full points for a project even if you have a few mistakes or missing pieces, as long as you've done the work mostly correctly.

These point values are pretty flexible - let me know if you think the point value of a particular project doesn't feel like it reflects how hard you're working on it, and I'll be happy to adjust it.

For course grading, this work will count as both the planned assignment 2 and assignment 3. I am still going to try to get an alternative assignment 3 out in a week or two, though, for anyone who wants more variety in the projects available to take on.

You're welcome to modify or delete any of the code in the project **except** the definition of the `Grid` type, which you may only add `deriving` clauses to. You can also modify the Cabal file.

## 1 point: Add a test suite

Add a dependency for a testing package like [QuickCheck](https://hackage.haskell.org/package/QuickCheck) or [hedgehog](https://hackage.haskell.org/package/hedgehog), and implement a suite of unit tests to validate the behavior of the algorithms in `src/Grid.hs`.

For example:

- `index (replace i x g) i == x`
- `index (generate f) i == f i`
- `length (neighborhood g i) >= 3 && length (neighborhood g i) <= 8`

You shouldn't need more than around 10 of these tests in order to get decent test coverage with them.

Make sure that you test your tests: ideally, if you **break** the implementation of an algorithm in any way, your tests should **fail**. This is not necessarily obvious when you're first learning to write tests!

## 1 point: Explain how the code works

Each of these provided code files is commented, but mostly only at the top level of each function. For example, this code is provided in src/Grid.hs without a comment explaining how it works:

```
fmap (index g) $ catMaybes $ fmap (uncurry (liftA2 (,)))
  [ (dec  r, dec c), (dec r, Just c), (dec  r, inc c)
  , (Just r, dec c),                  (Just r, inc c)
  , (inc  r, dec c), (inc r, Just c), (inc  r, inc c)
  ]
```

Pick at least three multi-line definitions like this and explain how they work in English as clearly as possible. You can choose code from any of the three .hs files.

It may help to rewrite the definition using additional `let`/`in` expressions to give names to subexpressions.

In your explanation, identify the **specific** types of each major subexpression used in the definition.

For example, consider this short single-line definition of `gridToLists`:

```
gridToLists :: Grid w h a -> [[a]]
gridToLists = toList . fmap toList . gridCells
```

In this expression:

- `toList :: Vector h [a] -> [[a]]` (in the first occurrence of `toList`)
- `fmap toList :: Vector h (Vector w a) -> Vector h [a]`
- `gridCells :: Grid w h a -> Vector h (Vector w a)`

## 2 points: Quit automatically when the user wins or loses

In the provided code, the game does not end when the user wins or loses; it keeps going until they press the Escape key, and then it tells them if they won or lost.

Modify the codebase so that the game ends as soon as the user wins or loses. You can use the `gameLost` and `gameWon` functions from `src/Minesweeper.hs` to check for these conditions.

You should quit the game **after** uncovering the last move in the `AppState`, so that the user can see the final result of the game in the output of the program after the game quits.

To get started, look at the `selectCell` function, which is called every time the user selects a cell.

## 2 points: Refactor with `Reader` monad

In `uncoverState`, the `Survey w h` argument never changes through all of the recursive calls. This makes it a candidate for modeling with the Reader monad, so we don't have to keep passing it around explicitly.

Modify the code so that `uncoverState` and `uncoverCell` have these types:

```
uncoverState ::
  forall w h.
  KnownBounds w h =>
  StateT (SearchState w h) (Reader (Survey w h)) ()

uncoverCell ::
  forall w h.
  KnownBounds w h =>
  Index w h -> Cover w h -> Reader (Survey w h) (Cover w h)
```

This will also require changing any code that calls `uncoverCell`.

It's tricky to work out how the types should fit together in this exercise, but the resulting solution can be a pretty minimal code transformation.

## 3 points: Make sure the user never loses on the first move

In the provided version of this code, it is possible for the user to lose on the first move of a game by random chance, if the first cell they select happens to be covering a mine.

Traditional Minesweeper implementations often guarantee against this. There are two main strategies for implementing this guarantee:

- Wait to place any mines until after the user makes their first selection, and then make sure not to place a mine in the cell they selected.
- Place all the mines immediately when the game starts, but then if the user selects a mine on the first turn, move that one mine somewhere else and regenerate the numbers on the board before uncovering the cell they selected.

The user can't observe the difference between these two implementation techniques: from their perspective, they simply never lose on the first move, without any UI indication to show exactly how that happened.

Implement this feature so that the user never loses on the first move. You can use either of the implementation strategies outlined above, or any other that you want.

Here's a couple tips to start you off:

- In either of the strategies above, you'll probably want to add a `Field w h` value in the `AppState` type definition, and then you'll generate the mine placement somewhere in `selectCell`.
- You might also want a `Bool` in `AppState` to track whether it's the first move.
- To generate a random index on the board that's guaranteed not to include the selected cell, you can use the `chooseRandom` function defined in `src/Minesweeper.hs`: choose from a `Set` of all indices except the one you don't want.

## 3 points: Implement the "flag" feature

Traditional Minesweeper implementations have a feature that allows the user to place "flags" on covered cells to indicate where they think a mine might be.

To protect the user from accidentally selecting the wrong cell, some Minesweeper implementations also have an option to prevent uncovering a "flagged" cell.

Implement both of these features: the user should be able to flag a covered cell, and if they press the Enter key on a flagged cell, the UI should **not** uncover the cell.

In the UI, a flagged cell should show a `!` symbol, and the user should be able to flag a cell by pressing the spacebar.

In addition, when the `uncoverCell` algorithm uncovers cells other than the one the user selected, flagged cells should never be treated as zero-mine-count cells, even if there are zero adjacent mines. This means you'll have to modify the search algorithm to treat flagged cells differently so that it never automatically uncovers a flagged cell.

For 1 more point, make it so that it's impossible for the user to put down more flags than the total number of mines on the board.

Here's a couple tips to start you off:

- You'll probably want to add a `Flagged` constructor to the `CoverCell` type definition.
- You can do most of the work in the definition of `uncoveredState`.
- The `Key` constructor to use for the spacebar in `handleEvent` is `KChar ' '`.

## 5 points: Implement the classic Minesweeper cheat

Old versions of Windows Minesweeper have a "cheat code" built in: if you type `xyzzy`, then Shift+Enter, then Enter, the upper-left pixel of the monitor will start being used as an indicator of whether your cursor is over a mine or not.

Implement this feature so that this same sequence of keys triggers a "cheat mode" in the UI. Instead of changing the upper-left monitor pixel, your cheat mode should display a `$` symbol on the current highlighted cell if it is a covered mine.

(If you also implement the flag feature, it's your choice whether the `!` symbol takes precedence over the `$` symbol, either way is fine.)

The hardest part of this will be tracking the sequence of keys that activates the cheat: you can watch for an `x` input to start the cheat input, but the next six inputs (`yzzy`, Shift+Enter, Enter) must be done in the correct order, and with no other keys pressed in between them, or else the cheat mode shouldn't turn on.

Here's a couple tips to start you off:

- The `cellWidget` function is where the symbol on each cell is chosen each time the UI updates.
- To track the cheat input, you can use a list stored in `AppState` as a "ring buffer" (or a queue data structure); pushing to the end of a list has bad efficiency in the general case, but for a list of at most 7 elements it's perfectly fine.
- The `Key` constructors to use for the `x`/`y`/`z` keys are `KChar 'x'`/`KChar 'y'`/`KChar 'z'`. The Shift modifier constructor is `MShift`. The cheat shouldn't work if any modifiers are held down except in the Shift+Enter input.
