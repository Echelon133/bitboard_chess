# bitboard\_chess
Rust crate that implements a playable bitboard-based chessboard.

## Features
* serialization of chessboards into FEN strings
* creation of chessboards from FEN strings
* validation of moves
* iteration of all moves that are legal in the current position
* ending games prematurely (resignation/draws)

## Example use

```rust
use bitboard_chess::chessboard::{Chessboard, ChessboardError, GameResult};
use bitboard_chess::moves::UCIMove;

let mut board: Chessboard = Default::default();

// make a valid move
let mv = UCIMove::try_from("e2e4").unwrap();
let result = board.execute_move(&mv);
assert!(result.is_ok());

// try to make an illegal move
let mv = UCIMove::try_from("e4e5").unwrap();
let result = board.execute_move(&mv);
assert!(result.is_err());
assert_eq!(result.unwrap_err(), ChessboardError::IllegalMove);

// serialize the current position into a FEN string
let fen = board.as_fen();
assert_eq!(
     "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1",
     fen
 );

// count all legal moves of the current player (black pieces)
let num_legal_moves = board.iter_legal_moves().count();
assert_eq!(num_legal_moves, 20);

// force the game to end with a draw
board.set_draw();

// check if the game's result is set
let game_result = board.get_game_result();
assert_eq!(game_result, Some(GameResult::Draw));

// try to make a move despite the game being over
let mv = UCIMove::try_from("e7e5").unwrap();
let result = board.execute_move(&mv);
assert_eq!(result.unwrap_err(), ChessboardError::GameAlreadyFinished);
```

## perft

This crate includes an implementation of a [perft](https://www.chessprogramming.org/Perft)
function, which explores all possible nodes of a position and counts all leaf nodes of a certain depth.

Example perft run for a position represented by FEN string "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1".
```
// cargo test --release perft_fourth_position -- --show-output
================NODES===============
f1f2: 2703427
f3d4: 2928923
g1h1: 3212083
b4c5: 2027632
d2d4: 2816009
c4c5: 2145218
====================================
============PERFT RESULTS============
 D|       Nodes|  Checkmates|  Captures|     E.p| Castles| Promotions|  Checks
 1|           6|           0|         0|       0|       0|          0|       0
 2|         264|           0|        87|       0|       6|         48|      10
 3|        9467|          22|      1021|       4|       0|        120|      38
 4|      422333|           5|    131393|       0|    7795|      60032|   15492
 5|    15833292|       50562|   2046173|    6512|       0|     329464|  200568
```
