# bitboard\_chess
Rust crate that implements a playable bitboard-based chessboard.

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
