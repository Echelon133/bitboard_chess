//! This crate contains a chessboard implementation that's built using bitboards.
//!
//! Some of functionalities implemented by this crate:
//! * creation of chessboards from FEN strings (with validation)
//! * serialization of chessboards into FEN strings
//! * execution of moves (represented in the UCI format)
//! * rejection of moves that are invalid for the current state of the board
//! * iteration of all legal moves that are possible in a given position
//! * possibility of ending a game prematurely (propose draw or resign)

mod bitboard;
mod board;
pub mod chessboard;
mod chessboard_constants;
mod context;
mod movegen;
mod movegen_constants;
pub mod moves;
mod piece;
mod square;
