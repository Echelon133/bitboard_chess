//! This module implements a playable chessboard.
//!
//! `Chessboard` functionalities:
//! - creating board setups from FEN strings
//! - serializing boards into FEN strings
//! - iterating over all legal moves in a position
//! - executing legal moves
//! - ending games prematurely (resigning/agreed draws)
//! - probing for the result of the game
//!
//! ```
//! use bitboard_chess::chessboard::{Chessboard, ChessboardError, GameResult};
//! use bitboard_chess::moves::UCIMove;
//!
//! let mut board: Chessboard = Default::default();
//!
//! // make a valid move
//! let mv = UCIMove::try_from("e2e4").unwrap();
//! let result = board.execute_move(&mv);
//! assert!(result.is_ok());
//!
//! // try to make an illegal move
//! let mv = UCIMove::try_from("e4e5").unwrap();
//! let result = board.execute_move(&mv);
//! assert!(result.is_err());
//! assert_eq!(result.unwrap_err(), ChessboardError::IllegalMove);
//!
//! // serialize the current position into a FEN string
//! let fen = board.as_fen();
//! assert_eq!(
//!     "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1",
//!     fen
//! );
//!
//! // count all legal moves of the current player (black pieces)
//! let num_legal_moves = board.iter_legal_moves().count();
//! assert_eq!(num_legal_moves, 20);
//!
//! // force the game to end with a draw
//! board.set_draw();
//!
//! // check if the game's result is set
//! let game_result = board.get_game_result();
//! assert_eq!(game_result, Some(GameResult::Draw));
//!
//! // try to make a move despite the game being over
//! let mv = UCIMove::try_from("e7e5").unwrap();
//! let result = board.execute_move(&mv);
//! assert_eq!(result.unwrap_err(), ChessboardError::GameAlreadyFinished);
//! ```

use std::fmt::Debug;

use crate::bitboard;
use crate::board;
use crate::chessboard_constants::*;
use crate::context;
use crate::movegen;
use crate::moves;
use crate::piece;
use crate::square;

/// An iterator over [`movegen::MoveIter`] iterators.
///
/// There is a single [`movegen::MoveIter`] for every single square that's
/// occupied by the player who is about to make a move.
///
/// # Example
/// If the board has a default setup, and white is about to play, `MoveIterIter` will contain
/// 20 iterators (one for every white piece on the board), and each [`movegen::MoveIter`]
/// will give out pseudo-legal moves of a single piece.
#[derive(Clone, Copy)]
struct MoveIterIter {
    color_to_play: piece::Color,
    own_pieces: bitboard::SquareIter,
    inner_board: board::Board,
    context: context::Context,
}

impl MoveIterIter {
    pub fn new(inner_board: board::Board, context: context::Context) -> Self {
        let color_to_play = context.get_color_to_play();
        Self {
            color_to_play,
            own_pieces: inner_board.get_squares_taken(color_to_play).iter(),
            inner_board,
            context,
        }
    }
}

impl Iterator for MoveIterIter {
    type Item = movegen::MoveIter;

    /// Advances the iterator and returns a `MoveIter` which contains all
    /// pseudo-legal moves of a single piece on the board.
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(occupied_square) = self.own_pieces.next() {
            let piece_kind = self
                .inner_board
                .get_piece(occupied_square)
                .unwrap()
                .get_kind();

            let color_to_play = self.color_to_play;
            let (white, black) = self.inner_board.get_squares_taken_pair();
            let context = self.context;

            let move_iter = match piece_kind {
                piece::Kind::Pawn => {
                    movegen::find_pawn_moves(occupied_square, color_to_play, white, black, &context)
                }
                piece::Kind::Bishop => {
                    movegen::find_bishop_moves(occupied_square, color_to_play, white, black)
                }
                piece::Kind::Rook => {
                    movegen::find_rook_moves(occupied_square, color_to_play, white, black)
                }
                piece::Kind::Knight => {
                    movegen::find_knight_moves(occupied_square, color_to_play, white, black)
                }
                piece::Kind::Queen => {
                    movegen::find_queen_moves(occupied_square, color_to_play, white, black)
                }
                piece::Kind::King => {
                    movegen::find_king_moves(occupied_square, color_to_play, white, black, &context)
                }
            };
            Some(move_iter)
        } else {
            None
        }
    }
}

/// An iterator over legal moves of the player who is currently making a move.
///
/// This iterator returns items as long as there are pseudo-legal moves that are
/// verified as legal for the current position on the chessboard.
pub struct LegalMovesIter {
    iterators: MoveIterIter,
    current_iter: Option<movegen::MoveIter>,
    board: Chessboard,
}

impl LegalMovesIter {
    /// Creates a `LegalMovesIter`.
    fn new(mut iterators: MoveIterIter, board: Chessboard) -> Self {
        let current_iter = iterators.next();
        Self {
            iterators,
            current_iter,
            board,
        }
    }
}

impl Iterator for LegalMovesIter {
    type Item = moves::UCIMove;

    /// Advances the iterator and returns a legal move that can be executed
    /// on the board.
    ///
    /// If there are no more legal moves in the position, `None` is returned.
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match &mut self.current_iter {
                Some(iter) => match iter.next() {
                    Some(mv) => {
                        if self.board.is_move_legal(&mv) {
                            return Some(mv);
                        }
                    }
                    None => {
                        self.current_iter = self.iterators.next();
                        continue;
                    }
                },
                None => {
                    self.current_iter = self.iterators.next();
                    // two None in a row mean that there is no more moves
                    if self.current_iter.is_none() {
                        return None;
                    }
                }
            }
        }
    }
}

/// The end result of a game.
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum GameResult {
    /// Contains the color which won by resignation of the opposite color.
    SurrenderedWin(piece::Color),
    /// Contains the color which won by checkmating the opponent.
    Checkmate(piece::Color),
    Draw,
}

/// Information about a move that's been executed on the board.
///
/// This struct stores information about all events that might happen during
/// a move. These events include captures, en passants, castles, pawn promotions, 
/// and checks. Counting these events enables fast verification of the correctness
/// of the implementation.
#[derive(Copy, Clone, Debug)]
pub struct MoveInfo {
    pub captured_piece: bool,
    pub took_enpassant: bool,
    pub castled: bool,
    pub promoted: bool,
    pub checked_opponent: bool,
}

/// The result of a move on the board. It's used for signalizing whether a move
/// has resulted in the game being over. 
///
/// Every variant carries [`MoveInfo`] which stores information about events that
/// happened while executing a move. This includes captures, en passants, castles,
/// pawn promotions, and checks.
#[derive(Debug, Copy, Clone)]
pub enum MoveResult {
    /// The game has ended with a checkmate executed by the player whose color of
    /// pieces is stored in the `winner` field.
    Checkmate {
        winner: piece::Color,
        info: MoveInfo,
    },
    /// The game has ended with a draw (in case of a stalemate, `stalemate` is
    /// set to `true`).
    Draw {
        stalemate: bool,
        info: MoveInfo,
    },
    /// The game is not over yet.
    Continues {
        info: MoveInfo,
    },
}

/// Errors returned when `execute_move` method encounters a problem.
#[derive(Debug, PartialEq)]
pub enum ChessboardError {
    IllegalMove,
    GameAlreadyFinished,
}

/// A playable chessboard implementation.
///
/// This struct is responsible for:
/// - holding information about pieces
/// - holding information about castling rights, which color is to play, how many moves have
///     been played on the board
/// - providing methods for high-level manipulation of the board state
/// - making sure the move that's being executed is legal
pub struct Chessboard {
    inner_board: board::Board,
    context: context::Context,
    history: Vec<moves::TakenMove>,
    end_result: Option<GameResult>,
}

impl Clone for Chessboard {
    /// Returns a shallow copy of [`Chessboard`] that does not preserve the history
    /// of moves that have been played on it.
    fn clone(&self) -> Self {
        Self {
            inner_board: self.inner_board.clone(),
            context: self.context.clone(),
            history: Vec::new(),
            end_result: self.end_result,
        }
    }
}

impl Chessboard {
    /// Clones the [`board::Board`].
    fn clone_board(&self) -> board::Board {
        self.inner_board.clone()
    }

    /// Updates the chessboard context after making a move.
    ///
    /// # Updates
    ///
    /// ## Color to play
    /// After making a move, context's color should be flipped, so that it always
    /// shows who is making the next move.
    ///
    /// ## Halfmove counter
    /// The halfmove counter is reset after captures and pawn moves. Any other move
    /// results in the counter being incremented by one.
    ///
    /// ## Fullmove counter
    /// Fullmove counter should always be incremented after every move made by the
    /// player with black pieces.
    ///
    /// ## Castling rights
    /// Remove some (or all) castling rights of a player if:
    /// - the player castled
    /// - player's rook got captured on it's initial square
    /// - player's king moved from its initial square
    /// - player's rook moved from its initial square
    ///
    /// ## Enpassant target
    /// Set enpassant target if player's pawn moved two squares forward from its initial
    /// square. Any other move does not set the enpassant target.
    ///
    /// # Panics
    /// This method panics if chessboard's move history is empty.
    /// Apart from that,
    /// panics can occur if the method expects a piece on a certain square, but the
    /// square is actually empty. This can only happen if there are discrepancies between
    /// the move history and the actual state of the board, therefore shouldn't happen
    /// unless some invariant of the board is broken.
    fn update_context(&mut self) {
        // flip the color (which might also increment the fullmove counter)
        self.context.flip_color_to_play();

        // if the white is to play after the color flip, it means that the fullmove
        // counter should be incremented
        if self.context.get_color_to_play() == piece::Color::White {
            self.context.incr_fullmoves();
        }

        // always increment the halfmove counter
        // if it should have been reset, code below will reset it and
        // the counter state will be correct anyway
        self.context.incr_halfmoves();

        let last_move = self.history.last().unwrap();

        // disable castling if rook captured, and rook can be captured only by a piece
        // or a pawn that promotes
        let capture_info = match last_move {
            moves::TakenMove::PieceMove {
                m,
                captured_piece,
                ctx: _,
            } => Some((m, captured_piece)),
            moves::TakenMove::Promotion {
                m,
                captured_piece,
                ctx: _,
            } => Some((m, captured_piece)),
            _ => None,
        };
        if let Some((m, captured_piece)) = capture_info {
            if let Some(captured_piece) = captured_piece {
                // reset the halfmove counter because of a capture
                self.context.reset_halfmoves();

                let captured_square = m.get_target();
                if captured_piece.get_kind() == piece::Kind::Rook {
                    let side = match (captured_piece.get_color(), captured_square) {
                        // check if the captured square is a square where rooks of that color start
                        (piece::Color::White, WHITE_KSIDE_ROOK_START) => {
                            Some(context::Side::Kingside)
                        }
                        (piece::Color::White, WHITE_QSIDE_ROOK_START) => {
                            Some(context::Side::Queenside)
                        }
                        (piece::Color::Black, BLACK_KSIDE_ROOK_START) => {
                            Some(context::Side::Kingside)
                        }
                        (piece::Color::Black, BLACK_QSIDE_ROOK_START) => {
                            Some(context::Side::Queenside)
                        }
                        _ => None,
                    };
                    if let Some(side) = side {
                        self.context
                            .disable_castling(captured_piece.get_color(), side);
                    }
                }
            }
        }

        // enpassant always captures, which should result in reseting the halfmove counter
        if let moves::TakenMove::EnPassant { m: _, ctx: _ } = last_move {
            self.context.reset_halfmoves();
        }

        // captures always feature a pawn move (and sometimes a capture) so always reset
        if let moves::TakenMove::Promotion {
            m: _,
            captured_piece: _,
            ctx: _,
        } = last_move
        {
            self.context.reset_halfmoves();
        }

        // always set it to None, the code below will fix this if it actually should have
        // been set to Some square
        self.context.set_enpassant(None);

        // set enpassant if a pawn moved two squares from its initial square
        if let moves::TakenMove::PieceMove {
            m,
            captured_piece,
            ctx: _,
        } = last_move
        {
            // move that should set enpassant is noncapturing, so if the last move
            // was a capture, set enpassant to None
            if captured_piece.is_none() {
                let target_square = m.get_target();
                // if unwrap panics, it might mean that the last move's appeared in history,
                // but not actually on the board, which should never happen if invariants
                // are not broken
                let piece = self.inner_board.get_piece(target_square).unwrap();
                if piece.get_kind() == piece::Kind::Pawn {
                    let (start_rank, target_rank, color) = (
                        m.get_start().get_rank(),
                        m.get_target().get_rank(),
                        piece.get_color(),
                    );

                    let enpassant_target = match (start_rank, target_rank, color) {
                        (square::Rank::R2, square::Rank::R4, piece::Color::White) => Some(
                            square::Square::new(square::Rank::R3, m.get_target().get_file()),
                        ),
                        (square::Rank::R7, square::Rank::R5, piece::Color::Black) => Some(
                            square::Square::new(square::Rank::R6, m.get_target().get_file()),
                        ),
                        _ => None,
                    };
                    self.context.set_enpassant(enpassant_target);
                }
            }
        }

        // if last move was castling, the right to castle of the player who castled
        // should be revoked, as it no longer applies
        if let moves::TakenMove::Castling { s: _, ctx } = last_move {
            let castle_color = ctx.get_color_to_play();
            self.context
                .disable_castling(castle_color, context::Side::Kingside);
            self.context
                .disable_castling(castle_color, context::Side::Queenside);
        }

        // if last move was a rook move or a king move, check if they should disable
        // castling rights of the player who moved them
        if let moves::TakenMove::PieceMove {
            m,
            captured_piece: _,
            ctx: _,
        } = last_move
        {
            let (start_square, target_square) = (m.get_start(), m.get_target());
            // if unwrap panics, it might mean that the last move's appeared in history,
            // but not actually on the board, which should never happen if invariants
            // are not broken
            let piece = self.inner_board.get_piece(target_square).unwrap();
            let color = piece.get_color();
            match piece.get_kind() {
                // moving the king from its initial square removes both castling rights
                piece::Kind::King => match (color, start_square) {
                    (piece::Color::White, WHITE_KING_START) => {
                        self.context
                            .disable_castling(piece::Color::White, context::Side::Kingside);
                        self.context
                            .disable_castling(piece::Color::White, context::Side::Queenside);
                    }
                    (piece::Color::Black, BLACK_KING_START) => {
                        self.context
                            .disable_castling(piece::Color::Black, context::Side::Kingside);
                        self.context
                            .disable_castling(piece::Color::Black, context::Side::Queenside);
                    }
                    _ => (),
                },
                // moving the rook from its initial square removes the right to castle
                // on the side of that rook
                piece::Kind::Rook => match (color, start_square) {
                    (piece::Color::White, WHITE_QSIDE_ROOK_START) => self
                        .context
                        .disable_castling(piece::Color::White, context::Side::Queenside),
                    (piece::Color::White, WHITE_KSIDE_ROOK_START) => self
                        .context
                        .disable_castling(piece::Color::White, context::Side::Kingside),
                    (piece::Color::Black, BLACK_QSIDE_ROOK_START) => self
                        .context
                        .disable_castling(piece::Color::Black, context::Side::Queenside),
                    (piece::Color::Black, BLACK_KSIDE_ROOK_START) => self
                        .context
                        .disable_castling(piece::Color::Black, context::Side::Kingside),
                    _ => (),
                },
                // any pawn move resets the halfmove counter
                piece::Kind::Pawn => {
                    self.context.reset_halfmoves();
                }
                _ => (),
            }
        }
    }

    /// Executes a legal move on the board. Returns a [`ChessboardError`] if the
    /// move couldn't be executed for some reason. Otherwise [`MoveResult`] is
    /// returned.
    ///
    /// If the move is not legal, meaning:
    /// - it's incorrect for the type of piece which is being moved
    /// - the color of the piece is not correct for that turn
    /// - it puts its own king in check
    ///
    /// then such a move is rejected and does not appear on the board.
    /// 
    /// After executing a legal move, this method updates the context of the chessboard. This
    /// always includes flipping the color which is to play. Other context changes might include:
    /// - incrementing halfmove and fullmove counters
    /// - setting the en passant target
    /// - updating the castling rights of players
    ///
    /// Every move (except for illegal ones) returns an `Ok` that contains a `MoveResult` with
    /// information about what happend in that move (if there was a capture, en passant,
    /// castle, promotion, check). This information greatly improves the debugging experience.
    /// If the given move is illegal or the game is already finished, `Err` that contains
    /// `ChessboardError` is returned.
    pub fn execute_move(&mut self, m: &moves::UCIMove) -> Result<MoveResult, ChessboardError> {
        let captured_piece;
        let mut took_enpassant = false;
        let mut castled = false;
        let mut promoted = false;

        if self.end_result.is_some() {
            return Err(ChessboardError::GameAlreadyFinished);
        }

        // return error if the given move is a move that cannot even
        // appear on the board (for any disqualifying reason described in can_be_played
        // docs)
        if !self.can_be_played(m) {
            return Err(ChessboardError::IllegalMove);
        }

        match m {
            moves::UCIMove::Regular { m: mv } => {
                let (capture, enpassant, castle) = self.handle_regular_move(mv);
                captured_piece = capture;
                took_enpassant = enpassant;
                castled = castle;
            }
            moves::UCIMove::Promotion { m: mv, k: kind } => {
                captured_piece = self.handle_promotion_move(mv, *kind);
                promoted = true;
            }
        }

        self.update_context();

        // here it's the color of the next player which is yet to move
        let color_to_play = self.context.get_color_to_play();

        // check if the executed move has resulted in a checkmate
        let next_player_has_no_moves = self.iter_legal_moves().next().is_none();
        let is_king_in_check = self.is_king_in_check(color_to_play);

        let info = MoveInfo {
            captured_piece,
            took_enpassant,
            castled,
            promoted,
            checked_opponent: is_king_in_check,
        };

        if next_player_has_no_moves {
            let move_result;
            if is_king_in_check {
                // the winner is the previous color
                let winner = !color_to_play;
                move_result = MoveResult::Checkmate { winner, info };
                // set the result of the entire game
                self.end_result = Some(GameResult::Checkmate(winner));
            } else {
                // set the result of the entire game
                move_result = MoveResult::Draw {
                    stalemate: true,
                    info,
                };
                self.end_result = Some(GameResult::Draw);
            }
            Ok(move_result)
        } else {
            let (white_taken, black_taken) = self.inner_board.get_squares_taken_pair();
            // both players only have their kings, therefore it's a draw
            if white_taken.count_set() == 1 && black_taken.count_set() == 1 {
                let result = MoveResult::Draw {
                    stalemate: false,
                    info,
                };
                // set the result of the entire game
                self.end_result = Some(GameResult::Draw);
                Ok(result)
            } else {
                // TODO: implement detection of draws which happen because:
                // - the halfmoves counter reached 50
                // - there is not enough material to checkmate
                Ok(MoveResult::Continues { info })
            }
        }
    }

    /// Sets win as the game's final result and returns `true`. If the game already
    /// had a game result, `false` is returned and the existing game result is not changed.
    pub fn set_win(&mut self, winner: piece::Color) -> bool {
        match self.end_result {
            Some(_) => false,
            None => {
                self.end_result = Some(GameResult::SurrenderedWin(winner));
                true
            }
        }
    }

    /// Sets draw as the game's final result and returns `true`. If the game already
    /// had a game result, `false` is returned and the existing game result is not changed.
    pub fn set_draw(&mut self) -> bool {
        match self.end_result {
            Some(_) => false,
            None => {
                self.end_result = Some(GameResult::Draw);
                true
            }
        }
    }

    /// Returns `None` if the game is not over. If the game is over, `Some`
    /// containing a [`GameResult`] is returned.
    pub fn get_game_result(&self) -> Option<GameResult> {
        self.end_result
    }

    /// Returns true if the given move is a legal move in the current position
    /// on the board.
    ///
    /// This means, that the move:
    /// - has be start on a square that contains a piece
    /// - has to be an actual pseudo-legal move that's currently possible on the board for
    ///     the kind of piece that occupies that start square
    /// - has to be a pseudo-legal move that can be verified as legal by `is_move_legal`
    fn can_be_played(&mut self, m: &moves::UCIMove) -> bool {
        let start = match *m {
            moves::UCIMove::Regular { m } => m.get_start(),
            moves::UCIMove::Promotion { m, .. } => m.get_start(),
        };

        match self.inner_board.get_piece(start) {
            // there is a piece on the start square of the move
            Some(piece) => {
                let kind = piece.get_kind();
                let color_to_play = self.context.get_color_to_play();
                let (white, black) = self.inner_board.get_squares_taken_pair();
                let context = self.context;

                let pseudolegal_iter = match kind {
                    piece::Kind::Pawn => {
                        movegen::find_pawn_moves(start, color_to_play, white, black, &context)
                    }
                    piece::Kind::Bishop => {
                        movegen::find_bishop_moves(start, color_to_play, white, black)
                    }
                    piece::Kind::Rook => {
                        movegen::find_rook_moves(start, color_to_play, white, black)
                    }
                    piece::Kind::Knight => {
                        movegen::find_knight_moves(start, color_to_play, white, black)
                    }
                    piece::Kind::Queen => {
                        movegen::find_queen_moves(start, color_to_play, white, black)
                    }
                    piece::Kind::King => {
                        movegen::find_king_moves(start, color_to_play, white, black, &context)
                    }
                };

                // iterate over pseudolegal moves to find the same move as in the argument
                // of this method
                for pseudolegal_mv in pseudolegal_iter {
                    if pseudolegal_mv == *m {
                        // if there is such a pseudo-legal move, check if it's legal
                        return self.is_move_legal(&pseudolegal_mv);
                    }
                }
                // didn't find the move while iterating, therefore the move is not even
                // pseudo-legal, so it definitely cannot be played
                false
            }
            // no piece on the start square, therefore no move
            None => false,
        }
    }

    /// Checks whether the given pseudo-legal move is legal, i.e. does not put the king in check.
    ///
    /// This method executes the given move, checks whether the king is in check, and if it is,
    /// it deems that move illegal and returns `false`.
    /// A special case occurs when evaluating a castling move, because there are additional rules,
    /// which state that:
    /// - the king cannot castle when it's in check
    /// - the king cannot castle if any square between it and the rook is attacked (the rook
    ///     can be attacked)
    ///
    /// Conditions such as:
    /// - castling is possible if neither the king nor the particular rook has moved
    /// - squares between the rook and the king have to be empty
    ///
    /// are already satisfied by the [`movegen::find_king_moves`] function which only
    /// generates castling moves if both of these conditions are satisfied.
    ///
    /// # Safety:
    /// This method might break some invariants of the chessboard (e.g. might put the chessboard
    /// in a state in which an illegal move is temporarily placed on the board), but
    /// since it restores the chessboard to a previous valid state right before it returns,
    /// and it holds the mutable reference to the entire chessboard, it's safe to break
    /// these invariants because no other part of the code can access the board's state while
    /// these invariants are temporarily broken.
    ///
    /// # Panics:
    /// This method panics if the move that's given to it is not even pseudo-legal,
    /// i.e. it wants to move a piece from a square that's not even occupied on the board.
    fn is_move_legal(&mut self, m: &moves::UCIMove) -> bool {
        let own_color = self.context.get_color_to_play();
        let board_copy = self.clone_board();
        match m {
            moves::UCIMove::Regular { m: mv } => {
                self.handle_regular_move(mv);

                // special case, check if the last move was castling, and if it was,
                // check if any of the squares between the king (inclusive) and the rook
                // are being attacked
                let last_move = self.history.last().unwrap(); // guaranteed Some
                match last_move {
                    moves::TakenMove::Castling { s, ctx: _ } => {
                        let castling_path: &[square::Square] = match (*s, own_color) {
                            (context::Side::Kingside, piece::Color::White) => {
                                &WHITE_KINGSIDE_CASTLING_PATH
                            }
                            (context::Side::Kingside, piece::Color::Black) => {
                                &BLACK_KINGSIDE_CASTLING_PATH
                            }
                            (context::Side::Queenside, piece::Color::White) => {
                                &WHITE_QUEENSIDE_CASTLING_PATH
                            }
                            (context::Side::Queenside, piece::Color::Black) => {
                                &BLACK_QUEENSIDE_CASTLING_PATH
                            }
                        };

                        let mut castling_path_squares_attacked = false;
                        for square in castling_path.iter() {
                            if movegen::is_square_attacked(*square, own_color, &self.inner_board) {
                                castling_path_squares_attacked = true;
                                break;
                            }
                        }

                        self.undo_last_move(&board_copy);
                        // if any square on the castling path is attacked,
                        // then the entire castling move is illegal
                        // NOTE: this returns early, since there is no need to call
                        // is_king_in_check, because one of the squares that's checked above
                        // is a square where king's presence is guaranteed, so it makes
                        // no sense to check that again in case none of the squares
                        // in the castling path are attacked
                        return !castling_path_squares_attacked;
                    }
                    _ => (),
                }
            }
            moves::UCIMove::Promotion { m: mv, k: kind } => {
                self.handle_promotion_move(mv, *kind);
            }
        }

        let legal = !self.is_king_in_check(own_color);
        self.undo_last_move(&board_copy);
        legal
    }

    /// Restores the previous [`context::Context`] and sets the board's inner state
    /// to the state of the [`board::Board`] given as the argument.
    ///
    /// # Panics
    /// Panics if the history of moves that have been played on the board is empty.
    fn undo_last_move(&mut self, board_copy: &board::Board) {
        self.inner_board = *board_copy;
        let last_ctx = match self.history.pop().unwrap() {
            moves::TakenMove::PieceMove {
                m: _,
                captured_piece: _,
                ctx,
            } => ctx,
            moves::TakenMove::Promotion {
                m: _,
                captured_piece: _,
                ctx,
            } => ctx,
            moves::TakenMove::EnPassant { m: _, ctx } => ctx,
            moves::TakenMove::Castling { s: _, ctx } => ctx,
        };
        self.context = last_ctx;
        self.end_result = None;
    }

    /// Checks if the given piece and move of that piece describe castling. If they do,
    /// it returns `Some` with the side of castling which is described.
    /// Otherwise it returns `None`.
    #[inline(always)]
    fn is_castling(piece: &piece::Piece, m: &moves::Move) -> Option<context::Side> {
        if piece.get_kind() != piece::Kind::King {
            return None;
        }

        let king_color = piece.get_color();
        let (castle_start, kingside_target, queenside_target) = match king_color {
            piece::Color::White => (
                square::Square::new(square::Rank::R1, square::File::E),
                square::Square::new(square::Rank::R1, square::File::G),
                square::Square::new(square::Rank::R1, square::File::C),
            ),
            piece::Color::Black => (
                square::Square::new(square::Rank::R8, square::File::E),
                square::Square::new(square::Rank::R8, square::File::G),
                square::Square::new(square::Rank::R8, square::File::C),
            ),
        };

        let target = m.get_target();
        if m.get_start() == castle_start {
            if target == kingside_target {
                Some(context::Side::Kingside)
            } else if target == queenside_target {
                Some(context::Side::Queenside)
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Checks if the given arguments describe a move that's an enpassant move.
    /// This means that:
    /// - the `piece` must be a pawn
    /// - `enpassant_target` is not `None`
    /// - [`moves::Move`] target square is the same as the square in `enpassant_target`
    ///
    /// If any of these conditions is not met, then it's certain that the given arguments
    /// do not describe an en passant capture.
    #[inline(always)]
    fn is_enpassant(
        piece: &piece::Piece,
        m: &moves::Move,
        enpassant_target: Option<square::Square>,
    ) -> bool {
        if piece.get_kind() != piece::Kind::Pawn {
            false
        } else {
            if let Some(enpassant_target) = enpassant_target {
                enpassant_target == m.get_target()
            } else {
                false
            }
        }
    }

    /// Handles all legal or pseudo-legal moves that are not pawn promotions.
    /// Returns `(bool, bool, bool)` which represents information about events
    /// that happened during this move: (captured_piece, took_enpassant, castled).
    ///
    /// To handle pawn promotions, use [`Self::handle_promotion_move`]
    ///
    /// # Panics
    /// This method will panic if the given move is incorrect for the given board state,
    /// and e.g. wants to move a piece from a square that's not occupied.
    #[inline(always)]
    fn handle_regular_move(&mut self, m: &moves::Move) -> (bool, bool, bool) {
        let piece = self.inner_board.get_piece(m.get_start()).unwrap();

        if Chessboard::is_enpassant(&piece, m, self.context.get_enpassant()) {
            self.handle_enpassant_move(m);
            (true, true, false)
        } else if let Some(side) = Chessboard::is_castling(&piece, m) {
            self.handle_castling_move(m, side);
            (false, false, true)
        } else {
            let captured_piece = self.handle_piece_move(m);
            (captured_piece, false, false)
        }
    }

    /// Handles legal or pseudo-legal en passant moves.
    ///
    /// This method should only be given a `moves::Move` that is guaranteed
    /// to be an en passant move. Calling this method with any other type of move
    /// will put the board in a broken state, because this method does not even
    /// check whether the given move describes a pawn move.
    ///
    /// # Panics
    /// This method panics if there is no piece on the start square of the
    /// `moves::Move` that's passed as an argument.
    /// It also panics if the en passant target square's rank is neither
    /// the 3rd nor the 6th rank, because it means that the given move is
    /// definitely invalid.
    ///
    #[inline(always)]
    fn handle_enpassant_move(&mut self, m: &moves::Move) {
        let saved_context = self.context.clone();
        let (start, target) = (m.get_start(), m.get_target());
        let pawn = self.inner_board.remove_piece(start).unwrap();

        self.inner_board.place_piece(target, &pawn);
        // capture the piece en-passant
        let capture_rank = match target.get_rank() {
            square::Rank::R3 => square::Rank::R4,
            square::Rank::R6 => square::Rank::R5,
            _ => panic!("incorrect enpassant target"),
        };

        // calculate the square from which the pawn should be captured en-passant
        let captured_pawn_sq = square::Square::new(capture_rank, target.get_file());
        self.inner_board.remove_piece(captured_pawn_sq);
        self.history.push(moves::TakenMove::EnPassant {
            m: *m,
            ctx: saved_context,
        });
    }

    /// Handles legal or pseudo-legal castling.
    ///
    /// This method should only be given a `moves::Move` that is guaranteed
    /// to be a castling move. Calling this method with any other type of move
    /// will put the board in a broken state.
    ///
    /// # Panics
    /// This method panics if there is no piece on the start square of the
    /// `moves::Move`.
    #[inline(always)]
    fn handle_castling_move(&mut self, m: &moves::Move, side: context::Side) {
        let saved_context = self.context.clone();
        let (king_start, king_target) = (m.get_start(), m.get_target());
        let king_piece = self.inner_board.remove_piece(king_start).unwrap();

        let castling_side_color = king_piece.get_color();
        let (rook_start_square, rook_target_square) = match castling_side_color {
            piece::Color::White => {
                if side == context::Side::Kingside {
                    (
                        square::Square::new(square::Rank::R1, square::File::H),
                        square::Square::new(square::Rank::R1, square::File::F),
                    )
                } else {
                    (
                        square::Square::new(square::Rank::R1, square::File::A),
                        square::Square::new(square::Rank::R1, square::File::D),
                    )
                }
            }
            piece::Color::Black => {
                if side == context::Side::Kingside {
                    (
                        square::Square::new(square::Rank::R8, square::File::H),
                        square::Square::new(square::Rank::R8, square::File::F),
                    )
                } else {
                    (
                        square::Square::new(square::Rank::R8, square::File::A),
                        square::Square::new(square::Rank::R8, square::File::D),
                    )
                }
            }
        };
        // move both the king and the rook to achieve castling
        let rook_piece = self.inner_board.remove_piece(rook_start_square).unwrap();
        self.inner_board
            .place_piece(rook_target_square, &rook_piece);
        self.inner_board.place_piece(king_target, &king_piece);
        self.history.push(moves::TakenMove::Castling {
            s: side,
            ctx: saved_context,
        });
    }

    /// Handles all legal or pseudo-legal moves that are NOT castling,
    /// en passant or pawn promotions. Updates castling flags depending on which pieces
    /// moved. Sets en passant target square when a pawn gets moved two squares from it's
    /// initial rank. Returns `true` if the move resulted in a capture of opponent's piece.
    ///
    /// This means that:
    /// - both sides of castling are disabled if the king moves from it's initial square
    /// - certain side of castling is disabled if the rook from that side moves from it's
    ///     initial square
    ///
    /// # Panics
    /// This method panics if there is no piece on the start square of the
    /// `moves::Move`.
    #[inline(always)]
    fn handle_piece_move(&mut self, m: &moves::Move) -> bool {
        let saved_context = self.context.clone();
        let (start, target) = (m.get_start(), m.get_target());
        let piece = self.inner_board.remove_piece(start).unwrap();

        let captured_piece = self.inner_board.place_piece(target, &piece);
        let was_capturing = captured_piece.is_some();
        self.history.push(moves::TakenMove::PieceMove {
            m: *m,
            captured_piece,
            ctx: saved_context,
        });
        was_capturing
    }

    /// Handles all legal or pseudo-legal pawn promotions.
    ///
    /// # Panics
    /// This method panics if there is no piece on the start square of the
    /// `moves::Move`.
    #[inline(always)]
    fn handle_promotion_move(&mut self, m: &moves::Move, k: piece::Kind) -> bool {
        let saved_context = self.context.clone();

        let (start, target) = (m.get_start(), m.get_target());
        let promoted_pawn = self.inner_board.remove_piece(start).unwrap();
        let promotion_goal = piece::Piece::new(k, promoted_pawn.get_color());
        let captured_piece = self.inner_board.place_piece(target, &promotion_goal);
        let was_capturing = captured_piece.is_some();
        self.history.push(moves::TakenMove::Promotion {
            m: *m,
            captured_piece,
            ctx: saved_context,
        });
        was_capturing
    }

    /// Returns [`true`] if the king is in check. Currently not the fastest implementation
    /// there could be.
    ///
    /// # Panics
    /// Panics if the board does not contain a king of the given color.
    fn is_king_in_check(&self, king_color: piece::Color) -> bool {
        movegen::is_king_in_check(king_color, &self.inner_board)
    }

    /// Returns an iterator over legal moves of the player who is
    /// currently about to make a move.
    pub fn iter_legal_moves(&mut self) -> LegalMovesIter {
        let pseudolegal = MoveIterIter::new(self.inner_board, self.context);
        LegalMovesIter::new(pseudolegal, self.clone())
    }

    /// Serializes the current chessboard's position into a FEN string.
    pub fn as_fen(&self) -> String {
        let mut fen = String::new();

        for rank in (0..=7).rev() {
            let mut empty_in_row = 0;
            for file in 0..=7 {
                let square_i = (rank * 8) + file;
                let square = square::Square::from(square_i);
                let piece = self.inner_board.get_piece(square);
                match piece {
                    Some(piece) => {
                        if empty_in_row > 0 {
                            fen.push_str(empty_in_row.to_string().as_str());
                            empty_in_row = 0;
                        }
                        fen.push_str(piece.to_string().as_ref());
                    }
                    None => {
                        empty_in_row += 1;
                        if square.get_file() == square::File::H {
                            fen.push_str(empty_in_row.to_string().as_str());
                        }
                    }
                }
            }
            if rank != 0 {
                fen.push('/');
            }
        }
        let fen_context = format!(" {:?}", self.context);
        fen.push_str(&fen_context);
        fen
    }
}

impl Default for Chessboard {
    /// Creates a [`Chessboard`] with a default setup of pieces.
    ///
    /// The default chessboard is equivalent to a board started from this FEN:
    /// - "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    fn default() -> Self {
        Self {
            inner_board: board::Board::default(),
            context: context::Context::default(),
            history: Vec::new(),
            end_result: None,
        }
    }
}

impl TryFrom<&str> for Chessboard {
    type Error = &'static str;

    /// Creates a [`Chessboard`] from a FEN string.
    ///
    /// Example FEN strings:
    /// - "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    /// - "1k5b/8/r5p1/6P1/1P6/4P3/8/4K3 w - - 0 1"
    ///
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let all_fen_elements = value.split(' ').collect::<Vec<&str>>();

        if all_fen_elements.len() != 6 {
            return Err("invalid number of values in FEN");
        }

        let board = all_fen_elements[0];
        let context = &value[board.len() + 1..];

        let board = board::Board::try_from(board)?;
        let context = context::Context::try_from(context)?;

        Ok(Self {
            inner_board: board,
            context,
            history: Vec::new(),
            end_result: None,
        })
    }
}

impl Debug for Chessboard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}\n", self.inner_board)?;
        let color = match self.context.get_color_to_play() {
            piece::Color::White => "White",
            piece::Color::Black => "Black",
        };
        write!(f, "{} to play\n", color)?;

        let mut castling_symbols = String::new();
        // the order is important, because if each side has all rights
        // to castle, then the result should be "KQkq"
        let castling_to_check = [
            (piece::Color::White, context::Side::Kingside, 'K'),
            (piece::Color::White, context::Side::Queenside, 'Q'),
            (piece::Color::Black, context::Side::Kingside, 'k'),
            (piece::Color::Black, context::Side::Queenside, 'q'),
        ];
        for (color, side, symbol) in castling_to_check {
            if self.context.can_castle(color, side) {
                castling_symbols.push(symbol);
            }
        }
        if castling_symbols.len() == 0 {
            castling_symbols.push('-');
        }

        write!(f, "Castling: {}\n", castling_symbols)?;

        let enpassant_target = match self.context.get_enpassant() {
            Some(target) => format!("{:?}", target),
            None => String::from("-"),
        };

        write!(f, "En passant: {}\n", enpassant_target)?;
        write!(f, "Halfmove: {}\n", self.context.get_halfmoves())?;
        write!(f, "Fullmove: {}\n", self.context.get_fullmoves())
    }
}

#[cfg(test)]
mod tests {
    use crate::chessboard::*;

    #[test]
    fn chessboard_debug_works() {
        let board = Chessboard::default();

        let expected_debug = r#"8 [r][n][b][q][k][b][n][r]
7 [p][p][p][p][p][p][p][p]
6 [ ][ ][ ][ ][ ][ ][ ][ ]
5 [ ][ ][ ][ ][ ][ ][ ][ ]
4 [ ][ ][ ][ ][ ][ ][ ][ ]
3 [ ][ ][ ][ ][ ][ ][ ][ ]
2 [P][P][P][P][P][P][P][P]
1 [R][N][B][Q][K][B][N][R]
   A  B  C  D  E  F  G  H

White to play
Castling: KQkq
En passant: -
Halfmove: 0
Fullmove: 1
"#;

        let actual_debug = format!("{:?}", board);
        assert_eq!(expected_debug, actual_debug);
    }

    #[test]
    fn chessboard_try_from_str_works() {
        let board = Chessboard::try_from("1k5b/8/r5p1/6P1/1P6/4P3/8/4K3 w - - 1 14").unwrap();

        let expected_debug = r#"8 [ ][k][ ][ ][ ][ ][ ][b]
7 [ ][ ][ ][ ][ ][ ][ ][ ]
6 [r][ ][ ][ ][ ][ ][p][ ]
5 [ ][ ][ ][ ][ ][ ][P][ ]
4 [ ][P][ ][ ][ ][ ][ ][ ]
3 [ ][ ][ ][ ][P][ ][ ][ ]
2 [ ][ ][ ][ ][ ][ ][ ][ ]
1 [ ][ ][ ][ ][K][ ][ ][ ]
   A  B  C  D  E  F  G  H

White to play
Castling: -
En passant: -
Halfmove: 1
Fullmove: 14
"#;

        let actual_debug = format!("{:?}", board);
        assert_eq!(expected_debug, actual_debug);
    }

    #[test]
    fn chessboard_cannot_castle_kingside_through_check() {
        // white to play, castling should not be possible in any of these positions
        let fens = [
            "rnb1kbnr/p1pp4/1p4p1/1B2q3/8/N1P5/PP1P1PPP/R1BQK2R w KQkq - 0 1", // e1 attacked
            "rnb1kbnr/p1pp4/1p4p1/1B6/2q5/N1P5/PP1P1PPP/R1BQK2R w KQkq - 0 1", // f1 attacked
            "rnb1kbnr/p1pp4/1p4p1/1Bq5/8/N1P2P2/PP1P2PP/R1BQK2R w KQkq - 0 1", // g1 attacked
        ];

        let white_castling_move = moves::UCIMove::try_from("e1g1").unwrap();
        for fen in fens {
            let mut board = Chessboard::try_from(fen).unwrap();

            let available_moves = board.iter_legal_moves().collect::<Vec<moves::UCIMove>>();
            // should not contain the move, because castling through an attack is illegal
            assert!(!available_moves.contains(&white_castling_move));
        }

        // black to play, castling should not be possible in any of these positions
        let fens = [
            "rnbqk2r/pppp3p/8/8/3P4/4Q3/PPP1PPPP/R1B1K1NR b KQkq - 0 1", // e8 attacked
            "rnbqk2r/pppp3p/8/8/3P4/5Q2/PPP1PPPP/R1B1K1NR b KQkq - 0 1", // f8 attacked
            "rnbqk2r/pppp3p/8/8/3P4/6Q1/PPP1PPPP/R1B1K1NR w KQkq - 0 1", // g8 attacked
        ];
        let black_castling_move = moves::UCIMove::try_from("e8g8").unwrap();
        for fen in fens {
            let mut board = Chessboard::try_from(fen).unwrap();

            let available_moves = board.iter_legal_moves().collect::<Vec<moves::UCIMove>>();
            // should not contain the move, because castling through an attack is illegal
            assert!(!available_moves.contains(&black_castling_move));
        }
    }

    #[test]
    fn chessboard_cannot_castle_queenside_through_check() {
        // white to play, castling should not be possible in any of these positions
        let fens = [
            "rnb1kbnr/p3pppp/8/q7/8/8/P3PPPP/R3KBNR w KQkq - 0 1", // e1 attacked
            "rnb1kbnr/p3pppp/8/3q4/8/8/P3PPPP/R3KBNR w KQkq - 0 1", // d1 attacked
            "rnb1kbnr/p3pppp/8/2q5/8/8/P3PPPP/R3KBNR w KQkq - 0 1", // c1 attacked
        ];

        let white_castling_move = moves::UCIMove::try_from("e1c1").unwrap();
        for fen in fens {
            let mut board = Chessboard::try_from(fen).unwrap();

            let available_moves = board.iter_legal_moves().collect::<Vec<moves::UCIMove>>();
            // should not contain the move, because castling through an attack is illegal
            assert!(!available_moves.contains(&white_castling_move));
        }

        // black to play, castling should not be possible in any of these positions
        let fens = [
            "r3kbnr/p3pppp/8/8/Q7/8/P3PPPP/RNB1KBNR b KQkq - 0 1", // e8 attacked
            "r3kbnr/p3pppp/8/8/3Q4/8/P3PPPP/RNB1KBNR b KQkq - 0 1", // d8 attacked
            "r3kbnr/p3pppp/8/8/2Q5/8/P3PPPP/RNB1KBNR b KQkq - 0 1", // c8 attacked
        ];

        let black_castling_move = moves::UCIMove::try_from("e8c8").unwrap();
        for fen in fens {
            let mut board = Chessboard::try_from(fen).unwrap();

            let available_moves = board.iter_legal_moves().collect::<Vec<moves::UCIMove>>();
            // should not contain the move, because castling through an attack is illegal
            assert!(!available_moves.contains(&black_castling_move));
        }
    }

    #[test]
    fn chessboard_can_castle_kingside_when_only_rook_attacked() {
        // attack white h1 rook
        let rook_h1_attacked = "rnb1kbnr/ppp3pp/8/3q4/8/8/PPPPP2P/RNBQK2R w KQkq - 0 1";
        let white_castling_move = moves::UCIMove::try_from("e1g1").unwrap();
        let mut board = Chessboard::try_from(rook_h1_attacked).unwrap();
        let available_moves = board.iter_legal_moves().collect::<Vec<moves::UCIMove>>();
        // should contain the move, because castling when only the rook is attacked is legal
        assert!(available_moves.contains(&white_castling_move));

        // attack black h8 rook
        let rook_h8_attacked = "rnbqk2r/pppp3p/8/3P4/8/2Q5/PPP1PPPP/R1B1K1NR b KQkq - 0 1";
        let black_castling_move = moves::UCIMove::try_from("e8g8").unwrap();
        let mut board = Chessboard::try_from(rook_h8_attacked).unwrap();
        let available_moves = board.iter_legal_moves().collect::<Vec<moves::UCIMove>>();
        // should contain the move, because castling when only the rook is attacked is legal
        assert!(available_moves.contains(&black_castling_move));
    }

    #[test]
    fn chessboard_can_castle_queenside_when_outside_path_attacked() {
        // both rook on a1 and the square b1 can be attacked without temporarily disabling
        // queenside castling
        let fens = [
            "r3kbnr/p3pppp/8/4q3/8/8/P3PPPP/R3KBNR w KQkq - 0 1", // white rook a1 attacked
            "r3kbnr/p3pppp/8/8/8/n7/P3PPPP/R3KBNR w KQkq - 0 1",  // empty b1 is attacked
        ];

        for fen in fens {
            let white_castling_move = moves::UCIMove::try_from("e1c1").unwrap();
            let mut board = Chessboard::try_from(fen).unwrap();
            let available_moves = board.iter_legal_moves().collect::<Vec<moves::UCIMove>>();
            assert!(available_moves.contains(&white_castling_move));
        }

        // both rook on a8 and the square b8 can be attacked without temporarily disabling
        // queenside castling
        let fens = [
            "r3kbnr/p3pppp/8/8/1Q6/8/P3PPPP/RNB1KBNR b KQkq - 0 1", // black rook a8 attacked
            "r3kbnr/p3pppp/N7/8/8/8/P3PPPP/R1B1KBNR b KQkq - 0 1",  // empty b8 is attacked
        ];

        for fen in fens {
            let black_castling_move = moves::UCIMove::try_from("e8c8").unwrap();
            let mut board = Chessboard::try_from(fen).unwrap();
            let available_moves = board.iter_legal_moves().collect::<Vec<moves::UCIMove>>();
            assert!(available_moves.contains(&black_castling_move));
        }
    }

    #[test]
    fn chessboard_pawn_moves_reset_halfmove_counter() {
        let mut board = Chessboard::default();

        // make only pawn moves, which should result in the halfmove counter
        // always staying at 0
        let moves = ["e2e4", "e7e5", "d2d3", "d7d6", "f2f3", "f7f6"];
        for mv in moves {
            let _ = board.execute_move(&moves::UCIMove::try_from(mv).unwrap());
        }

        let expected_fen = "rnbqkbnr/ppp3pp/3p1p2/4p3/4P3/3P1P2/PPP3PP/RNBQKBNR w KQkq - 0 4";
        let actual_fen = board.as_fen();
        assert_eq!(expected_fen, actual_fen);
    }

    #[test]
    fn chessboard_regular_captures_reset_halfmove_counter() {
        let mut board = Chessboard::default();

        // make two knight moves to see if the halfmove counter is incremented
        let moves = ["e2e4", "e7e5", "g1f3", "b8c6"];
        for mv in moves {
            let _ = board.execute_move(&moves::UCIMove::try_from(mv).unwrap());
        }
        let expected_fen = "r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3";
        let actual_fen = board.as_fen();
        assert_eq!(expected_fen, actual_fen);

        // now make a capture with a knight and then check if the counter has been reset
        let _ = board.execute_move(&moves::UCIMove::try_from("f3e5").unwrap());
        let expected_fen = "r1bqkbnr/pppp1ppp/2n5/4N3/4P3/8/PPPP1PPP/RNBQKB1R b KQkq - 0 3";
        let actual_fen = board.as_fen();
        assert_eq!(expected_fen, actual_fen);
    }

    #[test]
    fn chessboard_enpassant_captures_reset_halfmove_counter() {
        let mut board = Chessboard::default();

        // setup enpassant for black
        let moves = ["e2e4", "e7e5", "f2f4", "e5f4", "g2g4"];
        for mv in moves {
            let _ = board.execute_move(&moves::UCIMove::try_from(mv).unwrap());
        }
        let expected_fen = "rnbqkbnr/pppp1ppp/8/8/4PpP1/8/PPPP3P/RNBQKBNR b KQkq g3 0 3";
        let actual_fen = board.as_fen();
        assert_eq!(expected_fen, actual_fen);

        // now take enpassant
        let _ = board.execute_move(&moves::UCIMove::try_from("f4g3").unwrap());
        let expected_fen = "rnbqkbnr/pppp1ppp/8/8/4P3/6p1/PPPP3P/RNBQKBNR w KQkq - 0 4";
        let actual_fen = board.as_fen();
        assert_eq!(expected_fen, actual_fen);
    }

    #[test]
    fn chessboard_promotion_noncapturing_resets_halfmove_counter() {
        let mut board = Chessboard::try_from("3n3k/2P5/8/8/8/8/8/K7 w - - 1 23").unwrap();

        // capture the knight and promote
        let _ = board.execute_move(&moves::UCIMove::try_from("c7d8n").unwrap());
        let expected_fen = "3N3k/8/8/8/8/8/8/K7 b - - 0 23";
        let actual_fen = board.as_fen();
        assert_eq!(expected_fen, actual_fen);
    }

    #[test]
    fn chessboard_promotion_capturing_resets_halfmove_counter() {
        let mut board = Chessboard::try_from("3n3k/2P5/8/8/8/8/8/K7 w - - 1 23").unwrap();

        // capture the knight and promote
        let _ = board.execute_move(&moves::UCIMove::try_from("c7c8n").unwrap());
        let expected_fen = "2Nn3k/8/8/8/8/8/8/K7 b - - 0 23";
        let actual_fen = board.as_fen();
        assert_eq!(expected_fen, actual_fen);
    }

    #[test]
    fn chessboard_castling_increments_halfmove_counter() {
        let mut board = Chessboard::default();

        // setup the board for a black kingside castle
        let moves = [
            "e2e4", "e7e5", "g1f3", "f7f6", "f1d3", "g8h6", "g2g3", "f8b4", "d1e2",
        ];
        for mv in moves {
            let _ = board.execute_move(&moves::UCIMove::try_from(mv).unwrap());
        }
        let expected_fen = "rnbqk2r/pppp2pp/5p1n/4p3/1b2P3/3B1NP1/PPPPQP1P/RNB1K2R b KQkq - 2 5";
        let actual_fen = board.as_fen();
        assert_eq!(expected_fen, actual_fen);

        // now castle and then check if the counter has been incremented
        let _ = board.execute_move(&moves::UCIMove::try_from("e8g8").unwrap());
        let expected_fen = "rnbq1rk1/pppp2pp/5p1n/4p3/1b2P3/3B1NP1/PPPPQP1P/RNB1K2R w KQ - 3 6";
        let actual_fen = board.as_fen();
        assert_eq!(expected_fen, actual_fen);
    }

    #[test]
    fn chessboard_black_move_increments_fullmove_counter() {
        let mut board = Chessboard::default();

        // make two white, two black moves, which should result in the fullmove counter being
        // incremented twice
        let moves = ["e2e4", "e7e5", "g1f3", "g8f6"];
        for mv in moves {
            let _ = board.execute_move(&moves::UCIMove::try_from(mv).unwrap());
        }

        let expected_fen = "rnbqkb1r/pppp1ppp/5n2/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3";
        let actual_fen = board.as_fen();
        assert_eq!(expected_fen, actual_fen);
    }

    #[test]
    fn chessboard_stalemating_sets_game_result() {
        // executing c8e6 in this position results in a stalemate
        let mut board =
            Chessboard::try_from("2Q2bnr/4p1pq/5pkr/7p/7P/4P3/PPPP1PP1/RNB1KBNR w KQ - 1 10")
                .unwrap();

        assert_eq!(board.get_game_result(), None);
        // execute stalemating move
        let _ = board.execute_move(&moves::UCIMove::try_from("c8e6").unwrap());
        assert_eq!(board.get_game_result(), Some(GameResult::Draw));
    }

    #[test]
    fn chessboard_checkmating_sets_game_result() {
        // executing d8h4 in this position results in a checkmate
        let mut board =
            Chessboard::try_from("rnbqkbnr/pppp1ppp/8/4p3/6P1/5P2/PPPPP2P/RNBQKBNR b KQkq g3 0 2")
                .unwrap();

        assert_eq!(board.get_game_result(), None);
        // execute checkmating move
        let _ = board.execute_move(&moves::UCIMove::try_from("d8h4").unwrap());
        assert_eq!(
            board.get_game_result(),
            Some(GameResult::Checkmate(piece::Color::Black))
        );
    }

    #[test]
    fn chessboard_surrendering_sets_game_result() {
        let mut board = Chessboard::default();
        board.set_win(piece::Color::White);
        assert_eq!(
            board.get_game_result(),
            Some(GameResult::SurrenderedWin(piece::Color::White))
        );
    }

    #[test]
    fn chessboard_draw_agreement_sets_game_result() {
        let mut board = Chessboard::default();
        board.set_draw();
        assert_eq!(board.get_game_result(), Some(GameResult::Draw));
    }

    #[test]
    fn chessboard_execute_move_err_when_move_illegal() {
        let mut board = Chessboard::default();

        let result = board.execute_move(&moves::UCIMove::try_from("e5e6").unwrap());
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), ChessboardError::IllegalMove);
    }

    #[test]
    fn chessboard_execute_move_err_when_game_already_finished() {
        let mut board = Chessboard::default();
        board.set_win(piece::Color::White);

        let result = board.execute_move(&moves::UCIMove::try_from("e2e4").unwrap());
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), ChessboardError::GameAlreadyFinished);
    }
}

#[cfg(test)]
mod perft {
    use crate::chessboard::*;
    use std::{collections::HashMap, ops::AddAssign};

    /// Used for storing results of running perft on a certain depth.
    #[derive(Debug, PartialEq, Copy, Clone)]
    struct PerftResult {
        pub nodes: u64,
        pub checkmates: u64,
        pub captures: u64,
        pub enpassants: u64,
        pub castles: u64,
        pub promotions: u64,
        pub checks: u64,
    }

    impl PerftResult {
        fn new(
            nodes: u64,
            checkmates: u64,
            captures: u64,
            enpassants: u64,
            castles: u64,
            promotions: u64,
            checks: u64,
        ) -> Self {
            Self {
                nodes,
                checkmates,
                captures,
                enpassants,
                castles,
                promotions,
                checks,
            }
        }
    }

    impl Default for PerftResult {
        fn default() -> Self {
            Self {
                nodes: 0,
                checkmates: 0,
                captures: 0,
                enpassants: 0,
                castles: 0,
                promotions: 0,
                checks: 0,
            }
        }
    }

    /// Updates `PerftResult` counters based on information received from the `MoveInfo`.
    #[inline(always)]
    fn perft_update_results(result: &mut PerftResult, info: MoveInfo) {
        if info.captured_piece {
            result.captures += 1;
        }

        if info.took_enpassant {
            result.enpassants += 1;
        }

        if info.castled {
            result.castles += 1;
        }

        if info.promoted {
            result.promotions += 1;
        }

        if info.checked_opponent {
            result.checks += 1;
        }
    }

    /// Enumerates all possible legal paths from a certain position up to `depth_max`. Apart
    /// from enumeration, it collects info about things like captures, en passants, promotions,
    /// checkmates, etc.
    ///
    /// This perft counts leaf nodes of move subtrees that have started from a certain
    /// initial move. This means that, e.g. initial position has 20 possible legal moves
    /// that can begin the game. On depth 2, after each one of these start moves, black
    /// also has 20 legal moves. This means, that all position subtrees have 20 leaf nodes
    /// for each initial move that could be played on the board.
    ///
    /// See: https://www.chessprogramming.org/Perft
    fn perft(board: &mut Chessboard, depth_max: u8, results: &mut HashMap<u8, PerftResult>) {
        let all_initial_moves = board.iter_legal_moves();
        let mut leaf_counters: HashMap<moves::UCIMove, std::cell::RefCell<usize>> = HashMap::new();

        let board_copy = board.clone_board();

        for initial_move in all_initial_moves {
            // when counting nodes, the first move is the key of the node which is then propagated
            // further, until a leaf node is reached, when the counter is incremented by 1
            leaf_counters.insert(initial_move.clone(), std::cell::RefCell::new(0));

            // propagate this counter to all perft calls that start from this initial_move
            //
            // e.g. if the first move was e2e4, propagate the "e2e4" counter to all perft calls
            // that continue the sequence started by that move
            let mut leaf_counter = leaf_counters.get(&initial_move).unwrap().borrow_mut();

            // if depth was 1
            if depth_max == 1 {
                leaf_counter.add_assign(1);
            }

            let result = results.entry(1).or_insert(PerftResult::default());
            result.nodes += 1;

            let game_result = board.execute_move(&initial_move).unwrap();
            match game_result {
                MoveResult::Continues { info } => {
                    perft_update_results(result, info);
                    // game is not over yet, keep playing,
                    // start from depth 2, because depth 1 has been explored by the execute_move
                    // above
                    perft_count_nodes(board, 2, depth_max, results, &mut leaf_counter);
                    board.undo_last_move(&board_copy);
                }
                MoveResult::Checkmate { winner: _, info } => {
                    perft_update_results(result, info);
                    result.checkmates += 1;
                    // checkmate on the board, undo the move and keep searching on the
                    // same depth
                    board.undo_last_move(&board_copy);
                    continue;
                }
                MoveResult::Draw { stalemate: _, info } => {
                    perft_update_results(result, info);
                    // draw on the board, undo the move and keep searching on the
                    // same depth
                    board.undo_last_move(&board_copy);
                    continue;
                }
            }
        }

        // there is as many counters as there were legal moves at the depth 1 of the explored position
        println!("================NODES===============");
        for (k, v) in leaf_counters.iter() {
            println!("{:?}: {}", k, v.borrow());
        }
        println!("====================================");
    }

    /// Perft that explores all possible positions that could occur after some initial move
    /// has been played on the board.
    fn perft_count_nodes(
        board: &mut Chessboard,
        depth: u8,
        depth_max: u8,
        results: &mut HashMap<u8, PerftResult>,
        leaf_counter: &mut std::cell::RefMut<usize>,
    ) {
        // maximum depth reached
        if depth > depth_max {
            return;
        }

        let board_copy = board.clone_board();

        let all_legal_moves = board.iter_legal_moves();
        for uci_move in all_legal_moves {
            let game_result = board.execute_move(&uci_move).unwrap();

            let result = results.entry(depth).or_insert(PerftResult::default());
            result.nodes += 1;

            // only count leaf nodes
            if depth == depth_max {
                leaf_counter.add_assign(1);
            }

            match game_result {
                MoveResult::Continues { info } => {
                    perft_update_results(result, info);
                    // game is not over yet, keep playing
                    perft_count_nodes(board, depth + 1, depth_max, results, leaf_counter);
                    board.undo_last_move(&board_copy);
                }
                MoveResult::Checkmate { winner: _, info } => {
                    perft_update_results(result, info);
                    result.checkmates += 1;
                    // checkmate on the board, undo the move and keep searching on the
                    // same depth
                    board.undo_last_move(&board_copy);
                    continue;
                }
                MoveResult::Draw { stalemate: _, info } => {
                    perft_update_results(result, info);
                    // draw on the board, undo the move and keep searching on the
                    // same depth
                    board.undo_last_move(&board_copy);
                    continue;
                }
            }
        }
    }

    /// Helper function for printing results of perft.
    fn perft_print_results(results: &HashMap<u8, PerftResult>, depth_max: u8) {
        println!("============PERFT RESULTS============");
        println!(
            "{:>2}|{:>12}|{:>12}|{:>10}|{:>8}|{:>8}|{:>11}|{:>8}",
            "D", "Nodes", "Checkmates", "Captures", "E.p", "Castles", "Promotions", "Checks",
        );
        for dpth in 1u8..=depth_max {
            let result = &results[&dpth];

            println!(
                "{:>2}|{:>12}|{:>12}|{:>10}|{:>8}|{:>8}|{:>11}|{:>8}",
                dpth,
                result.nodes,
                result.checkmates,
                result.captures,
                result.enpassants,
                result.castles,
                result.promotions,
                result.checks,
            );
        }
    }

    // See: https://www.chessprogramming.org/Perft_Results#Initial_Position
    #[test]
    fn perft_initial_position() {
        let mut board = Chessboard::default();
        let depth_max = 6;
        let mut results = HashMap::<u8, PerftResult>::new();

        // perft(&mut board, 1, depth_max, &mut results);
        perft(&mut board, depth_max, &mut results);
        perft_print_results(&results, depth_max);

        // order (starting from depth 1)
        let expected_results = [
            PerftResult::new(20, 0, 0, 0, 0, 0, 0),
            PerftResult::new(400, 0, 0, 0, 0, 0, 0),
            PerftResult::new(8902, 0, 34, 0, 0, 0, 12),
            PerftResult::new(197_281, 8, 1576, 0, 0, 0, 469),
            PerftResult::new(4_865_609, 347, 82_719, 258, 0, 0, 27_351),
            PerftResult::new(119_060_324, 10_828, 2_812_008, 5248, 0, 0, 809_099),
        ];

        for i in 1u8..=depth_max {
            let result = &results[&i];
            let i = (i - 1) as usize;
            assert_eq!(expected_results[i], *result)
        }
    }

    // See: https://www.chessprogramming.org/Perft_Results#Position_2
    #[test]
    fn perft_second_position() {
        let mut board = Chessboard::try_from(
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",
        )
        .unwrap();

        let depth_max = 5;
        let mut results = HashMap::<u8, PerftResult>::new();

        perft(&mut board, depth_max, &mut results);
        perft_print_results(&results, depth_max);

        // order (starting from depth 1)
        let expected_results = [
            PerftResult::new(48, 0, 8, 0, 2, 0, 0),
            PerftResult::new(2039, 0, 351, 1, 91, 0, 3),
            PerftResult::new(97_862, 1, 17_102, 45, 3162, 0, 993),
            PerftResult::new(4_085_603, 43, 757_163, 1929, 128_013, 15_172, 25_523),
            PerftResult::new(
                193_690_690,
                30_171,
                35_043_416,
                73_365,
                4_993_637,
                8392,
                3_309_887,
            ),
        ];

        for i in 1u8..=depth_max {
            let result = &results[&i];
            let i = (i - 1) as usize;
            assert_eq!(expected_results[i], *result)
        }
    }

    // See: https://www.chessprogramming.org/Perft_Results#Position_3
    #[test]
    fn perft_third_position() {
        let mut board = Chessboard::try_from("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1").unwrap();

        let depth_max = 7;
        let mut results = HashMap::<u8, PerftResult>::new();

        perft(&mut board, depth_max, &mut results);
        perft_print_results(&results, depth_max);

        // order (starting from depth 1)
        let expected_results = [
            PerftResult::new(14, 0, 1, 0, 0, 0, 2),
            PerftResult::new(191, 0, 14, 0, 0, 0, 10),
            PerftResult::new(2812, 0, 209, 2, 0, 0, 267),
            PerftResult::new(43_238, 17, 3348, 123, 0, 0, 1680),
            PerftResult::new(674_624, 0, 52_051, 1165, 0, 0, 52_950),
            PerftResult::new(11_030_083, 2733, 940_350, 33_325, 0, 7552, 452_473),
            PerftResult::new(178_633_661, 87, 14_519_036, 294_874, 0, 140_024, 12_797_406),
        ];

        for i in 1u8..=depth_max {
            let result = &results[&i];
            let i = (i - 1) as usize;
            assert_eq!(expected_results[i], *result)
        }
    }

    // See: https://www.chessprogramming.org/Perft_Results#Position_4
    #[test]
    fn perft_fourth_position() {
        let mut board = Chessboard::try_from(
            "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1",
        )
        .unwrap();

        let depth_max = 5;
        let mut results = HashMap::<u8, PerftResult>::new();

        perft(&mut board, depth_max, &mut results);
        perft_print_results(&results, depth_max);

        // order (starting from depth 1)
        let expected_results = [
            PerftResult::new(6, 0, 0, 0, 0, 0, 0),
            PerftResult::new(264, 0, 87, 0, 6, 48, 10),
            PerftResult::new(9467, 22, 1021, 4, 0, 120, 38),
            PerftResult::new(422_333, 5, 131_393, 0, 7795, 60_032, 15_492),
            PerftResult::new(15_833_292, 50_562, 2_046_173, 6512, 0, 329_464, 200_568),
        ];

        for i in 1u8..=depth_max {
            let result = &results[&i];
            let i = (i - 1) as usize;
            assert_eq!(expected_results[i], *result)
        }
    }

    // See: https://www.chessprogramming.org/Perft_Results#Position_5
    #[test]
    fn perft_fifth_position() {
        let mut board =
            Chessboard::try_from("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8")
                .unwrap();

        let depth_max = 5;
        let mut results = HashMap::<u8, PerftResult>::new();

        perft(&mut board, depth_max, &mut results);
        perft_print_results(&results, depth_max);

        // order (starting from depth 1)
        let expected_results = [44, 1486, 62_379, 2_103_487, 89_941_194];

        for i in 1u8..=depth_max {
            let result = &results[&i];
            let i = (i - 1) as usize;
            assert_eq!(expected_results[i], result.nodes);
        }
    }

    // See: https://www.chessprogramming.org/Perft_Results#Position_6
    #[test]
    fn perft_sixth_position() {
        let mut board = Chessboard::try_from(
            "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10",
        )
        .unwrap();

        let depth_max = 5;
        let mut results = HashMap::<u8, PerftResult>::new();

        perft(&mut board, depth_max, &mut results);
        perft_print_results(&results, depth_max);

        // order (starting from depth 1)
        let expected_results = [46, 2079, 89_890, 3_894_594, 164_075_551];

        for i in 1u8..=depth_max {
            let result = &results[&i];
            let i = (i - 1) as usize;
            assert_eq!(expected_results[i], result.nodes);
        }
    }
}
