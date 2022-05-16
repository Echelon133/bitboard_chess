use std::fmt::Debug;

use crate::bitboard;
use crate::board;
use crate::context;
use crate::movegen;
use crate::moves;
use crate::piece;
use crate::square;

/// Squares that create a path between the white king and H1 rook,
/// which cannot be attacked by a black piece if the player wants to
/// legally castle.
const WHITE_KINGSIDE_CASTLING_PATH: [square::Square; 3] = [
    square::Square::new(square::Rank::R1, square::File::E),
    square::Square::new(square::Rank::R1, square::File::F),
    square::Square::new(square::Rank::R1, square::File::G),
];

/// Squares that create a path between the black king and H8 rook,
/// which cannot be attacked by a white piece if the player wants to
/// legally castle.
const BLACK_KINGSIDE_CASTLING_PATH: [square::Square; 3] = [
    square::Square::new(square::Rank::R8, square::File::E),
    square::Square::new(square::Rank::R8, square::File::F),
    square::Square::new(square::Rank::R8, square::File::G),
];

/// Squares that create a path between the white king and A1 rook,
/// which cannot be attacked by a black piece if the player wants to
/// legally castle.
const WHITE_QUEENSIDE_CASTLING_PATH: [square::Square; 3] = [
    square::Square::new(square::Rank::R1, square::File::C),
    square::Square::new(square::Rank::R1, square::File::D),
    square::Square::new(square::Rank::R1, square::File::E),
];

/// Squares that create a path between the black king and A8 rook,
/// which cannot be attacked by a white piece if the player wants to
/// legally castle.
const BLACK_QUEENSIDE_CASTLING_PATH: [square::Square; 3] = [
    square::Square::new(square::Rank::R8, square::File::C),
    square::Square::new(square::Rank::R8, square::File::D),
    square::Square::new(square::Rank::R8, square::File::E),
];

/// Initial square of the white king.
const WHITE_KING_START: square::Square = square::Square::new(square::Rank::R1, square::File::E);

/// Initial square of the black king.
const BLACK_KING_START: square::Square = square::Square::new(square::Rank::R8, square::File::E);

/// Initial square of the white rook (queenside).
const WHITE_QSIDE_ROOK_START: square::Square =
    square::Square::new(square::Rank::R1, square::File::A);

/// Initial square of the black rook (queenside).
const BLACK_QSIDE_ROOK_START: square::Square =
    square::Square::new(square::Rank::R8, square::File::A);

/// Initial square of the white rook (kingside).
const WHITE_KSIDE_ROOK_START: square::Square =
    square::Square::new(square::Rank::R1, square::File::H);

/// Initial square of the black rook (kingside).
const BLACK_KSIDE_ROOK_START: square::Square =
    square::Square::new(square::Rank::R8, square::File::H);

/// Contains info about squares which are used during undoing of white kingside castling.
///
/// Order of elements: (
/// rook_start_square,
/// rook_target_square,
/// king_start_square,
/// king_target_square
/// )
const WHITE_KSIDE_CASTLING_INFO: (
    square::Square,
    square::Square,
    square::Square,
    square::Square,
) = (
    square::Square::new(square::Rank::R1, square::File::F),
    square::Square::new(square::Rank::R1, square::File::H),
    square::Square::new(square::Rank::R1, square::File::G),
    square::Square::new(square::Rank::R1, square::File::E),
);

/// Contains info about squares which are used during undoing of black kingside castling.
///
/// Order of elements: (
/// rook_start_square,
/// rook_target_square,
/// king_start_square,
/// king_target_square
/// )
const BLACK_KSIDE_CASTLING_INFO: (
    square::Square,
    square::Square,
    square::Square,
    square::Square,
) = (
    square::Square::new(square::Rank::R8, square::File::F),
    square::Square::new(square::Rank::R8, square::File::H),
    square::Square::new(square::Rank::R8, square::File::G),
    square::Square::new(square::Rank::R8, square::File::E),
);

/// Contains info about squares which are used during undoing of white queenside castling.
///
/// Order of elements: (
/// rook_start_square,
/// rook_target_square,
/// king_start_square,
/// king_target_square
/// )
const WHITE_QSIDE_CASTLING_INFO: (
    square::Square,
    square::Square,
    square::Square,
    square::Square,
) = (
    square::Square::new(square::Rank::R1, square::File::D),
    square::Square::new(square::Rank::R1, square::File::A),
    square::Square::new(square::Rank::R1, square::File::C),
    square::Square::new(square::Rank::R1, square::File::E),
);

/// Contains info about squares which are used during undoing of black queenside castling.
///
/// Order of elements: (
/// rook_start_square,
/// rook_target_square,
/// king_start_square,
/// king_target_square
/// )
const BLACK_QSIDE_CASTLING_INFO: (
    square::Square,
    square::Square,
    square::Square,
    square::Square,
) = (
    square::Square::new(square::Rank::R8, square::File::D),
    square::Square::new(square::Rank::R8, square::File::A),
    square::Square::new(square::Rank::R8, square::File::C),
    square::Square::new(square::Rank::R8, square::File::E),
);

/// Iterator over [`movegen::MoveIter`] iterators. There is a single [`movegen::MoveIter`] for
/// every single square that's occupied by the player who is about to make a move.
///
/// # Example
/// If the board has a default setup, and white is about to play, `MoveIterIter` will contain
/// 20 iterators (one for every white piece on the board), and each [`movegen::MoveIter`]
/// will give out moves of a single piece.
///
/// This iterator only calls movegen functions when `next()` is called, which means that
/// the search for pseudo-legal moves of a particular piece only happens when it's necessary.
#[derive(Clone, Copy)]
pub struct MoveIterIter {
    color_to_play: piece::Color,
    own_pieces: bitboard::SquareIter,
    white_taken: bitboard::Bitboard,
    black_taken: bitboard::Bitboard,
    inner_board: board::Board,
    context: context::Context,
}

impl MoveIterIter {
    pub fn new(inner_board: board::Board, context: context::Context) -> Self {
        let color_to_play = context.get_color_to_play();
        Self {
            color_to_play,
            own_pieces: inner_board.get_squares_taken(color_to_play).iter(),
            white_taken: *inner_board.get_squares_taken(piece::Color::White),
            black_taken: *inner_board.get_squares_taken(piece::Color::Black),
            inner_board,
            context,
        }
    }
}

impl Iterator for MoveIterIter {
    type Item = movegen::MoveIter;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(occupied_square) = self.own_pieces.next() {
            let piece_kind = self
                .inner_board
                .get_piece(occupied_square)
                .unwrap()
                .get_kind();

            let color_to_play = self.color_to_play;
            let white = self.white_taken;
            let black = self.black_taken;
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

/// Iterator over legal moves of the player that is currently making a move.
///
/// It iterates over [`MoveIterIter`], which gives out iterators of pseudo-legal moves.
/// It returns items as long as there are pseudo-legal moves that are verified as legal
/// by the [`Self::is_move_legal`] method.
pub struct LegalMovesIter {
    iterators: MoveIterIter,
    current_iter: Option<movegen::MoveIter>,
    board: Chessboard,
}

impl LegalMovesIter {
    pub fn new(mut iterators: MoveIterIter, board: Chessboard) -> Self {
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

/// Holds information about a move that's been executed on the board.
///
/// This struct is used for storing information that's used for verifying correctness
/// of the implementation. All captures, en passants, castles, promotions and checks
/// are being counted, so that they can be later compared to results of other chess engines.
#[derive(Copy, Clone, Debug)]
pub struct MoveInfo {
    pub captured_piece: bool,
    pub took_enpassant: bool,
    pub castled: bool,
    pub promoted: bool,
    pub checked_opponent: bool,
}

/// Describes the result of a move. Apart from that, it carries debug information about captures,
/// en passants, castles, promotions and checks that happened in that move.
#[derive(Debug, Copy, Clone)]
pub enum MoveResult {
    Checkmate {
        winner: piece::Color,
        info: MoveInfo,
    },
    Draw {
        stalemate: bool,
        info: MoveInfo,
    },
    Continues {
        info: MoveInfo,
    },
}

/// Represents a playable chessboard.
///
/// This struct is responsible for:
/// - holding information about pieces
/// - holding information about castling rights, which color is to play, how many moves have
///     been played on the board
/// - providing methods for high-level manipulation of the board state
/// - making sure the move that's being executed is legal
///
pub struct Chessboard {
    inner_board: board::Board,
    context: context::Context,
    history: Vec<moves::TakenMove>,
    end_result: Option<MoveResult>,
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
    /// Executes a move on the board. If the move is not legal, meaning:
    /// - it's incorrect for the type of piece which is being moved
    /// - the color of the piece is not correct for that turn
    /// - it puts its own king in check
    ///
    /// then such a move is rejected and does not appear on the board.
    /// Apart from that, this method updates the context of the chessboard, so that
    /// the color of the next player is set (which might also update the fullmove counter).
    ///
    /// Every move (except illegal ones) returns an `Ok` that contains a `MoveResult` with
    /// information about what happend in that move (if there was a capture, en passant,
    /// castle, promotion, check). This information greatly improves the debugging experience.
    ///
    /// NOTE: this method checks moves for their legality only in the release mode.
    /// While in test mode, these checks are disabled, because all moves that are
    /// given to this method to execute are taken from legal move generating functions,
    /// so move legality checks are omitted for performance reasons.
    pub fn execute_move(&mut self, m: &moves::UCIMove) -> Result<MoveResult, &'static str> {
        let captured_piece;
        let mut took_enpassant = false;
        let mut castled = false;
        let mut promoted = false;

        // don't compile this check in test mode, because all tests immediately stop
        // playing the game when they receive `GameResult` and do not attempt to further
        // play a game that's already finished
        #[cfg(not(test))]
        {
            if self.end_result.is_some() {
                return Ok(self.end_result.unwrap());
            }
        }

        // don't compile this check in test mode, because tests only execute moves that
        // had previously been checked for their legality, therefore this check is
        // unnecessary, as it does never return an error
        #[cfg(not(test))]
        {
            // return error if the given move is a move that cannot even
            // appear on the board (for any disqualifying reason described in can_be_played
            // docs)
            if !self.can_be_played(m) {
                return Err("illegal move");
            }
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

        self.context.flip_color_to_play();

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
            if is_king_in_check {
                // the winner is the previous color
                let winner = match color_to_play {
                    piece::Color::White => piece::Color::Black,
                    piece::Color::Black => piece::Color::White,
                };
                self.end_result = Some(MoveResult::Checkmate { winner, info });
            } else {
                self.end_result = Some(MoveResult::Draw {
                    stalemate: true,
                    info,
                });
            }
            Ok(self.end_result.unwrap())
        } else {
            let (white_taken, black_taken) = self.inner_board.get_squares_taken_pair();
            // both players only have their kings, therefore it's a draw
            if white_taken.count_set() == 1 && black_taken.count_set() == 1 {
                self.end_result = Some(MoveResult::Draw {
                    stalemate: false,
                    info,
                });
                Ok(self.end_result.unwrap())
            } else {
                // TODO: implement detection of draws which happen because:
                // - the halfmoves counter reached 50
                // - there is not enough material to checkmate
                Ok(MoveResult::Continues { info })
            }
        }
    }

    /// Returns true if the given move is a move that can appear on the board with its current
    /// state.
    ///
    /// This means, that the move:
    /// - has be start on a square that contains a piece
    /// - has to be an actual pseudo-legal move that's currently possible on the board for
    ///     the kind of piece that occupies that start square
    /// - has to be a pseudo-legal move that can be verified as legal by `is_move_legal`
    pub fn can_be_played(&mut self, m: &moves::UCIMove) -> bool {
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
    ///
    fn is_move_legal(&mut self, m: &moves::UCIMove) -> bool {
        let own_color = self.context.get_color_to_play();
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
                        self.undo_last_move();
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
        self.undo_last_move();
        legal
    }

    /// Checks if the given piece and move of that piece describe castling. If they do, it returns
    /// `Some` with the side of castling which is described. Otherwise it returns `None`.
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

    /// Disables one side of castling if player's rook had been captured on
    /// it's original square and is no longer there to allow castling.
    ///
    /// Should be called after every move that can capture a piece, so that there is no
    /// possibility of castling flags remaining set when player no longer has a certain rook.
    fn disable_castling_if_rook_captured(
        &mut self,
        captured_piece: &Option<piece::Piece>,
        captured_square: square::Square,
    ) {
        if let Some(captured_piece) = captured_piece {
            if captured_piece.get_kind() == piece::Kind::Rook {
                let side = match (captured_piece.get_color(), captured_square) {
                    // check if the captured square is a square where rooks of that color start
                    (piece::Color::White, WHITE_KSIDE_ROOK_START) => context::Side::Kingside,
                    (piece::Color::White, WHITE_QSIDE_ROOK_START) => context::Side::Queenside,
                    (piece::Color::Black, BLACK_KSIDE_ROOK_START) => context::Side::Kingside,
                    (piece::Color::Black, BLACK_QSIDE_ROOK_START) => context::Side::Queenside,
                    _ => return,
                };
                self.context
                    .disable_castling(captured_piece.get_color(), side);
            }
        }
    }

    /// Returns [`square::Square`] with en passant target square if, and only if:
    /// - the `piece` is a pawn
    /// - the [`moves::Move`] describes a move forward by two squares from the initial rank
    ///     of the pawn
    ///
    /// If both conditions are satisfied, then the target square (the square behind the moved
    /// pawn) is wrapped in [`Some`] and returned.
    /// If any of these two conditions is not satisied, `None` is returned.
    #[inline(always)]
    fn should_set_enpassant(piece: &piece::Piece, m: &moves::Move) -> Option<square::Square> {
        if piece.get_kind() != piece::Kind::Pawn {
            None
        } else {
            let (start_rank, target_rank, color) = (
                m.get_start().get_rank(),
                m.get_target().get_rank(),
                piece.get_color(),
            );

            match (start_rank, target_rank, color) {
                (square::Rank::R2, square::Rank::R4, piece::Color::White) => Some(
                    square::Square::new(square::Rank::R3, m.get_target().get_file()),
                ),
                (square::Rank::R7, square::Rank::R5, piece::Color::Black) => Some(
                    square::Square::new(square::Rank::R6, m.get_target().get_file()),
                ),
                _ => None,
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
        self.context.set_enpassant(None);
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
        // if castling appears on the board, castling rights no longer apply
        self.context
            .disable_castling(castling_side_color, context::Side::Kingside);
        self.context
            .disable_castling(castling_side_color, context::Side::Queenside);

        self.history.push(moves::TakenMove::Castling {
            s: side,
            ctx: saved_context,
        });
        self.context.set_enpassant(None);
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

        // figure out whether the current move changes the state of castling flags
        match piece.get_kind() {
            piece::Kind::King => {
                if piece.get_color() == piece::Color::White {
                    if start == WHITE_KING_START {
                        // white king moving from e1 disables both sides of castling
                        self.context
                            .disable_castling(piece::Color::White, context::Side::Kingside);
                        self.context
                            .disable_castling(piece::Color::White, context::Side::Queenside);
                    }
                } else {
                    if start == BLACK_KING_START {
                        // black king moving from e8 disables both sides of castling
                        self.context
                            .disable_castling(piece::Color::Black, context::Side::Kingside);
                        self.context
                            .disable_castling(piece::Color::Black, context::Side::Queenside);
                    }
                }
            }
            piece::Kind::Rook => {
                if piece.get_color() == piece::Color::White {
                    if start == WHITE_QSIDE_ROOK_START {
                        self.context
                            .disable_castling(piece::Color::White, context::Side::Queenside);
                    } else if start == WHITE_KSIDE_ROOK_START {
                        self.context
                            .disable_castling(piece::Color::White, context::Side::Kingside);
                    }
                } else {
                    if start == BLACK_QSIDE_ROOK_START {
                        self.context
                            .disable_castling(piece::Color::Black, context::Side::Queenside);
                    } else if start == BLACK_KSIDE_ROOK_START {
                        self.context
                            .disable_castling(piece::Color::Black, context::Side::Kingside);
                    }
                }
            }
            _ => (),
        }

        let captured_piece = self.inner_board.place_piece(target, &piece);
        self.disable_castling_if_rook_captured(&captured_piece, target);
        let was_capturing = captured_piece.is_some();
        self.history.push(moves::TakenMove::PieceMove {
            m: *m,
            captured_piece,
            ctx: saved_context,
        });

        let enpassant_target = Chessboard::should_set_enpassant(&piece, m);
        self.context.set_enpassant(enpassant_target);
        was_capturing
    }

    /// Handles all legal or pseudo-legal pawn promotions.
    ///
    /// # Panics
    /// This method panics if there is no piece on the start square of the
    /// `moves::Move`.
    fn handle_promotion_move(&mut self, m: &moves::Move, k: piece::Kind) -> bool {
        let saved_context = self.context.clone();

        let (start, target) = (m.get_start(), m.get_target());
        let promoted_pawn = self.inner_board.remove_piece(start).unwrap();
        let promotion_goal = piece::Piece::new(k, promoted_pawn.get_color());
        let captured_piece = self.inner_board.place_piece(target, &promotion_goal);
        self.disable_castling_if_rook_captured(&captured_piece, target);
        let was_capturing = captured_piece.is_some();
        self.history.push(moves::TakenMove::Promotion {
            m: *m,
            captured_piece,
            ctx: saved_context,
        });
        was_capturing
    }

    /// Undoes the last legal or pseudo-legal move and restores the context of
    /// the board, so that castling rights or move counters don't end up broken.
    ///
    /// # Panics
    /// This method panics if there are no elements in the chessboard move history or
    /// the board state (like piece position) does not match the expected board state.
    ///
    fn undo_last_move(&mut self) {
        let last_move = self.history.pop().unwrap();

        match last_move {
            moves::TakenMove::PieceMove {
                m,
                captured_piece,
                ctx,
            } => {
                self.undo_piece_move(&m, captured_piece, &ctx);
            }
            moves::TakenMove::Promotion {
                m,
                captured_piece,
                ctx,
            } => {
                self.undo_promotion(&m, captured_piece, &ctx);
            }
            moves::TakenMove::EnPassant { m, ctx } => self.undo_enpassant(&m, &ctx),
            moves::TakenMove::Castling { s, ctx } => {
                self.undo_castling(s, &ctx);
            }
        }
        self.end_result = None
    }

    /// Undoes a move that's NOT castling, en passant or pawn promotion, then restores the
    /// context of the board, so that castling rights or move counters don't end up broken.
    ///
    /// # Panics
    /// This method panics if there is no piece on the target square of the `moves::Move`
    /// that's given as an argument.
    #[inline(always)]
    fn undo_piece_move(
        &mut self,
        m: &moves::Move,
        captured_piece: Option<piece::Piece>,
        ctx: &context::Context,
    ) {
        let removed_piece = self.inner_board.remove_piece(m.get_target()).unwrap();
        self.inner_board.place_piece(m.get_start(), &removed_piece);
        // if the move was capturing, put the captured piece back on the board
        if let Some(captured_piece) = captured_piece {
            self.inner_board
                .place_piece(m.get_target(), &captured_piece);
        }
        self.context = *ctx;
    }

    /// Undoes a pawn promotion move and restores the context of the board, so that
    /// castling rights or move counters don't end up broken.
    ///
    /// # Panics
    /// This method panics if there is no piece on the target square of the `moves::Move`
    /// that's given as an argument.
    #[inline(always)]
    fn undo_promotion(
        &mut self,
        m: &moves::Move,
        captured_piece: Option<piece::Piece>,
        ctx: &context::Context,
    ) {
        // removed piece was the promoted piece, so it dissappears from the board
        // completely
        let removed_piece = self.inner_board.remove_piece(m.get_target()).unwrap();
        let pawn = piece::Piece::new(piece::Kind::Pawn, removed_piece.get_color());
        self.inner_board.place_piece(m.get_start(), &pawn);
        // if the promotion move was capturing, put the captured piece back on the board
        if let Some(captured_piece) = captured_piece {
            self.inner_board
                .place_piece(m.get_target(), &captured_piece);
        }
        self.context = *ctx;
    }

    /// Undoes an en passant capture and restores the context of the board, so that castling
    /// rights or move counters don't end up broken.
    ///
    /// # Panics
    /// This method panics if there is no piece on the target square of the `moves::Move`
    /// that's given as an argument.
    #[inline(always)]
    fn undo_enpassant(&mut self, m: &moves::Move, ctx: &context::Context) {
        let removed_pawn = self.inner_board.remove_piece(m.get_target()).unwrap();
        self.inner_board.place_piece(m.get_start(), &removed_pawn);
        // restore the enemy pawn
        let captured_pawn_sq =
            square::Square::new(m.get_start().get_rank(), m.get_target().get_file());
        let captured_pawn_color = match removed_pawn.get_color() {
            piece::Color::White => piece::Color::Black,
            piece::Color::Black => piece::Color::White,
        };
        let pawn_to_restore = piece::Piece::new(piece::Kind::Pawn, captured_pawn_color);
        self.inner_board
            .place_piece(captured_pawn_sq, &pawn_to_restore);
        self.context = *ctx;
    }

    /// Undoes castling and restores the context of the board, so that castling rights or
    /// move counters don't end up broken.
    ///
    /// # Panics
    /// This method panics if any of the squares which it expects to be taken are empty.
    /// Kingside/queenside castling always ends with the king and rook on certain squares,
    /// so when these squares are unexpectedly empty, a panic occurs.
    #[inline(always)]
    fn undo_castling(&mut self, s: context::Side, ctx: &context::Context) {
        let color_castled = ctx.get_color_to_play();

        let (rook_start_square, rook_target_square, king_start_square, king_target_square) =
            match (color_castled, s) {
                (piece::Color::White, context::Side::Queenside) => WHITE_QSIDE_CASTLING_INFO,
                (piece::Color::White, context::Side::Kingside) => WHITE_KSIDE_CASTLING_INFO,
                (piece::Color::Black, context::Side::Queenside) => BLACK_QSIDE_CASTLING_INFO,
                (piece::Color::Black, context::Side::Kingside) => BLACK_KSIDE_CASTLING_INFO,
            };

        // swap the king and the rook
        let king_piece = self.inner_board.remove_piece(king_start_square).unwrap();
        let rook_piece = self.inner_board.remove_piece(rook_start_square).unwrap();
        self.inner_board
            .place_piece(king_target_square, &king_piece);
        self.inner_board
            .place_piece(rook_target_square, &rook_piece);

        self.context = *ctx;
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
}

impl Default for Chessboard {
    /// Creates a [`Chessboard`] with a default setup of pieces.
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
                    board.undo_last_move();
                }
                MoveResult::Checkmate { winner: _, info } => {
                    perft_update_results(result, info);
                    result.checkmates += 1;
                    // checkmate on the board, undo the move and keep searching on the
                    // same depth
                    board.undo_last_move();
                    continue;
                }
                MoveResult::Draw { stalemate: _, info } => {
                    perft_update_results(result, info);
                    // draw on the board, undo the move and keep searching on the
                    // same depth
                    board.undo_last_move();
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
                    board.undo_last_move();
                }
                MoveResult::Checkmate { winner: _, info } => {
                    perft_update_results(result, info);
                    result.checkmates += 1;
                    // checkmate on the board, undo the move and keep searching on the
                    // same depth
                    board.undo_last_move();
                    continue;
                }
                MoveResult::Draw { stalemate: _, info } => {
                    perft_update_results(result, info);
                    // draw on the board, undo the move and keep searching on the
                    // same depth
                    board.undo_last_move();
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
