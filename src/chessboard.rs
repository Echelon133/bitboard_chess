use std::fmt::Debug;

use crate::board;
use crate::context;
use crate::movegen;
use crate::moves;
use crate::piece;
use crate::square;

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
    /// NOTE: this method checks moves for their legality only in the release mode.
    /// While in test mode, these checks are disabled, because all moves that are
    /// given to this method to execute are taken from legal move generating functions,
    /// so move legality checks are omitted for performance reasons.
    ///
    pub fn execute_move(&mut self, m: &moves::UCIMove) -> Result<bool, &'static str> {
        // don't compile this in test mode, because during tests legality of moves
        // is enforced by only executing this method with moves that have been generated
        // by movegen functions for the current state of the board
        #[cfg(not(test))]
        {
            let legal_moves = self.find_all_legal_moves();
            if !legal_moves.contains(&m) {
                return Err("illegal move");
            }
        }

        match m {
            moves::UCIMove::Regular { m: mv } => {
                self.handle_regular_move(mv);
            }
            moves::UCIMove::Promotion { m: mv, k: kind } => {
                self.handle_promotion_move(mv, *kind);
            }
        }

        self.context.flip_color_to_play();

        // check if the executed move has resulted in a checkmate
        // NOTE: current implementation is very naive, because the only two states
        // it recognizes are either checkmate or no checkmate
        // TODO: implement things like stalemate or insufficient material detection
        let next_player_moves = self.find_all_legal_moves().iter().count();
        let is_king_in_check = self.is_king_in_check(self.context.get_color_to_play());
        if next_player_moves == 0 && is_king_in_check {
            Ok(true)
        } else {
            Ok(false)
        }
    }

    /// Checks whether the given move is legal, i.e. does not put the king in check.
    ///
    /// This method executes the given move, checks whether the king is in check, and if it is,
    /// it deems that move illegal and returns `false`.
    ///
    /// # Safety:
    /// This method might break some invariants of the chessboard (e.g. might put the chessboard
    /// in a state in which an illegal move is temporarily placed on the board), but
    /// since it restores the chessboard to the previous valid state right before it returns,
    /// and it holds the mutable reference to the entire chessboard, it's safe to break
    /// these invariants because no other part of the code can access the board's state while
    /// these invariants are broken.
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
                square::Square::try_from("e1").unwrap(),
                square::Square::try_from("g1").unwrap(),
                square::Square::try_from("c1").unwrap(),
            ),
            piece::Color::Black => (
                square::Square::try_from("e8").unwrap(),
                square::Square::try_from("g8").unwrap(),
                square::Square::try_from("c8").unwrap(),
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

    /// Handles piece moves (without pawn promotions), castling, en-passant captures.
    ///
    /// # Panics
    /// This method will panic if the given move is incorrect for the given board state,
    /// and e.g. wants to move a piece from a square that's not occupied.
    fn handle_regular_move(&mut self, m: &moves::Move) {
        let saved_context = self.context.clone();
        let (start, target) = (m.get_start(), m.get_target());
        let piece = self.inner_board.remove_piece(start).unwrap();

        // move is enpassant
        if Chessboard::is_enpassant(&piece, m, self.context.get_enpassant()) {
            self.inner_board.place_piece(target, &piece);
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
        } else if let Some(_side) = Chessboard::is_castling(&piece, m) {
            self.context.set_enpassant(None);
            unimplemented!("castling unimplemented");
        } else {
            let enpassant_target = Chessboard::should_set_enpassant(&piece, m);
            self.context.set_enpassant(enpassant_target);

            let captured_piece = self.inner_board.place_piece(target, &piece);
            self.history.push(moves::TakenMove::PieceMove {
                m: *m,
                captured_piece,
                ctx: saved_context,
            });
        }
    }

    /// Handles pawn promotions.
    fn handle_promotion_move(&mut self, m: &moves::Move, k: piece::Kind) {
        let saved_context = self.context.clone();

        let (start, target) = (m.get_start(), m.get_target());
        let promoted_pawn = self.inner_board.remove_piece(start).unwrap();
        let promotion_goal = piece::Piece::new(k, promoted_pawn.get_color());
        let captured_piece = self.inner_board.place_piece(target, &promotion_goal);

        self.history.push(moves::TakenMove::Promotion {
            m: *m,
            captured_piece,
            ctx: saved_context,
        });
    }

    /// Undoes the last test move and restores the context of the board so that castling
    /// rights or move counters are not broken by undoing the move.
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
                let removed_piece = self.inner_board.remove_piece(m.get_target()).unwrap();
                self.inner_board.place_piece(m.get_start(), &removed_piece);
                // if the move was capturing, put the captured piece back on the board
                if let Some(captured_piece) = captured_piece {
                    self.inner_board
                        .place_piece(m.get_target(), &captured_piece);
                }
                self.context = ctx;
            }
            moves::TakenMove::Promotion {
                m,
                captured_piece,
                ctx,
            } => {
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
                self.context = ctx;
            }
            moves::TakenMove::EnPassant { m, ctx } => {
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
                self.context = ctx;
            }
            _ => unimplemented!("other moves not implemented"),
        }
    }

    /// Returns [`true`] if the king is in check. Currently not the fastest implementation
    /// there could be.
    ///
    /// # Panics
    /// Panics if the board does not contain a king of the given color.
    fn is_king_in_check(&self, king_color: piece::Color) -> bool {
        movegen::is_king_in_check(king_color, &self.inner_board)
    }

    /// Finds all pseudo-legal moves for all pieces of the given color.
    fn find_all_pseudolegal_moves(&self, color: piece::Color) -> Vec<moves::UCIMove> {
        let mut all_moves = Vec::new();

        let own_pieces = self.inner_board.get_squares_taken(color);
        let (white, black) = self.inner_board.get_squares_taken_pair();

        for occupied_square in own_pieces.iter() {
            let piece_kind = self
                .inner_board
                .get_piece(occupied_square)
                .unwrap()
                .get_kind();
            let mut moves = match piece_kind {
                piece::Kind::Pawn => {
                    movegen::find_pawn_moves(occupied_square, color, white, black, &self.context)
                }
                piece::Kind::Bishop => {
                    movegen::find_bishop_moves(occupied_square, color, white, black)
                }
                piece::Kind::Rook => movegen::find_rook_moves(occupied_square, color, white, black),
                piece::Kind::Knight => {
                    movegen::find_knight_moves(occupied_square, color, white, black)
                }
                piece::Kind::Queen => {
                    movegen::find_queen_moves(occupied_square, color, white, black)
                }
                piece::Kind::King => {
                    movegen::find_king_moves(occupied_square, color, white, black, &self.context)
                }
            };

            all_moves.append(&mut moves);
        }
        all_moves
    }

    /// Finds all legal moves which can be executed by the current player.
    pub fn find_all_legal_moves(&mut self) -> Vec<moves::UCIMove> {
        let color_to_play = self.context.get_color_to_play();
        let mut moves = self.find_all_pseudolegal_moves(color_to_play);
        moves.retain(|mv| self.is_move_legal(&mv));
        moves
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
}
