//! This module contains structs and enums which represent moves between two squares
//! on the chessboard.

use std::fmt::Debug;

use crate::context;
use crate::piece;
use crate::square;

/// An immutable move which simply stores the information about the start and the target square.
#[derive(PartialEq, Copy, Clone, Eq, Hash)]
pub struct Move {
    start: square::Square,
    target: square::Square,
}

impl Move {
    /// Creates a [`Move`] between `start` and `target` squares.
    pub fn new(start: square::Square, target: square::Square) -> Self {
        Self { start, target }
    }

    /// Returns the start square of the move.
    pub fn get_start(&self) -> square::Square {
        self.start
    }

    /// Returns the target square of the move.
    pub fn get_target(&self) -> square::Square {
        self.target
    }
}

impl TryFrom<&str> for Move {
    type Error = &'static str;

    /// Creates a [`Move`] from 4 ascii characters that represent two squares between
    /// which a piece moves. First two characters represent the start square,
    /// and the other two characters represent the target square.
    ///
    /// Example valid values:
    /// - "a1b2"
    /// - "c5c6"
    /// - "h1h8"
    ///
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if !value.is_ascii() {
            Err("cannot create move from str containing non-ascii chars")
        } else if value.len() != 4 {
            Err("cannot create move from str that is not 4 chars long")
        } else {
            // value is guaranteed to have 4 ascii characters
            let start = &value[0..2];
            let target = &value[2..4];

            let start_square = match square::Square::try_from(start) {
                Ok(square) => square,
                Err(_) => {
                    return Err("invalid start square representation");
                }
            };

            let target_square = match square::Square::try_from(target) {
                Ok(square) => square,
                Err(_) => {
                    return Err("invalid target square representation");
                }
            };

            Ok(Move::new(start_square, target_square))
        }
    }
}

impl Debug for Move {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}{:?}", self.get_start(), self.get_target())
    }
}

/// A move between two squares represented in the UCI notation.
///
/// Example moves in this notation:
/// - "e2e4"
/// - "e1g1"
/// - "e1c1"
/// - "e7d8n"
/// - "e7e8q"
/// 
/// All moves which are represented by 5 characters are moves that promote pawns. 
/// The last character symbolizes the type of piece that is supposed to appear on the
/// board during the promotion.
///
/// All other moves are represented by 4 characters. These moves can be:
/// - capturing (regular captures, en passant captures)
/// - noncapturing (castling, moving piece from one square to another)
#[derive(PartialEq, Eq, Hash, Copy, Clone)]
pub enum UCIMove {
    /// Represents any move that is NOT a pawn promotion.
    Regular { m: Move },
    /// Represents a move that promotes a pawn.
    Promotion { m: Move, k: piece::Kind },
}

impl Debug for UCIMove {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UCIMove::Regular { m } => {
                write!(f, "{:?}", m)
            }
            UCIMove::Promotion { m, k } => {
                write!(f, "{:?}{}", m, k.as_char())
            }
        }
    }
}

impl TryFrom<&str> for UCIMove {
    type Error = &'static str;

    /// Creates a [`UCIMove`] from 4 or 5 ascii characters.
    ///
    /// First 4 characters always represent two squares between which a piece moves.
    /// First two characters represent the start square, and the other two characters
    /// represent the target square.
    ///
    /// If only 4 characters are provided, the returned enum's variant will be
    /// [`UCIMove::Regular`] and will only contain a [`Move`] object that holds
    /// start and target square information.
    ///
    /// If an additional, 5th character is provided, then the returned enum's variant
    /// will be [`UCIMove::Promotion`] which, apart from containing a [`Move`] object,
    /// also contains a [`piece::Kind`] that holds the information about the
    /// piece that is supposed to appear on the board during the pawn promotion.
    /// This 5th character must be a valid character that represents a piece that's not
    /// a pawn or a king.
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if !value.is_ascii() {
            Err("cannot create uci move from str containing non-ascii chars")
        } else if (value.len() != 4) && (value.len() != 5) {
            Err("cannot create uci move from str that is not 4 or 5 chars long")
        } else {
            // value guaranteed to be 4 or 5 ascii chars
            let uci_move = match value.len() {
                4 => UCIMove::Regular {
                    m: Move::try_from(value)?,
                },
                5 => {
                    let squares = &value[0..4];
                    // guaranteed to have 5th ascii char
                    let kind = value.chars().nth(4).unwrap();

                    let mv = Move::try_from(squares)?;
                    let kind = piece::Kind::try_from(kind)?;

                    if kind == piece::Kind::Pawn || kind == piece::Kind::King {
                        return Err("cannot promote to this piece");
                    }

                    UCIMove::Promotion { m: mv, k: kind }
                }
                _ => unreachable!(),
            };

            Ok(uci_move)
        }
    }
}

/// A history entry of a move that has appeared on the chessboard.
///
/// Every variant of this enum contains a [`context::Context`]. This field should be used for
/// storing the copy of chessboard's context state that was valid for the previous move. 
/// Undoing a move should not only include moving pieces to their previous squares, but also
/// restoring the context that was valid for the previous board state.
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum TakenMove {
    /// Represents all moves that are not pawn promotions, en passant captures, or castling.
    PieceMove {
        m: Move,
        captured_piece: Option<piece::Piece>,
        ctx: context::Context,
    },
    /// Represents pawn promotions.
    Promotion {
        m: Move,
        captured_piece: Option<piece::Piece>,
        ctx: context::Context,
    },
    /// Represents en passant captures.
    EnPassant { m: Move, ctx: context::Context },
    /// Represents castling.
    Castling {
        s: context::Side,
        ctx: context::Context,
    },
}

#[cfg(test)]
mod tests {

    use crate::moves::*;
    use crate::square;

    #[test]
    fn move_try_from_str_rejects_incorrect_len() {
        let invalid = ["a", "aaaaa", "asdfasdfasdfasdf", "awr"];
        for s in invalid {
            let m = Move::try_from(s);
            assert!(m.is_err());
            assert_eq!(
                m.unwrap_err(),
                "cannot create move from str that is not 4 chars long"
            );
        }
    }

    #[test]
    fn move_try_from_str_rejects_nonascii_chars() {
        let nonascii = ["¡aaa", "©aaa", "¢bbb", "¼bbb"];
        for s in nonascii {
            let m = Move::try_from(s);
            assert!(m.is_err());
            assert_eq!(
                m.unwrap_err(),
                "cannot create move from str containing non-ascii chars"
            );
        }
    }

    #[test]
    fn move_try_from_str_accepts_valid_squares() {
        let values = [
            ("a1a2", "a1", "a2"),
            ("e2e4", "e2", "e4"),
            ("e1g1", "e1", "g1"),
            ("e7d8", "e7", "d8"),
        ];

        for (move_str, start_str, target_str) in values {
            let expected_start = square::Square::try_from(start_str).unwrap();
            let expected_target = square::Square::try_from(target_str).unwrap();

            let created_move = Move::try_from(move_str).unwrap();

            assert_eq!(created_move.get_start(), expected_start);
            assert_eq!(created_move.get_target(), expected_target);
        }
    }

    #[test]
    fn move_try_from_str_rejects_invalid_start_square() {
        for ch in 'i'..='z' {
            let s = format!("{}1a4", ch);
            let m = Move::try_from(s.as_ref());
            assert!(m.is_err());
            assert_eq!(m.unwrap_err(), "invalid start square representation");
        }
    }

    #[test]
    fn move_try_from_str_rejects_invalid_target_square() {
        for ch in 'i'..='z' {
            let s = format!("a1{}4", ch);
            let m = Move::try_from(s.as_ref());
            assert!(m.is_err());
            assert_eq!(m.unwrap_err(), "invalid target square representation");
        }
    }

    #[test]
    fn move_debug_is_correct() {
        let m = Move::try_from("e2e4").unwrap();
        let debug = format!("{:?}", m);
        assert_eq!("e2e4", debug);
    }

    #[test]
    fn ucimove_try_from_str_rejects_incorrect_len() {
        let invalid = ["a", "aaaaab", "asdfasdfasdfasdf", "awr"];
        for s in invalid {
            let m = UCIMove::try_from(s);
            assert!(m.is_err());
            assert_eq!(
                m.unwrap_err(),
                "cannot create uci move from str that is not 4 or 5 chars long"
            );
        }
    }

    #[test]
    fn ucimove_try_from_str_rejects_nonascii_chars() {
        let nonascii = ["¡aaa", "©aaa", "¢bbb", "¼bbb"];
        for s in nonascii {
            let m = UCIMove::try_from(s);
            assert!(m.is_err());
            assert_eq!(
                m.unwrap_err(),
                "cannot create uci move from str containing non-ascii chars"
            );
        }
    }

    #[test]
    fn ucimove_try_from_str_accepts_valid_regular_moves() {
        let valid_moves = ["e2e4", "e7e5", "e1g1", "g1f3"];

        for mv in valid_moves {
            let expected_move = Move::try_from(mv).unwrap();
            let uci_m = UCIMove::try_from(mv).unwrap();

            if let UCIMove::Regular { m } = uci_m {
                assert_eq!(expected_move, m);
            } else {
                panic!("incorrect uci move variant")
            }
        }
    }

    #[test]
    fn ucimove_try_from_str_accepts_valid_promotion_moves() {
        let valid_moves = [
            ("e7e8q", "e7e8", 'q'),
            ("a7b8n", "a7b8", 'n'),
            ("d2d1b", "d2d1", 'b'),
            ("h2g1r", "h2g1", 'r'),
        ];

        for (full_move, expected_mv, expected_k) in valid_moves {
            let expected_move = Move::try_from(expected_mv).unwrap();
            let expected_kind = piece::Kind::try_from(expected_k).unwrap();

            let uci_m = UCIMove::try_from(full_move).unwrap();

            if let UCIMove::Promotion { m, k } = uci_m {
                assert_eq!(expected_move, m);
                assert_eq!(expected_kind, k);
            } else {
                panic!("incorrect uci move variant")
            }
        }
    }

    #[test]
    fn ucimove_debug_is_correct() {
        let m1 = UCIMove::try_from("e2e4").unwrap();
        let debug = format!("{:?}", m1);
        assert_eq!("e2e4", debug);

        let m2 = UCIMove::try_from("e7e8q").unwrap();
        let debug = format!("{:?}", m2);
        assert_eq!("e7e8q", debug);
    }
}
