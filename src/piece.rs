use std::{fmt::Display, ops::Not};

/// Represents all types of pieces that can be found on the chessboard.
#[derive(Debug, Copy, Clone, PartialEq, Hash, Eq)]
pub enum Kind {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
}

impl Kind {
    /// Returns the index of the piece kind.
    pub fn index(&self) -> usize {
        *self as usize
    }

    /// Returns the lowercase ascii character that represents the kind of the piece.
    ///
    /// This character is consistent with the way pieces in FEN format are represented.
    /// This means that:
    /// - pawn is 'p'
    /// - rook is 'r'
    /// - knight is 'n'
    /// - bishop is 'b'
    /// - queen is 'q'
    /// - king is 'k'
    ///
    pub fn as_char(&self) -> char {
        match self {
            Kind::Pawn => 'p',
            Kind::Rook => 'r',
            Kind::Knight => 'n',
            Kind::Bishop => 'b',
            Kind::Queen => 'q',
            Kind::King => 'k',
        }
    }
}

impl TryFrom<usize> for Kind {
    type Error = &'static str;

    /// Converts an index into a [`Kind`]. Any value bigger than 5
    /// cannot be converted and results in an error.
    fn try_from(value: usize) -> Result<Self, Self::Error> {
        let val = match value {
            0 => Kind::Pawn,
            1 => Kind::Knight,
            2 => Kind::Bishop,
            3 => Kind::Rook,
            4 => Kind::Queen,
            5 => Kind::King,
            _ => {
                return Err("this value does not represent any kind");
            }
        };

        Ok(val)
    }
}

impl TryFrom<char> for Kind {
    type Error = &'static str;

    /// Converts a character into a [`Kind`]. Expected characters
    /// are consistent with the characters used to represent pieces
    /// in the FEN format.
    /// Both lower- and uppercase characters are allowed.
    fn try_from(value: char) -> Result<Self, Self::Error> {
        let kind = match value.to_ascii_lowercase() {
            'p' => Kind::Pawn,
            'r' => Kind::Rook,
            'n' => Kind::Knight,
            'b' => Kind::Bishop,
            'q' => Kind::Queen,
            'k' => Kind::King,
            _ => return Err("char does not represent a valid piece"),
        };
        Ok(kind)
    }
}

/// Represents colors of pieces that can be found on the chessboard.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Color {
    Black,
    White,
}

impl Not for Color {
    type Output = Color;

    /// Changes `Color::White` to `Color::Black` and vice versa.
    fn not(self) -> Self::Output {
        match self {
            Color::White => Color::Black,
            Color::Black => Color::White,
        }   
    }

}

/// Represents a chess piece.
#[derive(Debug, PartialEq, Hash, Eq)]
pub struct Piece {
    kind: Kind,
    color: Color,
}

impl Piece {
    /// Creates an immutable piece object from given [`Kind`] and [`Color`] arguments.
    pub const fn new(kind: Kind, color: Color) -> Self {
        Self { kind, color }
    }

    /// Returns the [`Kind`] of the piece.
    pub fn get_kind(&self) -> Kind {
        self.kind
    }

    /// Returns the [`Color`] of the piece.
    pub fn get_color(&self) -> Color {
        self.color
    }
}

impl Display for Piece {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut ch = self.get_kind().as_char();
        if self.get_color() == Color::White {
            ch = ch.to_ascii_uppercase();
        }

        write!(f, "{}", ch)
    }
}

impl TryFrom<char> for Piece {
    type Error = &'static str;

    /// Takes an ascii character and tries to convert it to a [`Piece`].
    /// This has the same rules of conversion as in the FEN format, i.e.
    /// there are 6 characters that represent kinds of pieces:
    /// - 'p' becomes a pawn
    /// - 'r' becomes a rook
    /// - 'n' becomes a knight
    /// - 'b' becomes a bishop
    /// - 'q' becomes a queen
    /// - 'k' becomes a king
    ///
    /// Lowercase character means that the piece is black and uppercase
    /// character means that the piece is white.
    ///
    /// Any other character or non-ascii characters cause this converter to return
    /// an error of conversion.
    fn try_from(value: char) -> Result<Self, Self::Error> {
        if !value.is_ascii() {
            Err("non-ascii characters cannot represent pieces")
        } else {
            let color = match value.is_ascii_uppercase() {
                true => Color::White,
                false => Color::Black,
            };
            let kind = Kind::try_from(value)?;
            Ok(Piece::new(kind, color))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::piece::*;
    use std::convert::TryFrom;

    #[test]
    fn kind_index_returns_correct_values() {
        let expected_values = [
            (0, Kind::Pawn),
            (1, Kind::Knight),
            (2, Kind::Bishop),
            (3, Kind::Rook),
            (4, Kind::Queen),
            (5, Kind::King),
        ];

        for (expected_index, kind) in expected_values {
            assert_eq!(expected_index, kind.index());
        }
    }

    #[test]
    fn piece_try_from_works_for_expected_values() {
        let valid_black_chars = [
            ('p', Kind::Pawn),
            ('r', Kind::Rook),
            ('n', Kind::Knight),
            ('b', Kind::Bishop),
            ('q', Kind::Queen),
            ('k', Kind::King),
        ];
        let valid_white_chars = valid_black_chars
            .clone()
            .map(|(ch, kind)| (ch.to_ascii_uppercase(), kind));

        // check valid black chars
        for (ch, kind) in valid_black_chars {
            let expected_piece = Piece::new(kind, Color::Black);
            let piece = Piece::try_from(ch).unwrap();
            assert_eq!(expected_piece, piece);
            // check the Display implementation
            assert_eq!(ch.to_string(), piece.to_string());
        }

        // check valid white chars
        for (ch, kind) in valid_white_chars {
            let expected_piece = Piece::new(kind, Color::White);
            let piece = Piece::try_from(ch).unwrap();
            assert_eq!(expected_piece, piece);
            // check the Display implementation
            assert_eq!(ch.to_string(), piece.to_string());
        }
    }

    #[test]
    fn piece_try_from_fails_for_incorrect_ascii_values() {
        let valid_chars = ['p', 'r', 'n', 'b', 'q', 'k', 'P', 'R', 'N', 'B', 'Q', 'K'];

        for ch in 'a'..='z' {
            if !valid_chars.contains(&ch) {
                let piece = Piece::try_from(ch);
                assert!(piece.is_err());
                assert_eq!(piece.unwrap_err(), "char does not represent a valid piece");
            }
        }

        for ch in 'A'..='Z' {
            if !valid_chars.contains(&ch) {
                let piece = Piece::try_from(ch);
                assert!(piece.is_err());
                assert_eq!(piece.unwrap_err(), "char does not represent a valid piece");
            }
        }
    }

    #[test]
    fn piece_try_from_fails_for_nonascii_values() {
        let nonascii = ['¡', '©', '¢', '¼'];

        for ch in nonascii {
            let piece = Piece::try_from(ch);
            assert!(piece.is_err());
            assert_eq!(
                piece.unwrap_err(),
                "non-ascii characters cannot represent pieces"
            );
        }
    }
}
