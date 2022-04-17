/// Represents all types of pieces that can be found on the chessboard.
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Kind {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
}

/// Represents colors of pieces that can be found on the chessboard.
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Color {
    Black,
    White,
}

/// Represents a chess piece.
#[derive(Debug, PartialEq)]
pub struct Piece {
    kind: Kind,
    color: Color,
}

impl Piece {

    /// Creates an immutable piece object from given [`Kind`] and [`Color`] arguments.
    pub fn new(kind: Kind, color: Color) -> Self {
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
            let kind = match value.to_ascii_lowercase() {
                'p' => Kind::Pawn,
                'r' => Kind::Rook,
                'n' => Kind::Knight,
                'b' => Kind::Bishop,
                'q' => Kind::Queen,
                'k' => Kind::King,
                _ => return Err("char does not represent a valid piece")
            };
            Ok(Piece::new(kind, color))
        }
    }
}

#[cfg(test)] 
mod tests {
    use crate::piece::*;
    use std::convert::TryFrom;

    #[test]
    fn piece_try_from_works_for_expected_values() {
        let valid_black_chars = [
            ('p', Kind::Pawn),
            ('r', Kind::Rook),
            ('n', Kind::Knight),
            ('b', Kind::Bishop),
            ('q', Kind::Queen),
            ('k', Kind::King)
        ];
        let valid_white_chars = valid_black_chars
            .clone().map(|(ch, kind)| (ch.to_ascii_uppercase(), kind));

        // check valid black chars
        for (ch, kind) in valid_black_chars {
            let expected_piece = Piece::new(kind, Color::Black);
            let piece = Piece::try_from(ch).unwrap();
            assert_eq!(expected_piece, piece);
        }
        
        // check valid white chars
        for (ch, kind) in valid_white_chars {
            let expected_piece = Piece::new(kind, Color::White);
            let piece = Piece::try_from(ch).unwrap();
            assert_eq!(expected_piece, piece);
        }
    }

    #[test]
    fn piece_try_from_fails_for_incorrect_ascii_values() {
        let valid_chars = [
            'p', 'r', 'n', 'b', 'q', 'k',
            'P', 'R', 'N', 'B', 'Q', 'K',
        ];

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
            assert_eq!(piece.unwrap_err(), "non-ascii characters cannot represent pieces");
        }
    }
}
