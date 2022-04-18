use crate::bitboard;
use crate::piece;
use crate::square;

/// Represents the chessboard on a very low level. Only contains methods
/// that either read or manipulate the state of the board (what pieces are placed where).
pub struct Board {
    white_pieces: [bitboard::Bitboard; 6],
    black_pieces: [bitboard::Bitboard; 6],
    white_taken: bitboard::Bitboard,
    black_taken: bitboard::Bitboard,
}

impl Board {
    /// Returns an empty board with no pieces.
    pub fn new() -> Self {
        Self {
            white_pieces: [bitboard::Bitboard::default(); 6],
            black_pieces: [bitboard::Bitboard::default(); 6],
            white_taken: bitboard::Bitboard::default(),
            black_taken: bitboard::Bitboard::default(),
        }
    }

    /// Places a piece on the given [`square::Square`] of the board and returns an
    /// [`Option`] which might contain a [`piece::Piece`] that was previously
    /// on that square. Returns [`None`] it the taken square was empty.
    pub fn place_piece(
        &mut self,
        square: square::Square,
        piece: piece::Piece,
    ) -> Option<piece::Piece> {
        unimplemented!()
    }

    /// Removes a piece from the [`square::Square`] and returns [`Some`]
    /// that contains that piece. If the square was empty, returns [`None`].
    pub fn remove_piece(&mut self, square: square::Square) -> Option<piece::Piece> {
        unimplemented!()
    }

    /// Returns [`Some`] that contains the piece that is placed on the
    /// [`square::Square`] given as an argument. If the square is empty, [`None`]
    /// is returned.
    pub fn get_piece(&self, square: square::Square) -> Option<piece::Piece> {
        unimplemented!()
    }

    /// Returns [`true`] if the given [`square::Square`] is empty, [`false`] if it's taken.
    pub fn is_square_empty(&self, square: square::Square) -> bool {
        unimplemented!()
    }

    /// Returns how many pieces of the given [`piece::Color`] are on the board.
    pub fn count_pieces(&self, color: piece::Color) -> usize {
        unimplemented!()
    }

    /// Returns an immutable reference to the bitboard that represents pieces of kind
    /// [`piece::Kind`] and color [`piece::Color`].
    fn get_piece_bitboard(&self, color: piece::Color, kind: piece::Kind) -> &bitboard::Bitboard {
        unimplemented!()
    }

    /// Returns a mutable reference to the bitboard that represents pieces of kind
    /// [`piece::Kind`] and color [`piece::Color`].
    fn get_piece_bitboard_mut(
        &mut self,
        color: piece::Color,
        kind: piece::Kind,
    ) -> &mut bitboard::Bitboard {
        unimplemented!()
    }
}

#[cfg(test)]
mod tests {}
