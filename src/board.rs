use crate::bitboard;
use crate::piece;
use crate::square;

/// Represents the chessboard on a very low level. Only contains methods
/// that either read or manipulate the state of the board (what pieces are placed where).
pub struct Board {
    // Both white_pieces and black_pieces are arrays of 6 elements, with every element
    // representing squares taken by a single piece kind.
    // Order of kinds in these arrays: pawn, knight, bishop, rook, queen, king.
    //
    // Calling index() on a variant of piece::Kind gives us the index into these
    // arrays.
    //
    // These arrays should be accessed like this:
    // let kind = piece::Kind::King;
    // self.white_pieces[kind.index()]
    //
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
        piece: &piece::Piece,
    ) -> Option<piece::Piece> {

        let removed_piece = self.remove_piece(square);

        let squares_taken = self.get_squares_taken_mut(piece.get_color());
        // set the square on the bitboard that marks which squares are taken
        // by certain color
        squares_taken.set(square);

        let kind_bitboard = self.get_piece_bitboard_mut(piece);
        kind_bitboard.set(square);

        removed_piece
    }

    /// Removes a piece from the [`square::Square`] and returns [`Some`]
    /// that contains that piece. If the square was empty, returns [`None`].
    pub fn remove_piece(&mut self, square: square::Square) -> Option<piece::Piece> {
        match self.get_piece(square) {
            Some(piece) => {
                // unset the taken square for the color of the piece
                let squares_taken = self.get_squares_taken_mut(piece.get_color());
                squares_taken.clear(square);

                // unset the square in the piece bitboard
                let kind_bitboard = self.get_piece_bitboard_mut(&piece);
                kind_bitboard.clear(square);

                Some(piece)
            },
            None => None,
        }
    }

    /// Returns [`Some`] that contains the piece that is placed on the
    /// [`square::Square`] given as an argument. If the square is empty, [`None`]
    /// is returned.
    pub fn get_piece(&self, square: square::Square) -> Option<piece::Piece> {
        if self.is_square_empty(square) {
            None
        } else {
            // it's guaranteed that either white or black has a piece there
            // so if it's not white then it's certainly black (and vice versa)
            let color = match self.white_taken.is_set(square) {
                true => piece::Color::White,
                false => piece::Color::Black,
            }; 

            let piece_bitboards = match color {
                piece::Color::White => &self.white_pieces,
                piece::Color::Black => &self.black_pieces,
            };

            let mut found_kind_index = None;
            for (i, piece_bitboard) in piece_bitboards.iter().enumerate() {
                if piece_bitboard.is_set(square) {
                    found_kind_index = Some(i);
                    break;
                }
            }
            // found_kind_index is safe to unwrap, because if it's not Some, then it means
            // that there is an inconsistency between bitboards. E.g. a white_taken
            // bitboard says that 'a1' is taken, but not a single bitboard with pieces
            // has 'a1' taken
            let kind_index = found_kind_index.unwrap();
            // try_from is safe to unwrap because it's guaranteed that kind_index
            // is not bigger than 5 (because enumerate() of an array of 6 only gives
            // out indexes from 0..=5)
            let kind = piece::Kind::try_from(kind_index).unwrap();

            Some(piece::Piece::new(kind, color))
        }
    }

    /// Returns [`true`] if the given [`square::Square`] is empty, [`false`] if it's taken.
    pub fn is_square_empty(&self, square: square::Square) -> bool {
        // bitwise OR of both bitboards that represent taken squares results
        // in a bitboard that represents all taken squares
        let all_taken_squares = self.white_taken | self.black_taken;
        !all_taken_squares.is_set(square)
    }

    /// Returns how many pieces of the given [`piece::Color`] are on the board.
    pub fn count_pieces(&self, color: piece::Color) -> u8 {
        self.get_squares_taken(color).count_set()
    }

    /// Returns an immutable reference to the bitboard that represents a [`piece::Piece`].
    #[inline(always)]
    fn get_piece_bitboard(&self, piece: &piece::Piece) -> &bitboard::Bitboard {
        let piece_array = match piece.get_color() {
            piece::Color::White => &self.white_pieces,
            piece::Color::Black => &self.black_pieces,
        };
        &piece_array[piece.get_kind().index()]
    }

    /// Returns a mutable reference to the bitboard that represents a [`piece::Piece`].
    #[inline(always)]
    fn get_piece_bitboard_mut(&mut self, piece: &piece::Piece) -> &mut bitboard::Bitboard {
        let piece_array = match piece.get_color() {
            piece::Color::White => &mut self.white_pieces,
            piece::Color::Black => &mut self.black_pieces,
        };
        &mut piece_array[piece.get_kind().index()]
    }

    /// Returns an immutable reference to the bitboard that represens squares taken by
    /// the given color.
    #[inline(always)]
    fn get_squares_taken(&self, color: piece::Color) -> &bitboard::Bitboard {
        match color {
            piece::Color::White => &self.white_taken,
            piece::Color::Black => &self.black_taken,
        }
    }
    
    /// Returns a mutable reference to the bitboard that represents squares taken by
    /// the given color.
    #[inline(always)]
    fn get_squares_taken_mut(&mut self, color: piece::Color) -> &mut bitboard::Bitboard {
        match color {
            piece::Color::White => &mut self.white_taken,
            piece::Color::Black => &mut self.black_taken,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::board::*;
    use crate::square;
    use crate::piece;

    #[test]
    fn board_count_pieces_correct_when_no_pieces() {
        let board = Board::new();
        assert_eq!(board.count_pieces(piece::Color::White), 0);
        assert_eq!(board.count_pieces(piece::Color::Black), 0);
    }

    #[test]
    fn board_is_square_empty_correct_when_no_pieces() {
        let board = Board::new();

        for index in 0..=63 {
            let square = square::Square::from(index);
            assert!(board.is_square_empty(square));
        }
    }

    #[test]
    fn board_get_piece_none_for_all_when_no_pieces() {
        let board = Board::new();

        for index in 0..=63 {
            let square = square::Square::from(index);
            let piece = board.get_piece(square);
            assert!(piece.is_none());
        }
    }
    
    #[test]
    fn board_remove_piece_none_for_all_when_no_pieces() {
        let mut board = Board::new();

        for index in 0..=63 {
            let square = square::Square::from(index);
            let piece = board.remove_piece(square);
            assert!(piece.is_none());
        }
    }

    #[test]
    fn board_place_piece_on_empty_square_works() {
        let mut board = Board::new();

        let square = square::Square::try_from("a1").unwrap();

        // place a white rook on "a1"
        let rook = piece::Piece::try_from('R').unwrap();
        let captured_piece = board.place_piece(square, &rook);

        // the a1 square was empty before, so no pieces should have been captured
        assert!(captured_piece.is_none());
        // only white player has pieces on the board
        assert_eq!(board.count_pieces(piece::Color::White), 1);
        assert_eq!(board.count_pieces(piece::Color::Black), 0);
    }

    #[test]
    fn board_place_piece_on_taken_square_works() {
        let mut board = Board::new();

        let square = square::Square::try_from("a1").unwrap();

        // place a white rook on "a1"
        let rook_white = piece::Piece::try_from('R').unwrap();
        let captured_piece = board.place_piece(square, &rook_white);
        // the a1 square was empty before, so no pieces should have been captured
        assert!(captured_piece.is_none());

        // place a black rook on "a1"
        let rook_black = piece::Piece::try_from('r').unwrap();
        let captured_piece = board.place_piece(square, &rook_black);
        // captured the white rook that was on the square before
        assert_eq!(captured_piece.unwrap(), rook_white);

        // only black player has pieces on the board
        assert_eq!(board.count_pieces(piece::Color::White), 0);
        assert_eq!(board.count_pieces(piece::Color::Black), 1);
    }

    #[test]
    fn board_get_piece_works_when_square_not_empty() {
        let mut board = Board::new();

        let square = square::Square::try_from("a1").unwrap();

        // place a white queen on "a1"
        let queen = piece::Piece::try_from('Q').unwrap();
        let captured_piece = board.place_piece(square, &queen);

        // the a1 square was empty before, so no pieces should have been captured
        assert!(captured_piece.is_none());
        assert_eq!(board.get_piece(square).unwrap(), queen);
    }

    #[test]
    fn board_remove_piece_works_when_square_not_empty() {
        let mut board = Board::new();

        let square = square::Square::try_from("a1").unwrap();
        
        // place a white rook on "a1"
        let rook_white = piece::Piece::try_from('R').unwrap();
        board.place_piece(square, &rook_white);

        // remove piece that's on "a1"
        let removed_piece = board.remove_piece(square);

        // the a1 square was empty before, so no pieces should have been captured
        assert!(removed_piece.is_some());
        assert_eq!(removed_piece.unwrap(), rook_white);

        // neither color has any pieces
        assert_eq!(board.count_pieces(piece::Color::White), 0);
        assert_eq!(board.count_pieces(piece::Color::Black), 0);
    }
}
