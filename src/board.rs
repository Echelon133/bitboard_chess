use std::fmt::Debug;

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
            }
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

    /// Returns how many pieces in general are on the board.
    pub fn count_all_pieces(&self) -> u8 {
        self.white_taken.count_set() + self.black_taken.count_set()
    }

    /// Returns an immutable reference to the bitboard that represents a [`piece::Piece`].
    #[inline(always)]
    pub fn get_piece_bitboard(&self, piece: &piece::Piece) -> &bitboard::Bitboard {
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
    pub fn get_squares_taken(&self, color: piece::Color) -> &bitboard::Bitboard {
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

    /// Returns a pair of (white_taken, black_taken) bitboards.
    pub fn get_squares_taken_pair(&self) -> (&bitboard::Bitboard, &bitboard::Bitboard) {
        (&self.white_taken, &self.black_taken)
    }
}

impl TryFrom<&str> for Board {
    type Error = &'static str;

    /// Parses a partial FEN string (only the part that describes where to put
    /// pieces) and returns a [`Board`] that represents that piece setup.
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let ranks = value.split('/').collect::<Vec<&str>>();

        if ranks.len() != 8 {
            return Err("invalid number of ranks describing the board");
        }

        let mut board = Board::new();

        for (rank_i, rank) in ranks.iter().enumerate() {
            let mut file_i = 0;
            let mut chars_in_rank = rank.chars();

            let mut last_was_number = false;

            while file_i < 8 {
                let ch = match chars_in_rank.next() {
                    Some(ch) => ch,
                    None => {
                        return Err("rank should describe 8 squares");
                    }
                };

                match piece::Piece::try_from(ch) {
                    // it's possible to construct a piece from the given character
                    Ok(piece) => {
                        // unwrap right away because both file_i and rank_i are
                        // in the correct range (0..=7)
                        let file = square::File::try_from(file_i).unwrap();
                        // our rank_i grows from 0..=7 but ranks on the board
                        // go from 7 down to 0, so the rank_i needs to be transformed
                        let rank = square::Rank::try_from(7 - rank_i).unwrap();
                        let square = square::Square::new(rank, file);
                        board.place_piece(square, &piece);
                        last_was_number = false;
                        file_i += 1;
                    }
                    // it's not possible to construct a piece from the given character
                    Err(_) => {
                        // check if the character was a digit that tells the parser
                        // how many squares on the file should be skipped
                        match ch.to_digit(10) {
                            Some(skip_files) => {
                                if last_was_number {
                                    return Err("two numbers next to each other in a rank");
                                }
                                last_was_number = true;
                                file_i += skip_files as usize;
                                if file_i > 8 {
                                    return Err("rank should describe 8 squares");
                                }
                            }
                            None => {
                                return Err("failed to parse because of invalid character");
                            }
                        }
                    }
                }
                // if we already filled the last square and there are still symbols in
                // this rank
                if file_i == 8 && (chars_in_rank.next().is_some()) {
                    return Err("rank should describe 8 squares");
                }
            }
        }

        Ok(board)
    }
}

impl Default for Board {
    /// Returns a [`Board`] with the starting position already setup.
    fn default() -> Self {
        Board::try_from("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR").unwrap()
    }
}

impl Debug for Board {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // iterate top-to-bottom
        for rank_i in (0..=7).rev() {
            write!(f, "{} ", rank_i + 1)?;
            // guaranteed to not panic because 0..=7 is in range
            let rank = square::Rank::try_from(rank_i).unwrap();
            // iterate left-to-right
            for file_i in 0..=7 {
                // guaranteed to not panic because 0..=7 is in range
                let file = square::File::try_from(file_i).unwrap();

                let square = square::Square::new(rank, file);
                let piece_symbol = match self.get_piece(square) {
                    Some(piece) => piece.to_string(),
                    None => " ".to_string(),
                };
                write!(f, "[{}]", piece_symbol)?;
            }
            write!(f, "\n")?;
        }
        write!(f, "   A  B  C  D  E  F  G  H\n")
    }
}

#[cfg(test)]
mod tests {
    use crate::board::*;
    use crate::piece;
    use crate::square;

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
    fn board_is_square_empty_correct_when_square_taken() {
        let mut board = Board::new();
        let square = square::Square::try_from("a1").unwrap();
        let rook = piece::Piece::try_from('R').unwrap();
        board.place_piece(square, &rook);
        assert!(!board.is_square_empty(square));
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
    fn board_get_piece_correctly_reads_pieces_for_both_colors() {
        let mut board = Board::new();

        let white_square = square::Square::try_from("c5").unwrap();
        let white_pieces = ['P', 'B', 'N', 'R', 'Q', 'K'];
        for piece_symbol in white_pieces {
            let piece = piece::Piece::try_from(piece_symbol).unwrap();
            board.place_piece(white_square, &piece);
            let received_piece = board.get_piece(white_square);
            assert_eq!(received_piece.unwrap(), piece);
        }

        let black_square = square::Square::try_from("g5").unwrap();
        let black_pieces = white_pieces.clone().map(|p| p.to_ascii_lowercase());
        for piece_symbol in black_pieces {
            let piece = piece::Piece::try_from(piece_symbol).unwrap();
            board.place_piece(black_square, &piece);
            let received_piece = board.get_piece(black_square);
            assert_eq!(received_piece.unwrap(), piece);
        }

        assert_eq!(board.count_pieces(piece::Color::White), 1);
        assert_eq!(board.count_pieces(piece::Color::Black), 1);
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
    fn board_remove_piece_works_when_square_not_empty() {
        let mut board = Board::new();

        let square = square::Square::try_from("a1").unwrap();

        // place a white rook on "a1"
        let rook_white = piece::Piece::try_from('R').unwrap();
        board.place_piece(square, &rook_white);

        // remove piece that's on "a1"
        let removed_piece = board.remove_piece(square);
        assert_eq!(removed_piece.unwrap(), rook_white);

        // neither color has any pieces
        assert_eq!(board.count_pieces(piece::Color::White), 0);
        assert_eq!(board.count_pieces(piece::Color::Black), 0);
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
    fn board_try_from_str_empty_board() {
        let board = Board::try_from("8/8/8/8/8/8/8/8").unwrap();

        let expected_board = r#"8 [ ][ ][ ][ ][ ][ ][ ][ ]
7 [ ][ ][ ][ ][ ][ ][ ][ ]
6 [ ][ ][ ][ ][ ][ ][ ][ ]
5 [ ][ ][ ][ ][ ][ ][ ][ ]
4 [ ][ ][ ][ ][ ][ ][ ][ ]
3 [ ][ ][ ][ ][ ][ ][ ][ ]
2 [ ][ ][ ][ ][ ][ ][ ][ ]
1 [ ][ ][ ][ ][ ][ ][ ][ ]
   A  B  C  D  E  F  G  H
"#;

        let actual_board = format!("{:?}", board);
        assert_eq!(expected_board, actual_board);

        assert_eq!(board.count_pieces(piece::Color::White), 0);
        assert_eq!(board.count_pieces(piece::Color::Black), 0);
    }

    #[test]
    fn board_try_from_str_fails_when_fen_ranks_num_incorrect() {
        let incorrect = [
            "",
            "8/",
            "8/8/8/8/8/8/8",
            "8/8/8/8/8/8/8/8/8",
            "8/8/8/8/8/",
            "q7/pppppppp/8/2b5/8/8/8/8/8/8",
        ];

        for i_fen in incorrect {
            let board = Board::try_from(i_fen);
            assert!(board.is_err());
            assert_eq!(
                board.unwrap_err(),
                "invalid number of ranks describing the board"
            );
        }
    }

    #[test]
    fn board_try_from_str_fails_when_two_numbers_in_row() {
        let incorrect = ["62/8/8/8/8/8/8/8", "17/8/8/8/8/8/8/8", "8/8/8/8/8/8/8/53"];

        for i_fen in incorrect {
            let board = Board::try_from(i_fen);
            assert!(board.is_err());
            assert_eq!(
                board.unwrap_err(),
                "two numbers next to each other in a rank"
            );
        }
    }

    #[test]
    fn board_try_from_str_fails_when_rank_too_short() {
        let incorrect = [
            "6p/8/8/8/8/8/8/8",
            "8/ppppp/8/8/8/8/8/8",
            "8/5pp/8/8/8/8/8/8",
        ];

        for i_fen in incorrect {
            let board = Board::try_from(i_fen);
            assert!(board.is_err());
            assert_eq!(board.unwrap_err(), "rank should describe 8 squares");
        }
    }

    #[test]
    fn board_try_from_str_fails_when_rank_too_long() {
        let incorrect = [
            "9/8/8/8/8/8/8/8",
            "8/pppppppppp/8/8/8/8/8/8",
            "8/8/8/8/bbbbbbbbbbbbb/8/8/8",
        ];

        for i_fen in incorrect {
            let board = Board::try_from(i_fen);
            assert!(board.is_err());
            assert_eq!(board.unwrap_err(), "rank should describe 8 squares");
        }
    }

    #[test]
    fn board_try_from_fails_when_invalid_char_encountered() {
        let correct = ['p', 'r', 'n', 'b', 'q', 'k', 'P', 'R', 'N', 'B', 'Q', 'K'];

        for ch in 'a'..='z' {
            if !correct.contains(&ch) {
                let fen = format!("{}7/8/8/8/8/8/8/8", ch);
                let board = Board::try_from(fen.as_ref());
                assert!(board.is_err());
                assert_eq!(
                    board.unwrap_err(),
                    "failed to parse because of invalid character"
                );
            }
        }

        for ch in 'A'..='Z' {
            if !correct.contains(&ch) {
                let fen = format!("{}7/8/8/8/8/8/8/8", ch);
                let board = Board::try_from(fen.as_ref());
                assert!(board.is_err());
                assert_eq!(
                    board.unwrap_err(),
                    "failed to parse because of invalid character"
                );
            }
        }
    }

    #[test]
    fn board_try_from_str_corners_taken() {
        let board = Board::try_from("q6n/8/8/8/8/8/8/B6R").unwrap();

        let expected_board = r#"8 [q][ ][ ][ ][ ][ ][ ][n]
7 [ ][ ][ ][ ][ ][ ][ ][ ]
6 [ ][ ][ ][ ][ ][ ][ ][ ]
5 [ ][ ][ ][ ][ ][ ][ ][ ]
4 [ ][ ][ ][ ][ ][ ][ ][ ]
3 [ ][ ][ ][ ][ ][ ][ ][ ]
2 [ ][ ][ ][ ][ ][ ][ ][ ]
1 [B][ ][ ][ ][ ][ ][ ][R]
   A  B  C  D  E  F  G  H
"#;

        let actual_board = format!("{:?}", board);
        assert_eq!(expected_board, actual_board);

        assert_eq!(board.count_pieces(piece::Color::White), 2);
        assert_eq!(board.count_pieces(piece::Color::Black), 2);
    }

    #[test]
    fn board_default_returns_starting_position() {
        let board = Board::default();

        let expected_board = r#"8 [r][n][b][q][k][b][n][r]
7 [p][p][p][p][p][p][p][p]
6 [ ][ ][ ][ ][ ][ ][ ][ ]
5 [ ][ ][ ][ ][ ][ ][ ][ ]
4 [ ][ ][ ][ ][ ][ ][ ][ ]
3 [ ][ ][ ][ ][ ][ ][ ][ ]
2 [P][P][P][P][P][P][P][P]
1 [R][N][B][Q][K][B][N][R]
   A  B  C  D  E  F  G  H
"#;

        let actual_board = format!("{:?}", board);
        assert_eq!(expected_board, actual_board);

        assert_eq!(board.count_pieces(piece::Color::White), 16);
        assert_eq!(board.count_pieces(piece::Color::Black), 16);
    }
}
