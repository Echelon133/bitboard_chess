use crate::bitboard;
use crate::context;
use crate::moves;
use crate::piece;
use crate::square;

/// Finds all pseudo-legal moves for the pawn on the given square.
/// This function assumes that a piece that is placed on the given
/// square is actually a pawn. It does not check whether that is true,
/// so incorrect call to this function will yield invalid moves.
///
/// Since [`square::Square`] holds the index of the square on the board
/// (growing left-to-right, bottom-to-top), it's possible to calculate indexes
/// of squares relative to the square where our pawn is:
/// - one rank above has index = (index + 8)
/// - one rank below has index = (index - 8)
/// - two ranks above index = (index + 16)
/// - two ranks below index = (index - 16)
///
/// This is a safe assumption, since during the game pawns do not occur
/// on the 1st and 8th rank of the board.
fn find_pawn_moves(
    piece_square: square::Square,
    white_taken: &bitboard::Bitboard,
    black_taken: &bitboard::Bitboard,
    _context: &context::Context,
) -> Vec<moves::UCIMove> {
    let mut moves = Vec::with_capacity(4);

    let all_taken = *white_taken | *black_taken;

    // the invariant: piece_square must be a square that's set to 1 either
    // for white_taken or black_taken, so if it's set for white_taken, then it should be
    // impossible for it to be set to 1 for black_taken
    let piece_color = match white_taken.is_set(piece_square)
    {
        true => piece::Color::White,
        false => piece::Color::Black,
    };

    let mut can_move_once = false;
    let pawn_rank = piece_square.get_rank();

    match piece_color {
        piece::Color::White => {
            // if we shift the bitboard that represents all taken squares
            // right by 8, this means we are moving all ranks one below,
            // so if we move all ranks one below and then check the square where our
            // pawn was before the shift, we'll know whether the square above our pawn
            // was free
            let shift_one_rank = all_taken >> 8;
            if !shift_one_rank.is_set(piece_square) {
                can_move_once = true;
                let square_above = square::Square::from((piece_square.get_index() + 8) as u8);
                let mv = moves::Move::new(piece_square, square_above);
                // this move results in promotion
                if pawn_rank == square::Rank::R7 {
                    moves.push(moves::UCIMove::Promotion {
                        m: mv,
                        k: piece::Kind::Knight,
                    });
                    moves.push(moves::UCIMove::Promotion {
                        m: mv,
                        k: piece::Kind::Bishop,
                    });
                    moves.push(moves::UCIMove::Promotion {
                        m: mv,
                        k: piece::Kind::Queen,
                    });
                    moves.push(moves::UCIMove::Promotion {
                        m: mv,
                        k: piece::Kind::Rook,
                    });
                } else {
                    moves.push(moves::UCIMove::Regular { m: mv });
                }
            }

            let shift_two_ranks = all_taken >> 16;
            if can_move_once
                && (pawn_rank == square::Rank::R2)
                && !shift_two_ranks.is_set(piece_square)
            {
                let square_two_above = square::Square::from((piece_square.get_index() + 16) as u8);
                let mv = moves::Move::new(piece_square, square_two_above);
                moves.push(moves::UCIMove::Regular { m: mv });
            }
        }
        piece::Color::Black => {
            // if we shift the bitboard that represents all taken squares
            // left by 8, this means we are moving all ranks one above,
            // so if we move all ranks one above and then check the square where our
            // pawn was before the shift, we'll know whether the square below our pawn
            // was free
            let shift_one_rank = all_taken << 8;
            if !shift_one_rank.is_set(piece_square) {
                can_move_once = true;
                let square_below = square::Square::from((piece_square.get_index() - 8) as u8);
                let mv = moves::Move::new(piece_square, square_below);
                // this move results in promotion
                if pawn_rank == square::Rank::R2 {
                    moves.push(moves::UCIMove::Promotion {
                        m: mv,
                        k: piece::Kind::Knight,
                    });
                    moves.push(moves::UCIMove::Promotion {
                        m: mv,
                        k: piece::Kind::Bishop,
                    });
                    moves.push(moves::UCIMove::Promotion {
                        m: mv,
                        k: piece::Kind::Queen,
                    });
                    moves.push(moves::UCIMove::Promotion {
                        m: mv,
                        k: piece::Kind::Rook,
                    });
                } else {
                    moves.push(moves::UCIMove::Regular { m: mv });
                }
            }

            let shift_two_ranks = all_taken << 16;
            if can_move_once
                && (pawn_rank == square::Rank::R7)
                && !shift_two_ranks.is_set(piece_square)
            {
                let square_two_below = square::Square::from((piece_square.get_index() - 16) as u8);
                let mv = moves::Move::new(piece_square, square_two_below);
                moves.push(moves::UCIMove::Regular { m: mv });
            }
        }
    }

    moves
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::bitboard;
    use crate::board;
    use crate::context;
    use crate::movegen::*;
    use crate::moves;
    use crate::piece;
    use crate::square;

    fn extract_squares_taken(board: &board::Board) -> (&bitboard::Bitboard, &bitboard::Bitboard) {
        (
            board.get_squares_taken(piece::Color::White),
            board.get_squares_taken(piece::Color::Black),
        )
    }

    /// Extracts target squares from all [`moves::UCIMove`].
    fn extract_targets(m: &Vec<moves::UCIMove>) -> Vec<square::Square> {
        let mut result = Vec::with_capacity(m.len());
        for mv in m.iter() {
            match mv {
                moves::UCIMove::Regular { m } => {
                    result.push(m.get_target());
                }
                moves::UCIMove::Promotion { m, .. } => {
                    result.push(m.get_target());
                }
            }
        }
        result
    }

    /// Transforms an array of squares represented by strings to a [`Vec`] that
    /// holds squares created from those strings.
    ///
    /// Example input:
    /// let input = ["a2", "g5", "c3", "a8"];
    fn notation_to_squares(v: &[&str]) -> Vec<square::Square> {
        v.into_iter()
            .map(|notation| square::Square::try_from(*notation).unwrap())
            .collect()
    }

    #[test]
    fn pawn_unmoved_has_two_moves_when_not_blocked() {
        let board = board::Board::try_from("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR").unwrap();
        let (white_taken, black_taken) = extract_squares_taken(&board);

        let square = square::Square::try_from("a2").unwrap();
        let found_moves = find_pawn_moves(
            square,
            white_taken,
            black_taken,
            &context::Context::default(),
        );

        assert_eq!(found_moves.len(), 2);
        let targets = extract_targets(&found_moves);
        let expected_targets = notation_to_squares(&["a3", "a4"]);
        for target in expected_targets {
            assert!(targets.contains(&target));
        }
    }

    #[test]
    fn pawn_unmoved_has_correct_number_of_moves_when_blocked() {
        // pawn on a2 blocked by other piece on a4 has 1 move
        let board = board::Board::try_from("rnbqkbnr/pppppppp/8/8/n7/8/PPPPPPPP/RNBQKBNR").unwrap();
        let (white_taken, black_taken) = extract_squares_taken(&board);

        let square = square::Square::try_from("a2").unwrap();
        let found_moves = find_pawn_moves(
            square,
            white_taken,
            black_taken,
            &context::Context::default(),
        );

        assert_eq!(found_moves.len(), 1);
        let targets = extract_targets(&found_moves);
        let expected_targets = notation_to_squares(&["a3"]);
        for target in expected_targets {
            assert!(targets.contains(&target));
        }

        // pawn on a2 blocked by other piece on a3 has 0 moves
        let board = board::Board::try_from("rnbqkbnr/pppppppp/8/8/8/n7/PPPPPPPP/RNBQKBNR").unwrap();
        let (white_taken, black_taken) = extract_squares_taken(&board);

        let square = square::Square::try_from("a2").unwrap();
        let found_moves = find_pawn_moves(
            square,
            white_taken,
            black_taken,
            &context::Context::default(),
        );

        assert_eq!(found_moves.len(), 0);
    }

    #[test]
    fn pawn_already_moved_can_only_move_one_forward() {
        // pawn has already made it's first move before
        let board =
            board::Board::try_from("rnbqkbnr/ppp1pppp/3p4/8/4P3/8/PPPP1PPP/RNBQKBNR").unwrap();
        let (white_taken, black_taken) = extract_squares_taken(&board);

        let square = square::Square::try_from("e4").unwrap();
        let found_moves = find_pawn_moves(
            square,
            white_taken,
            black_taken,
            &context::Context::default(),
        );

        assert_eq!(found_moves.len(), 1);
        let targets = extract_targets(&found_moves);
        let expected_targets = notation_to_squares(&["e5"]);
        for target in expected_targets {
            assert!(targets.contains(&target));
        }
    }

    #[test]
    fn pawn_already_moved_has_no_moves_when_blocked() {
        // pawn has already made it's first move before
        let board =
            board::Board::try_from("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR").unwrap();
        let (white_taken, black_taken) = extract_squares_taken(&board);

        let square = square::Square::try_from("e4").unwrap();
        let found_moves = find_pawn_moves(
            square,
            white_taken,
            black_taken,
            &context::Context::default(),
        );

        assert_eq!(found_moves.len(), 0);
    }

    #[test]
    fn pawn_white_on_promotion_square_has_promotion_moves() {
        let board =
            board::Board::try_from("8/1P5k/8/8/8/8/8/6K1").unwrap();
        let (white_taken, black_taken) = extract_squares_taken(&board);

        let square = square::Square::try_from("b7").unwrap();
        let found_moves = find_pawn_moves(
            square,
            white_taken,
            black_taken,
            &context::Context::default(),
        );

        assert_eq!(found_moves.len(), 4);
        let expected_moves = [
            moves::UCIMove::try_from("b7b8q").unwrap(),
            moves::UCIMove::try_from("b7b8b").unwrap(),
            moves::UCIMove::try_from("b7b8n").unwrap(),
            moves::UCIMove::try_from("b7b8r").unwrap(),
        ];

        let expected_set = expected_moves.into_iter().collect::<HashSet<moves::UCIMove>>();
        
        for found_move in &found_moves {
            assert!(expected_set.contains(found_move));
        }
    }

    #[test]
    fn pawn_black_on_promotion_square_has_promotion_moves() {
        let board =
            board::Board::try_from("8/7k/8/8/8/8/1p6/6K1").unwrap();
        let (white_taken, black_taken) = extract_squares_taken(&board);

        let square = square::Square::try_from("b2").unwrap();
        let found_moves = find_pawn_moves(
            square,
            white_taken,
            black_taken,
            &context::Context::default(),
        );

        assert_eq!(found_moves.len(), 4);
        let expected_moves = [
            moves::UCIMove::try_from("b2b1q").unwrap(),
            moves::UCIMove::try_from("b2b1b").unwrap(),
            moves::UCIMove::try_from("b2b1n").unwrap(),
            moves::UCIMove::try_from("b2b1r").unwrap(),
        ];

        let expected_set = expected_moves.into_iter().collect::<HashSet<moves::UCIMove>>();
        
        for found_move in &found_moves {
            assert!(expected_set.contains(found_move));
        }
    }
}
