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

    // if the pawn is white, then it moves towards the 8th rank (positive direction)
    // if the pawn is black, then it moves towards the 1st rank (negative direction)
    let (piece_color, piece_direction): (piece::Color, i8) = match white_taken.is_set(piece_square)
    {
        true => (piece::Color::White, 1),
        false => (piece::Color::Black, -1),
    };

    let mut can_move_once = false;

    // calculate the index of a square that's right above (in case of white)
    // or right below (in case of black) the square on which the pawn is
    let one_rank_move_index = piece_square.get_index() + ((8 * piece_direction) as usize);
    // piece_square index is always in range 0..=63 and adding 8 or subtracting 8 is never
    // going outside of that range, since the invariant is that pawns in the game cannot
    // remain on the 1st and 8th rank
    let target_square1 = square::Square::from(one_rank_move_index as u8);
    // if that square is free, then it's possible to move there
    if !all_taken.is_set(target_square1) {
        can_move_once = true;
        let mv = moves::Move::new(piece_square, target_square1);
        match target_square1.get_rank() {
            // move is a promotion
            square::Rank::R8 | square::Rank::R1 => {
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
            }
            // move is not a promotion
            _ => {
                moves.push(moves::UCIMove::Regular { m: mv });
            }
        };
    }

    // pawn can move two squares only if it can move one square and is on its home rank
    if can_move_once {
        let start_rank = piece_square.get_rank();
        if (piece_color == piece::Color::White && start_rank == square::Rank::R2)
            || (piece_color == piece::Color::Black && start_rank == square::Rank::R7)
        {
            // just like in the one square move case: it's safe to add/subtract 16 from
            // the square's index, because we know that:
            // - white is on the 2nd rank, so adding 16 lets us move two ranks above
            // - black is on the 7th rank, so subtracting 16 lets us move two ranks below
            let two_rank_move_index = piece_square.get_index() + ((16 * piece_direction) as usize);
            let target_square2 = square::Square::from(two_rank_move_index as u8);

            if !all_taken.is_set(target_square2) {
                let mv = moves::Move::new(piece_square, target_square2);
                moves.push(moves::UCIMove::Regular { m: mv });
            }
        }
    }

    moves
}

#[cfg(test)]
mod tests {
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
}
