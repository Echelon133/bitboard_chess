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
///
/// To calculate whether a pawn can be pushed once, a bitboard that contains
/// information about all taken squares should be shifted by one rank towards the
/// pawn. To shift all bits of the bitboard by one rank requires a left or right bitshift
/// by 8. Since the board goes left-to-right, bottom-to-top, shifting right moves
/// ranks towards the bottom of the board, and shifting left moves ranks towards the top
/// of the board.
///
/// To calculate captures, offsets (+- 7,9) can be used to find squares that are on diagonals
/// of the pawn square.
///
/// For white:
/// -  - -  - - - - -
/// - +7 - +9 - - - -
/// -  - P  - - - - -
///
/// For black:
/// -  - -  - - - - -
/// -  - p  - - - - -
/// - -9 - -7 - - - -
///
/// If the pawn is on the A or H file, it can only attack one side. Calculating offsets
/// for pawns on these files does not work exactly, because then one attacking square index
/// wraps arround and ends on the same rank as the pawn, but on the opposite side of the board.
/// To only take squares on the attacked rank into account (and eliminate wrapped around squares)
/// the bitboard that stores attacked squares and a mask that has 8 lit bits on the attacked
/// rank should have bitwise AND be performed on them. The resulting bitboard does not have 
/// squares that got marked incorrectly due to being wrapped around.
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
    let piece_color = match white_taken.is_set(piece_square) {
        true => piece::Color::White,
        false => piece::Color::Black,
    };

    let mut can_move_once = false;
    let pawn_rank = piece_square.get_rank();
    let square_index = piece_square.get_index() as i8;

    // only non-capturing moves
    // shift_one - bitboard where other pieces got shifted a rank towards the pawn
    // shift_two - bitboard where other pieces got shifted two ranks towards the pawn
    // square_dist - how many indexes away the square above (for white) and below (for black) is
    // start_rank - rank on which the pawn starts and can potentially move two squares at once
    // promotion_rank - rank on which the pawn can promote on its next move
    let (shift_one, shift_two, square_dist, start_rank, promotion_rank) = match piece_color {
        piece::Color::White => (
            all_taken >> 8,
            all_taken >> 16,
            8i8,
            square::Rank::R2,
            square::Rank::R7,
        ),
        piece::Color::Black => (
            all_taken << 8,
            all_taken << 16,
            -8i8,
            square::Rank::R7,
            square::Rank::R2,
        ),
    };

    if !shift_one.is_set(piece_square) {
        can_move_once = true;
        // square that's one rank above (for white) or one rank below (for black)
        let square1 = square::Square::from((square_index + square_dist) as u8);
        let mv = moves::Move::new(piece_square, square1);
        if pawn_rank == promotion_rank {
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

    if can_move_once && (pawn_rank == start_rank) && !shift_two.is_set(piece_square) {
        // square that's two ranks above (for white) and two ranks below (for black)
        let square2 = square::Square::from((square_index + (2 * square_dist)) as u8);
        let mv = moves::Move::new(piece_square, square2);
        moves.push(moves::UCIMove::Regular { m: mv });
    }

    // only capturing moves
    // left_square - attacked square to the left of the pawn (from the pawn's perspective)
    // right_square - attacked square to the right of the pawn (from the pawn's perspective)
    // attacked_rank_index - how many times a 0b11111111 mask has to be shifted
    //      left by 8 to completely cover the bits of the rank attacked by the pawn
    // opponent_taken - bitboard that represents squares taken by the opposite color
    // promotion_rank - rank on which the pawn is placed before it can promote in it's next move
    let (left_square, right_square, attacked_rank_index, opponent_taken, promotion_rank) =
        match piece_color {
            piece::Color::White => {
                let left = square::Square::from(square_index as u8 + 7);
                let right = square::Square::from(square_index as u8 + 9);
                let attacked_rank_index = pawn_rank.index() + 1;
                (
                    left,
                    right,
                    attacked_rank_index,
                    black_taken,
                    square::Rank::R7,
                )
            }
            piece::Color::Black => {
                let left = square::Square::from(square_index as u8 - 9);
                let right = square::Square::from(square_index as u8 - 7);
                let attacked_rank_index = pawn_rank.index() - 1;
                (
                    left,
                    right,
                    attacked_rank_index,
                    white_taken,
                    square::Rank::R2,
                )
            }
        };

    let mut attack_bitboard = bitboard::Bitboard::default();
    attack_bitboard.set(left_square);
    attack_bitboard.set(right_square);

    let mask = 0b11111111u64 << (8 * attacked_rank_index);
    let attacked_rank_mask = bitboard::Bitboard::from(mask);

    // in case the pawn was on A or H file, remove squares that got incorrectly
    // set because of the wrap-around (more info in this function's docs)
    let attack_bitboard = attack_bitboard & attacked_rank_mask;

    // TODO: this is the place where en-passant square should be checked,
    // because the next step erases info about squares that are attacked
    // but don't have an enemy piece on them (and since the pawn attacked during
    // en-passant is not attacked directly, the information will be lost)
    let actually_attacked_squares = attack_bitboard & *opponent_taken;

    // moves that not only can capture, but also promote at the same time
    if pawn_rank == promotion_rank {
        for attacked_square in actually_attacked_squares.iter() {
            let mv = moves::Move::new(piece_square, attacked_square);
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
    } else {
        for attacked_square in actually_attacked_squares.iter() {
            let mv = moves::Move::new(piece_square, attacked_square);
            moves.push(moves::UCIMove::Regular { m: mv });
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
    fn pawn_white_on_promotion_square_has_noncapturing_promotion_moves() {
        let board = board::Board::try_from("8/1P5k/8/8/8/8/8/6K1").unwrap();
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

        let expected_set = expected_moves
            .into_iter()
            .collect::<HashSet<moves::UCIMove>>();

        for found_move in &found_moves {
            assert!(expected_set.contains(found_move));
        }
    }

    #[test]
    fn pawn_white_on_promotion_square_has_capturing_promotion_moves() {
        let board = board::Board::try_from("bn2b2k/P7/8/8/8/8/8/1K6").unwrap();
        let (white_taken, black_taken) = extract_squares_taken(&board);

        let square = square::Square::try_from("a7").unwrap();
        let found_moves = find_pawn_moves(
            square,
            white_taken,
            black_taken,
            &context::Context::default(),
        );

        assert_eq!(found_moves.len(), 4);
        let expected_moves = [
            moves::UCIMove::try_from("a7b8q").unwrap(),
            moves::UCIMove::try_from("a7b8b").unwrap(),
            moves::UCIMove::try_from("a7b8n").unwrap(),
            moves::UCIMove::try_from("a7b8r").unwrap(),
        ];

        let expected_set = expected_moves
            .into_iter()
            .collect::<HashSet<moves::UCIMove>>();

        for found_move in &found_moves {
            assert!(expected_set.contains(found_move));
        }
    }

    #[test]
    fn pawn_black_on_promotion_square_has_noncapturing_promotion_moves() {
        let board = board::Board::try_from("8/7k/8/8/8/8/1p6/6K1").unwrap();
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

        let expected_set = expected_moves
            .into_iter()
            .collect::<HashSet<moves::UCIMove>>();

        for found_move in &found_moves {
            assert!(expected_set.contains(found_move));
        }
    }

    #[test]
    fn pawn_black_on_promotion_square_has_capturing_promotion_moves() {
        let board = board::Board::try_from("7k/8/8/8/8/8/7p/1K4NB").unwrap();
        let (white_taken, black_taken) = extract_squares_taken(&board);

        let square = square::Square::try_from("h2").unwrap();
        let found_moves = find_pawn_moves(
            square,
            white_taken,
            black_taken,
            &context::Context::default(),
        );

        assert_eq!(found_moves.len(), 4);
        let expected_moves = [
            moves::UCIMove::try_from("h2g1q").unwrap(),
            moves::UCIMove::try_from("h2g1b").unwrap(),
            moves::UCIMove::try_from("h2g1n").unwrap(),
            moves::UCIMove::try_from("h2g1r").unwrap(),
        ];

        let expected_set = expected_moves
            .into_iter()
            .collect::<HashSet<moves::UCIMove>>();

        for found_move in &found_moves {
            assert!(expected_set.contains(found_move));
        }
    }

    #[test]
    fn pawn_white_can_attack_enemy_pieces() {
        let board = board::Board::try_from("8/7k/8/8/1n1n4/2P5/8/6K1").unwrap();
        let (white_taken, black_taken) = extract_squares_taken(&board);

        let square = square::Square::try_from("c3").unwrap();
        let found_moves = find_pawn_moves(
            square,
            white_taken,
            black_taken,
            &context::Context::default(),
        );

        assert_eq!(found_moves.len(), 3);
        let targets = extract_targets(&found_moves);
        let expected_targets = notation_to_squares(&["b4", "c4", "d4"]);
        for target in expected_targets {
            assert!(targets.contains(&target));
        }
    }

    #[test]
    fn pawn_white_cannot_attack_own_pieces() {
        let board = board::Board::try_from("8/7k/8/8/1R1R4/2P5/8/6K1").unwrap();
        let (white_taken, black_taken) = extract_squares_taken(&board);

        let square = square::Square::try_from("c3").unwrap();
        let found_moves = find_pawn_moves(
            square,
            white_taken,
            black_taken,
            &context::Context::default(),
        );

        assert_eq!(found_moves.len(), 1);
        let targets = extract_targets(&found_moves);
        let expected_targets = notation_to_squares(&["c4"]);
        for target in expected_targets {
            assert!(targets.contains(&target));
        }
    }

    #[test]
    fn pawn_white_on_file_a_attacks_enemy_pieces() {
        let board = board::Board::try_from("8/7k/8/8/1p6/P6p/8/6K1").unwrap();
        let (white_taken, black_taken) = extract_squares_taken(&board);

        let square = square::Square::try_from("a3").unwrap();
        let found_moves = find_pawn_moves(
            square,
            white_taken,
            black_taken,
            &context::Context::default(),
        );

        assert_eq!(found_moves.len(), 2);
        let targets = extract_targets(&found_moves);
        let expected_targets = notation_to_squares(&["b4", "a4"]);
        for target in expected_targets {
            assert!(targets.contains(&target));
        }
    }

    #[test]
    fn pawn_white_on_file_a_cannot_attack_own_pieces() {
        let board = board::Board::try_from("8/7k/8/8/RP6/P6p/8/6K1").unwrap();
        let (white_taken, black_taken) = extract_squares_taken(&board);

        let square = square::Square::try_from("a3").unwrap();
        let found_moves = find_pawn_moves(
            square,
            white_taken,
            black_taken,
            &context::Context::default(),
        );

        assert_eq!(found_moves.len(), 0);
    }

    #[test]
    fn pawn_white_on_file_h_attacks_enemy_pieces() {
        let board = board::Board::try_from("8/7k/8/p7/6p1/7P/8/6K1").unwrap();
        let (white_taken, black_taken) = extract_squares_taken(&board);

        let square = square::Square::try_from("h3").unwrap();
        let found_moves = find_pawn_moves(
            square,
            white_taken,
            black_taken,
            &context::Context::default(),
        );

        assert_eq!(found_moves.len(), 2);
        let targets = extract_targets(&found_moves);
        let expected_targets = notation_to_squares(&["g4", "h4"]);
        for target in expected_targets {
            assert!(targets.contains(&target));
        }
    }

    #[test]
    fn pawn_white_on_file_h_cannot_attack_own_pieces() {
        let board = board::Board::try_from("8/7k/8/p7/6PR/7P/8/6K1").unwrap();
        let (white_taken, black_taken) = extract_squares_taken(&board);

        let square = square::Square::try_from("h3").unwrap();
        let found_moves = find_pawn_moves(
            square,
            white_taken,
            black_taken,
            &context::Context::default(),
        );

        assert_eq!(found_moves.len(), 0);
    }

    #[test]
    fn pawn_black_can_attack_enemy_pieces() {
        let board = board::Board::try_from("8/7k/3p4/2N1N3/8/8/8/6K1").unwrap();
        let (white_taken, black_taken) = extract_squares_taken(&board);

        let square = square::Square::try_from("d6").unwrap();
        let found_moves = find_pawn_moves(
            square,
            white_taken,
            black_taken,
            &context::Context::default(),
        );

        assert_eq!(found_moves.len(), 3);
        let targets = extract_targets(&found_moves);
        let expected_targets = notation_to_squares(&["c5", "d5", "e5"]);
        for target in expected_targets {
            assert!(targets.contains(&target));
        }
    }

    #[test]
    fn pawn_black_cannot_attack_own_pieces() {
        let board = board::Board::try_from("8/7k/3p4/2n1n3/8/8/8/6K1").unwrap();
        let (white_taken, black_taken) = extract_squares_taken(&board);

        let square = square::Square::try_from("d6").unwrap();
        let found_moves = find_pawn_moves(
            square,
            white_taken,
            black_taken,
            &context::Context::default(),
        );

        assert_eq!(found_moves.len(), 1);
        let targets = extract_targets(&found_moves);
        let expected_targets = notation_to_squares(&["d5"]);
        for target in expected_targets {
            assert!(targets.contains(&target));
        }
    }

    #[test]
    fn pawn_black_on_file_a_attacks_enemy_pieces() {
        let board = board::Board::try_from("8/7k/p7/1P6/7P/8/8/6K1").unwrap();
        let (white_taken, black_taken) = extract_squares_taken(&board);

        let square = square::Square::try_from("a6").unwrap();
        let found_moves = find_pawn_moves(
            square,
            white_taken,
            black_taken,
            &context::Context::default(),
        );

        assert_eq!(found_moves.len(), 2);
        let targets = extract_targets(&found_moves);
        let expected_targets = notation_to_squares(&["b5", "a5"]);
        for target in expected_targets {
            assert!(targets.contains(&target));
        }
    }

    #[test]
    fn pawn_black_on_file_a_cannot_attack_own_pieces() {
        let board = board::Board::try_from("8/7k/p7/np6/8/8/8/6K1").unwrap();
        let (white_taken, black_taken) = extract_squares_taken(&board);

        let square = square::Square::try_from("a6").unwrap();
        let found_moves = find_pawn_moves(
            square,
            white_taken,
            black_taken,
            &context::Context::default(),
        );

        assert_eq!(found_moves.len(), 0);
    }

    #[test]
    fn pawn_black_on_file_h_attacks_enemy_pieces() {
        let board = board::Board::try_from("8/7k/P6p/6P1/8/8/8/6K1").unwrap();
        let (white_taken, black_taken) = extract_squares_taken(&board);

        let square = square::Square::try_from("h6").unwrap();
        let found_moves = find_pawn_moves(
            square,
            white_taken,
            black_taken,
            &context::Context::default(),
        );

        assert_eq!(found_moves.len(), 2);
        let targets = extract_targets(&found_moves);
        let expected_targets = notation_to_squares(&["g5", "h5"]);
        for target in expected_targets {
            assert!(targets.contains(&target));
        }
    }

    #[test]
    fn pawn_black_on_file_h_cannot_attack_own_pieces() {
        let board = board::Board::try_from("8/7k/7p/6pr/8/8/8/6K1").unwrap();
        let (white_taken, black_taken) = extract_squares_taken(&board);

        let square = square::Square::try_from("h6").unwrap();
        let found_moves = find_pawn_moves(
            square,
            white_taken,
            black_taken,
            &context::Context::default(),
        );

        assert_eq!(found_moves.len(), 0);
    }
}
