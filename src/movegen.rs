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
/// for pawns on these files does not work exactly as for pawns on other files, because
/// the index of one attacked square wraps arround and ends
/// up on an incorrect square that's (depending on the file and color of the pawn):
/// - on the same rank as the pawn, but on the other side of the board
/// - rank below/above the actually attacked rank, and still on the other side of the board
///
/// To only take squares on the attacked rank into account (and eliminate squares that
/// got set incorrectly due to the index wrap-around) there should be a bitwise AND operation
/// on the bits of a bitboard that contains the attacked squares and bits of a bitboard that
/// has all squares on the attacked rank lit. This way all of the squares that are not
/// on the attacked rank are eliminated.
///
fn find_pawn_moves(
    piece_square: square::Square,
    white_taken: &bitboard::Bitboard,
    black_taken: &bitboard::Bitboard,
    context: &context::Context,
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

    // check en-passant here, because the next bitwise AND only leaves squares that
    // are directly attacked (i.e. only squares on which enemy pieces are remain,
    // which is not the case when it comes to en-passant, because the piece is not
    // attacked directly)
    if let Some(enpassant_target) = context.get_enpassant() {
        // if the pawn attacks the en-passant target (which is placed behind the pawn that's
        // just moved two squares) then it's possible that a capture can take place
        if attack_bitboard.is_set(enpassant_target) {
            let mv = moves::Move::new(piece_square, enpassant_target);
            moves.push(moves::UCIMove::Regular { m: mv });
        }
    }

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

    macro_rules! check_pawn {
        ($square:literal on $board:ident having $context:ident can be moved to $targets:ident) => {
            let (white_taken, black_taken) = $crate::movegen::tests::extract_squares_taken($board);
            let square = $crate::square::Square::try_from($square).unwrap();
            let found_moves =
                $crate::movegen::find_pawn_moves(square, white_taken, black_taken, $context);
            assert_eq!(found_moves.len(), $targets.len());
            let targets = $crate::movegen::tests::extract_targets(&found_moves);
            let expected_targets = $crate::movegen::tests::notation_to_squares($targets);
            for target in expected_targets {
                assert!(targets.contains(&target));
            }
        };
        ($square:literal on $board:ident having $context:ident cannot be moved) => {
            let (white_taken, black_taken) = $crate::movegen::tests::extract_squares_taken($board);
            let square = $crate::square::Square::try_from($square).unwrap();
            let found_moves =
                $crate::movegen::find_pawn_moves(square, white_taken, black_taken, $context);
            assert_eq!(found_moves.len(), 0);
        };
        ($square:literal on $board:ident having $context:ident can be promoted on $targets:ident) => {
            let (white_taken, black_taken) = extract_squares_taken($board);
            let square = $crate::square::Square::try_from($square).unwrap();
            let found_moves =
                $crate::movegen::find_pawn_moves(square, white_taken, black_taken, $context);
            assert_eq!(found_moves.len(), $targets.len() * 4);
            let mut expected_moves = HashSet::new();
            for m in $targets {
                let m = $crate::square::Square::try_from(*m).unwrap();
                let mv = $crate::moves::Move::new(square, m);
                let kinds = [
                    $crate::piece::Kind::Queen,
                    $crate::piece::Kind::Bishop,
                    $crate::piece::Kind::Rook,
                    $crate::piece::Kind::Knight,
                ];
                for kind in kinds {
                    expected_moves.insert($crate::moves::UCIMove::Promotion { m: mv, k: kind });
                }
            }

            for found_move in &found_moves {
                assert!(expected_moves.contains(found_move));
            }
        };
    }

    #[test]
    fn pawn_unmoved_has_two_moves_when_not_blocked() {
        let board = &board::Board::try_from("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR").unwrap();
        let context = &context::Context::default();
        let squares = &["a3", "a4"];
        check_pawn!("a2" on board having context can be moved to squares);
    }

    #[test]
    fn pawn_unmoved_has_correct_number_of_moves_when_blocked() {
        // pawn on a2 blocked by other piece on a4 has 1 move
        let board =
            &board::Board::try_from("rnbqkbnr/pppppppp/8/8/n7/8/PPPPPPPP/RNBQKBNR").unwrap();
        let context = &context::Context::default();
        let squares = &["a3"];
        check_pawn!("a2" on board having context can be moved to squares);

        // pawn on a2 blocked by other piece on a3 has 0 moves
        let board =
            &board::Board::try_from("rnbqkbnr/pppppppp/8/8/8/n7/PPPPPPPP/RNBQKBNR").unwrap();
        let context = &context::Context::default();
        check_pawn!("a2" on board having context cannot be moved);
    }

    #[test]
    fn pawn_already_moved_can_only_move_one_forward() {
        // pawn has already made it's first move before
        let board =
            &board::Board::try_from("rnbqkbnr/ppp1pppp/3p4/8/4P3/8/PPPP1PPP/RNBQKBNR").unwrap();
        let context = &context::Context::default();
        let squares = &["e5"];
        check_pawn!("e4" on board having context can be moved to squares);
    }

    #[test]
    fn pawn_already_moved_has_no_moves_when_blocked() {
        // pawn has already made it's first move before
        let board =
            &board::Board::try_from("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR").unwrap();
        let context = &context::Context::default();
        check_pawn!("e4" on board having context cannot be moved);
    }

    #[test]
    fn pawn_white_on_promotion_square_has_noncapturing_promotion_moves() {
        let board = &board::Board::try_from("8/1P5k/8/8/8/8/8/6K1").unwrap();
        let context = &context::Context::default();
        let squares = &["b8"];
        check_pawn!("b7" on board having context can be promoted on squares);
    }

    #[test]
    fn pawn_white_on_promotion_square_has_capturing_promotion_moves() {
        let board = &board::Board::try_from("bn2b2k/P7/8/8/8/8/8/1K6").unwrap();
        let context = &context::Context::default();
        let squares = &["b8"];
        check_pawn!("a7" on board having context can be promoted on squares);
    }

    #[test]
    fn pawn_black_on_promotion_square_has_noncapturing_promotion_moves() {
        let board = &board::Board::try_from("8/7k/8/8/8/8/1p6/6K1").unwrap();
        let context = &context::Context::default();
        let squares = &["b1"];
        check_pawn!("b2" on board having context can be promoted on squares);
    }

    #[test]
    fn pawn_black_on_promotion_square_has_capturing_promotion_moves() {
        let board = &board::Board::try_from("7k/8/8/8/8/8/7p/1K4NB").unwrap();
        let context = &context::Context::default();
        let squares = &["g1"];
        check_pawn!("h2" on board having context can be promoted on squares);
    }

    #[test]
    fn pawn_white_can_attack_enemy_pieces() {
        let board = &board::Board::try_from("8/7k/8/8/1n1n4/2P5/8/6K1").unwrap();
        let context = &context::Context::default();
        let squares = &["b4", "c4", "d4"];
        check_pawn!("c3" on board having context can be moved to squares);
    }

    #[test]
    fn pawn_white_cannot_attack_own_pieces() {
        let board = &board::Board::try_from("8/7k/8/8/1R1R4/2P5/8/6K1").unwrap();
        let context = &context::Context::default();
        let squares = &["c4"];
        check_pawn!("c3" on board having context can be moved to squares);
    }

    #[test]
    fn pawn_white_on_file_a_attacks_enemy_pieces() {
        let board = &board::Board::try_from("8/7k/8/8/1p6/P6p/8/6K1").unwrap();
        let context = &context::Context::default();
        let squares = &["b4", "a4"];
        check_pawn!("a3" on board having context can be moved to squares);
    }

    #[test]
    fn pawn_white_on_file_a_cannot_attack_own_pieces() {
        let board = &board::Board::try_from("8/7k/8/8/RP6/P6p/8/6K1").unwrap();
        let context = &context::Context::default();
        check_pawn!("a3" on board having context cannot be moved);
    }

    #[test]
    fn pawn_white_on_file_h_attacks_enemy_pieces() {
        let board = &board::Board::try_from("8/7k/8/p7/6p1/7P/8/6K1").unwrap();
        let context = &context::Context::default();
        let squares = &["g4", "h4"];
        check_pawn!("h3" on board having context can be moved to squares);
    }

    #[test]
    fn pawn_white_on_file_h_cannot_attack_own_pieces() {
        let board = &board::Board::try_from("8/7k/8/p7/6PR/7P/8/6K1").unwrap();
        let context = &context::Context::default();
        check_pawn!("h3" on board having context cannot be moved);
    }

    #[test]
    fn pawn_black_can_attack_enemy_pieces() {
        let board = &board::Board::try_from("8/7k/3p4/2N1N3/8/8/8/6K1").unwrap();
        let context = &context::Context::default();
        let squares = &["c5", "d5", "e5"];
        check_pawn!("d6" on board having context can be moved to squares);
    }

    #[test]
    fn pawn_black_cannot_attack_own_pieces() {
        let board = &board::Board::try_from("8/7k/3p4/2n1n3/8/8/8/6K1").unwrap();
        let context = &context::Context::default();
        let squares = &["d5"];
        check_pawn!("d6" on board having context can be moved to squares);
    }

    #[test]
    fn pawn_black_on_file_a_attacks_enemy_pieces() {
        let board = &board::Board::try_from("8/7k/p7/1P6/7P/8/8/6K1").unwrap();
        let context = &context::Context::default();
        let squares = &["b5", "a5"];
        check_pawn!("a6" on board having context can be moved to squares);
    }

    #[test]
    fn pawn_black_on_file_a_cannot_attack_own_pieces() {
        let board = &board::Board::try_from("8/7k/p7/np6/8/8/8/6K1").unwrap();
        let context = &context::Context::default();
        check_pawn!("a6" on board having context cannot be moved);
    }

    #[test]
    fn pawn_black_on_file_h_attacks_enemy_pieces() {
        let board = &board::Board::try_from("8/7k/P6p/6P1/8/8/8/6K1").unwrap();
        let context = &context::Context::default();
        let squares = &["g5", "h5"];
        check_pawn!("h6" on board having context can be moved to squares);
    }

    #[test]
    fn pawn_black_on_file_h_cannot_attack_own_pieces() {
        let board = &board::Board::try_from("8/7k/7p/6pr/8/8/8/6K1").unwrap();
        let context = &context::Context::default();
        check_pawn!("h6" on board having context cannot be moved);
    }

    #[test]
    fn pawn_white_can_capture_enpassant() {
        let board =
            &board::Board::try_from("rnbqkbnr/ppp1p1pp/3p4/4Pp2/8/8/PPPP1PPP/RNBQKBNR").unwrap();
        // en-passant possible on the f6 square
        let context = &context::Context::try_from("w KQkq f6 0 3").unwrap();
        let squares = &["d6", "e6", "f6"];
        check_pawn!("e5" on board having context can be moved to squares);
    }

    #[test]
    fn pawn_black_can_capture_enpassant() {
        let board =
            &board::Board::try_from("rnbqkbnr/pp1ppppp/8/4P3/2pP4/8/PPP2PPP/RNBQKBNR").unwrap();
        // en-passant possible on the d3 square
        let context = &context::Context::try_from("w KQkq d3 0 3").unwrap();
        let squares = &["c3", "d3"];
        check_pawn!("c4" on board having context can be moved to squares);
    }
}
