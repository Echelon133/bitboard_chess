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

/// Precalculated attack patterns for knights (for each one of 64 squares).
/// Each square has a 64 bit number in which set bits represent which
/// squares are attacked by a knight from that square.
/// Since there is no differences between how white or black knights
/// operate, these attack patterns are universal.
///
/// Patterns are ordered left-to-right, bottom-to-top (from white's perspective).
/// This means that:
/// - pattern for a knight on "a1" has index 0
/// - pattern for a knight on "b1" has index 1
/// - pattern for a knight on "h8" has index 63
///
static KNIGHT_ATTACK_PATTERNS: [u64; 64] = [
    0x20400,
    0x50800,
    0xa1100,
    0x142200,
    0x284400,
    0x508800,
    0xa01000,
    0x402000,
    0x2040004,
    0x5080008,
    0xa110011,
    0x14220022,
    0x28440044,
    0x50880088,
    0xa0100010,
    0x40200020,
    0x204000402,
    0x508000805,
    0xa1100110a,
    0x1422002214,
    0x2844004428,
    0x5088008850,
    0xa0100010a0,
    0x4020002040,
    0x20400040200,
    0x50800080500,
    0xa1100110a00,
    0x142200221400,
    0x284400442800,
    0x508800885000,
    0xa0100010a000,
    0x402000204000,
    0x2040004020000,
    0x5080008050000,
    0xa1100110a0000,
    0x14220022140000,
    0x28440044280000,
    0x50880088500000,
    0xa0100010a00000,
    0x40200020400000,
    0x204000402000000,
    0x508000805000000,
    0xa1100110a000000,
    0x1422002214000000,
    0x2844004428000000,
    0x5088008850000000,
    0xa0100010a0000000,
    0x4020002040000000,
    0x400040200000000,
    0x800080500000000,
    0x1100110a00000000,
    0x2200221400000000,
    0x4400442800000000,
    0x8800885000000000,
    0x100010a000000000,
    0x2000204000000000,
    0x4020000000000,
    0x8050000000000,
    0x110a0000000000,
    0x22140000000000,
    0x44280000000000,
    0x88500000000000,
    0x10a00000000000,
    0x20400000000000,
];

/// Finds all pseudo-legal moves for the knight on the given square.
/// This function assumes that a piece that is placed on the given
/// square is actually a knight and does not check whether that's true.
///
/// This implementation uses precalculated attack patterns, so that
/// instead of calculating them on-the-fly every call, they can
/// simply be accessed using the square's index (which is consistent
/// with the order of attack patterns in KNIGHT_ATTACK_PATTERNS).
///
/// The only thing that needs to be done once there is an attack pattern
/// ready, is to bitwise AND that attack pattern with bitwise NOT of pieces that
/// have the same color as the knight for which we calculate moves.
/// That operations only leaves those bits on the attack pattern that represent
/// either empty squares or squares of the opponent.
///
fn find_knight_moves(
    piece_square: square::Square,
    white_taken: &bitboard::Bitboard,
    black_taken: &bitboard::Bitboard,
) -> Vec<moves::UCIMove> {
    let mut moves = Vec::with_capacity(4);

    // the invariant: piece_square must be a square that's set to 1 either
    // for white_taken or black_taken, so if it's set for white_taken, then it should be
    // impossible for it to be set to 1 for black_taken
    let own_pieces = match white_taken.is_set(piece_square) {
        true => *white_taken,
        false => *black_taken,
    };

    // retrieve a precalculated knight attack pattern and make a bitboard using
    // all bits of that pattern
    let index = piece_square.get_index();
    let attack_bitboard = bitboard::Bitboard::from(KNIGHT_ATTACK_PATTERNS[index]);
    // only attack squares where there are no pieces the same color as the knight
    let attack_bitboard = attack_bitboard & (!own_pieces);

    for attacked_square in attack_bitboard.iter() {
        let mv = moves::Move::new(piece_square, attacked_square);
        moves.push(moves::UCIMove::Regular { m: mv });
    }

    moves
}

/// Precalculated attack patterns for kings (for each one of 64 squares).
/// Each square has a 64 bit number in which set bits represent which
/// squares are attacked by a knight from that square.
///
/// Patterns are ordered left-to-right, bottom-to-top (from white's perspective).
/// This means that:
/// - pattern for a king on "a1" has index 0
/// - pattern for a king on "b1" has index 1
/// - pattern for a king on "h8" has index 63
///
static KING_ATTACK_PATTERNS: [u64; 64] = [
    0x302,
    0x705,
    0xe0a,
    0x1c14,
    0x3828,
    0x7050,
    0xe0a0,
    0xc040,
    0x30203,
    0x70507,
    0xe0a0e,
    0x1c141c,
    0x382838,
    0x705070,
    0xe0a0e0,
    0xc040c0,
    0x3020300,
    0x7050700,
    0xe0a0e00,
    0x1c141c00,
    0x38283800,
    0x70507000,
    0xe0a0e000,
    0xc040c000,
    0x302030000,
    0x705070000,
    0xe0a0e0000,
    0x1c141c0000,
    0x3828380000,
    0x7050700000,
    0xe0a0e00000,
    0xc040c00000,
    0x30203000000,
    0x70507000000,
    0xe0a0e000000,
    0x1c141c000000,
    0x382838000000,
    0x705070000000,
    0xe0a0e0000000,
    0xc040c0000000,
    0x3020300000000,
    0x7050700000000,
    0xe0a0e00000000,
    0x1c141c00000000,
    0x38283800000000,
    0x70507000000000,
    0xe0a0e000000000,
    0xc040c000000000,
    0x302030000000000,
    0x705070000000000,
    0xe0a0e0000000000,
    0x1c141c0000000000,
    0x3828380000000000,
    0x7050700000000000,
    0xe0a0e00000000000,
    0xc040c00000000000,
    0x203000000000000,
    0x507000000000000,
    0xa0e000000000000,
    0x141c000000000000,
    0x2838000000000000,
    0x5070000000000000,
    0xa0e0000000000000,
    0x40c0000000000000,
];

/// Finds all pseudo-legal moves for the king on the given square.
/// This function assumes that a piece that is placed on the given square
/// is actually a king and does not check whether that's true.
///
/// This implementation uses precalculated attack patterns, so that
/// instead of calculating them on-the-fly every call, they can
/// simply be accessed using the square's index (which is consistent
/// with the order of attack patterns in KING_ATTACK_PATTERNS).
///
fn find_king_moves(
    piece_square: square::Square,
    own_color: piece::Color,
    white_taken: &bitboard::Bitboard,
    black_taken: &bitboard::Bitboard,
    context: &context::Context,
) -> Vec<moves::UCIMove> {
    let mut moves = Vec::with_capacity(4);

    let own_pieces = match own_color {
        piece::Color::White => *white_taken,
        piece::Color::Black => *black_taken,
    };

    let all_taken = *white_taken | *black_taken;

    // retrieve a precalculated king attack pattern and make a bitboard using
    // all bits of that pattern
    let index = piece_square.get_index();
    let attack_bitboard = bitboard::Bitboard::from(KING_ATTACK_PATTERNS[index]);
    // only attack squares where there are no pieces with the same color as the king
    let attack_bitboard = attack_bitboard & (!own_pieces);

    for attacked_square in attack_bitboard.iter() {
        let mv = moves::Move::new(piece_square, attacked_square);
        moves.push(moves::UCIMove::Regular { m: mv });
    }

    // if castling is available, then it means that the king
    // is on its original square, therefore g1/g8 and c1/c8 target
    // squares can be calculated by adding/subtracting
    // 2 from the index of the king's square
    if context.can_castle(own_color, context::Side::Kingside) {
        // two squares between the king and the kingside rook need to be empty
        // to make castling possible
        let f_file_square = square::Square::from((index + 1) as u8);
        let g_file_square = square::Square::from((index + 2) as u8);

        if !all_taken.is_set(f_file_square) && !all_taken.is_set(g_file_square) {
            let mv = moves::Move::new(piece_square, g_file_square);
            moves.push(moves::UCIMove::Regular { m: mv });
        }
    }
    if context.can_castle(own_color, context::Side::Queenside) {
        // three squares between the king and the queenside rook need to be empty
        // to make castling possible
        let b_file_square = square::Square::from((index - 3) as u8);
        let c_file_square = square::Square::from((index - 2) as u8);
        let d_file_square = square::Square::from((index - 1) as u8);

        if !all_taken.is_set(b_file_square)
            && !all_taken.is_set(c_file_square)
            && !all_taken.is_set(d_file_square)
        {
            let mv = moves::Move::new(piece_square, c_file_square);
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

    macro_rules! check_knight {
        ($square:literal on $board:ident can be moved to $targets:ident) => {
            let (white, black) = $crate::movegen::tests::extract_squares_taken($board);
            let square = $crate::square::Square::try_from($square).unwrap();
            let found_moves = $crate::movegen::find_knight_moves(square, white, black);
            assert_eq!(found_moves.len(), $targets.len());
            let targets = $crate::movegen::tests::extract_targets(&found_moves);
            let expected_targets = notation_to_squares($targets);
            for target in expected_targets {
                assert!(targets.contains(&target));
            }
        };
        ($square:literal on $board:ident cannot be moved) => {
            let (white, black) = $crate::movegen::tests::extract_squares_taken($board);
            let square = $crate::square::Square::try_from($square).unwrap();
            let found_moves = $crate::movegen::find_knight_moves(square, white, black);
            assert_eq!(found_moves.len(), 0);
        };
    }

    macro_rules! check_king {
        (number of correct moves of $color:ident $square:ident on $board:ident is $num:expr) => {
            // in this case, context should by default not allow any castling
            let context = $crate::context::Context::try_from("w - - 0 1").unwrap();
            let (white, black) = $crate::movegen::tests::extract_squares_taken($board);
            let square = $crate::square::Square::try_from($square).unwrap();
            let found_moves =
                $crate::movegen::find_king_moves(square, $color, white, black, &context);
            assert_eq!(found_moves.len(), $num);

            // get (file, rank) indexes of the king's square
            let (file_i, rank_i) = (square.get_file().index(), square.get_rank().index());
            let (file_i, rank_i) = (file_i as i8, rank_i as i8);

            let targets = $crate::movegen::tests::extract_targets(&found_moves);

            // calculate differences between (file, rank) indexes of the king's square
            // and (file, rank) indexes of all squares that find_king_moves has found
            for target_sq in targets {
                let t_file_i = target_sq.get_file().index() as i8;
                let t_rank_i = target_sq.get_rank().index() as i8;

                // calculate absolute value of the difference between king's (file, rank) pair
                // and target square (file, rank) pair
                let (diff_file, diff_rank) = (file_i - t_file_i, rank_i - t_rank_i);
                let (diff_file, diff_rank) = (diff_file.abs(), diff_rank.abs());

                // sum of these differences should either be 1 or 2
                let sum_diff = diff_file + diff_rank;
                assert!(sum_diff == 1 || sum_diff == 2);
                // file diff and rank diff can be either 1 or 0
                assert!(diff_file == 0 || diff_file == 1);
                assert!(diff_rank == 0 || diff_rank == 1);
            }
        };
        ($color:ident can castle $castle:literal on $board:ident having $context:ident) => {
            let (white, black) = $crate::movegen::tests::extract_squares_taken($board);
            let (king_square, kingside_target, queenside_target) = match $color {
                $crate::piece::Color::White => ("e1", "g1", "c1"),
                $crate::piece::Color::Black => ("e8", "g8", "c8"),
            };

            let square = $crate::square::Square::try_from(king_square).unwrap();
            let found_moves =
                $crate::movegen::find_king_moves(square, $color, white, black, $context);

            let targets = $crate::movegen::tests::extract_targets(&found_moves);
            println!("{:?}", targets);

            let kside = $crate::square::Square::try_from(kingside_target).unwrap();
            let qside = $crate::square::Square::try_from(queenside_target).unwrap();
            match $castle {
                "kq" => {
                    assert!(targets.contains(&kside));
                    assert!(targets.contains(&qside));
                }
                "k" => {
                    assert!(targets.contains(&kside));
                    assert!(!targets.contains(&qside));
                }
                "q" => {
                    assert!(!targets.contains(&kside));
                    assert!(targets.contains(&qside));
                }
                "-" => {
                    assert!(!targets.contains(&kside));
                    assert!(!targets.contains(&qside));
                }
                _ => panic!("invalid symbol"),
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

    #[test]
    fn knight_on_edges_attacks_two_enemy_squares() {
        // correct for both black and white
        let squares_a1 = &["b3", "c2"];
        let squares_h1 = &["g3", "f2"];
        let squares_a8 = &["b6", "c7"];
        let squares_h8 = &["f7", "g6"];

        // white knights
        let white_board = &board::Board::try_from("N6N/8/8/8/8/8/8/N6N").unwrap();
        check_knight!("a1" on white_board can be moved to squares_a1);
        check_knight!("h1" on white_board can be moved to squares_h1);
        check_knight!("a8" on white_board can be moved to squares_a8);
        check_knight!("h8" on white_board can be moved to squares_h8);

        // black knights
        let black_board = &board::Board::try_from("n6n/8/8/8/8/8/8/n6n").unwrap();
        check_knight!("a1" on black_board can be moved to squares_a1);
        check_knight!("h1" on black_board can be moved to squares_h1);
        check_knight!("a8" on black_board can be moved to squares_a8);
        check_knight!("h8" on black_board can be moved to squares_h8);
    }

    #[test]
    fn knight_on_edges_cannot_attack_two_own_squares() {
        let white_board = &board::Board::try_from("N6N/2B2B2/1B4B1/8/8/1B4B1/2B2B2/N6N").unwrap();
        check_knight!("a1" on white_board cannot be moved);
        check_knight!("h1" on white_board cannot be moved);
        check_knight!("a8" on white_board cannot be moved);
        check_knight!("h8" on white_board cannot be moved);

        let black_board = &board::Board::try_from("n6n/2b2b2/1b4b1/8/8/1b4b1/2b2b2/n6n").unwrap();
        check_knight!("a1" on black_board cannot be moved);
        check_knight!("h1" on black_board cannot be moved);
        check_knight!("a8" on black_board cannot be moved);
        check_knight!("h8" on black_board cannot be moved);
    }

    #[test]
    fn knight_near_edges_attacks_four_enemy_squares() {
        // correct for both black and white
        let squares_b2 = &["a4", "c4", "d3", "d1"];
        let squares_b7 = &["a5", "c5", "d6", "d8"];
        let squares_g2 = &["e1", "e3", "f4", "h4"];
        let squares_g7 = &["e8", "e6", "f5", "h5"];

        // white knights
        let white_board = &board::Board::try_from("8/1N4N1/8/8/8/8/1N4N1/8").unwrap();
        check_knight!("b2" on white_board can be moved to squares_b2);
        check_knight!("b7" on white_board can be moved to squares_b7);
        check_knight!("g2" on white_board can be moved to squares_g2);
        check_knight!("g7" on white_board can be moved to squares_g7);

        // black knights
        let black_board = &board::Board::try_from("8/1n4n1/8/8/8/8/1n4n1/8").unwrap();
        check_knight!("b2" on black_board can be moved to squares_b2);
        check_knight!("b7" on black_board can be moved to squares_b7);
        check_knight!("g2" on black_board can be moved to squares_g2);
        check_knight!("g7" on black_board can be moved to squares_g7);
    }

    #[test]
    fn knight_near_edges_cannot_attack_four_own_squares() {
        // white knights
        let white_board =
            &board::Board::try_from("3BB3/1N4N1/3BB3/B1B2B1B/B1B2B1B/3BB3/1N4N1/3BB3").unwrap();
        check_knight!("b2" on white_board cannot be moved);
        check_knight!("b7" on white_board cannot be moved);
        check_knight!("g2" on white_board cannot be moved);
        check_knight!("g7" on white_board cannot be moved);

        // black knights
        let black_board =
            &board::Board::try_from("3bb3/1n4n1/3bb3/b1b2b1b/b1b2b1b/3bb3/1n4n1/3bb3").unwrap();
        check_knight!("b2" on black_board cannot be moved);
        check_knight!("b7" on black_board cannot be moved);
        check_knight!("g2" on black_board cannot be moved);
        check_knight!("g7" on black_board cannot be moved);
    }

    #[test]
    fn knight_near_edges_attacks_six_enemy_squares() {
        // common for both black and white
        let squares_c2 = &["a1", "a3", "b4", "d4", "e3", "e1"];
        let squares_c7 = &["a8", "a6", "b5", "d5", "e6", "e8"];
        let squares_f2 = &["d1", "d3", "e4", "g4", "h1", "h3"];
        let squares_f7 = &["d8", "d6", "e5", "g5", "h6", "h8"];

        // white knight
        let white_board = &board::Board::try_from("8/2N2N2/8/8/8/8/2N2N2/8").unwrap();
        check_knight!("c2" on white_board can be moved to squares_c2);
        check_knight!("c7" on white_board can be moved to squares_c7);
        check_knight!("f2" on white_board can be moved to squares_f2);
        check_knight!("f7" on white_board can be moved to squares_f7);

        // black knight
        let black_board = &board::Board::try_from("8/2n2n2/8/8/8/8/2n2n2/8").unwrap();
        check_knight!("c2" on black_board can be moved to squares_c2);
        check_knight!("c7" on black_board can be moved to squares_c7);
        check_knight!("f2" on black_board can be moved to squares_f2);
        check_knight!("f7" on black_board can be moved to squares_f7);
    }

    #[test]
    fn knight_near_edges_cannot_attack_six_own_squares() {
        // white knight
        let white_board =
            &board::Board::try_from("R2RR2R/2N2N2/R2RR2R/1R1RR1R1/1R1RR1R1/R2RR2R/2N2N2/R2RR2R")
                .unwrap();
        check_knight!("c2" on white_board cannot be moved);
        check_knight!("c7" on white_board cannot be moved);
        check_knight!("f2" on white_board cannot be moved);
        check_knight!("f7" on white_board cannot be moved);

        // black knight
        let black_board =
            &board::Board::try_from("r2rr2r/2n2n2/r2rr2r/1r1rr1r1/1r1rr1r1/r2rr2r/2n2n2/r2rr2r")
                .unwrap();
        check_knight!("c2" on black_board cannot be moved);
        check_knight!("c7" on black_board cannot be moved);
        check_knight!("f2" on black_board cannot be moved);
        check_knight!("f7" on black_board cannot be moved);
    }

    #[test]
    fn knight_in_middle_attacks_eight_enemy_squares() {
        // common for both black and white
        let squares_c3 = &["a2", "a4", "b1", "b5", "d5", "d1", "e2", "e4"];
        let squares_c6 = &["a5", "a7", "b4", "b8", "d4", "d8", "e5", "e7"];
        let squares_f3 = &["d2", "d4", "e1", "e5", "g1", "g5", "h2", "h4"];
        let squares_f6 = &["d5", "d7", "e4", "e8", "g4", "g8", "h5", "h7"];

        // white knights
        let white_board = &board::Board::try_from("8/8/2N2N2/8/8/2N2N2/8/8").unwrap();
        check_knight!("c3" on white_board can be moved to squares_c3);
        check_knight!("c6" on white_board can be moved to squares_c6);
        check_knight!("f3" on white_board can be moved to squares_f3);
        check_knight!("f6" on white_board can be moved to squares_f6);

        // black knights
        let black_board = &board::Board::try_from("8/8/2n2n2/8/8/2n2n2/8/8").unwrap();
        check_knight!("c3" on black_board can be moved to squares_c3);
        check_knight!("c6" on black_board can be moved to squares_c6);
        check_knight!("f3" on black_board can be moved to squares_f3);
        check_knight!("f6" on black_board can be moved to squares_f6);
    }

    #[test]
    fn knight_in_middle_cannot_attack_eight_own_squares() {
        // white knights
        let white_board = &board::Board::try_from(
            "1R1RR1R1/R2RR2R/2N2N2/RR1RR1RR/RR1RR1RR/2N2N2/R2RR2R/1R1RR1R1",
        )
        .unwrap();
        check_knight!("c3" on white_board cannot be moved);
        check_knight!("c6" on white_board cannot be moved);
        check_knight!("f3" on white_board cannot be moved);
        check_knight!("f6" on white_board cannot be moved);

        // black knights
        let black_board = &board::Board::try_from(
            "1r1rr1r1/r2rr2r/2n2n2/rr1rr1rr/rr1rr1rr/2n2n2/r2rr2r/1r1rr1r1",
        )
        .unwrap();
        check_knight!("c3" on black_board cannot be moved);
        check_knight!("c6" on black_board cannot be moved);
        check_knight!("f3" on black_board cannot be moved);
        check_knight!("f6" on black_board cannot be moved);
    }

    #[test]
    fn king_all_three_square_attacks_work() {
        // only places on the board where kings attack only 3 squares
        // are the corners of the board

        let board = &board::Board::try_from("K6K/8/8/8/8/8/8/K6K").unwrap();
        let white = piece::Color::White;
        for king in ["a1", "a8", "h1", "h8"] {
            check_king!(number of correct moves of white king on board is 3);
        }
    }

    #[test]
    fn king_all_three_square_attacks_blocked() {
        // only places on the board where kings attack only 3 squares
        // are the corners of the board

        let board = &board::Board::try_from("KN4NK/NN4NN/8/8/8/8/NN4NN/KN4NK").unwrap();
        let white = piece::Color::White;
        for blocked_king in ["a1", "a8", "h1", "h8"] {
            check_king!(number of correct moves of white blocked_king on board is 0);
        }
    }

    #[test]
    fn king_all_five_square_attacks_work() {
        // places where kings attack exactly 5 squares:
        // - files b-g on the 1st and 8th rank
        // - ranks 2-7 on the a and h file

        // use the fact that the move finding function does not check whether the
        // king is actually on the board where it's told it is (the invariant),
        // to simplify setup of the test board by ignoring that invariant
        // (which should NOT be done outside of tests)
        let board = &board::Board::try_from("8/8/8/8/8/8/8/8").unwrap();

        // crate all squares from which kings attack exactly 5 squares
        let mut all_squares = vec![];
        // variant 1
        for file in 'b'..='g' {
            for rank in ['1', '8'] {
                let square_s = format!("{}{}", file, rank);
                all_squares.push(square_s);
            }
        }
        // variant 2
        for rank in '2'..='7' {
            for file in ['a', 'h'] {
                let square_s = format!("{}{}", file, rank);
                all_squares.push(square_s);
            }
        }

        let white = piece::Color::White;
        for king in all_squares {
            let king = king.as_str();
            check_king!(number of correct moves of white king on board is 5);
        }
    }

    #[test]
    fn king_all_five_square_attacks_blocked() {
        // places where kings attack exactly 5 squares:
        // - files b-g on the 1st rank and 8th rank
        // - ranks 2-7 on the a and h file

        // use the fact that the move finding function does not check whether the
        // king is actually on the board where it's told it is (the invariant),
        // to simplify setup of the test board by ignoring that invariant
        // (which should NOT be done outside of tests)
        let board = &board::Board::try_from(
            "RRRRRRRR/RRRRRRRR/RRRRRRRR/RRRRRRRR/RRRRRRRR/RRRRRRRR/RRRRRRRR/RRRRRRRR",
        )
        .unwrap();

        // crate all squares from which kings attack exactly 5 squares
        let mut all_squares = vec![];
        // variant 1
        for file in 'b'..='g' {
            for rank in ['1', '8'] {
                let square_s = format!("{}{}", file, rank);
                all_squares.push(square_s);
            }
        }
        // variant 2
        for rank in '2'..='7' {
            for file in ['a', 'h'] {
                let square_s = format!("{}{}", file, rank);
                all_squares.push(square_s);
            }
        }

        let white = piece::Color::White;
        for blocked_king in all_squares {
            let blocked_king = blocked_king.as_str();
            check_king!(number of correct moves of white blocked_king on board is 0);
        }
    }

    #[test]
    fn king_all_eight_square_attacks_work() {
        // places where kings attack exactly 8 squares:
        // - ranks b-h and files 2-7

        // use the fact that the move finding function does not check whether the
        // king is actually on the board where it's told it is (the invariant),
        // to simplify setup of the test board by ignoring that invariant
        // (which should NOT be done outside of tests)
        let board = &board::Board::try_from("8/8/8/8/8/8/8/8").unwrap();

        // crate all squares from which kings attack exactly 8 squares
        let mut all_squares = vec![];
        for file in 'b'..='g' {
            for rank in '2'..='7' {
                let square_s = format!("{}{}", file, rank);
                all_squares.push(square_s);
            }
        }

        let white = piece::Color::White;
        for king in all_squares {
            let king = king.as_str();
            check_king!(number of correct moves of white king on board is 8);
        }
    }

    #[test]
    fn king_all_eight_square_attacks_blocked() {
        // places where kings attack exactly 8 squares:
        // - ranks b-h and files 2-7

        // use the fact that the move finding function does not check whether the
        // king is actually on the board where it's told it is (the invariant),
        // to simplify setup of the test board by ignoring that invariant
        // (which should NOT be done outside of tests)
        let board = &board::Board::try_from(
            "RRRRRRRR/RRRRRRRR/RRRRRRRR/RRRRRRRR/RRRRRRRR/RRRRRRRR/RRRRRRRR/RRRRRRRR",
        )
        .unwrap();

        // crate all squares from which kings attack exactly 8 squares
        let mut all_squares = vec![];

        for file in 'b'..='g' {
            for rank in '2'..='7' {
                let square_s = format!("{}{}", file, rank);
                all_squares.push(square_s);
            }
        }

        let white = piece::Color::White;
        for blocked_king in all_squares {
            let blocked_king = blocked_king.as_str();
            check_king!(number of correct moves of white blocked_king on board is 0);
        }
    }

    #[test]
    fn king_castle_kingside_both_colors() {
        let board =
            &board::Board::try_from("rnbqk2r/ppppp2p/5npb/5p2/5P2/5NPB/PPPPP2P/RNBQK2R").unwrap();
        let context = &context::Context::try_from("w KQkq - 0 1").unwrap();

        let white = piece::Color::White;
        let black = piece::Color::Black;
        check_king!(white can castle "k" on board having context);
        check_king!(black can castle "k" on board having context);
    }

    #[test]
    fn king_castle_kingside_both_colors_path_obscured() {
        // f1 and f8 obscured
        let board1 =
            &board::Board::try_from("rnbqkb1r/ppppp2p/5np1/5p2/5P2/5NP1/PPPPP2P/RNBQKB1R").unwrap();
        // g1 and g8 obscured
        let board2 =
            &board::Board::try_from("rnbqk1nr/ppppp2p/6pb/5p2/5P2/6PB/PPPPP2P/RNBQK1NR").unwrap();

        let context = &context::Context::try_from("w KQkq - 0 1").unwrap();

        let white = piece::Color::White;
        let black = piece::Color::Black;

        check_king!(white can castle "-" on board1 having context);
        check_king!(black can castle "-" on board1 having context);

        check_king!(white can castle "-" on board2 having context);
        check_king!(black can castle "-" on board2 having context);
    }

    #[test]
    fn king_castle_queenside_both_colors() {
        let board =
            &board::Board::try_from("r3kbnr/ppp1pppp/2nq4/3pb3/3P4/2NQB3/PPP1PPPP/R3KBNR").unwrap();
        let context = &context::Context::try_from("w KQkq - 0 1").unwrap();

        let white = piece::Color::White;
        let black = piece::Color::Black;
        check_king!(white can castle "q" on board having context);
        check_king!(black can castle "q" on board having context);
    }

    #[test]
    fn king_castle_queenside_both_colors_path_obscured() {
        // b1 and b8 obscured
        let board1 =
            &board::Board::try_from("rn2kbnr/ppp1pppp/3q4/3pb3/3P4/3QB3/PPP1PPPP/RN2KBNR").unwrap();
        // c1 and c8 obscured
        let board2 =
            &board::Board::try_from("r1b1kbnr/ppp1pppp/2n1q3/3p4/3P4/2NQ4/PPP1PPPP/R1B1KBNR")
                .unwrap();

        // d1 and d8 obscured
        let board3 =
            &board::Board::try_from("r2qkbnr/ppp1pppp/2n1b3/3p4/3P4/2N1B3/PPP1PPPP/R2QKBNR")
                .unwrap();

        let context = &context::Context::try_from("w KQkq - 0 1").unwrap();

        let white = piece::Color::White;
        let black = piece::Color::Black;

        check_king!(white can castle "-" on board1 having context);
        check_king!(black can castle "-" on board1 having context);

        check_king!(white can castle "-" on board2 having context);
        check_king!(black can castle "-" on board2 having context);

        check_king!(white can castle "-" on board3 having context);
        check_king!(black can castle "-" on board3 having context);
    }

    #[test]
    fn king_castle_bothside_both_colors() {
        let board =
            &board::Board::try_from("r3k2r/ppp1ppbp/2nqb1pn/3p4/3P4/2NQBNP1/PPP1PPBP/R3K2R")
                .unwrap();
        let context = &context::Context::try_from("w KQkq - 0 1").unwrap();

        let white = piece::Color::White;
        let black = piece::Color::Black;
        check_king!(white can castle "kq" on board having context);
        check_king!(black can castle "kq" on board having context);
    }
}
