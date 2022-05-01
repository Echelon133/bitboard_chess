use crate::bitboard;
use crate::context;
use crate::movegen_constants;
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
pub fn find_pawn_moves(
    piece_square: square::Square,
    color: piece::Color,
    white_taken: &bitboard::Bitboard,
    black_taken: &bitboard::Bitboard,
    context: &context::Context,
) -> Vec<moves::UCIMove> {
    let mut moves = Vec::with_capacity(4);

    let all_taken = *white_taken | *black_taken;

    let mut can_move_once = false;
    let pawn_rank = piece_square.get_rank();
    let square_index = piece_square.get_index() as i8;

    // only non-capturing moves
    // shift_one - bitboard where all pieces got shifted a rank towards the pawn
    // shift_two - bitboard where all pieces got shifted two ranks towards the pawn
    // square_dist - how many indexes away the square above (for white) and below (for black) is
    // start_rank - rank on which the pawn starts and can potentially move two squares at once
    // promotion_rank - rank on which the pawn can promote on its next move
    let (shift_one, shift_two, square_dist, start_rank, promotion_rank) = match color {
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
        match color {
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
pub fn find_knight_moves(
    piece_square: square::Square,
    color: piece::Color,
    white_taken: &bitboard::Bitboard,
    black_taken: &bitboard::Bitboard,
) -> Vec<moves::UCIMove> {
    let mut moves = Vec::with_capacity(4);

    let own_pieces = match color {
        piece::Color::White => *white_taken,
        piece::Color::Black => *black_taken,
    };

    // retrieve a precalculated knight attack pattern and make a bitboard using
    // all bits of that pattern
    let index = piece_square.get_index();
    let attack_bitboard =
        bitboard::Bitboard::from(movegen_constants::KNIGHT_ATTACK_PATTERNS[index]);
    // only attack squares where there are no pieces the same color as the knight
    let attack_bitboard = attack_bitboard & (!own_pieces);

    for attacked_square in attack_bitboard.iter() {
        let mv = moves::Move::new(piece_square, attacked_square);
        moves.push(moves::UCIMove::Regular { m: mv });
    }

    moves
}

/// Finds all pseudo-legal moves for the king on the given square.
/// This function assumes that a piece that is placed on the given square
/// is actually a king and does not check whether that's true.
///
/// This implementation uses precalculated attack patterns, so that
/// instead of calculating them on-the-fly every call, they can
/// simply be accessed using the square's index (which is consistent
/// with the order of attack patterns in KING_ATTACK_PATTERNS).
///
pub fn find_king_moves(
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
    let attack_bitboard = bitboard::Bitboard::from(movegen_constants::KING_ATTACK_PATTERNS[index]);
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

/// Generates code that handles the creation of attack bitboards for rays that are positive
/// (i.e. each next square on the ray's trajectory has a bigger index than the previous
/// square on that ray's path).
///
/// There has to be a distinction between positive and negative rays, because
/// during the processing of attack bitboards, one of the steps requires finding
/// the first bit in a bitboard that's set to 1 (that bit represents
/// the first occupied square that's on the attacking ray's path).
///
/// The direction of the bitscan has to be the same as the direction of the attacking ray
/// (so positive rays require bitscan start from the least significant bit, and
/// negative rays need bitscans to start from the most significant bit).
/// Having the correct direction of the bitscan is important, because incorect
/// direction of the scan will not return the index of the square occupied by the first
/// blocker on the path, but instead will return the index of the last occupied square
/// that's attacked by the ray.
///
/// # Example - finding the first square that blocks the ray's path
///
/// The bitboard below (in ASCII representation) is the result of finding common bits between:
/// - a bitboard that represents all squares that are occupied on the board
/// - a bitboard that represents an attack towards the north-west side of the board
///
/// ```
/// 10000000
/// 01000000
/// 00000000
/// 00000000
/// 00000000
/// 00000000
/// 00000000
/// 00000000
/// ```
///
/// In this case, since the attack comes from squares with lower indexes towards squares
/// with higher indexes, bitscan-forward has to be used.
/// The result is the index of the b7 square (which is in fact the square containing the
/// blocking piece).
///
macro_rules! positive_ray_attack {
    ($rays:ident, $own_pieces:ident, $all_taken:ident, $index:expr) => {{
        // see: https://www.chessprogramming.org/Classical_Approach#Conditional
        let mut attack = $crate::bitboard::Bitboard::from($rays[$index]);
        let blocker = attack & $all_taken;
        if blocker.count_set() != 0 {
            let blocker_index = blocker.bitscan_forward() as usize;
            let blocker_ray = $crate::bitboard::Bitboard::from($rays[blocker_index]);
            attack = attack ^ blocker_ray;
            // if the blocker piece and the attacking piece have the same color,
            // do not attack the blocking piece
            let blocker_square = $crate::square::Square::from(blocker_index as u8);
            if $own_pieces.is_set(blocker_square) {
                attack.clear(blocker_square);
            }
        }
        attack
    }};
}

/// Generates code that handles the creation of attack bitboards for rays that are negative
/// (i.e. each next square on the ray's trajectory has a smaller index than the previous
/// square on that ray's path).
///
/// There has to be a distinction between positive and negative rays, because
/// during the processing of attack bitboards, one of the steps requires finding
/// the first bit in a bitboard that's set to 1 (that bit represents
/// the first occupied square that's on the attacking ray's path).
///
/// The direction of the bitscan has to be the same as the direction of the attacking ray
/// (so positive rays require bitscan start from the least significant bit, and
/// negative rays need bitscans to start from the most significant bit).
/// Having the correct direction of the bitscan is important, because incorect
/// direction of the scan will not return the index of the square occupied by the first
/// blocker on the path, but instead will return the index of the last occupied square
/// that's attacked by the ray.
///
/// # Example - finding the first square that blocks the ray's path
///
/// The bitboard below (in ASCII representation) is the result of finding common bits between:
/// - a bitboard that represents all squares that are occupied on the board
/// - a bitboard that represents an attack towards the south-east side of the board
///
/// ```
/// 00000000
/// 00000000
/// 00000000
/// 00000000
/// 00000000
/// 00000100
/// 00000000
/// 00000001
/// ```
///
/// In this case, since the attack comes from squares with higher indexes towards squares
/// with lower indexes, bitscan-reverse has to be used.
/// The result is the index of the f3 square (which is in fact the square containing the
/// blocking piece).
///
macro_rules! negative_ray_attack {
    ($rays:ident, $own_pieces:ident, $all_taken:ident, $index:expr) => {{
        // see: https://www.chessprogramming.org/Classical_Approach#Conditional_2
        let mut attack = $crate::bitboard::Bitboard::from($rays[$index]);
        let blocker = attack & $all_taken;
        if blocker.count_set() != 0 {
            let blocker_index = blocker.bitscan_reverse() as usize;
            let blocker_ray = $crate::bitboard::Bitboard::from($rays[blocker_index]);
            attack = attack ^ blocker_ray;
            // if the blocker piece and the attacking piece have the same color,
            // do not attack the blocking piece
            let blocker_square = $crate::square::Square::from(blocker_index as u8);
            if $own_pieces.is_set(blocker_square) {
                attack.clear(blocker_square);
            }
        }
        attack
    }};
}

/// Finds all pseudo-legal moves for a sliding piece that only moves
/// on its file or rank.
/// This can be either a rook or a queen.
///
/// [How to calculate for positive rays](https://www.chessprogramming.org/Classical_Approach#Conditional)
///
/// [How to calculate for negative rays](https://www.chessprogramming.org/Classical_Approach#Conditional_2)
///
fn find_file_rank_moves(
    piece_square: square::Square,
    color: piece::Color,
    white_taken: &bitboard::Bitboard,
    black_taken: &bitboard::Bitboard,
    moves: &mut Vec<moves::UCIMove>,
) {
    let own_pieces = match color {
        piece::Color::White => white_taken,
        piece::Color::Black => black_taken,
    };

    let all_taken = *white_taken | *black_taken;
    let index = piece_square.get_index();

    let north_attack_rays = movegen_constants::NORTH_ATTACK_RAYS;
    let south_attack_rays = movegen_constants::SOUTH_ATTACK_RAYS;
    let east_attack_rays = movegen_constants::EAST_ATTACK_RAYS;
    let west_attack_rays = movegen_constants::WEST_ATTACK_RAYS;

    let north_attack = positive_ray_attack!(north_attack_rays, own_pieces, all_taken, index);
    let south_attack = negative_ray_attack!(south_attack_rays, own_pieces, all_taken, index);
    let east_attack = positive_ray_attack!(east_attack_rays, own_pieces, all_taken, index);
    let west_attack = negative_ray_attack!(west_attack_rays, own_pieces, all_taken, index);

    // sum all attacked squares from north, south, east and west
    let all_attacks = north_attack | south_attack | east_attack | west_attack;
    for target_square in all_attacks.iter() {
        let mv = moves::Move::new(piece_square, target_square);
        moves.push(moves::UCIMove::Regular { m: mv });
    }
}

/// Finds all pseudo-legal moves for the rook on the given square.
/// This function assumes that a piece that is placed on the given square
/// is actually a rook. It does not check whether that is true,
/// so incorrect call to this function will yield invalid moves.
///
pub fn find_rook_moves(
    piece_square: square::Square,
    color: piece::Color,
    white_taken: &bitboard::Bitboard,
    black_taken: &bitboard::Bitboard,
) -> Vec<moves::UCIMove> {
    let mut moves = Vec::with_capacity(4);
    find_file_rank_moves(piece_square, color, white_taken, black_taken, &mut moves);
    moves
}

/// Finds all pseudo-legal moves for a sliding piece that moves
/// diagonally.
/// This can be either a bishop or a queen.
///
fn find_diagonal_moves(
    piece_square: square::Square,
    color: piece::Color,
    white_taken: &bitboard::Bitboard,
    black_taken: &bitboard::Bitboard,
    moves: &mut Vec<moves::UCIMove>,
) {
    let own_pieces = match color {
        piece::Color::White => white_taken,
        piece::Color::Black => black_taken,
    };

    let all_taken = *white_taken | *black_taken;
    let index = piece_square.get_index();

    let ne_attack_rays = movegen_constants::NORTHEAST_ATTACK_RAYS;
    let se_attack_rays = movegen_constants::SOUTHEAST_ATTACK_RAYS;
    let nw_attack_rays = movegen_constants::NORTHWEST_ATTACK_RAYS;
    let sw_attack_rays = movegen_constants::SOUTHWEST_ATTACK_RAYS;

    let ne_attack = positive_ray_attack!(ne_attack_rays, own_pieces, all_taken, index);
    let se_attack = negative_ray_attack!(se_attack_rays, own_pieces, all_taken, index);
    let nw_attack = positive_ray_attack!(nw_attack_rays, own_pieces, all_taken, index);
    let sw_attack = negative_ray_attack!(sw_attack_rays, own_pieces, all_taken, index);

    // sum all attacked squares from north-east, north-west, south-east and south-west
    let all_attacks = nw_attack | ne_attack | sw_attack | se_attack;
    for target_square in all_attacks.iter() {
        let mv = moves::Move::new(piece_square, target_square);
        moves.push(moves::UCIMove::Regular { m: mv });
    }
}

/// Finds all pseudo-legal moves for the bishop on the given square.
/// This function assumes that a piece that is placed on the given square
/// is actually a bishop. It does not check whether that is true,
/// so incorrect call to this function will yield invalid moves.
///
pub fn find_bishop_moves(
    piece_square: square::Square,
    color: piece::Color,
    white_taken: &bitboard::Bitboard,
    black_taken: &bitboard::Bitboard,
) -> Vec<moves::UCIMove> {
    let mut moves = Vec::with_capacity(4);
    find_diagonal_moves(piece_square, color, white_taken, black_taken, &mut moves);
    moves
}

/// Finds all pseudo-legal moves for the queen on the given square.
/// This function assumes that a piece that is placed on the given square
/// is actually a queen. It does not check whether that is true,
/// so incorrect call to this function will yield invalid moves.
///
pub fn find_queen_moves(
    piece_square: square::Square,
    color: piece::Color,
    white_taken: &bitboard::Bitboard,
    black_taken: &bitboard::Bitboard,
) -> Vec<moves::UCIMove> {
    let mut moves = Vec::with_capacity(4);
    find_diagonal_moves(piece_square, color, white_taken, black_taken, &mut moves);
    find_file_rank_moves(piece_square, color, white_taken, black_taken, &mut moves);
    moves
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::bitboard;
    use crate::board;
    use crate::context;
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
        ($color:ident $square:literal on $board:ident having $context:ident can be moved to $targets:ident) => {
            let (white_taken, black_taken) = $crate::movegen::tests::extract_squares_taken($board);
            let square = $crate::square::Square::try_from($square).unwrap();
            let found_moves =
                $crate::movegen::find_pawn_moves(square, $color, white_taken, black_taken, $context);
            assert_eq!(found_moves.len(), $targets.len());
            let targets = $crate::movegen::tests::extract_targets(&found_moves);
            let expected_targets = $crate::movegen::tests::notation_to_squares($targets);
            for target in expected_targets {
                assert!(targets.contains(&target));
            }
        };
        ($color:ident $square:literal on $board:ident having $context:ident cannot be moved) => {
            let (white_taken, black_taken) = $crate::movegen::tests::extract_squares_taken($board);
            let square = $crate::square::Square::try_from($square).unwrap();
            let found_moves =
                $crate::movegen::find_pawn_moves(square, $color, white_taken, black_taken, $context);
            assert_eq!(found_moves.len(), 0);
        };
        ($color:ident $square:literal on $board:ident having $context:ident can be promoted on $targets:ident) => {
            let (white_taken, black_taken) = extract_squares_taken($board);
            let square = $crate::square::Square::try_from($square).unwrap();
            let found_moves =
                $crate::movegen::find_pawn_moves(square, $color, white_taken, black_taken, $context);
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
        (number of correct moves of $color:ident $square:ident on $board:ident is $num:expr) => {
            let (white, black) = $crate::movegen::tests::extract_squares_taken($board);
            let square = $crate::square::Square::try_from($square).unwrap();
            let found_moves = $crate::movegen::find_knight_moves(square, $color, white, black);
            assert_eq!(found_moves.len(), $num);

            // get (file, rank) indexes of the knight's square
            let (file_i, rank_i) = (square.get_file().index(), square.get_rank().index());
            let (file_i, rank_i) = (file_i as i8, rank_i as i8);

            let targets = $crate::movegen::tests::extract_targets(&found_moves);

            // calculate differences between (file, rank) indexes of the knight's square
            // and (file, rank) indexes of all squares that find_knight_moves has found
            for target_sq in targets {
                let t_file_i = target_sq.get_file().index() as i8;
                let t_rank_i = target_sq.get_rank().index() as i8;

                // calculate absolute value of the difference between knight's (file, rank) pair
                // and target square (file, rank) pair
                let (diff_file, diff_rank) = (file_i - t_file_i, rank_i - t_rank_i);
                let (diff_file, diff_rank) = (diff_file.abs(), diff_rank.abs());

                // sum of these differences should ALWAYS be 3
                assert!(diff_file + diff_rank == 3);
                // file diff and rank diff can be either 2 or 1
                assert!(diff_file == 2 || diff_file == 1);
                assert!(diff_rank == 2 || diff_rank == 1);
            }
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

    macro_rules! check_rook {
        (number of correct moves of $color:ident $square:ident on $board:ident is $num:expr) => {
            let (white, black) = $crate::movegen::tests::extract_squares_taken($board);
            let square = $crate::square::Square::try_from($square).unwrap();
            let found_moves = $crate::movegen::find_rook_moves(square, $color, white, black);
            assert_eq!(found_moves.len(), $num);

            // get (file, rank) indexes of the rook's square
            let (file_i, rank_i) = (square.get_file().index(), square.get_rank().index());
            let (file_i, rank_i) = (file_i as i8, rank_i as i8);

            let targets = $crate::movegen::tests::extract_targets(&found_moves);

            // calculate differences between (file, rank) indexes of the rook's square
            // and (file, rank) indexes of all squares that find_rook_moves has found
            for target_sq in targets {
                let t_file_i = target_sq.get_file().index() as i8;
                let t_rank_i = target_sq.get_rank().index() as i8;

                // calculate absolute value of the difference between rook's (file, rank) pair
                // and target square (file, rank) pair
                let (diff_file, diff_rank) = (file_i - t_file_i, rank_i - t_rank_i);

                // if diff_file is non zero, then diff_rank is zero
                // if diff_file is zero, then diff_rank is non zero
                assert!((diff_file != 0 && diff_rank == 0) || (diff_file == 0 && diff_rank != 0));
            }
        };
        ($color:ident $square:literal on $board:ident can be moved to $targets:ident) => {
            let (white_taken, black_taken) = $crate::movegen::tests::extract_squares_taken($board);
            let square = $crate::square::Square::try_from($square).unwrap();
            let found_moves =
                $crate::movegen::find_rook_moves(square, $color, white_taken, black_taken);
            assert_eq!(found_moves.len(), $targets.len());
            let targets = $crate::movegen::tests::extract_targets(&found_moves);
            let expected_targets = $crate::movegen::tests::notation_to_squares($targets);
            for target in expected_targets {
                assert!(targets.contains(&target));
            }
        };
    }

    macro_rules! check_bishop {
        (number of correct moves of $color:ident $square:ident on $board:ident is $num:expr) => {
            let (white, black) = $crate::movegen::tests::extract_squares_taken($board);
            let square = $crate::square::Square::try_from($square).unwrap();
            let found_moves = $crate::movegen::find_bishop_moves(square, $color, white, black);
            assert_eq!(found_moves.len(), $num);

            // get (file, rank) indexes of the rook's square
            let (file_i, rank_i) = (square.get_file().index(), square.get_rank().index());
            let (file_i, rank_i) = (file_i as i8, rank_i as i8);

            let targets = $crate::movegen::tests::extract_targets(&found_moves);

            // calculate differences between (file, rank) indexes of the bishop's square
            // and (file, rank) indexes of all squares that find_bishop_moves has found
            for target_sq in targets {
                let t_file_i = target_sq.get_file().index() as i8;
                let t_rank_i = target_sq.get_rank().index() as i8;

                // calculate absolute value of the difference between bishop's (file, rank) pair
                // and target square (file, rank) pair
                let (diff_file, diff_rank) = (file_i - t_file_i, rank_i - t_rank_i);
                let (diff_file, diff_rank) = (diff_file.abs(), diff_rank.abs());

                // diff_file == diff_rank
                assert!(diff_file != 0 && diff_rank != 0);
                assert_eq!(diff_file, diff_rank);
            }
        };
        ($color:ident $square:literal on $board:ident can be moved to $targets:ident) => {
            let (white_taken, black_taken) = $crate::movegen::tests::extract_squares_taken($board);
            let square = $crate::square::Square::try_from($square).unwrap();
            let found_moves =
                $crate::movegen::find_bishop_moves(square, $color, white_taken, black_taken);
            assert_eq!(found_moves.len(), $targets.len());
            let targets = $crate::movegen::tests::extract_targets(&found_moves);
            let expected_targets = $crate::movegen::tests::notation_to_squares($targets);
            for target in expected_targets {
                assert!(targets.contains(&target));
            }
        };
    }

    macro_rules! check_queen {
        (number of correct moves of $color:ident $square:ident on $board:ident is $num:expr) => {
            let (white, black) = $crate::movegen::tests::extract_squares_taken($board);
            let square = $crate::square::Square::try_from($square).unwrap();
            let found_moves = $crate::movegen::find_queen_moves(square, $color, white, black);
            assert_eq!(found_moves.len(), $num);

            // get (file, rank) indexes of the rook's square
            let (file_i, rank_i) = (square.get_file().index(), square.get_rank().index());
            let (file_i, rank_i) = (file_i as i8, rank_i as i8);

            let targets = $crate::movegen::tests::extract_targets(&found_moves);

            // calculate differences between (file, rank) indexes of the queen's square
            // and (file, rank) indexes of all squares that find_queen_moves has found
            for target_sq in targets {
                let t_file_i = target_sq.get_file().index() as i8;
                let t_rank_i = target_sq.get_rank().index() as i8;

                // calculate absolute value of the difference between queen's (file, rank) pair
                // and target square (file, rank) pair
                let (diff_file, diff_rank) = (file_i - t_file_i, rank_i - t_rank_i);
                let (diff_file, diff_rank) = (diff_file.abs(), diff_rank.abs());

                // (diff_file, diff_rank) should either be equal (diagonal move) or
                // one should be 0 and the other non-zero (moving only on between ranks
                // or files)
                assert!(
                        diff_file == diff_rank ||             // diagonal move
                        (diff_file == 0 && diff_rank != 0) || // changing rank
                        (diff_file != 0 && diff_rank == 0)    // changing file
                    );
            }
        };
        ($color:ident $square:literal on $board:ident can be moved to $targets:ident) => {
            let (white_taken, black_taken) = $crate::movegen::tests::extract_squares_taken($board);
            let square = $crate::square::Square::try_from($square).unwrap();
            let found_moves =
                $crate::movegen::find_queen_moves(square, $color, white_taken, black_taken);
            assert_eq!(found_moves.len(), $targets.len());
            let targets = $crate::movegen::tests::extract_targets(&found_moves);
            let expected_targets = $crate::movegen::tests::notation_to_squares($targets);
            for target in expected_targets {
                assert!(targets.contains(&target));
            }
        };
    }

    #[test]
    fn pawn_unmoved_has_two_moves_when_not_blocked() {
        let board = &board::Board::try_from("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR").unwrap();
        let context = &context::Context::default();
        let squares = &["a3", "a4"];
        let white = piece::Color::White;
        check_pawn!(white "a2" on board having context can be moved to squares);
    }

    #[test]
    fn pawn_unmoved_has_correct_number_of_moves_when_blocked() {
        // pawn on a2 blocked by other piece on a4 has 1 move
        let board =
            &board::Board::try_from("rnbqkbnr/pppppppp/8/8/n7/8/PPPPPPPP/RNBQKBNR").unwrap();
        let context = &context::Context::default();
        let squares = &["a3"];
        let white = piece::Color::White;
        check_pawn!(white "a2" on board having context can be moved to squares);

        // pawn on a2 blocked by other piece on a3 has 0 moves
        let board =
            &board::Board::try_from("rnbqkbnr/pppppppp/8/8/8/n7/PPPPPPPP/RNBQKBNR").unwrap();
        let context = &context::Context::default();
        let white = piece::Color::White;
        check_pawn!(white "a2" on board having context cannot be moved);
    }

    #[test]
    fn pawn_already_moved_can_only_move_one_forward() {
        // pawn has already made it's first move before
        let board =
            &board::Board::try_from("rnbqkbnr/ppp1pppp/3p4/8/4P3/8/PPPP1PPP/RNBQKBNR").unwrap();
        let context = &context::Context::default();
        let squares = &["e5"];
        let white = piece::Color::White;
        check_pawn!(white "e4" on board having context can be moved to squares);
    }

    #[test]
    fn pawn_already_moved_has_no_moves_when_blocked() {
        // pawn has already made it's first move before
        let board =
            &board::Board::try_from("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR").unwrap();
        let context = &context::Context::default();
        let white = piece::Color::White;
        check_pawn!(white "e4" on board having context cannot be moved);
    }

    #[test]
    fn pawn_white_on_promotion_square_has_noncapturing_promotion_moves() {
        let board = &board::Board::try_from("8/1P5k/8/8/8/8/8/6K1").unwrap();
        let context = &context::Context::default();
        let squares = &["b8"];
        let white = piece::Color::White;
        check_pawn!(white "b7" on board having context can be promoted on squares);
    }

    #[test]
    fn pawn_white_on_promotion_square_has_capturing_promotion_moves() {
        let board = &board::Board::try_from("bn2b2k/P7/8/8/8/8/8/1K6").unwrap();
        let context = &context::Context::default();
        let squares = &["b8"];
        let white = piece::Color::White;
        check_pawn!(white "a7" on board having context can be promoted on squares);
    }

    #[test]
    fn pawn_black_on_promotion_square_has_noncapturing_promotion_moves() {
        let board = &board::Board::try_from("8/7k/8/8/8/8/1p6/6K1").unwrap();
        let context = &context::Context::default();
        let squares = &["b1"];
        let black = piece::Color::Black;
        check_pawn!(black "b2" on board having context can be promoted on squares);
    }

    #[test]
    fn pawn_black_on_promotion_square_has_capturing_promotion_moves() {
        let board = &board::Board::try_from("7k/8/8/8/8/8/7p/1K4NB").unwrap();
        let context = &context::Context::default();
        let squares = &["g1"];
        let black = piece::Color::Black;
        check_pawn!(black "h2" on board having context can be promoted on squares);
    }

    #[test]
    fn pawn_white_can_attack_enemy_pieces() {
        let board = &board::Board::try_from("8/7k/8/8/1n1n4/2P5/8/6K1").unwrap();
        let context = &context::Context::default();
        let squares = &["b4", "c4", "d4"];
        let white = piece::Color::White;
        check_pawn!(white "c3" on board having context can be moved to squares);
    }

    #[test]
    fn pawn_white_cannot_attack_own_pieces() {
        let board = &board::Board::try_from("8/7k/8/8/1R1R4/2P5/8/6K1").unwrap();
        let context = &context::Context::default();
        let squares = &["c4"];
        let white = piece::Color::White;
        check_pawn!(white "c3" on board having context can be moved to squares);
    }

    #[test]
    fn pawn_white_on_file_a_attacks_enemy_pieces() {
        let board = &board::Board::try_from("8/7k/8/8/1p6/P6p/8/6K1").unwrap();
        let context = &context::Context::default();
        let squares = &["b4", "a4"];
        let white = piece::Color::White;
        check_pawn!(white "a3" on board having context can be moved to squares);
    }

    #[test]
    fn pawn_white_on_file_a_cannot_attack_own_pieces() {
        let board = &board::Board::try_from("8/7k/8/8/RP6/P6p/8/6K1").unwrap();
        let context = &context::Context::default();
        let white = piece::Color::White;
        check_pawn!(white "a3" on board having context cannot be moved);
    }

    #[test]
    fn pawn_white_on_file_h_attacks_enemy_pieces() {
        let board = &board::Board::try_from("8/7k/8/p7/6p1/7P/8/6K1").unwrap();
        let context = &context::Context::default();
        let squares = &["g4", "h4"];
        let white = piece::Color::White;
        check_pawn!(white "h3" on board having context can be moved to squares);
    }

    #[test]
    fn pawn_white_on_file_h_cannot_attack_own_pieces() {
        let board = &board::Board::try_from("8/7k/8/p7/6PR/7P/8/6K1").unwrap();
        let context = &context::Context::default();
        let white = piece::Color::White;
        check_pawn!(white "h3" on board having context cannot be moved);
    }

    #[test]
    fn pawn_black_can_attack_enemy_pieces() {
        let board = &board::Board::try_from("8/7k/3p4/2N1N3/8/8/8/6K1").unwrap();
        let context = &context::Context::default();
        let squares = &["c5", "d5", "e5"];
        let black = piece::Color::Black;
        check_pawn!(black "d6" on board having context can be moved to squares);
    }

    #[test]
    fn pawn_black_cannot_attack_own_pieces() {
        let board = &board::Board::try_from("8/7k/3p4/2n1n3/8/8/8/6K1").unwrap();
        let context = &context::Context::default();
        let squares = &["d5"];
        let black = piece::Color::Black;
        check_pawn!(black "d6" on board having context can be moved to squares);
    }

    #[test]
    fn pawn_black_on_file_a_attacks_enemy_pieces() {
        let board = &board::Board::try_from("8/7k/p7/1P6/7P/8/8/6K1").unwrap();
        let context = &context::Context::default();
        let squares = &["b5", "a5"];
        let black = piece::Color::Black;
        check_pawn!(black "a6" on board having context can be moved to squares);
    }

    #[test]
    fn pawn_black_on_file_a_cannot_attack_own_pieces() {
        let board = &board::Board::try_from("8/7k/p7/np6/8/8/8/6K1").unwrap();
        let context = &context::Context::default();
        let black = piece::Color::Black;
        check_pawn!(black "a6" on board having context cannot be moved);
    }

    #[test]
    fn pawn_black_on_file_h_attacks_enemy_pieces() {
        let board = &board::Board::try_from("8/7k/P6p/6P1/8/8/8/6K1").unwrap();
        let context = &context::Context::default();
        let squares = &["g5", "h5"];
        let black = piece::Color::Black;
        check_pawn!(black "h6" on board having context can be moved to squares);
    }

    #[test]
    fn pawn_black_on_file_h_cannot_attack_own_pieces() {
        let board = &board::Board::try_from("8/7k/7p/6pr/8/8/8/6K1").unwrap();
        let context = &context::Context::default();
        let black = piece::Color::Black;
        check_pawn!(black "h6" on board having context cannot be moved);
    }

    #[test]
    fn pawn_white_can_capture_enpassant() {
        let board =
            &board::Board::try_from("rnbqkbnr/ppp1p1pp/3p4/4Pp2/8/8/PPPP1PPP/RNBQKBNR").unwrap();
        // en-passant possible on the f6 square
        let context = &context::Context::try_from("w KQkq f6 0 3").unwrap();
        let squares = &["d6", "e6", "f6"];
        let white = piece::Color::White;
        check_pawn!(white "e5" on board having context can be moved to squares);
    }

    #[test]
    fn pawn_black_can_capture_enpassant() {
        let board =
            &board::Board::try_from("rnbqkbnr/pp1ppppp/8/4P3/2pP4/8/PPP2PPP/RNBQKBNR").unwrap();
        // en-passant possible on the d3 square
        let context = &context::Context::try_from("w KQkq d3 0 3").unwrap();
        let squares = &["c3", "d3"];
        let black = piece::Color::Black;
        check_pawn!(black "c4" on board having context can be moved to squares);
    }

    #[test]
    fn knight_all_two_square_attacks_work() {
        // only places on the board where knights attack only 2 squares
        // are the corners of the board
        let board = &board::Board::try_from("N6N/8/8/8/8/8/8/N6N").unwrap();
        let white = piece::Color::White;
        for knight in ["a1", "a8", "h1", "h8"] {
            check_king!(number of correct moves of white knight on board is 3);
        }
    }

    #[test]
    fn knight_all_two_square_attacks_blocked() {
        // only places on the board where knights attack only 2 squares
        // are the corners of the board
        let board = &board::Board::try_from("N6N/2B2B2/1B4B1/8/8/1B4B1/2B2B2/N6N").unwrap();
        let white = piece::Color::White;
        for knight in ["a1", "a8", "h1", "h8"] {
            check_knight!(number of correct moves of white knight on board is 0);
        }
    }

    #[test]
    fn knight_all_three_square_attacks_work() {
        // squares with 3 attacks:
        // a2, a7, b1, b8, g1, g8, h2, h7
        let board = &board::Board::try_from("1n4n1/n6n/8/8/8/8/n6n/1n4n1").unwrap();
        let black = piece::Color::Black;
        for knight in ["a2", "a7", "b1", "b8", "g1", "g8", "h2", "h7"] {
            check_knight!(number of correct moves of black knight on board is 3);
        }
    }

    #[test]
    fn knight_all_three_square_attacks_blocked() {
        // squares with 3 attacks:
        // a2, a7, b1, b8, g1, g8, h2, h7
        let board =
            &board::Board::try_from("1np2pn1/n2pp2n/p1p2p1p/1p4p1/1p4p1/p1p2p1p/n2pp2n/1np2pn1")
                .unwrap();
        let black = piece::Color::Black;
        for knight in ["a2", "a7", "b1", "b8", "g1", "g8", "h2", "h7"] {
            check_knight!(number of correct moves of black knight on board is 0);
        }
    }

    #[test]
    fn knight_all_four_square_attacks_work() {
        // squares with 4 attacks:
        // - file a, ranks 3-6
        // - file h, ranks 3-6
        // - rank 1, files c-f
        // - rank 8, files c-f
        // - b2, b7, g2, g7

        // use the fact that the move finding function does not check whether the
        // knight is actually on the board where it's told it is (the invariant),
        // to simplify setup of the test board by ignoring that invariant
        // (which should NOT be done outside of tests)
        let board = &board::Board::try_from("8/8/8/8/8/8/8/8").unwrap();

        // create all squares from which knights attack exactly 4 squares
        let mut all_squares = vec!["b2", "b7", "g2", "g7"];
        let mut all_squares: Vec<String> = all_squares
            .iter_mut()
            .map(|elem| elem.to_string())
            .collect();
        for file in ['a', 'h'] {
            for rank in '3'..='6' {
                let square_s = format!("{}{}", file, rank);
                all_squares.push(square_s);
            }
        }
        for rank in ['1', '8'] {
            for file in 'c'..='f' {
                let square_s = format!("{}{}", file, rank);
                all_squares.push(square_s);
            }
        }

        let black = piece::Color::Black;
        for knight in all_squares {
            let knight = knight.as_str();
            check_knight!(number of correct moves of black knight on board is 4);
        }
    }

    #[test]
    fn knight_all_four_square_attacks_blocked() {
        // squares with 4 attacks:
        // - file a, ranks 3-6
        // - file h, ranks 3-6
        // - rank 1, files c-f
        // - rank 8, files c-f
        // - b2, b7, g2, g7

        // use the fact that the move finding function does not check whether the
        // knight is actually on the board where it's told it is (the invariant),
        // to simplify setup of the test board by ignoring that invariant
        // (which should NOT be done outside of tests)
        let board = &board::Board::try_from(
            "pppppppp/pppppppp/pppppppp/pppppppp/pppppppp/pppppppp/pppppppp/pppppppp",
        )
        .unwrap();

        // create all squares from which knights attack exactly 4 squares
        let mut all_squares = vec!["b2", "b7", "g2", "g7"];
        let mut all_squares: Vec<String> = all_squares
            .iter_mut()
            .map(|elem| elem.to_string())
            .collect();
        for file in ['a', 'h'] {
            for rank in '3'..='6' {
                let square_s = format!("{}{}", file, rank);
                all_squares.push(square_s);
            }
        }
        for rank in ['1', '8'] {
            for file in 'c'..='f' {
                let square_s = format!("{}{}", file, rank);
                all_squares.push(square_s);
            }
        }

        let black = piece::Color::Black;
        for knight in all_squares {
            let knight = knight.as_str();
            check_knight!(number of correct moves of black knight on board is 0);
        }
    }

    #[test]
    fn knight_all_six_square_attacks_work() {
        // squares with 6 attacks:
        // - file b, ranks 3-6
        // - file g, ranks 3-6
        // - rank 2, files c-f
        // - rank 7, files c-f

        // use the fact that the move finding function does not check whether the
        // knight is actually on the board where it's told it is (the invariant),
        // to simplify setup of the test board by ignoring that invariant
        // (which should NOT be done outside of tests)
        let board = &board::Board::try_from("8/8/8/8/8/8/8/8").unwrap();

        // create all squares from which knights attack exactly 6 squares
        let mut all_squares = vec![];
        for file in ['b', 'g'] {
            for rank in '3'..='6' {
                let square_s = format!("{}{}", file, rank);
                all_squares.push(square_s);
            }
        }
        for rank in ['2', '7'] {
            for file in 'c'..='f' {
                let square_s = format!("{}{}", file, rank);
                all_squares.push(square_s);
            }
        }

        let white = piece::Color::White;
        for knight in all_squares {
            let knight = knight.as_str();
            check_knight!(number of correct moves of white knight on board is 6);
        }
    }

    #[test]
    fn knight_all_six_square_attacks_blocked() {
        // squares with 6 attacks:
        // - file b, ranks 3-6
        // - file g, ranks 3-6
        // - rank 2, files c-f
        // - rank 7, files c-f

        // use the fact that the move finding function does not check whether the
        // knight is actually on the board where it's told it is (the invariant),
        // to simplify setup of the test board by ignoring that invariant
        // (which should NOT be done outside of tests)
        let board = &board::Board::try_from(
            "PPPPPPPP/PPPPPPPP/PPPPPPPP/PPPPPPPP/PPPPPPPP/PPPPPPPP/PPPPPPPP/PPPPPPPP",
        )
        .unwrap();

        // create all squares from which knights attack exactly 6 squares
        let mut all_squares = vec![];
        for file in ['b', 'g'] {
            for rank in '3'..='6' {
                let square_s = format!("{}{}", file, rank);
                all_squares.push(square_s);
            }
        }
        for rank in ['2', '7'] {
            for file in 'c'..='f' {
                let square_s = format!("{}{}", file, rank);
                all_squares.push(square_s);
            }
        }

        let white = piece::Color::White;
        for knight in all_squares {
            let knight = knight.as_str();
            check_knight!(number of correct moves of white knight on board is 0);
        }
    }

    #[test]
    fn knight_all_eight_square_attacks_work() {
        // squares with 8 attacks:
        // files c-f, ranks 3-6

        // use the fact that the move finding function does not check whether the
        // knight is actually on the board where it's told it is (the invariant),
        // to simplify setup of the test board by ignoring that invariant
        // (which should NOT be done outside of tests)
        let board = &board::Board::try_from("8/8/8/8/8/8/8/8").unwrap();

        // create all squares from which knights attack exactly 8 squares
        let mut all_squares = vec![];
        for file in 'c'..='f' {
            for rank in '3'..='6' {
                let square_s = format!("{}{}", file, rank);
                all_squares.push(square_s);
            }
        }

        let black = piece::Color::Black;
        for knight in all_squares {
            let knight = knight.as_str();
            check_knight!(number of correct moves of black knight on board is 8);
        }
    }

    #[test]
    fn knight_all_eight_square_attacks_blocked() {
        // squares with 8 attacks:
        // files c-f, ranks 3-6

        // use the fact that the move finding function does not check whether the
        // knight is actually on the board where it's told it is (the invariant),
        // to simplify setup of the test board by ignoring that invariant
        // (which should NOT be done outside of tests)
        let board = &board::Board::try_from(
            "pppppppp/pppppppp/pppppppp/pppppppp/pppppppp/pppppppp/pppppppp/pppppppp",
        )
        .unwrap();

        // create all squares from which knights attack exactly 8 squares
        let mut all_squares = vec![];
        for file in 'c'..='f' {
            for rank in '3'..='6' {
                let square_s = format!("{}{}", file, rank);
                all_squares.push(square_s);
            }
        }

        let black = piece::Color::Black;
        for knight in all_squares {
            let knight = knight.as_str();
            check_knight!(number of correct moves of black knight on board is 0);
        }
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

    #[test]
    fn rook_all_fourteen_square_attacks_work() {
        // use the fact that the move finding function does not check whether the
        // rook is actually on the board where it's told it is (the invariant),
        // to simplify setup of the test board by ignoring that invariant
        // (which should NOT be done outside of tests)
        let board = &board::Board::try_from("8/8/8/8/8/8/8/8").unwrap();

        // when a board is empty, rook on every square attacks 14 other squares
        let mut all_squares = vec![];
        for file in 'a'..='h' {
            for rank in '1'..='8' {
                let square_s = format!("{}{}", file, rank);
                all_squares.push(square_s);
            }
        }

        let white = piece::Color::White;
        let black = piece::Color::Black;

        for rook in all_squares {
            let rook = rook.as_str();
            check_rook!(number of correct moves of white rook on board is 14);
            check_rook!(number of correct moves of black rook on board is 14);
        }
    }

    #[test]
    fn rook_does_not_attack_own_pieces() {
        let squares = &["c4", "b4", "d5", "d6", "d3", "e4"];

        let white_board = &board::Board::try_from("7k/1b1N4/7n/8/B2R1P2/8/3Q4/K5n1").unwrap();
        let white = piece::Color::White;
        check_rook!(white "d4" on white_board can be moved to squares);

        let black_board = &board::Board::try_from("7K/1B1n4/7N/8/b2r1p2/8/3q4/k5N1").unwrap();
        let black = piece::Color::Black;
        check_rook!(black "d4" on black_board can be moved to squares);
    }

    #[test]
    fn rook_attacks_enemy_pieces() {
        let squares = &["c4", "b4", "a4", "d5", "d6", "d7", "d3", "d2", "e4", "f4"];

        let white_board = &board::Board::try_from("7k/1b1n4/7n/8/b2R1p2/8/3q4/K5n1").unwrap();
        let white = piece::Color::White;
        check_rook!(white "d4" on white_board can be moved to squares);

        let black_board = &board::Board::try_from("7K/1B1N4/7N/8/B2r1P2/8/3Q4/k5N1").unwrap();
        let black = piece::Color::Black;
        check_rook!(black "d4" on black_board can be moved to squares);
    }

    #[test]
    fn bishop_all_seven_square_attacks_work() {
        // squares where bishops attack exactly 7 squares:
        // - ranks 1 and 8, and files a and h

        // use the fact that the move finding function does not check whether the
        // bishop is actually on the board where it's told it is (the invariant),
        // to simplify setup of the test board by ignoring that invariant
        // (which should NOT be done outside of tests)
        let board = &board::Board::try_from("8/8/8/8/8/8/8/8").unwrap();

        // create all squares from which bishops attack exactly 7 squares
        let mut all_squares = vec![];
        for file in ['a', 'h'] {
            for rank in ['1', '8'] {
                let square_s = format!("{}{}", file, rank);
                all_squares.push(square_s);
            }
        }

        let white = piece::Color::White;
        for bishop in all_squares {
            let bishop = bishop.as_str();
            check_bishop!(number of correct moves of white bishop on board is 7);
        }
    }

    #[test]
    fn bishop_all_nine_square_attacks_work() {
        // squares where bishops attack exactly 9 squares:
        // - ranks 2-7, file b and g
        // - ranks 2 and 7, files c-f

        // use the fact that the move finding function does not check whether the
        // bishop is actually on the board where it's told it is (the invariant),
        // to simplify setup of the test board by ignoring that invariant
        // (which should NOT be done outside of tests)
        let board = &board::Board::try_from("8/8/8/8/8/8/8/8").unwrap();

        // create all squares from which bishops attack exactly 9 squares
        let mut all_squares = vec![];
        for rank in '2'..='7' {
            for file in ['b', 'g'] {
                let square_s = format!("{}{}", file, rank);
                all_squares.push(square_s);
            }
        }
        for rank in ['2', '7'] {
            for file in 'c'..='f' {
                let square_s = format!("{}{}", file, rank);
                all_squares.push(square_s);
            }
        }

        let black = piece::Color::Black;
        for bishop in all_squares {
            let bishop = bishop.as_str();
            check_bishop!(number of correct moves of black bishop on board is 9);
        }
    }

    #[test]
    fn bishop_all_eleven_square_attacks_work() {
        // squares where bishop attack exactly 11 squares:
        // - ranks 3-6, file c and f
        // - ranks 3 and 6, file d-e

        // use the fact that the move finding function does not check whether the
        // bishop is actually on the board where it's told it is (the invariant),
        // to simplify setup of the test board by ignoring that invariant
        // (which should NOT be done outside of tests)
        let board = &board::Board::try_from("8/8/8/8/8/8/8/8").unwrap();

        // create all squares from which bishops attack exactly 11 squares
        let mut all_squares = vec![];
        for rank in '3'..='6' {
            for file in ['c', 'f'] {
                let square_s = format!("{}{}", file, rank);
                all_squares.push(square_s);
            }
        }
        for rank in ['3', '6'] {
            for file in 'd'..='e' {
                let square_s = format!("{}{}", file, rank);
                all_squares.push(square_s);
            }
        }

        let white = piece::Color::White;
        for bishop in all_squares {
            let bishop = bishop.as_str();
            check_bishop!(number of correct moves of white bishop on board is 11);
        }
    }

    #[test]
    fn bishop_all_thirteen_square_attacks_work() {
        // squares where bishop attack exactly 13 squares:
        // - ranks 4-5, files d-e

        // use the fact that the move finding function does not check whether the
        // bishop is actually on the board where it's told it is (the invariant),
        // to simplify setup of the test board by ignoring that invariant
        // (which should NOT be done outside of tests)
        let board = &board::Board::try_from("8/8/8/8/8/8/8/8").unwrap();

        // create all squares from which bishops attack exactly 11 squares
        let mut all_squares = vec![];
        for rank in '4'..='5' {
            for file in ['d', 'e'] {
                let square_s = format!("{}{}", file, rank);
                all_squares.push(square_s);
            }
        }

        let black = piece::Color::Black;
        for bishop in all_squares {
            let bishop = bishop.as_str();
            check_bishop!(number of correct moves of black bishop on board is 13);
        }
    }

    #[test]
    fn bishop_does_not_attack_own_pieces() {
        let squares = &["b3", "c4", "c6", "e4", "e6", "f7"];

        let white_board = &board::Board::try_from("6N1/1R6/8/3B4/8/5Q2/K7/8").unwrap();
        let white = piece::Color::White;
        check_bishop!(white "d5" on white_board can be moved to squares);

        let black_board = &board::Board::try_from("6n1/1r6/8/3b4/8/5q2/k7/8").unwrap();
        let black = piece::Color::Black;
        check_bishop!(black "d5" on black_board can be moved to squares);
    }

    #[test]
    fn bishop_attacks_enemy_pieces() {
        let squares = &["a2", "b3", "b7", "c4", "c6", "e4", "e6", "f3", "f7", "g8"];

        let white_board = &board::Board::try_from("6n1/1r6/8/3B4/8/5q2/k7/8").unwrap();
        let white = piece::Color::White;
        check_bishop!(white "d5" on white_board can be moved to squares);

        let black_board = &board::Board::try_from("6N1/1R6/8/3b4/8/5Q2/K7/8").unwrap();
        let black = piece::Color::Black;
        check_bishop!(black "d5" on black_board can be moved to squares);
    }

    #[test]
    fn queen_all_twenty_one_square_attacks_work() {
        // squares where queens attack exactly 21 squares:
        // - ranks 1 and 8, and files a and h

        // use the fact that the move finding function does not check whether the
        // queen is actually on the board where it's told it is (the invariant),
        // to simplify setup of the test board by ignoring that invariant
        // (which should NOT be done outside of tests)
        let board = &board::Board::try_from("8/8/8/8/8/8/8/8").unwrap();

        // create all squares from which queens attack exactly 21 squares
        let mut all_squares = vec![];
        for file in ['a', 'h'] {
            for rank in ['1', '8'] {
                let square_s = format!("{}{}", file, rank);
                all_squares.push(square_s);
            }
        }

        let white = piece::Color::White;
        for queen in all_squares {
            let queen = queen.as_str();
            check_queen!(number of correct moves of white queen on board is 21);
        }
    }

    #[test]
    fn queen_all_twenty_three_square_attacks_work() {
        // squares where queens attack exactly 23 squares:
        // - ranks 2-7, file b and g
        // - ranks 2 and 7, files c-f

        // use the fact that the move finding function does not check whether the
        // queen is actually on the board where it's told it is (the invariant),
        // to simplify setup of the test board by ignoring that invariant
        // (which should NOT be done outside of tests)
        let board = &board::Board::try_from("8/8/8/8/8/8/8/8").unwrap();

        // create all squares from which queens attack exactly 23 squares
        let mut all_squares = vec![];
        for rank in '2'..='7' {
            for file in ['b', 'g'] {
                let square_s = format!("{}{}", file, rank);
                all_squares.push(square_s);
            }
        }
        for rank in ['2', '7'] {
            for file in 'c'..='f' {
                let square_s = format!("{}{}", file, rank);
                all_squares.push(square_s);
            }
        }

        let black = piece::Color::Black;
        for queen in all_squares {
            let queen = queen.as_str();
            check_queen!(number of correct moves of black queen on board is 23);
        }
    }

    #[test]
    fn queen_all_twenty_five_square_attacks_work() {
        // squares where queens attack exactly 25 squares:
        // - ranks 3-6, file c and f
        // - ranks 3 and 6, file d-e

        // use the fact that the move finding function does not check whether the
        // queen is actually on the board where it's told it is (the invariant),
        // to simplify setup of the test board by ignoring that invariant
        // (which should NOT be done outside of tests)
        let board = &board::Board::try_from("8/8/8/8/8/8/8/8").unwrap();

        // create all squares from which queens attack exactly 25 squares
        let mut all_squares = vec![];
        for rank in '3'..='6' {
            for file in ['c', 'f'] {
                let square_s = format!("{}{}", file, rank);
                all_squares.push(square_s);
            }
        }
        for rank in ['3', '6'] {
            for file in 'd'..='e' {
                let square_s = format!("{}{}", file, rank);
                all_squares.push(square_s);
            }
        }

        let white = piece::Color::White;
        for queen in all_squares {
            let queen = queen.as_str();
            check_queen!(number of correct moves of white queen on board is 25);
        }
    }

    #[test]
    fn queen_all_twenty_seven_square_attacks_work() {
        // squares where queens attack exactly 27 squares:
        // - ranks 4-5, files d-e

        // use the fact that the move finding function does not check whether the
        // queen is actually on the board where it's told it is (the invariant),
        // to simplify setup of the test board by ignoring that invariant
        // (which should NOT be done outside of tests)
        let board = &board::Board::try_from("8/8/8/8/8/8/8/8").unwrap();

        // create all squares from which queens attack exactly 27 squares
        let mut all_squares = vec![];
        for rank in '4'..='5' {
            for file in ['d', 'e'] {
                let square_s = format!("{}{}", file, rank);
                all_squares.push(square_s);
            }
        }

        let black = piece::Color::Black;
        for queen in all_squares {
            let queen = queen.as_str();
            check_queen!(number of correct moves of black queen on board is 27);
        }
    }

    #[test]
    fn queen_does_not_attack_own_pieces() {
        let squares = &[
            "b6", "c3", "c4", "c5", "d2", "d3", "d5", "d6", "e3", "e4", "e5", "f2", "f4",
        ];

        let white_board = &board::Board::try_from("8/P2B4/5R2/8/1P1Q2P1/8/1K6/3R2N1").unwrap();
        let white = piece::Color::White;
        check_queen!(white "d4" on white_board can be moved to squares);

        let black_board = &board::Board::try_from("8/p2b4/5r2/8/1p1q2p1/8/1k6/3r2n1").unwrap();
        let black = piece::Color::Black;
        check_queen!(black "d4" on black_board can be moved to squares);
    }

    #[test]
    fn queen_attacks_enemy_pieces() {
        let squares = &[
            "a7", "b2", "b4", "b6", "c3", "c4", "c5", "d1", "d2", "d3", "d5", "d6", "d7", "e3",
            "e4", "e5", "f2", "f4", "f6", "g1", "g4",
        ];

        let white_board = &board::Board::try_from("8/p2b4/5r2/8/1p1Q2p1/8/1k6/3r2n1").unwrap();
        let white = piece::Color::White;
        check_queen!(white "d4" on white_board can be moved to squares);

        let black_board = &board::Board::try_from("8/P2B4/5R2/8/1P1q2P1/8/1K6/3R2N1").unwrap();
        let black = piece::Color::Black;
        check_queen!(black "d4" on black_board can be moved to squares);
    }
}
