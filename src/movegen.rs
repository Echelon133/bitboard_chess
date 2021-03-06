//! This module contains all functions which are used during pseudo-legal move generation and
//! during checking moves for their legality.
//!
//! All move generating functions return an iterator which returns all pseudo-legal moves of a
//! particular piece in a particular position on the board. These pseudo-legal moves have to be
//! checked for their legality (e.g. they mustn't put player's own king
//! in check, mustn't allow illegal castling, etc.) before they are played on the board.

use crate::bitboard;
use crate::board;
use crate::context;
use crate::movegen_constants::*;
use crate::moves;
use crate::piece;
use crate::square;

/// An iterator over pseudo-legal moves of pieces. Has two possible modes of work: one for
/// generating pawn promotion moves and one for generating the rest of the moves.
///
/// If `promoting` is set to true, each pair of (start_square, target_square) is
/// generated four times, for each possible kind of piece that can be a promotion goal.
/// If `promoting` is set to false, each pair of (start_square, target_square) is
/// generated only once.
#[derive(Debug)]
pub struct MoveIter {
    start_square: square::Square,
    targets: bitboard::Bitboard,
    promoting: bool,
    kind_index: usize,
}

impl MoveIter {
    /// Creates an iterator which returns [`moves::UCIMove`] which start on
    /// `start_square` and end on squares that are set in `targets` bitboard.
    ///
    /// If `promoting` is set to `true`, for every square set in `targets` bitboard there
    /// are 4 moves (one for each piece that can be a target of pawn promotion).
    pub fn new(start_square: square::Square, targets: bitboard::Bitboard, promoting: bool) -> Self {
        Self {
            start_square,
            targets,
            promoting,
            kind_index: 0,
        }
    }
}

impl Iterator for MoveIter {
    type Item = moves::UCIMove;

    /// Advances the iterator and returns the next pseudo-legal move.
    ///
    /// Returns [`None`] when iteration is finished.
    ///
    /// If `promoting` was set to `true`, each move is given out in 4 variants,
    /// one for each piece kind that can appear on the board during pawn promotion.
    /// Otherwise, if the iterator's `promoting` was set to `false`, every move is
    /// given out only once.
    fn next(&mut self) -> Option<Self::Item> {
        let count = self.targets.count_set();

        if count == 0 {
            None
        } else {
            match self.promoting {
                // moves are not promoting, so only spit out `UCIMove::Regular` and
                // then move on to the next possible move
                false => {
                    // find first bit set to 1
                    let first_set = self.targets.bitscan_forward();
                    let target_square = square::Square::from(first_set);
                    // clear the bit
                    self.targets.clear(target_square);
                    let mv = moves::Move::new(self.start_square, target_square);
                    Some(moves::UCIMove::Regular { m: mv })
                }
                // moves are promoting, so only spit out `UCIMove::Promotion`, and each
                // move should be generated four times, for each kind of piece that can
                // appear on the board when promoting a pawn
                true => {
                    let promotion_kinds = [
                        piece::Kind::Knight,
                        piece::Kind::Bishop,
                        piece::Kind::Queen,
                        piece::Kind::Rook,
                    ];

                    // find first bit set to 1
                    let mut first_set = self.targets.bitscan_forward();
                    let mut target_square = square::Square::from(first_set);

                    // if kind_index is 4, it means that the previous promotion_target was
                    // exhausted and it's time to move to the next promotion_target
                    if self.kind_index == 4 {
                        self.targets.clear(target_square);
                        // it all promotion moves got generated for the last target square
                        // and there is no more targets, quit early with `None`
                        if self.targets.count_set() == 0 {
                            return None;
                        }

                        first_set = self.targets.bitscan_forward();
                        target_square = square::Square::from(first_set);
                        self.kind_index = 0;
                    }

                    let mv = moves::Move::new(self.start_square, target_square);
                    let result = moves::UCIMove::Promotion {
                        m: mv,
                        k: promotion_kinds[self.kind_index],
                    };
                    self.kind_index += 1;
                    Some(result)
                }
            }
        }
    }
}

impl ExactSizeIterator for MoveIter {
    fn len(&self) -> usize {
        let count_targets = self.targets.count_set() as usize;
        let exact_remaining = match self.promoting {
            true => {
                // for every promotion target, give out 4 elements
                let upper_bound_remaining = count_targets * 4;
                // remove those variants of moves that have already been given out
                let exact_remaining = upper_bound_remaining - self.kind_index;
                exact_remaining
            }
            false => {
                // nonpromoting iter returns exactly 1 element for each bit set
                count_targets
            }
        };
        exact_remaining
    }
}

/// Finds all pseudo-legal moves of a `color` colored pawn on the given `piece_square`.
/// Provided `context` is required for determining if there is an en passant move available.
///
/// This function assumes that a piece that is placed on the `piece_square` is actually
/// a pawn of `color` color. It does not check whether that is true, so incorrect call
/// to this function will yield invalid moves.
///
/// Bitboards `white_taken` and `black_taken` should hold all squares occupied by
/// white and black pieces, respectively.
///
/// # More info
/// [How to calculate pawn pushes](https://www.chessprogramming.org/Pawn_Pushes_(Bitboards))
///
/// [Hot to calculate captures](https://www.chessprogramming.org/Pawn_Attacks_(Bitboards)#Single_Pawn)
///
pub fn find_pawn_moves(
    piece_square: square::Square,
    color: piece::Color,
    white_taken: bitboard::Bitboard,
    black_taken: bitboard::Bitboard,
    context: &context::Context,
) -> MoveIter {
    let all_taken = white_taken | black_taken;

    let pawn_index = piece_square.get_index();
    let pawn_rank = piece_square.get_rank();

    let (enemy_pieces, push_direction, attack_pattern, start_rank, promotion_rank) = match color {
        piece::Color::White => (
            black_taken,
            1i8,
            bitboard::Bitboard::from(WHITE_PAWN_ATTACK_PATTERNS[pawn_index]),
            square::Rank::R2,
            square::Rank::R7,
        ),
        piece::Color::Black => (
            white_taken,
            -1i8,
            bitboard::Bitboard::from(BLACK_PAWN_ATTACK_PATTERNS[pawn_index]),
            square::Rank::R7,
            square::Rank::R2,
        ),
    };

    if pawn_rank == promotion_rank {
        let mut target_squares = bitboard::Bitboard::default();
        // calculate single push forward (guaranteed to be within the board)
        let sq_index = (pawn_index as i8) + (push_direction * 8);
        let target_sq = square::Square::from(sq_index as u8);
        // if the square is empty, pawn can promote on it
        if !all_taken.is_set(target_sq) {
            target_squares.set(target_sq);
        }

        // calculate attacks
        let attack_squares = attack_pattern & enemy_pieces;
        let target_squares = target_squares | attack_squares;

        MoveIter::new(piece_square, target_squares, true)
    } else {
        let mut target_squares = bitboard::Bitboard::default();
        // calculate single push forward (guaranteed to be within the board)
        let push_one_i = (pawn_index as i8) + (push_direction * 8);
        let push_one_sq = square::Square::from(push_one_i as u8);

        // if the square is empty, pawn can be pushed there
        if !all_taken.is_set(push_one_sq) {
            target_squares.set(push_one_sq);

            // if pawn is on its initial square, check if it can be pushed twice
            if pawn_rank == start_rank {
                let push_two_i = (pawn_index as i8) + (push_direction * 16);
                let push_two_sq = square::Square::from(push_two_i as u8);
                if !all_taken.is_set(push_two_sq) {
                    target_squares.set(push_two_sq);
                }
            }
        }

        // calculate attacks
        let attack_squares = attack_pattern & enemy_pieces;
        let mut target_squares = target_squares | attack_squares;

        // calculate en passant
        if let Some(enpassant_target) = context.get_enpassant() {
            // white enpassant_target is on the 3rd rank, whereas
            // black enpassant_target is on the 6th rank
            //
            // any other value means that somehow enpassant_target got incorrectly set
            // in the board context, which means that some invariant got broken and the board
            // might be in an invalid state
            let capture_rank = match enpassant_target.get_rank() {
                square::Rank::R3 => square::Rank::R4,
                square::Rank::R6 => square::Rank::R5,
                _ => panic!("invalid enpassant target"),
            };
            // enpassant is only possible if both conditions below are true:
            // - the square from which the pawn would be captured en passant is actually taken
            //      by the opponent piece, otherwise there is nothing to capture
            // - the pawn attacks the enpassant_target square, which means that it's in position
            //      to take advantage of the en passant rule
            let enpassant_sq = square::Square::new(capture_rank, enpassant_target.get_file());
            if enemy_pieces.is_set(enpassant_sq) && attack_pattern.is_set(enpassant_target) {
                target_squares.set(enpassant_target);
            }
        }
        MoveIter::new(piece_square, target_squares, false)
    }
}

/// Finds all pseudo-legal moves of a `color` colored knight on the given `piece_square`.
///
/// This function assumes that a piece that is placed on the given `piece_square` is
/// actually a knight of `color` color. It does not check whether that is true,
/// so incorrect call to this function will yield invalid moves.
///
/// Bitboards `white_taken` and `black_taken` should hold all squares occupied by
/// white and black pieces, respectively.
///
/// # More info
/// [How to calculate knight moves](https://www.chessprogramming.org/Knight_Pattern)
///
pub fn find_knight_moves(
    piece_square: square::Square,
    color: piece::Color,
    white_taken: bitboard::Bitboard,
    black_taken: bitboard::Bitboard,
) -> MoveIter {
    let own_pieces = match color {
        piece::Color::White => white_taken,
        piece::Color::Black => black_taken,
    };

    // retrieve a precalculated knight attack pattern and make a bitboard using
    // all bits of that pattern
    let index = piece_square.get_index();
    let attack_bitboard = bitboard::Bitboard::from(KNIGHT_ATTACK_PATTERNS[index]);
    // only attack squares where there are no pieces the same color as the knight
    let attack_bitboard = attack_bitboard & (!own_pieces);

    MoveIter::new(piece_square, attack_bitboard, false)
}

/// Finds all pseudo-legal moves of a `color` colored king on the given
/// `piece_square`. Provided `context` is required for determining if the king
/// has rights to castle.
///
/// This function assumes that a piece that is placed on the given `piece_square`
/// is actually a king of `color` color. It does not check whether that is true,
/// so incorrect call to this function will yield invalid moves.
///
/// Bitboards `white_taken` and `black_taken` should hold all squares occupied by
/// white and black pieces, respectively.
///
/// # More info
/// [How to calculate king moves](https://www.chessprogramming.org/King_Pattern)
///
pub fn find_king_moves(
    piece_square: square::Square,
    color: piece::Color,
    white_taken: bitboard::Bitboard,
    black_taken: bitboard::Bitboard,
    context: &context::Context,
) -> MoveIter {
    let own_pieces = match color {
        piece::Color::White => white_taken,
        piece::Color::Black => black_taken,
    };

    let all_taken = white_taken | black_taken;

    // retrieve a precalculated king attack pattern and make a bitboard using
    // all bits of that pattern
    let index = piece_square.get_index();
    let attack_bitboard = bitboard::Bitboard::from(KING_ATTACK_PATTERNS[index]);
    // only attack squares where there are no pieces with the same color as the king
    let mut attack_bitboard = attack_bitboard & (!own_pieces);

    // if castling is available, then it means that the king
    // is on its original square, therefore g1/g8 and c1/c8 target
    // squares can be calculated by adding/subtracting
    // 2 from the index of the king's square
    if context.can_castle(color, context::Side::Kingside) {
        // two squares between the king and the kingside rook need to be empty
        // to make castling possible
        let f_file_square = square::Square::from((index + 1) as u8);
        let g_file_square = square::Square::from((index + 2) as u8);

        if !all_taken.is_set(f_file_square) && !all_taken.is_set(g_file_square) {
            attack_bitboard.set(g_file_square);
        }
    }
    if context.can_castle(color, context::Side::Queenside) {
        // three squares between the king and the queenside rook need to be empty
        // to make castling possible
        let b_file_square = square::Square::from((index - 3) as u8);
        let c_file_square = square::Square::from((index - 2) as u8);
        let d_file_square = square::Square::from((index - 1) as u8);

        if !all_taken.is_set(b_file_square)
            && !all_taken.is_set(c_file_square)
            && !all_taken.is_set(d_file_square)
        {
            attack_bitboard.set(c_file_square);
        }
    }

    MoveIter::new(piece_square, attack_bitboard, false)
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
            // remove a potential attack on player's own piece if the blocking piece
            // is also a part of own_pieces
            attack = attack & (!$own_pieces);
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
            // remove a potential attack on player's own piece if the blocking piece
            // is also a part of own_pieces
            attack = attack & (!$own_pieces);
        }
        attack
    }};
}

/// Finds all pseudo-legal moves of a sliding piece that only moves on its file or rank.
/// This can be either a rook or a queen.
///
/// # More info
///
/// [How to calculate for positive rays](https://www.chessprogramming.org/Classical_Approach#Conditional)
///
/// [How to calculate for negative rays](https://www.chessprogramming.org/Classical_Approach#Conditional_2)
#[inline(always)]
fn find_file_rank_moves(
    piece_square: square::Square,
    color: piece::Color,
    white_taken: bitboard::Bitboard,
    black_taken: bitboard::Bitboard,
) -> bitboard::Bitboard {
    let own_pieces = match color {
        piece::Color::White => white_taken,
        piece::Color::Black => black_taken,
    };

    let all_taken = white_taken | black_taken;
    let index = piece_square.get_index();

    let north_attack = positive_ray_attack!(NORTH_ATTACK_RAYS, own_pieces, all_taken, index);
    let south_attack = negative_ray_attack!(SOUTH_ATTACK_RAYS, own_pieces, all_taken, index);
    let east_attack = positive_ray_attack!(EAST_ATTACK_RAYS, own_pieces, all_taken, index);
    let west_attack = negative_ray_attack!(WEST_ATTACK_RAYS, own_pieces, all_taken, index);

    // sum all attacked squares from north, south, east and west
    let all_attacks = north_attack | south_attack | east_attack | west_attack;
    all_attacks
}

/// Finds all pseudo-legal moves of a `color` colored rook on the given `piece_square`.
///
/// This function assumes that a piece that is placed on the `piece_square` is actually
/// a rook of `color` color. It does not check whether that is true, so incorrect call
/// to this function will yield invalid moves.
///
/// Bitboards `white_taken` and `black_taken` should hold all squares occupied by
/// white and black pieces, respectively.
///
pub fn find_rook_moves(
    piece_square: square::Square,
    color: piece::Color,
    white_taken: bitboard::Bitboard,
    black_taken: bitboard::Bitboard,
) -> MoveIter {
    let attack_bitboard = find_file_rank_moves(piece_square, color, white_taken, black_taken);
    MoveIter::new(piece_square, attack_bitboard, false)
}

/// Finds all pseudo-legal moves of a sliding piece that moves diagonally. This can be
/// either a bishop or a queen.
///
/// # More info
/// [How to calculate for positive rays](https://www.chessprogramming.org/Classical_Approach#Conditional)
///
/// [How to calculate for negative rays](https://www.chessprogramming.org/Classical_Approach#Conditional_2)
#[inline(always)]
fn find_diagonal_moves(
    piece_square: square::Square,
    color: piece::Color,
    white_taken: bitboard::Bitboard,
    black_taken: bitboard::Bitboard,
) -> bitboard::Bitboard {
    let own_pieces = match color {
        piece::Color::White => white_taken,
        piece::Color::Black => black_taken,
    };

    let all_taken = white_taken | black_taken;
    let index = piece_square.get_index();

    let ne_attack = positive_ray_attack!(NORTHEAST_ATTACK_RAYS, own_pieces, all_taken, index);
    let se_attack = negative_ray_attack!(SOUTHEAST_ATTACK_RAYS, own_pieces, all_taken, index);
    let nw_attack = positive_ray_attack!(NORTHWEST_ATTACK_RAYS, own_pieces, all_taken, index);
    let sw_attack = negative_ray_attack!(SOUTHWEST_ATTACK_RAYS, own_pieces, all_taken, index);

    // sum all attacked squares from north-east, north-west, south-east and south-west
    let all_attacks = nw_attack | ne_attack | sw_attack | se_attack;
    all_attacks
}

/// Finds all pseudo-legal moves of a `color` colored bishop on the given `piece_square`.
///
/// This function assumes that a piece that is placed on the `piece_square` is actually
/// a bishop. It does not check whether that is true, so incorrect call to this function
/// will yield invalid moves.
///
/// Bitboards `white_taken` and `black_taken` should hold all squares occupied by
/// white and black pieces, respectively.
///
pub fn find_bishop_moves(
    piece_square: square::Square,
    color: piece::Color,
    white_taken: bitboard::Bitboard,
    black_taken: bitboard::Bitboard,
) -> MoveIter {
    let attack_bitboard = find_diagonal_moves(piece_square, color, white_taken, black_taken);
    MoveIter::new(piece_square, attack_bitboard, false)
}

/// Finds all pseudo-legal moves of a `color` colored queen on the given `piece_square`.
///
/// This function assumes that a piece that is placed on the `piece_square` is actually
/// a queen. It does not check whether that is true, so incorrect call to this function
/// will yield invalid moves.
///
/// Bitboards `white_taken` and `black_taken` should hold all squares occupied by
/// white and black pieces, respectively.
///
pub fn find_queen_moves(
    piece_square: square::Square,
    color: piece::Color,
    white_taken: bitboard::Bitboard,
    black_taken: bitboard::Bitboard,
) -> MoveIter {
    let attack_bitboard1 = find_diagonal_moves(piece_square, color, white_taken, black_taken);
    let attack_bitboard2 = find_file_rank_moves(piece_square, color, white_taken, black_taken);
    let sum_attacks = attack_bitboard1 | attack_bitboard2;

    MoveIter::new(piece_square, sum_attacks, false)
}

/// Generates code that checks whether a positive ray cast from the given square
/// hits an unblocked enemy piece.
///
/// # How it works - an example
///
/// If there is a white king on e4, and there is an unobstructed path north, where
/// either a black rook or queen occupies some square, that rook/queen certainly attacks
/// the king.
///
/// ```compile_fail
/// let all_taken: bitboard::Bitboard = /* get all taken */;
/// let enemy_rooks_queens: bitboard::Bitboard =
///     /* calculate all enemy rooks and queens squares */;
/// let king_index = square::Square::try_from("e4").unwrap().get_index();
/// let is_in_check = positive_ray_attacks_piece!(
///     NORTH_ATTACK_RAYS,
///     all_taken,
///     enemy_rooks_queens,
///     king_index);
/// ```
///
/// # Arguments
///
/// First argument should always contain a reference to an array of positive rays. These are:
/// - [`NORTH_ATTACK_RAYS`] (when handling rooks/queens)
/// - [`EAST_ATTACK_RAYS`] (when handling rooks/queens)
/// - [`NORTHEAST_ATTACK_RAYS`] (when handling bishops/queens)
/// - [`NORTHWEST_ATTACK_RAYS`] (when handling bishops/queens)
///
/// Second argument should contain a [`bitboard::Bitboard`] that stores all squares that are
/// occupied on the board.
///
/// Third argument should contain a [`bitboard::Bitboard`] that stores all squares occupied
/// by enemy:
/// - rooks and queens if [`NORTH_ATTACK_RAYS`] or [`EAST_ATTACK_RAYS`]
///     was given as the first argument
/// - bishops and queens if either [`NORTHWEST_ATTACK_RAYS`] or
/// [`NORTHEAST_ATTACK_RAYS`] was given as the first argument
///
/// Last argument should contain the index of the square occupied by the piece which is
/// being checked for being attacked.
///
macro_rules! positive_ray_attacks_piece {
    ($rays:ident, $all_taken:ident, $enemy_attacking_pieces:ident, $index:expr) => {{
        let attack = $crate::bitboard::Bitboard::from($rays[$index]);
        let blocker = attack & $all_taken;
        if blocker.count_set() != 0 {
            let blocker_index = blocker.bitscan_forward();
            let blocker_square = $crate::square::Square::from(blocker_index as u8);
            $enemy_attacking_pieces.is_set(blocker_square)
        } else {
            false
        }
    }};
}

/// Generates code that checks whether a negative ray cast from the given square
/// hits an unblocked enemy piece.
///
/// # How it works - an example
///
/// If there is a white king on e4, and there is an unobstructed path south, where
/// either a black rook or queen occupies some square, that rook/queen certainly attacks
/// the king.
///
/// ```compile_fail
/// let all_taken: bitboard::Bitboard = /* get all taken */;
/// let enemy_rooks_queens: bitboard::Bitboard =
///     /* calculate all enemy rooks and queens squares */;
/// let king_index = square::Square::try_from("e4").unwrap().get_index();
/// let is_in_check = positive_ray_attacks_piece!(
///     SOUTH_ATTACK_RAYS,
///     all_taken,
///     enemy_rooks_queens,
///     king_index);
/// ```
///
/// # Arguments
///
/// First argument should always contain a reference to an array of negative rays. These are:
/// - [`SOUTH_ATTACK_RAYS`] (when handling rooks/queens)
/// - [`WEST_ATTACK_RAYS`] (when handling rooks/queens)
/// - [`SOUTHEAST_ATTACK_RAYS`] (when handling bishops/queens)
/// - [`SOUTHWEST_ATTACK_RAYS`] (when handling bishops/queens)
///
/// Second argument should contain a [`bitboard::Bitboard`] that stores all squares that are
/// occupied on the board.
///
/// Third argument should contain a [`bitboard::Bitboard`] that stores all squares occupied
/// by enemy:
/// - rooks and queens if [`SOUTH_ATTACK_RAYS`] or [`WEST_ATTACK_RAYS`]
///     was given as the first argument
/// - bishops and queens if either [`SOUTHWEST_ATTACK_RAYS`] or
/// [`SOUTHEAST_ATTACK_RAYS`] was given as the first argument
///
/// Last argument should contain the index of the square occupied by the piece which is
/// being checked for being attacked.
///
macro_rules! negative_ray_attacks_piece {
    ($rays:ident, $all_taken:ident, $enemy_attacking_pieces:ident, $index:expr) => {{
        let attack = $crate::bitboard::Bitboard::from($rays[$index]);
        let blocker = attack & $all_taken;
        if blocker.count_set() != 0 {
            let blocker_index = blocker.bitscan_reverse();
            let blocker_square = $crate::square::Square::from(blocker_index as u8);
            $enemy_attacking_pieces.is_set(blocker_square)
        } else {
            false
        }
    }};
}

/// Checks if the `square` occupied by a piece of `piece_color` is being attacked
/// on the given [`board::Board`].
///
/// This function backtraces from the piece's position to find any unblocked enemy pieces
/// which attack it.
///
/// # Panics
/// This function panics if there is no piece of the given color on the board.
#[inline(always)]
pub fn is_square_attacked(
    square: square::Square,
    piece_color: piece::Color,
    board: &board::Board,
) -> bool {
    let index = square.get_index();

    let (enemy_color, own_taken, enemy_taken) = match piece_color {
        piece::Color::White => (
            piece::Color::Black,
            board.get_squares_taken(piece::Color::White),
            board.get_squares_taken(piece::Color::Black),
        ),
        piece::Color::Black => (
            piece::Color::White,
            board.get_squares_taken(piece::Color::Black),
            board.get_squares_taken(piece::Color::White),
        ),
    };

    let all_taken = *own_taken | *enemy_taken;

    // =================CHECK ENEMY KNIGHTS ============================
    let knight_piece = piece::Piece::new(piece::Kind::Knight, enemy_color);
    let enemy_knights = *board.get_piece_bitboard(&knight_piece);
    // find all squares around the piece from which enemy knights could attack
    let knight_backward_attack = bitboard::Bitboard::from(KNIGHT_ATTACK_PATTERNS[index]);
    // if there is even a single common bit between knight_backward_attack and enemy_knights,
    // it means that the piece is being attacked
    let knight_backward_attack = knight_backward_attack & enemy_knights;
    if knight_backward_attack.count_set() != 0 {
        return true;
    }

    // =================CHECK FILES AND RANKS ============================
    let queen_piece = piece::Piece::new(piece::Kind::Queen, enemy_color);
    let rook_piece = piece::Piece::new(piece::Kind::Rook, enemy_color);
    // create a bitboard that holds info where enemy rooks and queens are, because these
    // are the pieces that can attack on files and ranks
    let enemy_rooks_queens =
        *board.get_piece_bitboard(&queen_piece) | *board.get_piece_bitboard(&rook_piece);

    if positive_ray_attacks_piece!(NORTH_ATTACK_RAYS, all_taken, enemy_rooks_queens, index)
        || negative_ray_attacks_piece!(SOUTH_ATTACK_RAYS, all_taken, enemy_rooks_queens, index)
        || positive_ray_attacks_piece!(EAST_ATTACK_RAYS, all_taken, enemy_rooks_queens, index)
        || negative_ray_attacks_piece!(WEST_ATTACK_RAYS, all_taken, enemy_rooks_queens, index)
    {
        return true;
    }

    // ================= CHECK DIAGONALS ============================
    let bishop_piece = piece::Piece::new(piece::Kind::Bishop, enemy_color);
    // create a bitboard that holds info where enemy bishops and queens are, because these
    // are the pieces that can attack on diagonals
    let enemy_diagonals =
        *board.get_piece_bitboard(&queen_piece) | *board.get_piece_bitboard(&bishop_piece);

    if positive_ray_attacks_piece!(NORTHEAST_ATTACK_RAYS, all_taken, enemy_diagonals, index)
        || positive_ray_attacks_piece!(NORTHWEST_ATTACK_RAYS, all_taken, enemy_diagonals, index)
        || negative_ray_attacks_piece!(SOUTHEAST_ATTACK_RAYS, all_taken, enemy_diagonals, index)
        || negative_ray_attacks_piece!(SOUTHWEST_ATTACK_RAYS, all_taken, enemy_diagonals, index)
    {
        return true;
    }

    // ================= CHECK KINGS ============================
    let king_piece = piece::Piece::new(piece::Kind::King, enemy_color);
    let enemy_kings = *board.get_piece_bitboard(&king_piece);
    // find all squares which the king attacks
    let king_attack = bitboard::Bitboard::from(KING_ATTACK_PATTERNS[index]);
    let king_attack = king_attack & enemy_kings;
    if king_attack.count_set() != 0 {
        return true;
    }

    // ================= CHECK PAWNS ============================
    let pawn_piece = piece::Piece::new(piece::Kind::Pawn, enemy_color);
    let enemy_pawns = *board.get_piece_bitboard(&pawn_piece);
    let pawn_attack_pattern = match piece_color {
        piece::Color::White => WHITE_PAWN_ATTACK_PATTERNS[index],
        piece::Color::Black => BLACK_PAWN_ATTACK_PATTERNS[index],
    };
    let pawn_attack_pattern = bitboard::Bitboard::from(pawn_attack_pattern);
    let pawn_attack = pawn_attack_pattern & enemy_pawns;
    if pawn_attack.count_set() != 0 {
        return true;
    }

    // if there is not a single piece that attacks the checked square
    false
}

/// Checks if the king of particular color is in check on the given [`board::Board`].
///
/// This function backtraces from the king's position to find any unblocked enemy pieces
/// which attack it.
///
/// # Panics
/// This function panics if there is no king of the given color on the board.
#[inline(always)]
pub fn is_king_in_check(king_color: piece::Color, board: &board::Board) -> bool {
    let king_piece = piece::Piece::new(piece::Kind::King, king_color);
    // expect the king to ALWAYS be on the board
    let king_square = board.get_piece_bitboard(&king_piece).iter().next().unwrap();
    is_square_attacked(king_square, king_color, &board)
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

    /// Extracts target squares from each item from [`MoveIter`].
    fn extract_targets(m: MoveIter) -> Vec<square::Square> {
        let mut result = Vec::with_capacity(m.len());
        for mv in m {
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
            let (white_taken, black_taken) = $board.get_squares_taken_pair();
            let square = $crate::square::Square::try_from($square).unwrap();
            let found_moves = $crate::movegen::find_pawn_moves(
                square,
                $color,
                white_taken,
                black_taken,
                $context,
            );
            assert_eq!(found_moves.len(), $targets.len());
            let targets = $crate::movegen::tests::extract_targets(found_moves);
            let expected_targets = $crate::movegen::tests::notation_to_squares($targets);
            for target in expected_targets {
                assert!(targets.contains(&target));
            }
        };
        ($color:ident $square:literal on $board:ident having $context:ident cannot be moved) => {
            let (white_taken, black_taken) = $board.get_squares_taken_pair();
            let square = $crate::square::Square::try_from($square).unwrap();
            let found_moves = $crate::movegen::find_pawn_moves(
                square,
                $color,
                white_taken,
                black_taken,
                $context,
            );
            assert_eq!(found_moves.len(), 0);
        };
        ($color:ident $square:literal on $board:ident having $context:ident can be promoted on $targets:ident) => {
            let (white_taken, black_taken) = $board.get_squares_taken_pair();
            let square = $crate::square::Square::try_from($square).unwrap();
            let found_moves = $crate::movegen::find_pawn_moves(
                square,
                $color,
                white_taken,
                black_taken,
                $context,
            );
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

            for found_move in found_moves {
                assert!(expected_moves.contains(&found_move));
            }
        };
    }

    macro_rules! check_knight {
        (number of correct moves of $color:ident $square:ident on $board:ident is $num:expr) => {
            let (white_taken, black_taken) = $board.get_squares_taken_pair();
            let square = $crate::square::Square::try_from($square).unwrap();
            let found_moves =
                $crate::movegen::find_knight_moves(square, $color, white_taken, black_taken);
            assert_eq!(found_moves.len(), $num);

            // get (file, rank) indexes of the knight's square
            let (file_i, rank_i) = (square.get_file().index(), square.get_rank().index());
            let (file_i, rank_i) = (file_i as i8, rank_i as i8);

            let targets = $crate::movegen::tests::extract_targets(found_moves);

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

            let (white_taken, black_taken) = $board.get_squares_taken_pair();
            let square = $crate::square::Square::try_from($square).unwrap();
            let found_moves = $crate::movegen::find_king_moves(
                square,
                $color,
                white_taken,
                black_taken,
                &context,
            );
            assert_eq!(found_moves.len(), $num);

            // get (file, rank) indexes of the king's square
            let (file_i, rank_i) = (square.get_file().index(), square.get_rank().index());
            let (file_i, rank_i) = (file_i as i8, rank_i as i8);

            let targets = $crate::movegen::tests::extract_targets(found_moves);

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
            let (white_taken, black_taken) = $board.get_squares_taken_pair();
            let (king_square, kingside_target, queenside_target) = match $color {
                $crate::piece::Color::White => ("e1", "g1", "c1"),
                $crate::piece::Color::Black => ("e8", "g8", "c8"),
            };

            let square = $crate::square::Square::try_from(king_square).unwrap();
            let found_moves = $crate::movegen::find_king_moves(
                square,
                $color,
                white_taken,
                black_taken,
                $context,
            );

            let targets = $crate::movegen::tests::extract_targets(found_moves);

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
            let (white_taken, black_taken) = $board.get_squares_taken_pair();
            let square = $crate::square::Square::try_from($square).unwrap();
            let found_moves =
                $crate::movegen::find_rook_moves(square, $color, white_taken, black_taken);
            assert_eq!(found_moves.len(), $num);

            // get (file, rank) indexes of the rook's square
            let (file_i, rank_i) = (square.get_file().index(), square.get_rank().index());
            let (file_i, rank_i) = (file_i as i8, rank_i as i8);

            let targets = $crate::movegen::tests::extract_targets(found_moves);

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
            let (white_taken, black_taken) = $board.get_squares_taken_pair();
            let square = $crate::square::Square::try_from($square).unwrap();
            let found_moves =
                $crate::movegen::find_rook_moves(square, $color, white_taken, black_taken);
            assert_eq!(found_moves.len(), $targets.len());
            let targets = $crate::movegen::tests::extract_targets(found_moves);
            let expected_targets = $crate::movegen::tests::notation_to_squares($targets);
            for target in expected_targets {
                assert!(targets.contains(&target));
            }
        };
    }

    macro_rules! check_bishop {
        (number of correct moves of $color:ident $square:ident on $board:ident is $num:expr) => {
            let (white_taken, black_taken) = $board.get_squares_taken_pair();
            let square = $crate::square::Square::try_from($square).unwrap();
            let found_moves =
                $crate::movegen::find_bishop_moves(square, $color, white_taken, black_taken);
            assert_eq!(found_moves.len(), $num);

            // get (file, rank) indexes of the rook's square
            let (file_i, rank_i) = (square.get_file().index(), square.get_rank().index());
            let (file_i, rank_i) = (file_i as i8, rank_i as i8);

            let targets = $crate::movegen::tests::extract_targets(found_moves);

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
            let (white_taken, black_taken) = $board.get_squares_taken_pair();
            let square = $crate::square::Square::try_from($square).unwrap();
            let found_moves =
                $crate::movegen::find_bishop_moves(square, $color, white_taken, black_taken);
            assert_eq!(found_moves.len(), $targets.len());
            let targets = $crate::movegen::tests::extract_targets(found_moves);
            let expected_targets = $crate::movegen::tests::notation_to_squares($targets);
            for target in expected_targets {
                assert!(targets.contains(&target));
            }
        };
    }

    macro_rules! check_queen {
        (number of correct moves of $color:ident $square:ident on $board:ident is $num:expr) => {
            let (white_taken, black_taken) = $board.get_squares_taken_pair();
            let square = $crate::square::Square::try_from($square).unwrap();
            let found_moves = $crate::movegen::find_queen_moves(square, $color, white_taken, black_taken);
            assert_eq!(found_moves.len(), $num);

            // get (file, rank) indexes of the rook's square
            let (file_i, rank_i) = (square.get_file().index(), square.get_rank().index());
            let (file_i, rank_i) = (file_i as i8, rank_i as i8);

            let targets = $crate::movegen::tests::extract_targets(found_moves);

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
            let (white_taken, black_taken) = $board.get_squares_taken_pair();
            let square = $crate::square::Square::try_from($square).unwrap();
            let found_moves =
                $crate::movegen::find_queen_moves(square, $color, white_taken, black_taken);
            assert_eq!(found_moves.len(), $targets.len());
            let targets = $crate::movegen::tests::extract_targets(found_moves);
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

    #[test]
    fn is_king_in_check_knight_attacks() {
        // white king always on d5
        let fens = [
            "7k/8/8/3K4/1n6/8/8/8", // black knight on b4
            "7k/8/1n6/3K4/8/8/8/8", // black knight on b6
            "7k/8/8/3K4/8/2n5/8/8", // black knight on c3
            "7k/2n5/8/3K4/8/8/8/8", // black knight on c7
            "7k/8/8/3K4/8/4n3/8/8", // black knight on e3
            "7k/4n3/8/3K4/8/8/8/8", // black knight on e7
            "7k/8/8/3K4/5n2/8/8/8", // black knight on f4
            "7k/8/5n2/3K4/8/8/8/8", // black knight on f6
        ];

        let white = piece::Color::White;
        for fen in fens {
            let board = board::Board::try_from(fen).unwrap();
            assert!(is_king_in_check(white, &board));
        }

        // black king always on d5
        let fens = [
            "7K/8/8/3k4/1N6/8/8/8", // white knight on b4
            "7K/8/1N6/3k4/8/8/8/8", // white knight on b6
            "7K/8/8/3k4/8/2N5/8/8", // white knight on c3
            "7K/2N5/8/3k4/8/8/8/8", // white knight on c7
            "7K/8/8/3k4/8/4N3/8/8", // white knight on e3
            "7K/4N3/8/3k4/8/8/8/8", // white knight on e7
            "7K/8/8/3k4/5N2/8/8/8", // white knight on f4
            "7K/8/5N2/3k4/8/8/8/8", // white knight on f6
        ];

        let black = piece::Color::Black;
        for fen in fens {
            let board = board::Board::try_from(fen).unwrap();
            assert!(is_king_in_check(black, &board));
        }
    }

    #[test]
    fn is_king_in_check_rank_and_file_attacks() {
        // white king always on d4
        let fens = [
            "7k/3r4/8/8/3K4/8/8/8", // black rook on d7
            "7k/8/8/8/3K4/8/8/3r4", // black rook on d1
            "7k/8/8/8/r2K4/8/8/8",  // black rook on a4
            "7k/8/8/8/3K3r/8/8/8",  // black rook on h4
            "7k/3q4/8/8/3K4/8/8/8", // black queen on d7
            "7k/8/8/8/3K4/8/8/3q4", // black queen on d1
            "7k/8/8/8/q2K4/8/8/8",  // black queen on a4
            "7k/8/8/8/3K3q/8/8/8",  // black queen on h4
        ];

        let white = piece::Color::White;
        for fen in fens {
            let board = board::Board::try_from(fen).unwrap();
            assert!(is_king_in_check(white, &board));
        }

        // black king always on d4
        let fens = [
            "7K/3R4/8/8/3k4/8/8/8", // white rook on d7
            "7K/8/8/8/3k4/8/8/3R4", // white rook on d1
            "7K/8/8/8/R2k4/8/8/8",  // white rook on a4
            "7K/8/8/8/3k3R/8/8/8",  // white rook on h4
            "7K/3Q4/8/8/3k4/8/8/8", // white queen on d7
            "7K/8/8/8/3k4/8/8/3Q4", // white queen on d1
            "7K/8/8/8/Q2k4/8/8/8",  // white queen on a4
            "7K/8/8/8/3k3Q/8/8/8",  // white queen on h4
        ];

        let black = piece::Color::Black;
        for fen in fens {
            let board = board::Board::try_from(fen).unwrap();
            assert!(is_king_in_check(black, &board));
        }
    }

    #[test]
    fn is_king_in_check_diagonal_attacks() {
        // white king always on e4
        let fens = [
            "b6k/8/8/8/4K3/8/8/8",  // black bishop on a8
            "7k/8/8/8/4K3/8/8/1b6", // black bishop on b1
            "7k/8/8/8/4K3/8/8/7b",  // black bishop on h1
            "7k/7b/8/8/4K3/8/8/8",  // black bishop on h7
            "q6k/8/8/8/4K3/8/8/8",  // black queen o a8
            "7k/8/8/8/4K3/8/8/1q6", // black queen on b1
            "7k/8/8/8/4K3/8/8/7q",  // black queen on h1
            "7k/7q/8/8/4K3/8/8/8",  // black queen on h7
        ];

        let white = piece::Color::White;
        for fen in fens {
            let board = board::Board::try_from(fen).unwrap();
            assert!(is_king_in_check(white, &board));
        }

        // black king always on e4
        let fens = [
            "B6K/8/8/8/4k3/8/8/8",  // white bishop on a8
            "7K/8/8/8/4k3/8/8/1B6", // white bishop on b1
            "7K/8/8/8/4k3/8/8/7B",  // white bishop on h1
            "7K/7B/8/8/4k3/8/8/8",  // white bishop on h7
            "Q6K/8/8/8/4k3/8/8/8",  // white queen o a8
            "7K/8/8/8/4k3/8/8/1Q6", // white queen on b1
            "7K/8/8/8/4k3/8/8/7Q",  // white queen on h1
            "7K/7Q/8/8/4k3/8/8/8",  // white queen on h7
        ];

        let black = piece::Color::Black;
        for fen in fens {
            let board = board::Board::try_from(fen).unwrap();
            assert!(is_king_in_check(black, &board));
        }
    }

    #[test]
    fn is_king_in_check_king_attacks() {
        // white king on d5
        let fens = [
            "8/8/8/8/4K3/3k4/8/8", // black king on d3
            "8/8/8/8/3kK3/8/8/8",  // black king on d4
            "8/8/8/3k4/4K3/8/8/8", // black king on d5
            "8/8/8/8/4K3/4k3/8/8", // black king on e3
            "8/8/8/4k3/4K3/8/8/8", // black king on e5
            "8/8/8/8/4K3/5k2/8/8", // black king on f3
            "8/8/8/8/4Kk2/8/8/8",  // black king on f4
            "8/8/8/5k2/4K3/8/8/8", // black king on f5
        ];

        let white = piece::Color::White;
        for fen in fens {
            let board = board::Board::try_from(fen).unwrap();
            assert!(is_king_in_check(white, &board));
        }

        // black king on d5
        let fens = [
            "8/8/8/8/4k3/3K4/8/8", // white king on d3
            "8/8/8/8/3Kk3/8/8/8",  // white king on d4
            "8/8/8/3K4/4k3/8/8/8", // white king on d5
            "8/8/8/8/4k3/4K3/8/8", // white king on e3
            "8/8/8/4K3/4k3/8/8/8", // white king on e5
            "8/8/8/8/4k3/5K2/8/8", // white king on f3
            "8/8/8/8/4kK2/8/8/8",  // white king on f4
            "8/8/8/5K2/4k3/8/8/8", // white king on f5
        ];

        let black = piece::Color::Black;
        for fen in fens {
            let board = board::Board::try_from(fen).unwrap();
            assert!(is_king_in_check(black, &board));
        }
    }

    #[test]
    fn is_king_in_check_pawn_attacks() {
        // white king getting checked
        let fens = [
            "8/8/8/5k2/8/8/p7/1K6",
            "8/8/8/5k2/8/8/2p5/1K6",
            "8/8/8/5k2/8/8/7p/6K1",
            "8/8/8/5k2/8/8/5p2/6K1",
            "2p5/1K6/8/5k2/8/8/8/8",
        ];

        let white = piece::Color::White;
        for fen in fens {
            let board = board::Board::try_from(fen).unwrap();
            assert!(is_king_in_check(white, &board));
        }

        // black king getting checked
        let fens = [
            "6k1/7P/8/8/8/8/8/K7",
            "6k1/5P2/8/8/8/8/8/K7",
            "1k6/2P5/8/8/8/8/8/K7",
            "1k6/P7/8/8/8/8/8/K7",
            "8/8/8/3k4/2P5/8/8/K7",
        ];

        let black = piece::Color::Black;
        for fen in fens {
            let board = board::Board::try_from(fen).unwrap();
            assert!(is_king_in_check(black, &board));
        }
    }

    #[test]
    fn is_king_in_check_blocked_attacks() {
        // white kings getting attacked through other blocking pieces
        let fens = [
            "rnb1kbnr/pppp1ppp/8/4p3/4P2q/3P4/PPP2PPP/RNBQKBNR", // blocked by pawn
            "8/5K2/8/5B2/1k6/8/5r2/8",                           // blocked by bishop
            "8/1q2PK2/8/8/1k6/8/8/8",                            // blocked by pawn
        ];
        let white = piece::Color::White;
        for fen in fens {
            let board = board::Board::try_from(fen).unwrap();
            assert!(!is_king_in_check(white, &board));
        }

        // black kings getting attacked through other blocking pieces
        let fens = [
            "rnbqkbnr/pppp1ppp/8/1B2p3/4P3/8/PPPP1PPP/RNBQK1NR", // blocked by pawn
            "8/5k2/8/5b2/1K6/8/5R2/8",                           // blocked by bishop
            "8/1Q2pk2/8/8/1K6/8/8/8",                            // blocked by pawn
        ];
        let black = piece::Color::Black;
        for fen in fens {
            let board = board::Board::try_from(fen).unwrap();
            assert!(!is_king_in_check(black, &board));
        }
    }

    #[test]
    fn moveiter_iterates_over_empty() {
        let start_square = square::Square::try_from("d7").unwrap();
        let targets = bitboard::Bitboard::default();

        let mut iter1 = MoveIter::new(start_square, targets, false);
        let mut iter2 = MoveIter::new(start_square, targets, true);
        assert_eq!(iter1.next(), None);
        assert_eq!(iter2.next(), None);
    }

    #[test]
    fn moveiter_iterates_over_nonpromoting_squares() {
        let start_square = square::Square::try_from("e2").unwrap();
        let mut targets = bitboard::Bitboard::default();

        targets.set(square::Square::try_from("e3").unwrap());
        targets.set(square::Square::try_from("e4").unwrap());

        let iter = MoveIter::new(start_square, targets, false);

        let all_found_moves = iter.collect::<Vec<moves::UCIMove>>();
        let expected_moves: Vec<moves::UCIMove> = ["e2e3", "e2e4"]
            .iter_mut()
            .map(|mv| moves::UCIMove::try_from(*mv).unwrap())
            .collect();

        assert_eq!(expected_moves.len(), all_found_moves.len());

        for expected_move in expected_moves {
            assert!(all_found_moves.contains(&expected_move));
        }
    }

    #[test]
    fn moveiter_iterates_over_promoting_squares() {
        let start_square = square::Square::try_from("d7").unwrap();
        let mut targets = bitboard::Bitboard::default();

        targets.set(square::Square::try_from("c8").unwrap());
        targets.set(square::Square::try_from("d8").unwrap());

        let iter = MoveIter::new(start_square, targets, true);

        let all_found_moves = iter.collect::<Vec<moves::UCIMove>>();
        let expected_moves: Vec<moves::UCIMove> = [
            "d7c8n", "d7c8b", "d7c8q", "d7c8r", "d7d8n", "d7d8b", "d7d8q", "d7d8r",
        ]
        .iter_mut()
        .map(|mv| moves::UCIMove::try_from(*mv).unwrap())
        .collect();

        assert_eq!(expected_moves.len(), all_found_moves.len());

        for expected_move in expected_moves {
            assert!(all_found_moves.contains(&expected_move));
        }
    }

    #[test]
    fn moveiter_nonpromoting_len_returns_exact_remaining_length() {
        let start_square = square::Square::try_from("e2").unwrap();
        let mut targets = bitboard::Bitboard::default();

        targets.set(square::Square::try_from("e3").unwrap());
        targets.set(square::Square::try_from("e4").unwrap());
        targets.set(square::Square::try_from("e5").unwrap());

        let mut iter = MoveIter::new(start_square, targets, false);
        let mut sum_all_sizes = iter.len();

        // sum of infinite series up to 3, all size_hint results added together
        // should amount to this number
        let expected_sum = 6;

        while let Some(_) = iter.next() {
            let size = iter.len();
            sum_all_sizes += size;
        }
        assert_eq!(expected_sum, sum_all_sizes);
    }

    #[test]
    fn moveiter_promoting_len_returns_exact_remaining_length() {
        let start_square = square::Square::try_from("d7").unwrap();
        let mut targets = bitboard::Bitboard::default();

        targets.set(square::Square::try_from("c8").unwrap());
        targets.set(square::Square::try_from("d8").unwrap());
        targets.set(square::Square::try_from("e8").unwrap());

        let mut iter = MoveIter::new(start_square, targets, true);
        let mut sum_all_sizes = iter.len();

        // sum of infinite series up to 12, all size_hint results added together
        // should amount to this number
        let expected_sum = 78;

        while let Some(_) = iter.next() {
            let size = iter.len();
            sum_all_sizes += size;
        }
        assert_eq!(expected_sum, sum_all_sizes);
    }
}
