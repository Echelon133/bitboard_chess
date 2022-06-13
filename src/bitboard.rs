//! This module implements a data structure that's the foundation of
//! the entire project, as it is used throughout it for multiple different purposes, all
//! of which require very fast performance.
//!
//! Some of these purposes are:
//! * representing all squares occupied by a color
//! * representing all squares occupied by a piece of certain kind
//! * representing all squares attacked by a piece
//! * representing attack patterns of pieces
//!
//! # Possibilities
//!
//! Some functionalities that are implemented in this module:
//! * find the index of the first bit that's set to 1
//! * count all bits set to 1
//! * find all bits that are common between two bitboards
//! * sum bits of two bitboards
//! * negate all bits of a bitboard
//!
//! # More information
//! * [Chess bitboards on Wikipedia](https://en.wikipedia.org/wiki/Bitboard#Chess_bitboards)
//! * [Chess bitboards on Chessprogramming](https://www.chessprogramming.org/Bitboards)

use std::{
    fmt::Debug,
    ops::{BitAnd, BitOr, BitXor, Not, Shl, Shr},
    u8,
};

use crate::square;

/// A bitboard which stores information about 64 squares of a chessboard.
///
/// The entire bitboard fits in 8 bytes, where each square is
/// represented by a single bit.
///
/// # Mapping of squares to bits
/// The ordering of bits is important, and in this implementation square:
/// - 'a1' is 0b0001 (only the least significant bit)
/// - 'b1' is 0b0010
/// - 'c1' is 0b0100
/// - 'd1' is 0b1000
/// - 'h8' is 0x8000000000000000 (only the most significant bit)
///
/// The indexes grow from left to right and bottom to top (from perspective in which the
/// 'a1' square is in the bottom left corner of the board).
/// Each [`square::Square`] holds its index into the bitboard.
///
/// # Example
///
/// ```
/// use bitboard_chess::bitboard::*;
/// use bitboard_chess::square::Square;
/// // creates an empty bitboard
/// let mut bitboard = Bitboard::default();
///
/// // set the 'a1' bit
/// bitboard.set(Square::try_from("a1").unwrap());
///
/// // set the 'h8' bit
/// bitboard.set(Square::try_from("h8").unwrap());
///
/// // check if the 'a1' bit is set
/// let is_a1_set = bitboard.is_set(Square::try_from("a1").unwrap());
/// assert!(is_a1_set);
///
/// // find the index of the first set bit (search starting from
/// // the least significant bit towards the most significant bit)
/// let index_forward = bitboard.bitscan_forward();
/// assert_eq!(index_forward, 0u8);
///
/// // find the index of the first set bit (search starting from
/// // the most significant bit towards the least significant bit)
/// let index_reverse = bitboard.bitscan_reverse();
/// assert_eq!(index_reverse, 63u8);
///
/// // get the iterator over the squares represented by lit bits
/// let mut iter = bitboard.iter();
/// assert_eq!(iter.next(), Some(Square::try_from("a1").unwrap()));
/// assert_eq!(iter.next(), Some(Square::try_from("h8").unwrap()));
/// assert_eq!(iter.next(), None);
///
/// // clear the 'a1' bit
/// bitboard.clear(Square::try_from("a1").unwrap());
///
/// // count all set bits
/// let all_set = bitboard.count_set();
/// assert_eq!(all_set, 1u8);
/// ```
///
#[derive(Copy, Clone, PartialEq)]
pub struct Bitboard {
    bits: u64,
}

impl Bitboard {
    /// Returns `true` if the `square` is set on the bitboard. Otherwise
    /// returns `false`.
    pub fn is_set(&self, square: square::Square) -> bool {
        ((self.bits >> square.get_index()) & 0b1) == 1
    }

    /// Sets the bit representing the given `square`.
    pub fn set(&mut self, square: square::Square) {
        self.bits |= 0b1 << square.get_index()
    }

    /// Clears the bit representing the given `square`.
    pub fn clear(&mut self, square: square::Square) {
        self.bits &= !(0b1 << square.get_index())
    }

    /// Counts how many bits of the bitboard are set.
    pub fn count_set(&self) -> u8 {
        self.bits.count_ones() as u8
    }

    /// Returns the `u64` which internally represents the entire bitboard.
    pub fn get_bits(&self) -> u64 {
        self.bits
    }

    /// Returns an iterator which returns a single [`square::Square`] item per
    /// each square that's set on the bitboard.
    pub fn iter(&self) -> SquareIter {
        SquareIter::new(self)
    }

    /// Returns the index of the least significant bit set to 1.
    ///
    /// **NOTE**: To get usable results make sure that the bitboard
    /// actually has any bits set.
    pub fn bitscan_forward(&self) -> u8 {
        u64::trailing_zeros(self.get_bits()) as u8
    }

    /// Returns the index of the most significant bit set to 1.
    ///
    /// **NOTE**: To get usable results make sure that the bitboard
    /// actually has any bits set.
    pub fn bitscan_reverse(&self) -> u8 {
        (u64::leading_zeros(self.get_bits()) as u8) ^ 63
    }
}

impl From<u64> for Bitboard {
    /// Creates a bitboard that uses the `v` value as its initial state.
    ///
    /// To create a bitboard with all of bits set to 0, use `Bitboard::default`.
    fn from(v: u64) -> Self {
        Self { bits: v }
    }
}

impl BitAnd for Bitboard {
    type Output = Bitboard;

    /// Returns a bitboard holding the result of bitwise AND of bits of
    /// `self` and `rhs`.
    #[inline]
    fn bitand(self, rhs: Bitboard) -> Self::Output {
        Bitboard::from(self.get_bits() & rhs.get_bits())
    }
}

impl BitOr for Bitboard {
    type Output = Bitboard;

    /// Returns a bitboard holding the result of bitwise OR of bits of
    /// `self` and `rhs`.
    #[inline]
    fn bitor(self, rhs: Self) -> Self::Output {
        Bitboard::from(self.get_bits() | rhs.get_bits())
    }
}

impl Shl<u8> for Bitboard {
    type Output = Bitboard;
    /// Returns a bitboard with bits of `self` shifted left by `rhs` places.
    #[inline]
    fn shl(self, rhs: u8) -> Self::Output {
        Bitboard::from(self.get_bits() << rhs)
    }
}

impl Shr<u8> for Bitboard {
    type Output = Bitboard;
    /// Returns a bitboard with bits of `self` shifted right by `rhs` places.
    #[inline]
    fn shr(self, rhs: u8) -> Self::Output {
        Bitboard::from(self.get_bits() >> rhs)
    }
}

impl Not for Bitboard {
    type Output = Bitboard;
    /// Returns a bitboard with negated bits of `self`.
    #[inline]
    fn not(self) -> Self::Output {
        Bitboard::from(!self.get_bits())
    }
}

impl BitXor for Bitboard {
    type Output = Bitboard;
    /// Returns a bitboard holding the result of bitwise XOR of bits of
    /// `self` and `rhs`.
    #[inline]
    fn bitxor(self, rhs: Self) -> Self::Output {
        Bitboard::from(self.get_bits() ^ rhs.get_bits())
    }
}

impl Debug for Bitboard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for rank in (1..=8).rev() {
            // every rank consists of 8 bits, so we need to get to those that
            // represent the rank from this loop
            let rank_bits = ((self.bits >> ((rank - 1) * 8)) & 0b11111111) as u8;
            // since the least significant bit is always righmost in the number
            // but represents the leftmost square,
            // reverse the bits before displaying
            let rank_bits = rank_bits.reverse_bits();
            write!(f, "{} {:08b}\n", rank, rank_bits)?;
        }
        write!(f, "  abcdefgh")
    }
}

/// Iterator which returns a single [`square::Square`] per every bit that's set
/// on the bitboard.
#[derive(Clone, Copy)]
pub struct SquareIter {
    bits: u64,
    size: u8,
}

impl SquareIter {
    /// Creates an iterator over the squares set on the bitboard.
    pub fn new(bboard: &Bitboard) -> Self {
        Self {
            bits: bboard.get_bits(),
            size: bboard.count_set(),
        }
    }
}

impl Iterator for SquareIter {
    type Item = square::Square;

    /// Advances the iterator and returns the next [`square::Square`] that's
    /// set on the bitboard. This iterator guarantees that each next square
    /// has a bigger index than the previous square.
    ///
    /// The ordering guarantee means that:
    /// - 'a1' will always be given out before 'b1', 'b1' before 'c1', etc.
    /// - squares from rank '1' will always be given out before squares from rank '2', etc.
    fn next(&mut self) -> Option<Self::Item> {
        if self.size == 0 {
            None
        } else {
            // find the next set bit
            let first_set = u64::trailing_zeros(self.bits) as u8;
            // clear the bit after it's been found
            self.bits &= !(0b1 << first_set);
            self.size -= 1;
            let square = square::Square::from(first_set);
            Some(square)
        }
    }
}

impl ExactSizeIterator for SquareIter {
    fn len(&self) -> usize {
        self.size as usize
    }
}

impl Default for Bitboard {
    /// Returns a default bitboard with all bits set to 0.
    fn default() -> Self {
        Self { bits: 0u64 }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::bitboard::*;
    use crate::square;

    #[test]
    fn bitboard_default_is_empty() {
        let bitboard = Bitboard::default();

        assert_eq!(bitboard.get_bits(), 0);
    }

    #[test]
    fn bitboard_setting_bits_works() {
        let mut bitboard = Bitboard::default();

        let s1 = square::Square::try_from("b3").unwrap();
        let s2 = square::Square::try_from("g5").unwrap();
        let s3 = square::Square::try_from("a8").unwrap();

        bitboard.set(s1);
        assert!(bitboard.is_set(s1));
        assert!(!bitboard.is_set(s2));
        assert!(!bitboard.is_set(s3));

        bitboard.set(s2);
        assert!(bitboard.is_set(s1));
        assert!(bitboard.is_set(s2));
        assert!(!bitboard.is_set(s3));

        bitboard.set(s3);
        assert!(bitboard.is_set(s1));
        assert!(bitboard.is_set(s2));
        assert!(bitboard.is_set(s3));

        assert_eq!(bitboard.count_set(), 3);
    }

    #[test]
    fn bitboard_clearing_bits_works() {
        let mut bitboard = Bitboard::default();

        let s1 = square::Square::try_from("b3").unwrap();
        let s2 = square::Square::try_from("g5").unwrap();
        let s3 = square::Square::try_from("a8").unwrap();

        bitboard.set(s1);
        bitboard.set(s2);
        bitboard.set(s3);

        assert!(bitboard.is_set(s1));
        assert!(bitboard.is_set(s2));
        assert!(bitboard.is_set(s3));

        bitboard.clear(s1);
        bitboard.clear(s2);
        bitboard.clear(s3);

        assert_eq!(bitboard.get_bits(), 0);
    }

    #[test]
    fn bitboard_iterator_over_empty_works() {
        let bitboard = Bitboard::default();
        assert_eq!(bitboard.iter().next(), None);
    }

    #[test]
    fn bitboard_iterator_over_elements_works() {
        let mut bitboard = Bitboard::default();

        let s1 = square::Square::try_from("a1").unwrap();
        let s2 = square::Square::try_from("a8").unwrap();
        let s3 = square::Square::try_from("h1").unwrap();
        let s4 = square::Square::try_from("h8").unwrap();

        bitboard.set(s1);
        bitboard.set(s2);
        bitboard.set(s3);
        bitboard.set(s4);

        let iterator = bitboard.iter();

        let collected = iterator.collect::<std::collections::HashSet<square::Square>>();

        assert!(collected.contains(&square::Square::try_from("a1").unwrap()));
        assert!(collected.contains(&square::Square::try_from("a8").unwrap()));
        assert!(collected.contains(&square::Square::try_from("h1").unwrap()));
        assert!(collected.contains(&square::Square::try_from("h8").unwrap()));
    }

    #[test]
    fn bitboard_debug_is_correct_for_empty() {
        let bitboard = Bitboard::default();

        let expected_empty = r#"8 00000000
7 00000000
6 00000000
5 00000000
4 00000000
3 00000000
2 00000000
1 00000000
  abcdefgh"#;

        let empty_debug = format!("{:?}", bitboard);
        assert_eq!(expected_empty, empty_debug);
    }

    #[test]
    fn bitboard_debug_is_correct_for_corners() {
        let mut bitboard = Bitboard::default();

        let corners = ["a1", "h1", "a8", "h8"];
        for corner in corners {
            bitboard.set(square::Square::try_from(corner).unwrap());
        }

        let expected_corners = r#"8 10000001
7 00000000
6 00000000
5 00000000
4 00000000
3 00000000
2 00000000
1 10000001
  abcdefgh"#;

        let corners_debug = format!("{:?}", bitboard);
        assert_eq!(expected_corners, corners_debug);
    }

    #[test]
    fn bitboard_debug_is_correct_for_ranks() {
        let mut bitboard = Bitboard::default();

        for file in 'a'..='h' {
            for rank in ['1', '3', '5', '7'] {
                let square = format!("{}{}", file, rank);
                bitboard.set(square::Square::try_from(square.as_ref()).unwrap());
            }
        }

        let expected_ranks = r#"8 00000000
7 11111111
6 00000000
5 11111111
4 00000000
3 11111111
2 00000000
1 11111111
  abcdefgh"#;

        let ranks_debug = format!("{:?}", bitboard);
        assert_eq!(expected_ranks, ranks_debug);
    }

    #[test]
    fn bitboard_debug_is_correct_for_files() {
        let mut bitboard = Bitboard::default();

        for file in ['a', 'c', 'e', 'g'] {
            for rank in '1'..='8' {
                let square = format!("{}{}", file, rank);
                bitboard.set(square::Square::try_from(square.as_ref()).unwrap());
            }
        }

        let expected_files = r#"8 10101010
7 10101010
6 10101010
5 10101010
4 10101010
3 10101010
2 10101010
1 10101010
  abcdefgh"#;

        let files_debug = format!("{:?}", bitboard);
        assert_eq!(expected_files, files_debug);
    }

    #[test]
    fn bitboard_bitand_works() {
        let mut white_squares = Bitboard::default();
        // set every white square (so all squares from 0..=63 which have an even index)
        for index in 0..=63 {
            if index % 2 == 0 {
                let square = square::Square::from(index);
                white_squares.set(square);
            }
        }

        let mut black_squares = Bitboard::default();
        // set every black square (so all squares from 0..=63 which have an odd index)
        for index in 0..=63 {
            if index % 2 != 0 {
                let square = square::Square::from(index);
                black_squares.set(square);
            }
        }

        let bitwise_and_result = white_squares & black_squares;
        let bitwise_and_self = white_squares & white_squares;

        // bitwise AND when two bitboard have alternating ones and zeroes offset by 1
        // should result in simply 0, because there is no position where bits are set to 1
        // in both bitboards
        assert_eq!(bitwise_and_result.get_bits(), 0);
        // (white_squares AND white_squares) should result in white_squares
        assert_eq!(bitwise_and_self.get_bits(), white_squares.get_bits())
    }

    #[test]
    fn bitboard_bitor_works() {
        let mut white_squares = Bitboard::default();
        // set every white square (so all squares from 0..=63 which have an even index)
        for index in 0..=63 {
            if index % 2 == 0 {
                let square = square::Square::from(index);
                white_squares.set(square);
            }
        }

        let mut black_squares = Bitboard::default();
        // set every black square (so all squares from 0..=63 which have an odd index)
        for index in 0..=63 {
            if index % 2 != 0 {
                let square = square::Square::from(index);
                black_squares.set(square);
            }
        }

        let bitwise_or_result = white_squares | black_squares;

        // expect all 64 unique squares to be set to 1
        let set = bitwise_or_result
            .iter()
            .collect::<HashSet<square::Square>>();
        assert_eq!(set.len(), 64);
        assert_eq!(bitwise_or_result.get_bits(), u64::MAX);
    }

    #[test]
    fn bitboard_bitor_is_not_xor() {
        let mut white_squares = Bitboard::default();
        // set every white square (so all squares from 0..=63 which have an even index)
        for index in 0..=63 {
            if index % 2 == 0 {
                let square = square::Square::from(index);
                white_squares.set(square);
            }
        }

        let white_squares_clone = white_squares.clone();

        let bitwise_or_result = white_squares | white_squares_clone;

        // expect 32 unique squares to be set to 1
        let set = bitwise_or_result
            .iter()
            .collect::<HashSet<square::Square>>();
        assert_eq!(set.len(), 32);

        // a result of bitwise OR of the value with itself should be the value itself
        assert_eq!(white_squares, bitwise_or_result);
    }

    #[test]
    fn bitboard_shl_works() {
        let mut white_squares = Bitboard::default();
        // set a1 square and h7 square on the board
        white_squares.set(square::Square::try_from("a1").unwrap());
        white_squares.set(square::Square::try_from("h7").unwrap());

        // this should move a1 to a2, and h7 to h8
        let shift_left_entire_rank = white_squares << 8;

        let squares_set = shift_left_entire_rank
            .iter()
            .collect::<HashSet<square::Square>>();

        assert!(squares_set.contains(&square::Square::try_from("a2").unwrap()));
        assert!(squares_set.contains(&square::Square::try_from("h8").unwrap()));
    }

    #[test]
    fn bitboard_shr_works() {
        let mut black_squares = Bitboard::default();
        // set a8 square and h7 square on the board
        black_squares.set(square::Square::try_from("a8").unwrap());
        black_squares.set(square::Square::try_from("h7").unwrap());

        // this should move a8 to a7, and h7 to h6
        let shift_right_entire_rank = black_squares >> 8;

        let squares_set = shift_right_entire_rank
            .iter()
            .collect::<HashSet<square::Square>>();

        assert!(squares_set.contains(&square::Square::try_from("a7").unwrap()));
        assert!(squares_set.contains(&square::Square::try_from("h6").unwrap()));
    }

    #[test]
    fn bitboard_bitwise_not_works() {
        let mut bitboard = Bitboard::default();

        bitboard = !bitboard;

        assert_eq!(bitboard.get_bits(), 0xFFFF_FFFF_FFFF_FFFF);
    }

    #[test]
    fn bitboard_bitwise_xor_works() {
        // all ones
        let bitboard1 = Bitboard::from(0xFFFF_FFFF_FFFF_FFFF);

        // 0b101010101010101010... pattern
        let bitboard2 = Bitboard::from(0xAAAA_AAAA_AAAA_AAAA);

        let xor_itself = bitboard1 ^ bitboard1;
        let xor_alternating = bitboard1 ^ bitboard2;

        assert_eq!(xor_itself.get_bits(), 0);
        // XOR of repeating ones (1111) and alternating ones (1010) should
        // result in alternating ones (0101)
        assert_eq!(xor_alternating.get_bits(), 0x5555_5555_5555_5555);
    }

    #[test]
    fn bitboard_bitscan_forward_works() {
        for square_index in 0..=63 {
            let square = square::Square::from(square_index);
            // set the most significant bit for every bitboard
            // to make sure that the tested method returns
            // the index of the least sigificant bit that's set to 1
            let mut bitboard = Bitboard::from(0x8000_0000_0000_0000);
            bitboard.set(square);
            let index_of_set_bit = bitboard.bitscan_forward();
            assert_eq!(square_index, index_of_set_bit);
        }
    }

    #[test]
    fn bitboard_bitscan_reverse_works() {
        for square_index in 0..=63 {
            let square = square::Square::from(square_index);
            // set the least significant bit for every bitboard
            // to make sure that the tested method returns
            // the index of the most significant bit that's set to 1
            let mut bitboard = Bitboard::from(0b1);
            bitboard.set(square);
            let index_of_set_bit = bitboard.bitscan_reverse();
            assert_eq!(square_index, index_of_set_bit);
        }
    }
}
