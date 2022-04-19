use std::{fmt::Debug, ops::BitOr};

use crate::square;

/// Represents the entire chessboard in 8 bytes, where each square is
/// represented by a single bit.
///
/// The ordering of bits is important, and in this implementation:
/// - 'a1' is 0b0001 (the least significant bit)
/// - 'b1' is 0b0010
/// - 'c1' is 0b0100
/// - 'd1' is 0b1000
///
/// - 'h8' is 0x8000000000000000 (only the most significant bit)
///
/// The ordering goes left-to-right and bottom-to-top. [`square::Square`] holds
/// its index (in range 0..=63) which is basically the number of bits of the bitboard
/// that have to be shifted right to get to the bit that represents that square.
///
/// If we want to get the value of the bit that represents the 'b1' square,
/// we get the index of that square, which is 1. This means that to get the bit
/// that represents that square we shift bitboard's bits to the right once
/// and then we check whether the least significant bit after that shift is
/// equal to 1.
///
#[derive(Copy, Clone, PartialEq)]
pub struct Bitboard {
    bits: u64,
}

impl Bitboard {
    /// Returns a [`bool`] which informs if the bit of the given
    /// square is set.
    pub fn is_set(&self, square: square::Square) -> bool {
        ((self.bits >> square.get_index()) & 0b1) == 1
    }

    /// Sets the bit of the given square.
    pub fn set(&mut self, square: square::Square) {
        self.bits |= 0b1 << square.get_index()
    }

    /// Clears the bit of the given square.
    pub fn clear(&mut self, square: square::Square) {
        self.bits &= !(0b1 << square.get_index())
    }

    /// Counts how many bits are set in the bitboard.
    ///
    /// This can be useful for different things depending on the context.
    /// For example, if the bitboard represents squares taken by pieces of certain color,
    /// this method returns the number of all pieces available to a certain player.
    pub fn count_set(&self) -> u8 {
        self.bits.count_ones() as u8
    }

    /// Returns the u64 which internally represents the entire bitboard.
    pub fn get_bits(&self) -> u64 {
        self.bits
    }

    /// Returns an iterator which gives out [`square::Square`] set by this bitboard.
    pub fn iter(&self) -> SquareIter {
        SquareIter::new(self)
    }
}

impl From<u64> for Bitboard {
    fn from(v: u64) -> Self {
        Self { bits: v }
    }
}

impl BitOr<Self> for Bitboard {
    type Output = Bitboard;
    /// Returns bitwise OR of the bits of two bitboards.
    fn bitor(self, rhs: Self) -> Self::Output {
        Bitboard::from(self.get_bits() | rhs.get_bits())
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

/// Iterator which returns [`square::Square`] objects of all bits
/// that are set on the bitboard.
///
pub struct SquareIter {
    bits: u64,
    shift: u8,
    size: u8,
}

impl SquareIter {
    /// Creates an iterator over the squares set on the bitboard.
    pub fn new(bboard: &Bitboard) -> Self {
        Self {
            bits: bboard.get_bits(),
            shift: 0,
            size: bboard.count_set(),
        }
    }
}

impl Iterator for SquareIter {
    type Item = square::Square;

    /// Advances the iterator and returns the next [`square::Square`] that's
    /// set on the bitboard. Because of the way this iterator works, each next
    /// square is given out in order left-to-right and bottom-to-top.
    ///
    /// It's guaranteed that:
    /// - 'a1' will always be given out before 'b1', 'b1' before 'c1', etc.
    /// - squares from rank '1' will always be given out before squares from rank '2', etc.
    fn next(&mut self) -> Option<Self::Item> {
        if self.size == 0 {
            None
        } else {
            loop {
                // find the next set bit
                let is_set = ((self.bits >> self.shift) & 0b1) == 1;
                if is_set {
                    let square = square::Square::from(self.shift);
                    self.shift += 1;
                    self.size -= 1;
                    break Some(square);
                } else {
                    self.shift += 1;
                }
            }
        }
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

        //for file in 'a'..='h' {
        //    let square = format!("{}1", file);
        //    bitboard.set(square::Square::try_from(square.as_ref()).unwrap());
        //}
        //println!("{:?}", bitboard);
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
}
