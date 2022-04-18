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
#[derive(Copy, Clone)]
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

pub struct SquareIter {
    bits: u64,
    shift: u8,
    size: u8,
}

impl SquareIter {
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
                    break Some(square)
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

        let collected = 
            iterator.collect::<std::collections::HashSet<square::Square>>();

        assert!(collected.contains(&square::Square::try_from("a1").unwrap()));
        assert!(collected.contains(&square::Square::try_from("a8").unwrap()));
        assert!(collected.contains(&square::Square::try_from("h1").unwrap()));
        assert!(collected.contains(&square::Square::try_from("h8").unwrap()));        
    }
}
