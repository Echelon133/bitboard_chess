use std::fmt::Debug;

static FILES : [File; 8] = [
    File::A, File::B, File::C, File::D, 
    File::E, File::F, File::G, File::H
];

static RANKS : [Rank; 8] = [
    Rank::R1, Rank::R2, Rank::R3, Rank::R4,
    Rank::R5, Rank::R6, Rank::R7, Rank::R8,
];

/// Represents a file on the chessboard.
#[derive(Debug, Copy, Clone)]
pub enum File {
    A, B, C, D, E, F, G, H
}

impl File {
    /// Returns the index of the file.
    /// Indexes start from A, so the index of 
    /// A is 0, then B is 1, and so on.
    pub fn index(&self) -> u8 {
        *self as u8
    }

    /// Returns a char representing the file.
    pub fn as_char(&self) -> char {
        let chrs = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'];
        chrs[self.index() as usize]
    }
}

impl TryFrom<usize> for File {
    type Error = &'static str;

    /// Converts an index between 0 and 7 into a [`File`].
    /// This index is consistent with the value that is returned 
    /// when index() is called a specific file.
    fn try_from(value: usize) -> Result<Self, Self::Error> {
        if value >= 8 {
            Err("cannot convert a value this big to a file")        
        } else {
            Ok(FILES[value])
        }
    }
}

/// Represents a rank on the chessboard.
#[derive(Debug, Copy, Clone)]
pub enum Rank {
    R1, R2, R3, R4, R5, R6, R7, R8
}

impl Rank {
    /// Returns the index of the rank.
    /// Indexes start from the first rank, so the 
    /// index of R1 is 0, then R2 is 1, and so on.
    pub fn index(&self) -> u8 {
        *self as u8
    }

    /// Returns a char representing the rank.
    pub fn as_char(&self) -> char {
        let chrs = ['1', '2', '3', '4', '5', '6', '7', '8'];
        chrs[self.index() as usize]
    }
}

impl TryFrom<usize> for Rank {
    type Error = &'static str;
    
    /// Converts an index between 0 and 7 into a [`Rank`].
    /// This index is consistent with the value that is returned 
    /// when index() is called a specific rank.
    fn try_from(value: usize) -> Result<Self, Self::Error> {
        if value >= 8 {
            Err("cannot convert a value this big to a rank")        
        } else {
            Ok(RANKS[value])
        }
    }
}

/// Represents a single square on the board with precise coordinates specified 
/// by the Square's [`File`] and [`Rank`].
///
/// Square indexes should grow from 0 to 63.
/// Indexes of files grow from left to right, and indexes of ranks grow
/// from bottom to top. This means that:
/// 
/// - 'a1' has index 0
/// - 'b1' has index 1
/// - 'a8' has index 56
/// - 'h8' has index 63
///
/// Index of the square = (rank_index * 8) + file_index
///
/// Bottom-left corner of the board has (0, 0) indexes, when
/// top-right corner of the board has (7, 7) indexes.
#[derive(Clone, Copy)]
pub struct Square {
    square_index: u8,
}

impl Square {

    /// Creates an immutable [`Square`] that represents given [`Rank`] and [`File`]
    /// on the board.
    pub fn new(rank: Rank, file: File) -> Self {
        Self {
            square_index: (rank.index() * 8) + file.index(),
        }
    }

    /// Recalculates the index of the [`Rank`] of the square. Square only holds its 
    /// index on the board. Indexes of rank and file have to be calculated from
    /// the square index. Returned index is guaranteed to be within the range 0..=7.
    #[inline(always)]
    fn rank_index(&self) -> usize {
        (self.square_index / 8u8) as usize
    }
    
    /// Recalculates the index of the [`File`] of the square. Square only holds its 
    /// index on the board. Indexes of rank and file have to be calculated from
    /// the square index. Returned index is guaranteed to be within the range 0..=7.
    #[inline(always)]
    fn file_index(&self) -> usize {
        (self.square_index % 8u8) as usize
    }

    /// Returns the index which the square represents.
    pub fn get_index(&self) -> usize {
        self.square_index as usize
    }

    /// Returns a [`Rank`] of the square. Needs to recalculated on every call
    /// because the [`Square`] internally only stores its index.
    pub fn get_rank(&self) -> Rank {
        // unwrap right away, because it's guaranteed that the calculated index
        // represents a rank
        Rank::try_from(self.rank_index()).unwrap()
    }

    /// Returns a [`File`] of the square. Needs to be recalculated on every call
    /// because the [`Square`] internally only stores its index.
    pub fn get_file(&self) -> File {
        // unwrap right away, because it's guaranteed that the calculated index
        // represents a file
        File::try_from(self.file_index()).unwrap()
    }

    /// Returns a (rank, file) pair of indexes. For example:
    /// - 'a1' is represented as (0, 0)
    /// - 'b1' is represented as (0, 1)
    /// - 'h8' is represented as (7, 7)
    pub fn as_indexes(&self) -> (usize, usize) {
        (self.rank_index(), self.file_index())
    }
}

impl Debug for Square {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.get_file().as_char(), self.get_rank().as_char())   
    }
}

#[cfg(test)]
mod tests {
    use crate::square::*;

    #[test]
    fn files_have_correct_indexes() {
        let expected_order = [
            File::A, File::B, File::C, File::D, 
            File::E, File::F, File::G, File::H
        ];

        for (expected_index, file) in expected_order.into_iter().enumerate() {
            assert_eq!(expected_index, file.index() as usize);
        }
    }

    #[test]
    fn files_as_char_is_correct() {
        let expected_chars = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'];

        for (index, file) in FILES.iter().enumerate() {
            assert_eq!(expected_chars[index], file.as_char());
        }
    }

    #[test]
    fn files_try_from_correctly_checks_bounardies() {
        for index in 0usize..=7usize {
            assert!(File::try_from(index).is_ok());
        }

        for index in 8usize..=1000usize {
            assert!(File::try_from(index).is_err());
        }
    }

    #[test]
    fn ranks_have_correct_indexes() {
        let expected_order = [
            Rank::R1, Rank::R2, Rank::R3, Rank::R4,
            Rank::R5, Rank::R6, Rank::R7, Rank::R8,
        ];
        for (expected_index, rank) in expected_order.into_iter().enumerate() {
            assert_eq!(expected_index, rank.index() as usize);
        }
    }

    #[test]
    fn ranks_as_char_is_correct() {
        let expected_chars = ['1', '2', '3', '4', '5', '6', '7', '8'];

        for (index, rank) in RANKS.iter().enumerate() {
            assert_eq!(expected_chars[index], rank.as_char());
        }
    }

    #[test]
    fn ranks_try_from_correctly_checks_bounardies() {
        for index in 0usize..=7usize {
            assert!(Rank::try_from(index).is_ok());
        }

        for index in 8usize..=1000usize {
            assert!(Rank::try_from(index).is_err());
        }
    }

    #[test]
    fn square_index_calculated_correctly() {
        let mut counter = 0;

        for rank in RANKS.iter() {
            for file in FILES.iter() {
                let square = Square::new(*rank, *file);
                // indexes grow left-to-right and bottom-to-top 
                // so we are iterating in a way in which indexes should
                // grow by one
                assert_eq!(counter, square.get_index());
                counter += 1;
            }
        }
        // incremented by one after the last check, so should be 64 and not 63
        assert_eq!(counter, 64);
    }

    #[test]
    fn square_file_and_rank_indexes_recalculated_correctly() {
        for (rank_index, rank) in RANKS.iter().enumerate() {
            for (file_index, file) in FILES.iter().enumerate() {
                let square = Square::new(*rank, *file);
                let expected_pair = (rank_index, file_index);
                let pair = square.as_indexes();
                assert_eq!(expected_pair, pair);
            }
        }
    }
}
