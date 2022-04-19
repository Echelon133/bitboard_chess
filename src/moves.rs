use std::fmt::Debug;

use crate::square;

/// Represents a move between two squares.
pub struct Move {
    start: square::Square,
    target: square::Square,
}

impl Move {
    /// Returns a [`Move`] between start and target squares.
    pub fn new(start: square::Square, target: square::Square) -> Self {
        Self { start, target }
    }

    /// Returns the start square of the move.
    pub fn get_start(&self) -> square::Square {
        self.start
    }

    /// Returns the target square of the move.
    pub fn get_target(&self) -> square::Square {
        self.target
    }
}

impl TryFrom<&str> for Move {
    type Error = &'static str;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if !value.is_ascii() {
            Err("cannot create move from str containing non-ascii chars")
        } else if value.len() != 4 {
            Err("cannot create move from str that is not 4 chars long")
        } else {
            // value is guaranteed to have 4 ascii characters
            let start = &value[0..2];
            let target = &value[2..4];

            let start_square = match square::Square::try_from(start) {
                Ok(square) => square,
                Err(_) => {
                    return Err("invalid start square representation");
                }
            };

            let target_square = match square::Square::try_from(target) {
                Ok(square) => square,
                Err(_) => {
                    return Err("invalid target square representation");
                }
            };

            Ok(Move::new(start_square, target_square))
        }
    }
}

impl Debug for Move {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}{:?}", self.get_start(), self.get_target())
    }
}

#[cfg(test)]
mod tests {

    use crate::moves::*;
    use crate::square;

    #[test]
    fn move_try_from_str_rejects_incorrect_len() {
        let invalid = ["a", "aaaaa", "asdfasdfasdfasdf", "awr"];
        for s in invalid {
            let m = Move::try_from(s);
            assert!(m.is_err());
            assert_eq!(
                m.unwrap_err(),
                "cannot create move from str that is not 4 chars long"
            );
        }
    }

    #[test]
    fn move_try_from_str_rejects_nonascii_chars() {
        let nonascii = ["¡aaa", "©aaa", "¢bbb", "¼bbb"];
        for s in nonascii {
            let m = Move::try_from(s);
            assert!(m.is_err());
            assert_eq!(
                m.unwrap_err(),
                "cannot create move from str containing non-ascii chars"
            );
        }
    }

    #[test]
    fn move_try_from_str_accepts_valid_squares() {
        let values = [
            ("a1a2", "a1", "a2"),
            ("e2e4", "e2", "e4"),
            ("e1g1", "e1", "g1"),
            ("e7d8", "e7", "d8"),
        ];

        for (move_str, start_str, target_str) in values {
            let expected_start = square::Square::try_from(start_str).unwrap();
            let expected_target = square::Square::try_from(target_str).unwrap();

            let created_move = Move::try_from(move_str).unwrap();

            assert_eq!(created_move.get_start(), expected_start);
            assert_eq!(created_move.get_target(), expected_target);
        }
    }

    #[test]
    fn move_try_from_str_rejects_invalid_start_square() {
        for ch in 'i'..='z' {
            let s = format!("{}1a4", ch);
            let m = Move::try_from(s.as_ref());
            assert!(m.is_err());
            assert_eq!(m.unwrap_err(), "invalid start square representation");
        }
    }

    #[test]
    fn move_try_from_str_rejects_invalid_target_square() {
        for ch in 'i'..='z' {
            let s = format!("a1{}4", ch);
            let m = Move::try_from(s.as_ref());
            assert!(m.is_err());
            assert_eq!(m.unwrap_err(), "invalid target square representation");
        }
    }

    #[test]
    fn move_debug_is_correct() {
        let m = Move::try_from("e2e4").unwrap();
        let debug = format!("{:?}", m);
        assert_eq!("e2e4", debug);
    }
}
