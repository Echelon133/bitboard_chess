use std::fmt::Debug;

use crate::piece;
use crate::square;

/// Represents a side of castling.
#[derive(Debug, Copy, Clone, PartialEq, Hash, Eq)]
pub enum Side {
    Queenside,
    Kingside,
}

/// Stores the context of the board at some point in time. Holds information such as:
/// - which color makes the next move
/// - which color can castle (and to which side)
/// - if en-passant is possible, and if yes, then on which square
/// - how many halfmoves have been made
/// - how many fullmoves have been made
///
/// This context should be saved after every move, so that in case of some move's
/// reversal the previous context can be restored.
/// Without restoring the context, reversal of moves that change it
/// might cause the board to enter an invalid state.
///
#[derive(Clone, Copy, PartialEq, Hash, Eq)]
pub struct Context {
    castling_flags: u8,
    enpassant: Option<square::Square>,
    to_play: piece::Color,
    halfmoves: u8,
    fullmoves: u16,
}

impl Context {
    pub fn new(
        queenside_white: bool,
        kingside_white: bool,
        queenside_black: bool,
        kingside_black: bool,
        enpassant: Option<square::Square>,
        to_play: piece::Color,
        halfmoves: u8,
        fullmoves: u16,
    ) -> Self {
        let mut castling_flags = 0;
        if queenside_white {
            castling_flags |= 0b1000;
        }
        if kingside_white {
            castling_flags |= 0b0100;
        }
        if queenside_black {
            castling_flags |= 0b0010;
        }
        if kingside_black {
            castling_flags |= 0b0001;
        }

        Self {
            castling_flags,
            enpassant,
            to_play,
            halfmoves,
            fullmoves,
        }
    }

    /// Checks whether a certain color can castle on kingside or queenside.
    pub fn can_castle(&self, color: piece::Color, side: Side) -> bool {
        let mut shift_right = match color {
            piece::Color::White => 3,
            piece::Color::Black => 1,
        };

        // bit layout is: WHITE_QUEENSIDE, WHITE_KINGSIDE, BLACK_QUEENSIDE, BLACK_KINGSIDE
        // the initial shift points at either white queenside or black queenside,
        // so to get the kingside bit, in both cases the right shift has to be decreased by 1
        if side == Side::Kingside {
            shift_right -= 1;
        }
        ((self.castling_flags >> shift_right) & 0b1) == 1
    }

    /// Disables castling rights of the player with given color and on the given side.
    pub fn disable_castling(&mut self, color: piece::Color, side: Side) {
        let mut mask = match color {
            piece::Color::White => 0b11110111,
            piece::Color::Black => 0b11111101,
        };

        // works similarly to the code in the can_castle method, the initial mask could
        // be used to disable either queenside castling bit, shifting the mask to the right
        // makes it possible to disable a kingside castling bit
        if side == Side::Kingside {
            mask = mask >> 1;
        }

        self.castling_flags &= mask;
    }

    /// Returns an [`Option`] that may contain a square behind the pawn that's moved two
    /// squares at once.
    pub fn get_enpassant(&self) -> Option<square::Square> {
        self.enpassant
    }

    /// Sets en-passant target square. The square should always be
    /// right behind the pawn that's just moved two squares in a single move.
    /// Whether the opposite player choses to capture en-passant or not, the
    /// en-passant should be set to [`None`] right after making the move.
    pub fn set_enpassant(&mut self, enpassant: Option<square::Square>) {
        self.enpassant = enpassant;
    }

    /// Returns color which is currently to play
    pub fn get_color_to_play(&self) -> piece::Color {
        self.to_play
    }

    /// Flips the color which is to play. This method increments fullmoves counter
    /// if the previous move was made with black pieces.
    pub fn flip_color_to_play(&mut self) {
        match self.to_play {
            piece::Color::White => self.to_play = piece::Color::Black,
            piece::Color::Black => self.to_play = piece::Color::White,
        }

        if self.to_play == piece::Color::White {
            self.incr_fullmoves();
        }
    }

    /// Sets color to play.
    pub fn set_to_play(&mut self, color: piece::Color) {
        self.to_play = color
    }

    /// Increments the halfmove counter by 1.
    pub fn incr_halfmoves(&mut self) {
        self.halfmoves += 1;
    }

    /// Returns the halfmove counter.
    pub fn get_halfmoves(&self) -> u8 {
        self.halfmoves
    }

    /// Resets the halfmove counter to 0.
    pub fn reset_halfmoves(&mut self) {
        self.halfmoves = 0;
    }

    /// Increments the fullmove counter by 1.
    pub fn incr_fullmoves(&mut self) {
        self.fullmoves += 1;
    }

    /// Returns the fullmove counter.
    pub fn get_fullmoves(&self) -> u16 {
        self.fullmoves
    }
}

impl Default for Context {
    /// Creates a default context, which:
    /// - says white is to play
    /// - gives both sides the right to castle either side
    /// - has no en-passant target square set
    /// - has halfmove counter set to 0
    /// - has fullmove counter set to 1
    fn default() -> Self {
        Context::try_from("w KQkq - 0 1").unwrap()
    }
}

impl TryFrom<&str> for Context {
    type Error = &'static str;

    /// Creates a [`Context`] from the second part of the FEN string (the
    /// part after the piece setup description).
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let elements = value.split(' ').collect::<Vec<&str>>();

        if elements.len() != 5 {
            return Err("invalid number of elements in the FEN record");
        }

        // parse which color is to play in this position
        let color_to_play = match elements.get(0).unwrap().chars().next() {
            Some(ch) => {
                if !ch.is_lowercase() {
                    return Err("color char is not lowercase");
                }
                match ch {
                    'w' => piece::Color::White,
                    'b' => piece::Color::Black,
                    _ => {
                        return Err("invalid color symbol");
                    }
                }
            }
            None => {
                return Err("invalid color symbol");
            }
        };

        // parse castling symbols
        let castling_symbols = elements.get(1).unwrap();
        let (mut q_white, mut k_white, mut q_black, mut k_black) = (false, false, false, false);
        if *castling_symbols != "-" {
            for symbol in castling_symbols.chars() {
                match symbol {
                    'K' => k_white = true,
                    'Q' => q_white = true,
                    'k' => k_black = true,
                    'q' => q_black = true,
                    _ => {
                        return Err("invalid castling symbol");
                    }
                }
            }
        }

        // parse en-passant target
        let enpassant_target = elements.get(2).unwrap();
        let target_square = match square::Square::try_from(*enpassant_target) {
            Ok(target) => {
                let rank = target.get_rank();
                if rank == square::Rank::R3 || rank == square::Rank::R6 {
                    Some(target)
                } else {
                    return Err("only the third and sixth rank can be enpassant targets");
                }
            }
            Err(_msg) => {
                if *enpassant_target == "-" {
                    None
                } else {
                    return Err("impossible to create enpassant target from given string");
                }
            }
        };

        let halfmoves = elements.get(3).unwrap();
        let halfmoves = match u8::from_str_radix(*halfmoves, 10) {
            Ok(hm) => hm,
            Err(_) => {
                return Err("invalid value for halfmoves");
            }
        };

        let fullmoves = elements.get(4).unwrap();
        let fullmoves = match u16::from_str_radix(*fullmoves, 10) {
            Ok(fm) => fm,
            Err(_) => {
                return Err("invalid value for fullmoves");
            }
        };

        Ok(Context::new(
            q_white,
            k_white,
            q_black,
            k_black,
            target_square,
            color_to_play,
            halfmoves,
            fullmoves,
        ))
    }
}

impl Debug for Context {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let to_play = match self.get_color_to_play() {
            piece::Color::White => 'w',
            piece::Color::Black => 'b',
        };

        let mut castling_symbols = String::new();
        if self.can_castle(piece::Color::White, Side::Kingside) {
            castling_symbols.push('K');
        }
        if self.can_castle(piece::Color::White, Side::Queenside) {
            castling_symbols.push('Q');
        }
        if self.can_castle(piece::Color::Black, Side::Kingside) {
            castling_symbols.push('k');
        }
        if self.can_castle(piece::Color::Black, Side::Queenside) {
            castling_symbols.push('q');
        }

        if castling_symbols.len() == 0 {
            castling_symbols.push('-');
        }

        let enpassant_target = match self.get_enpassant() {
            Some(target) => format!("{:?}", target),
            None => String::from("-"),
        };

        let fen_context = format!(
            "{} {} {} {} {}",
            to_play,
            castling_symbols,
            enpassant_target,
            self.get_halfmoves(),
            self.get_fullmoves()
        );
        write!(f, "{}", fen_context)
    }
}

#[cfg(test)]
mod tests {
    use crate::context::*;
    use crate::piece;
    use crate::square;

    #[test]
    fn context_has_correct_default_values() {
        let context = Context::default();

        assert!(context.can_castle(piece::Color::White, Side::Queenside));
        assert!(context.can_castle(piece::Color::Black, Side::Queenside));
        assert!(context.can_castle(piece::Color::White, Side::Kingside));
        assert!(context.can_castle(piece::Color::Black, Side::Kingside));
        assert_eq!(context.get_halfmoves(), 0);
        assert_eq!(context.get_fullmoves(), 1);
        assert_eq!(context.get_enpassant(), None);
    }

    #[test]
    fn context_disabling_queenside_castling_works() {
        let mut context = Context::default();

        context.disable_castling(piece::Color::White, Side::Queenside);

        assert!(!context.can_castle(piece::Color::White, Side::Queenside));
        assert!(context.can_castle(piece::Color::Black, Side::Queenside));
        assert!(context.can_castle(piece::Color::White, Side::Kingside));
        assert!(context.can_castle(piece::Color::Black, Side::Kingside));

        context.disable_castling(piece::Color::Black, Side::Queenside);

        assert!(!context.can_castle(piece::Color::White, Side::Queenside));
        assert!(!context.can_castle(piece::Color::Black, Side::Queenside));
        assert!(context.can_castle(piece::Color::White, Side::Kingside));
        assert!(context.can_castle(piece::Color::Black, Side::Kingside));
    }

    #[test]
    fn context_disabling_kingside_castling_works() {
        let mut context = Context::default();

        context.disable_castling(piece::Color::White, Side::Kingside);

        assert!(context.can_castle(piece::Color::White, Side::Queenside));
        assert!(context.can_castle(piece::Color::Black, Side::Queenside));
        assert!(!context.can_castle(piece::Color::White, Side::Kingside));
        assert!(context.can_castle(piece::Color::Black, Side::Kingside));

        context.disable_castling(piece::Color::Black, Side::Kingside);

        assert!(context.can_castle(piece::Color::White, Side::Queenside));
        assert!(context.can_castle(piece::Color::Black, Side::Queenside));
        assert!(!context.can_castle(piece::Color::White, Side::Kingside));
        assert!(!context.can_castle(piece::Color::Black, Side::Kingside));
    }

    #[test]
    fn context_enpassant_works() {
        let mut context = Context::default();

        context.set_enpassant(Some(square::Square::try_from("e3").unwrap()));

        assert_eq!(
            context.get_enpassant(),
            Some(square::Square::try_from("e3").unwrap())
        );

        context.set_enpassant(None);

        assert_eq!(context.get_enpassant(), None);
    }

    #[test]
    fn context_correctly_parses_partial_fen1() {
        let context = Context::try_from("w KQkq - 0 1").unwrap();

        assert_eq!(context.get_color_to_play(), piece::Color::White);
        assert!(context.can_castle(piece::Color::White, Side::Kingside));
        assert!(context.can_castle(piece::Color::White, Side::Queenside));
        assert!(context.can_castle(piece::Color::Black, Side::Kingside));
        assert!(context.can_castle(piece::Color::Black, Side::Queenside));
        assert_eq!(context.get_enpassant(), None);
        assert_eq!(context.get_halfmoves(), 0);
        assert_eq!(context.get_fullmoves(), 1);
    }

    #[test]
    fn context_correctly_parses_partial_fen2() {
        let context = Context::try_from("b - e3 10 22").unwrap();

        assert_eq!(context.get_color_to_play(), piece::Color::Black);
        assert!(!context.can_castle(piece::Color::White, Side::Kingside));
        assert!(!context.can_castle(piece::Color::White, Side::Queenside));
        assert!(!context.can_castle(piece::Color::Black, Side::Kingside));
        assert!(!context.can_castle(piece::Color::Black, Side::Queenside));
        assert_eq!(
            context.get_enpassant(),
            Some(square::Square::try_from("e3").unwrap())
        );
        assert_eq!(context.get_halfmoves(), 10);
        assert_eq!(context.get_fullmoves(), 22);
    }

    #[test]
    fn context_halfmove_operations_work() {
        let mut context = Context::default();

        assert_eq!(context.get_halfmoves(), 0);
        assert_eq!(context.get_fullmoves(), 1);

        context.incr_halfmoves();
        context.incr_fullmoves();

        assert_eq!(context.get_halfmoves(), 1);
        assert_eq!(context.get_fullmoves(), 2);

        context.reset_halfmoves();

        assert_eq!(context.get_halfmoves(), 0);
        assert_eq!(context.get_fullmoves(), 2);
    }

    #[test]
    fn context_flip_color_to_play_increments_fullmoves() {
        let mut context = Context::default();

        // flip once (shouldnt increment the fullmove counter)
        context.flip_color_to_play();
        assert_eq!(context.get_color_to_play(), piece::Color::Black);
        assert_eq!(context.get_fullmoves(), 1);

        // flip the second time (should increment the fullmove counter)
        context.flip_color_to_play();
        assert_eq!(context.get_color_to_play(), piece::Color::White);
        assert_eq!(context.get_fullmoves(), 2);
    }
}
