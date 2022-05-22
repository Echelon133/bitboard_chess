use crate::square;

/// Squares that create a path between the white king and H1 rook,
/// which cannot be attacked by a black piece if the player wants to
/// legally castle.
pub const WHITE_KINGSIDE_CASTLING_PATH: [square::Square; 3] = [
    square::Square::new(square::Rank::R1, square::File::E),
    square::Square::new(square::Rank::R1, square::File::F),
    square::Square::new(square::Rank::R1, square::File::G),
];

/// Squares that create a path between the black king and H8 rook,
/// which cannot be attacked by a white piece if the player wants to
/// legally castle.
pub const BLACK_KINGSIDE_CASTLING_PATH: [square::Square; 3] = [
    square::Square::new(square::Rank::R8, square::File::E),
    square::Square::new(square::Rank::R8, square::File::F),
    square::Square::new(square::Rank::R8, square::File::G),
];

/// Squares that create a path between the white king and A1 rook,
/// which cannot be attacked by a black piece if the player wants to
/// legally castle.
pub const WHITE_QUEENSIDE_CASTLING_PATH: [square::Square; 3] = [
    square::Square::new(square::Rank::R1, square::File::C),
    square::Square::new(square::Rank::R1, square::File::D),
    square::Square::new(square::Rank::R1, square::File::E),
];

/// Squares that create a path between the black king and A8 rook,
/// which cannot be attacked by a white piece if the player wants to
/// legally castle.
pub const BLACK_QUEENSIDE_CASTLING_PATH: [square::Square; 3] = [
    square::Square::new(square::Rank::R8, square::File::C),
    square::Square::new(square::Rank::R8, square::File::D),
    square::Square::new(square::Rank::R8, square::File::E),
];

/// Initial square of the white king.
pub const WHITE_KING_START: square::Square = square::Square::new(square::Rank::R1, square::File::E);

/// Initial square of the black king.
pub const BLACK_KING_START: square::Square = square::Square::new(square::Rank::R8, square::File::E);

/// Initial square of the white rook (queenside).
pub const WHITE_QSIDE_ROOK_START: square::Square =
    square::Square::new(square::Rank::R1, square::File::A);

/// Initial square of the black rook (queenside).
pub const BLACK_QSIDE_ROOK_START: square::Square =
    square::Square::new(square::Rank::R8, square::File::A);

/// Initial square of the white rook (kingside).
pub const WHITE_KSIDE_ROOK_START: square::Square =
    square::Square::new(square::Rank::R1, square::File::H);

/// Initial square of the black rook (kingside).
pub const BLACK_KSIDE_ROOK_START: square::Square =
    square::Square::new(square::Rank::R8, square::File::H);
