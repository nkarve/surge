#pragma once

#include "types.h"

extern const Bitboard KING_ATTACKS[NSQUARES];
extern const Bitboard KNIGHT_ATTACKS[NSQUARES];
extern const Bitboard WHITE_PAWN_ATTACKS[NSQUARES];
extern const Bitboard BLACK_PAWN_ATTACKS[NSQUARES];

extern Bitboard reverse(Bitboard b);
extern Bitboard sliding_attacks(Square square, Bitboard occ, Bitboard mask);

extern Bitboard get_rook_attacks_for_init(Square square, Bitboard occ);
extern const Bitboard ROOK_MAGICS[NSQUARES];
extern Bitboard ROOK_ATTACK_MASKS[NSQUARES];
extern int ROOK_ATTACK_SHIFTS[NSQUARES];
extern Bitboard ROOK_ATTACKS[NSQUARES][4096];
extern void initialise_rook_attacks();


//gk extern constexpr Bitboard get_rook_attacks(Square square, Bitboard occ);
extern Bitboard get_rook_attacks(Square square, Bitboard occ);
extern Bitboard get_xray_rook_attacks(Square square, Bitboard occ, Bitboard blockers);

extern Bitboard get_bishop_attacks_for_init(Square square, Bitboard occ);
extern const Bitboard BISHOP_MAGICS[NSQUARES];
extern Bitboard BISHOP_ATTACK_MASKS[NSQUARES];
extern int BISHOP_ATTACK_SHIFTS[NSQUARES];
extern Bitboard BISHOP_ATTACKS[NSQUARES][512];
extern void initialise_bishop_attacks();


//gk extern constexpr Bitboard get_bishop_attacks(Square square, Bitboard occ);
extern Bitboard get_bishop_attacks(Square square, Bitboard occ);
extern Bitboard get_xray_bishop_attacks(Square square, Bitboard occ, Bitboard blockers);

extern Bitboard SQUARES_BETWEEN_BB[NSQUARES][NSQUARES];
extern Bitboard LINE[NSQUARES][NSQUARES];
extern Bitboard PAWN_ATTACKS[NCOLORS][NSQUARES];
extern Bitboard PSEUDO_LEGAL_ATTACKS[NPIECE_TYPES][NSQUARES];

extern void initialise_squares_between();
extern void initialise_line();
extern void initialise_pseudo_legal();
extern void initialise_all_databases();


//Returns a bitboard containing all squares that a piece on a square can move to, in the given position
template<PieceType P>
constexpr Bitboard attacks(Square s, Bitboard occ) {
	static_assert(P != PAWN, "The piece type may not be a pawn; use pawn_attacks instead");
	return P == ROOK ? get_rook_attacks(s, occ) :
		P == BISHOP ? get_bishop_attacks(s, occ) :
		P == QUEEN ? attacks<ROOK>(s, occ) | attacks<BISHOP>(s, occ) :
		PSEUDO_LEGAL_ATTACKS[P][s];
}

//Returns a bitboard containing all squares that a piece on a square can move to, in the given position
//Used when the piece type is not known at compile-time
//gk (needs at least c++14)
constexpr Bitboard attacks(PieceType pt, Square s, Bitboard occ) {
	switch (pt) {
	case ROOK:
		return attacks<ROOK>(s, occ);
	case BISHOP:
		return attacks<BISHOP>(s, occ);
	case QUEEN:
		return attacks<QUEEN>(s, occ);
	default:
		return PSEUDO_LEGAL_ATTACKS[pt][s];
	}
}

//Returns a bitboard containing pawn attacks from all pawns in the given bitboard
template<Color C>
constexpr Bitboard pawn_attacks(Bitboard p) {
	return C == WHITE ? shift<NORTH_WEST>(p) | shift<NORTH_EAST>(p) :
		shift<SOUTH_WEST>(p) | shift<SOUTH_EAST>(p);
}

//Returns a bitboard containing pawn attacks from the pawn on the given square
template<Color C>
constexpr Bitboard pawn_attacks(Square s) {
	return PAWN_ATTACKS[C][s];
}
