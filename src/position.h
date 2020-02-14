#pragma once

#include "types.h"
#include <ostream>
#include <string>
#include "tables.h"
#include <utility>

class PRNG {
	uint64_t s;

	uint64_t rand64() {
		s ^= s >> 12, s ^= s << 25, s ^= s >> 27;
		return s * 2685821657736338717LL;
	}

public:
	PRNG(uint64_t seed) : s(seed) {}

	template<typename T> T rand() { return T(rand64()); }

	template<typename T> 
	T sparse_rand() {
		return T(rand64() & rand64() & rand64());
	}
};


namespace zobrist {
	extern uint64_t zobrist_table[NPIECES][NSQUARES];
	extern void initialise_zobrist_keys();
}


struct UndoInfo {
	Bitboard entry;
	Piece captured;
	Square epsq;

	constexpr UndoInfo() : entry(0), captured(NO_PIECE), epsq(NO_SQUARE) {}
	UndoInfo(const UndoInfo& prev) : 
		entry(prev.entry), captured(NO_PIECE), epsq(NO_SQUARE) {}
};

class Position {
private:
	Bitboard piece_bb[NPIECES];
	Piece board[NSQUARES];
	Color side_to_play;
	int game_ply;
	uint64_t hash;
public:
	UndoInfo history[64];
	Bitboard checkers;
	Bitboard pinned;
	
	inline void put_piece(Piece pc, Square s) {
		board[s] = pc;
		piece_bb[pc] |= SQUARE_BB[s];
		hash ^= zobrist::zobrist_table[pc][s];
	}

	inline void remove_piece(Square s) {
		hash ^= zobrist::zobrist_table[board[s]][s];
		piece_bb[board[s]] &= ~SQUARE_BB[s];
		board[s] = NO_PIECE;
	}

	void move_piece(Square from, Square to);
	void move_piece_quiet(Square from, Square to);

	Position() : piece_bb{ 0 }, side_to_play(WHITE), game_ply(0), board{}, 
		hash(0), pinned(0), checkers(0) {
		for (int i = 0; i < 64; i++) board[i] = NO_PIECE;
		history[0] = UndoInfo();
	}

	friend std::ostream& operator<<(std::ostream& os, const Position& p);
	static void set(const std::string& fen, Position& p);
	std::string fen() const;

	Position& operator=(const Position&) = delete;
	inline bool operator==(const Position& other) const { return hash == other.hash; }

	inline Bitboard bitboard_of(Piece pc) const { return piece_bb[pc]; }
	inline Bitboard bitboard_of(Color c, PieceType pt) const { return piece_bb[make_piece(c, pt)]; }
	inline Piece at(Square sq) const { return board[sq]; }
	inline Color turn() const { return side_to_play; }
	inline int ply() const { return game_ply; }
	inline uint64_t get_hash() const { return hash; }

	template<Color C> inline Bitboard diagonal_sliders() const;
	template<Color C> inline Bitboard orthogonal_sliders() const;
	template<Color C> inline Bitboard all_pieces() const;
	template<Color C> inline Bitboard attackers_from(Square s, Bitboard occ) const;

	template<Color C> inline bool in_check() const {
		return attackers_from<~C>(bsf(bitboard_of(C, KING)), all_pieces<WHITE>() | all_pieces<BLACK>());
	}

	template<Color C> void play(Move m);
	template<Color C> void undo(Move m);

	template<Color Us>
	Move *generate_legals(Move* list);
};

template<Color C> 
inline Bitboard Position::diagonal_sliders() const {
	return C == WHITE ? piece_bb[WHITE_BISHOP] | piece_bb[WHITE_QUEEN] :
		piece_bb[BLACK_BISHOP] | piece_bb[BLACK_QUEEN];
}

template<Color C> 
inline Bitboard Position::orthogonal_sliders() const {
	return C == WHITE ? piece_bb[WHITE_ROOK] | piece_bb[WHITE_QUEEN] :
		piece_bb[BLACK_ROOK] | piece_bb[BLACK_QUEEN];
}

template<Color C> 
inline Bitboard Position::all_pieces() const {
	return C == WHITE ? piece_bb[WHITE_PAWN] | piece_bb[WHITE_KNIGHT] | piece_bb[WHITE_BISHOP] |
		piece_bb[WHITE_ROOK] | piece_bb[WHITE_QUEEN] | piece_bb[WHITE_KING] :

		piece_bb[BLACK_PAWN] | piece_bb[BLACK_KNIGHT] | piece_bb[BLACK_BISHOP] |
		piece_bb[BLACK_ROOK] | piece_bb[BLACK_QUEEN] | piece_bb[BLACK_KING];
}

template<Color C> 
inline Bitboard Position::attackers_from(Square s, Bitboard occ) const {
	return C == WHITE ? (pawn_attacks<BLACK>(s) & piece_bb[WHITE_PAWN]) |
		(attacks<KNIGHT>(s, occ) & piece_bb[WHITE_KNIGHT]) |
		(attacks<BISHOP>(s, occ) & (piece_bb[WHITE_BISHOP] | piece_bb[WHITE_QUEEN])) |
		(attacks<ROOK>(s, occ) & (piece_bb[WHITE_ROOK] | piece_bb[WHITE_QUEEN])) :

		(pawn_attacks<WHITE>(s) & piece_bb[BLACK_PAWN]) |
		(attacks<KNIGHT>(s, occ) & piece_bb[BLACK_KNIGHT]) |
		(attacks<BISHOP>(s, occ) & (piece_bb[BLACK_BISHOP] | piece_bb[BLACK_QUEEN])) |
		(attacks<ROOK>(s, occ) & (piece_bb[BLACK_ROOK] | piece_bb[BLACK_QUEEN]));
}


/*template<Color C>
Bitboard Position::pinned(Square s, Bitboard us, Bitboard occ) const {
	Bitboard pinned = 0;

	Bitboard pinners = get_xray_rook_attacks(s, occ, us) & orthogonal_sliders<~C>();
	while (pinners) pinned |= SQUARES_BETWEEN_BB[s][pop_lsb(&pinners)] & us;

	pinners = get_xray_bishop_attacks(s, occ, us) & diagonal_sliders<~C>();
	while (pinners) pinned |= SQUARES_BETWEEN_BB[s][pop_lsb(&pinners)] & us;

	return pinned;
}

template<Color C>
Bitboard Position::blockers_to(Square s, Bitboard occ) const {
	Bitboard blockers = 0;
	Bitboard candidates = get_rook_attacks(s, occ) & occ;
	Bitboard attackers = get_rook_attacks(s, occ ^ candidates) & orthogonal_sliders<~C>();

	candidates = get_bishop_attacks(s, occ) & occ;
	attackers |= get_bishop_attacks(s, occ ^ candidates) & diagonal_sliders<~C>();

	while (attackers) blockers |= SQUARES_BETWEEN_BB[s][pop_lsb(&attackers)];
	return blockers;
}*/


template<Color C>
void Position::play(const Move m) {
	side_to_play = ~side_to_play;
	++game_ply;
	history[game_ply] = UndoInfo(history[game_ply - 1]);

	MoveFlags type = m.flags();
	history[game_ply].entry |= SQUARE_BB[m.to()] | SQUARE_BB[m.from()];

	switch (type) {
	case QUIET:
		move_piece_quiet(m.from(), m.to());
		break;
	case DOUBLE_PUSH:
		move_piece_quiet(m.from(), m.to());
		history[game_ply].epsq = m.from() + relative_dir<C>(NORTH);
		break;
	case OO:
		if (C == WHITE) {
			move_piece_quiet(e1, g1);
			move_piece_quiet(h1, f1);
		} else {
			move_piece_quiet(e8, g8);
			move_piece_quiet(h8, f8);
		}			
		break;
	case OOO:
		if (C == WHITE) {
			move_piece_quiet(e1, c1); 
			move_piece_quiet(a1, d1);
		} else {
			move_piece_quiet(e8, c8);
			move_piece_quiet(a8, d8);
		}
		break;
	case EN_PASSANT:
		move_piece_quiet(m.from(), m.to());
		remove_piece(m.to() + relative_dir<C>(SOUTH));
		break;
	case PR_KNIGHT:
		remove_piece(m.from());
		put_piece(make_piece(C, KNIGHT), m.to());
		break;
	case PR_BISHOP:
		remove_piece(m.from());
		put_piece(make_piece(C, BISHOP), m.to());
		break;
	case PR_ROOK:
		remove_piece(m.from());
		put_piece(make_piece(C, ROOK), m.to());
		break;
	case PR_QUEEN:
		remove_piece(m.from());
		put_piece(make_piece(C, QUEEN), m.to());
		break;
	case PC_KNIGHT:
		remove_piece(m.from());
		history[game_ply].captured = board[m.to()];
		remove_piece(m.to());
		
		put_piece(make_piece(C, KNIGHT), m.to());
		break;
	case PC_BISHOP:
		remove_piece(m.from());
		history[game_ply].captured = board[m.to()];
		remove_piece(m.to());

		put_piece(make_piece(C, BISHOP), m.to());
		break;
	case PC_ROOK:
		remove_piece(m.from());
		history[game_ply].captured = board[m.to()];
		remove_piece(m.to());

		put_piece(make_piece(C, ROOK), m.to());
		break;
	case PC_QUEEN:
		remove_piece(m.from());
		history[game_ply].captured = board[m.to()];
		remove_piece(m.to());

		put_piece(make_piece(C, QUEEN), m.to());
		break;
	case CAPTURE:
		history[game_ply].captured = board[m.to()];
		move_piece(m.from(), m.to());
		
		break;
	}
}

template<Color C>
void Position::undo(const Move m) {
	MoveFlags type = m.flags();
	switch (type) {
	case QUIET:
		move_piece_quiet(m.to(), m.from());
		break;
	case DOUBLE_PUSH:
		move_piece_quiet(m.to(), m.from());
		break;
	case OO:
		if (C == WHITE) {
			move_piece_quiet(g1, e1);
			move_piece_quiet(f1, h1);
		} else {
			move_piece_quiet(g8, e8);
			move_piece_quiet(f8, h8);
		}
		break;
	case OOO:
		if (C == WHITE) {
			move_piece_quiet(c1, e1);
			move_piece_quiet(d1, a1);
		} else {
			move_piece_quiet(c8, e8);
			move_piece_quiet(d8, a8);
		}
		break;
	case EN_PASSANT:
		move_piece_quiet(m.to(), m.from());
		put_piece(make_piece(~C, PAWN), m.to() + relative_dir<C>(SOUTH));
		break;
	case PR_KNIGHT:
	case PR_BISHOP:
	case PR_ROOK:
	case PR_QUEEN:
		remove_piece(m.to());
		put_piece(make_piece(C, PAWN), m.from());
		break;
	case PC_KNIGHT:
	case PC_BISHOP:
	case PC_ROOK:
	case PC_QUEEN:
		remove_piece(m.to());
		put_piece(make_piece(C, PAWN), m.from());
		put_piece(history[game_ply].captured, m.to());
		break;
	case CAPTURE:
		move_piece_quiet(m.to(), m.from());
		put_piece(history[game_ply].captured, m.to());
		break;
	}

	side_to_play = ~side_to_play;
	--game_ply;
}

template<Color Us>
Move* Position::generate_legals(Move* list) {
	constexpr Color Them = ~Us;

	const Bitboard us_bb = all_pieces<Us>();
	const Bitboard them_bb = all_pieces<Them>();
	const Bitboard all = us_bb | them_bb;

	const Square our_king = bsf(bitboard_of(Us, KING));
	const Square their_king = bsf(bitboard_of(Them, KING));

	const Bitboard our_diag_sliders = diagonal_sliders<Us>();
	const Bitboard their_diag_sliders = diagonal_sliders<Them>();
	const Bitboard our_orth_sliders = orthogonal_sliders<Us>();
	const Bitboard their_orth_sliders = orthogonal_sliders<Them>();

	Move q[218], c[64];
	Bitboard b1, b2, b3, danger = 0;

	danger |= pawn_attacks<Them>(bitboard_of(Them, PAWN)) | attacks<KING>(their_king, all);
	b1 = bitboard_of(Them, KNIGHT); while (b1) danger |= attacks<KNIGHT>(pop_lsb(&b1), all);
	b1 = their_diag_sliders;
	while (b1) danger |= attacks<BISHOP>(pop_lsb(&b1), all ^ SQUARE_BB[our_king]);
	b1 = their_orth_sliders;
	while (b1) danger |= attacks<ROOK>(pop_lsb(&b1), all ^ SQUARE_BB[our_king]);

	b1 = attacks<KING>(our_king, all) & ~(us_bb | danger);
	list = make<QUIET>(our_king, b1 & ~them_bb, list);
	list = make<CAPTURE>(our_king, b1 & them_bb, list);

	Bitboard capture_mask, quiet_mask;
	Square s;

	checkers = attacks<KNIGHT>(our_king, all) & bitboard_of(Them, KNIGHT)
		| pawn_attacks<Us>(our_king) & bitboard_of(Them, PAWN);
	Bitboard candidates = attacks<ROOK>(our_king, them_bb) & their_orth_sliders
		| attacks<BISHOP>(our_king, them_bb) & their_diag_sliders;

	pinned = 0;
	while (candidates) {
		s = pop_lsb(&candidates);
		b1 = SQUARES_BETWEEN_BB[our_king][s] & us_bb;
		if (b1 == 0) checkers ^= SQUARE_BB[s];
		else if ((b1 & b1 - 1) == 0) pinned ^= b1;
	}

	const Bitboard not_pinned = ~pinned;

	switch (sparse_pop_count(checkers)) {
	case 2:
		return list;
	case 1: {
		Square checker_square = bsf(checkers);

		switch (board[checker_square]) {
		case make_piece(Them, PAWN):
			if (checkers == shift<relative_dir<Us>(SOUTH)>(SQUARE_BB[history[game_ply].epsq])) {
				b1 = pawn_attacks<Them>(history[game_ply].epsq) & bitboard_of(Us, PAWN) & not_pinned;
				while (b1) *list++ = Move(pop_lsb(&b1), history[game_ply].epsq, EN_PASSANT);
			}
		case make_piece(Them, KNIGHT):
			b1 = attackers_from<Us>(checker_square, all) & not_pinned;
			while (b1) *list++ = Move(pop_lsb(&b1), checker_square, CAPTURE);

			return list;
		default:
			capture_mask = checkers;
			quiet_mask = SQUARES_BETWEEN_BB[our_king][checker_square];
			break;
		}

		break;
	}

	default:
		capture_mask = them_bb;
		quiet_mask = ~all;

		if (history[game_ply].epsq != NO_SQUARE) {
			b1 = pawn_attacks<Them>(history[game_ply].epsq) & bitboard_of(Us, PAWN) & not_pinned;
			while (b1) {
				s = pop_lsb(&b1);
				if ((sliding_attacks(our_king, all ^ SQUARE_BB[s]
					^ shift<relative_dir<Us>(SOUTH)>(SQUARE_BB[history[game_ply].epsq]),
					MASK_RANK[rank_of(our_king)])&
					their_orth_sliders) == 0)
					*list++ = Move(s, history[game_ply].epsq, EN_PASSANT);
			}
		}

		if (!((history[game_ply].entry & oo_mask<Us>()) | ((all | danger) & oo_blockers_mask<Us>())))
			*list++ = Us == WHITE ? Move(e1, h1, OO) : Move(e8, h8, OO);
		if (!((history[game_ply].entry & ooo_mask<Us>()) |
			((all | (danger & ~ignore_ooo_danger<Us>())) & ooo_blockers_mask<Us>())))
			*list++ = Us == WHITE ? Move(e1, c1, OOO) : Move(e8, c8, OOO);

		b1 = ~(not_pinned | bitboard_of(Us, KNIGHT));
		while (b1) {
			s = pop_lsb(&b1);
			b2 = attacks(type_of(board[s]), s, all) & LINE[our_king][s];
			list = make<QUIET>(s, b2 & quiet_mask, list);
			list = make<CAPTURE>(s, b2 & capture_mask, list);
		}

		b1 = ~not_pinned & bitboard_of(Us, PAWN);
		while (b1) {
			s = pop_lsb(&b1);

			if (rank_of(s) == relative_rank<Us>(RANK7)) {
				b2 = pawn_attacks<Us>(s) & capture_mask & LINE[our_king][s];
				list = make<PROMOTION_CAPTURES>(s, b2, list);
			}
			else {
				b2 = pawn_attacks<Us>(s) & them_bb & LINE[s][our_king];
				list = make<CAPTURE>(s, b2, list);

				b2 = shift<relative_dir<Us>(NORTH)>(SQUARE_BB[s]) & ~all & LINE[our_king][s];
				b3 = shift<relative_dir<Us>(NORTH)>(b2 &
					MASK_RANK[relative_rank<Us>(RANK3)]) & ~all & LINE[our_king][s];
				list = make<QUIET>(s, b2, list);
				list = make<DOUBLE_PUSH>(s, b3, list);
			}
		}

		break;
	}


	b1 = bitboard_of(Us, KNIGHT) & not_pinned;
	while (b1) {
		s = pop_lsb(&b1);
		b2 = attacks<KNIGHT>(s, all);
		list = make<QUIET>(s, b2 & quiet_mask, list);
		list = make<CAPTURE>(s, b2 & capture_mask, list);
	}

	b1 = our_diag_sliders & not_pinned;
	while (b1) {
		s = pop_lsb(&b1);
		b2 = attacks<BISHOP>(s, all);
		list = make<QUIET>(s, b2 & quiet_mask, list);
		list = make<CAPTURE>(s, b2 & capture_mask, list);
	}

	b1 = our_orth_sliders & not_pinned;
	while (b1) {
		s = pop_lsb(&b1);
		b2 = attacks<ROOK>(s, all);
		list = make<QUIET>(s, b2 & quiet_mask, list);
		list = make<CAPTURE>(s, b2 & capture_mask, list);
	}


	b1 = bitboard_of(Us, PAWN) & not_pinned & ~MASK_RANK[relative_rank<Us>(RANK7)];
	b2 = shift<relative_dir<Us>(NORTH)>(b1) & ~all;
	b3 = shift<relative_dir<Us>(NORTH)>(b2 & MASK_RANK[relative_rank<Us>(RANK3)]) & quiet_mask;
	b2 &= quiet_mask;

	while (b2) {
		s = pop_lsb(&b2);
		*list++ = Move(s - relative_dir<Us>(NORTH), s, QUIET);
	}

	while (b3) {
		s = pop_lsb(&b3);
		*list++ = Move(s - relative_dir<Us>(NORTH_NORTH), s, DOUBLE_PUSH);
	}

	b2 = shift<relative_dir<Us>(NORTH_WEST)>(b1) & capture_mask;
	b3 = shift<relative_dir<Us>(NORTH_EAST)>(b1) & capture_mask;

	while (b2) {
		s = pop_lsb(&b2);
		*list++ = Move(s - relative_dir<Us>(NORTH_WEST), s, CAPTURE);
	}

	while (b3) {
		s = pop_lsb(&b3);
		*list++ = Move(s - relative_dir<Us>(NORTH_EAST), s, CAPTURE);
	}


	b1 = bitboard_of(Us, PAWN) & not_pinned & MASK_RANK[relative_rank<Us>(RANK7)];
	if (b1) {
		b2 = shift<relative_dir<Us>(NORTH)>(b1) & quiet_mask;
		while (b2) {
			s = pop_lsb(&b2);
			*list++ = Move(s - relative_dir<Us>(NORTH), s, PR_KNIGHT);
			*list++ = Move(s - relative_dir<Us>(NORTH), s, PR_BISHOP);
			*list++ = Move(s - relative_dir<Us>(NORTH), s, PR_ROOK);
			*list++ = Move(s - relative_dir<Us>(NORTH), s, PR_QUEEN);
		}

		b2 = shift<relative_dir<Us>(NORTH_WEST)>(b1) & capture_mask;
		b3 = shift<relative_dir<Us>(NORTH_EAST)>(b1) & capture_mask;

		while (b2) {
			s = pop_lsb(&b2);
			*list++ = Move(s - relative_dir<Us>(NORTH_WEST), s, PC_KNIGHT);
			*list++ = Move(s - relative_dir<Us>(NORTH_WEST), s, PC_BISHOP);
			*list++ = Move(s - relative_dir<Us>(NORTH_WEST), s, PC_ROOK);
			*list++ = Move(s - relative_dir<Us>(NORTH_WEST), s, PC_QUEEN);
		}

		while (b3) {
			s = pop_lsb(&b3);
			*list++ = Move(s - relative_dir<Us>(NORTH_EAST), s, PC_KNIGHT);
			*list++ = Move(s - relative_dir<Us>(NORTH_EAST), s, PC_BISHOP);
			*list++ = Move(s - relative_dir<Us>(NORTH_EAST), s, PC_ROOK);
			*list++ = Move(s - relative_dir<Us>(NORTH_EAST), s, PC_QUEEN);
		}
	}

	return list;
}


template<Color Us>
class MoveList {
public:
	explicit MoveList(Position& p) : last(p.generate_legals<Us>(list)) {}

	const Move* begin() const { return list; }
	const Move* end() const { return last; }
	size_t size() const { return last - list; }
private:
	Move list[218];
	Move *last;
};