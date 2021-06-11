#include "position.h"
#include "tables.h"
#include <sstream>

//Zobrist keys for each piece and each square
//Used to incrementally update the hash key of a position
uint64_t zobrist::zobrist_table[NPIECES][NSQUARES];

//Initializes the zobrist table with random 64-bit numbers
void zobrist::initialise_zobrist_keys() {
	PRNG rng(70026072);
	//gk comparison of integer expressions of different signedness
	//gk for (int i = 0; i < NPIECES; i++)
	//gk    for (int j = 0; j < NSQUARES; j++)
	for (size_t i = 0; i < NPIECES; i++)
		for (size_t j = 0; j < NSQUARES; j++)
			zobrist::zobrist_table[i][j] = rng.rand<uint64_t>();
}

//Pretty-prints the position (including FEN and hash key)
std::ostream& operator<< (std::ostream& os, const Position& p) {
	const char* s = "   +---+---+---+---+---+---+---+---+\n";
	const char* t = "     A   B   C   D   E   F   G   H\n";
	os << t;
	for (int i = 56; i >= 0; i -= 8) {
		os << s << " " << i / 8 + 1 << " ";
		for (int j = 0; j < 8; j++)
			os << "| " << PIECE_STR[p.board[i + j]] << " ";
		os << "| " << i / 8 + 1 << "\n";
	}
	os << s;
	os << t << "\n";

	os << "FEN: " << p.fen() << "\n";
	os << "Hash: 0x" << std::hex << p.hash << std::dec << "\n";

	return os;
}

//Returns the FEN (Forsyth-Edwards Notation) representation of the position
std::string Position::fen() const {
	std::ostringstream fen;
	int empty;

	for (int i = 56; i >= 0; i -= 8) {
		empty = 0;
		for (int j = 0; j < 8; j++) {
			Piece p = board[i + j];
			if (p == NO_PIECE) empty++;
			else {
				fen << (empty == 0 ? "" : std::to_string(empty))
					<< PIECE_STR[p];
				empty = 0;
			}
		}

		if (empty != 0) fen << empty;
		if (i > 0) fen << '/';
	}

	fen << (side_to_play == WHITE ? " w " : " b ")
		<< (history[game_ply].entry & WHITE_OO_MASK ? "" : "K")
		<< (history[game_ply].entry & WHITE_OOO_MASK ? "" : "Q")
		<< (history[game_ply].entry & BLACK_OO_MASK ? "" : "k")
		<< (history[game_ply].entry & BLACK_OOO_MASK ? "" : "q")
		<< (history[game_ply].entry & ALL_CASTLING_MASK ? "- " : "")
		<< (history[game_ply].epsq == NO_SQUARE ? " -" : SQSTR[history[game_ply].epsq]);

	return fen.str();
}

//Updates a position according to an FEN string
void Position::set(const std::string& fen, Position& p) {
	int square = a8;
	for (char ch : fen.substr(0, fen.find(' '))) {
		if (isdigit(ch))
			square += (ch - '0') * EAST;
		else if (ch == '/')
			square += 2 * SOUTH;
		else
			p.put_piece(Piece(PIECE_STR.find(ch)), Square(square++));
	}

	std::istringstream ss(fen.substr(fen.find(' ')));
	unsigned char token;

	ss >> token;
	p.side_to_play = token == 'w' ? WHITE : BLACK;

	p.history[p.game_ply].entry = ALL_CASTLING_MASK;
	while (ss >> token && !isspace(token)) {
		switch (token) {
		case 'K':
			p.history[p.game_ply].entry &= ~WHITE_OO_MASK;
			break;
		case 'Q':
			p.history[p.game_ply].entry &= ~WHITE_OOO_MASK;
			break;
		case 'k':
			p.history[p.game_ply].entry &= ~BLACK_OO_MASK;
			break;
		case 'q':
			p.history[p.game_ply].entry &= ~BLACK_OOO_MASK;
			break;
		}
	}
}
	

//Moves a piece to a (possibly empty) square on the board and updates the hash
void Position::move_piece(Square from, Square to) {
	hash ^= zobrist::zobrist_table[board[from]][from] ^ zobrist::zobrist_table[board[from]][to]
		^ zobrist::zobrist_table[board[to]][to];
	Bitboard mask = SQUARE_BB[from] | SQUARE_BB[to];
	piece_bb[board[from]] ^= mask;
	piece_bb[board[to]] &= ~mask;
	board[to] = board[from];
	board[from] = NO_PIECE;
}

//Moves a piece to an empty square. Note that it is an error if the <to> square contains a piece
void Position::move_piece_quiet(Square from, Square to) {
	hash ^= zobrist::zobrist_table[board[from]][from] ^ zobrist::zobrist_table[board[from]][to];
	piece_bb[board[from]] ^= (SQUARE_BB[from] | SQUARE_BB[to]);
	board[to] = board[from];
	board[from] = NO_PIECE;
}


