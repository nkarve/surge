#include <iostream>
#include <chrono>
#include "tables.h"
#include "position.h"
#include "types.h"
#include "search.h"



template<Color Us>
unsigned long long perft(Position& p, unsigned int depth) {
	int nmoves;
	unsigned long long nodes = 0;

	MoveList<Us> list(p);

	if (depth == 1) return (unsigned long long) list.size();

	for (Move move : list) {
		p.play<Us>(move);
		nodes += perft<~Us>(p, depth - 1);
		p.undo<Us>(move);
	}

	return nodes;
}

template<Color Us>
void perftdiv(Position& p, unsigned int depth) {
	unsigned long long nodes = 0, pf;

	MoveList<Us> list(p);

	for (Move move : list) {
		std::cout << move;

		p.play<Us>(move);
		pf = perft<~Us>(p, depth - 1);
		std::cout << ": " << pf << " moves\n";
		nodes += pf;
		p.undo<Us>(move);
	}

	std::cout << "\nTotal: " << nodes << " moves\n";
}

void test_perft() {
	Position p;
	Position::set("rnbqkbnr/pppppppp/8/8/8/8/PPPP1PPP/RNBQKBNR w KQkq -", p);
	std::cout << p;

	std::chrono::steady_clock::time_point begin = std::chrono::steady_clock::now();
	auto n = perft<WHITE>(p, 6);
	std::chrono::steady_clock::time_point end = std::chrono::steady_clock::now();
	auto diff = end - begin;

	std::cout << "Nodes: " << n << "\n";
	std::cout << "NPS: "
		<< int(n * 1000000.0 / std::chrono::duration_cast<std::chrono::microseconds>(diff).count())
		<< "\n";
	std::cout << "Time difference = "
		<< std::chrono::duration_cast<std::chrono::microseconds>(diff).count() << " [microseconds]\n";
}

int main() {
	initialise_all_databases();
	zobrist::initialise_zobrist_keys();
}
