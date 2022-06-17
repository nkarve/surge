## Files

- `chess_engine.cpp` - implementations of **perft**/perftdiv; main
- `types.h` - definitions of piece, square, move, masks and associated helper functions
- `types.cpp` - implementations of fast bitboard operations
- `tables.cpp` - lookup tables for piece moves, **magic bitboard** creation using hyperbola quintessence
- `position.cpp` - zobrist key creation, FEN manipulation
- `position.h` - probably what you're here for! **Position data structure**, **legal move generation**
