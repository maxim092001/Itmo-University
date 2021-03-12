#pragma once

#include "board.h"
#include<unordered_map>
#include <unordered_set>
#include <queue>

class Solver {
public:
    explicit Solver(const Board &b);

    Solver(const Solver &s) = default;

    Solver &operator=(const Solver &other) = default;

    auto begin() const {
        return boards.begin();
    }

    auto end() const {
        return boards.end();
    }

    size_t moves() const;

private:

    Board start_board, board_goal;
    std::vector<Board> boards;
    std::unordered_map<Board, Board> parent;

    void solve();

    void get_parents();

    std::vector<Board> get_all_possible_boards(const Board &board);

    bool check(size_t x, size_t y) const;
};
