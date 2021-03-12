#include "solver.h"
#include <queue>
#include <algorithm>

Solver::Solver(const Board &b) {
    start_board = b;
    board_goal = start_board.create_goal();
    solve();
}

void Solver::solve() {
    if (!start_board.is_solvable()) {
        return;
    }

    std::priority_queue<Board, std::vector<Board>> queue;
    queue.push(start_board);

    std::unordered_set<Board> used;

    std::unordered_map<Board, size_t> distance;
    distance[start_board] = 0;


    while (!queue.empty()) {
        Board current = queue.top();

        if (current.is_goal()) {
            get_parents();
            return;
        }

        queue.pop();
        used.insert(current);

        for (const Board &board : get_all_possible_boards(current)) {
            size_t current_distance = distance[current] + board.hamming() + board.manhattan();

            if (!used.count(board) || current_distance < distance[board]) {
                parent[board] = current;
                distance[board] = current_distance;
                queue.push(board);
            }
        }
    }
}

std::vector<Board> Solver::get_all_possible_boards(const Board &board) {
    std::vector<Board> res;
    std::pair<size_t, size_t> empty_cell = board.get_empty_cell();
    for (int i = -1; i <= 1; i++) {
        for (int j = -1; j <= 1; j++) {
            if (abs(i) != abs(j)) {
                size_t x = empty_cell.first + i;
                size_t y = empty_cell.second + j;
                if (check(x, y)) {
                    auto temp_board = board.get_board();
                    std::swap(temp_board[empty_cell.first][empty_cell.second], temp_board[x][y]);
                    res.emplace_back(Board(temp_board));
                }
            }
        }
    }
    return res;
}

void Solver::get_parents() {
    Board v = board_goal;
    while (v != start_board) {
        boards.emplace_back(v);
        v = parent[v];
    }
    boards.emplace_back(v);
    reverse(boards.begin(), boards.end());
}

bool Solver::check(const size_t x, const size_t y) const {
    return x < start_board.size() && y < start_board.size();
}

size_t Solver::moves() const {
    return boards.empty() ? 0 : boards.size() - 1;
}