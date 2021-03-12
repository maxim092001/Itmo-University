#include "board.h"
#include <algorithm>
#include <random>
#include <iostream>

Board::Board(const std::vector<std::vector<unsigned>> &other) : board(other), board_size((other.size())) {
    find_empty_cell();
}

Board Board::create_goal() const {
    size_t cnt = 1;
    size_t size = this->size();
    std::vector<std::vector<unsigned>> t;
    t.resize(size, std::vector<unsigned>(size));

    for (size_t i = 0; i < size; i++) {
        for (size_t j = 0; j < size; j++) {
            t[i][j] = cnt % (size * size);
            cnt++;
        }
    }
    return Board(t);
}

Board::Board(const unsigned size) {

    board.resize(size, std::vector<unsigned>(size));
    std::vector<unsigned> temp_board(size * size);
    std::iota(temp_board.begin(), temp_board.end(), 0);
    board_size = size;

    std::shuffle(temp_board.begin(), temp_board.end(), std::mt19937(std::random_device()()));
    for (unsigned i = 0; i < size * size; i++) {
        board[i / size][i % size] = temp_board[i];
    }
    find_empty_cell();

}

size_t Board::size() const {
    return board.size();
}

bool Board::is_goal() const {
    return hamming() == 0;
}

unsigned Board::hamming() const {
    size_t hamming_distance = 0;
    size_t counter = 0;
    for (size_t i = 0; i < board_size; i++) {
        for (size_t j = 0; j < board_size; j++) {
            counter < board_size * board_size - 1 ? (counter++) : (counter = 0);
            if (board[i][j] != counter)
                hamming_distance++;
        }
    }

    return hamming_distance;
}

unsigned Board::manhattan() const {
    size_t manhattan_distance = 0;

    for (size_t i = 0; i < board_size; i++) {
        for (size_t j = 0; j < board_size; j++) {
            if (board[i][j]) {
                manhattan_distance += abs(static_cast<int>(i - (board[i][j] - 1) / board_size))
                                      + abs(static_cast<int>(j - (board[i][j] - 1) % board_size));
            }
        }
    }

    return manhattan_distance;
}

std::string Board::to_string() const {
    std::string res;

    for (size_t i = 0; i < board_size; i++) {
        for (size_t j = 0; j < board_size; j++) {
            res += std::to_string(board[i][j]) + ' ';
        }

        res.pop_back();
        res.push_back('\n');
    }

    return res;
}

bool Board::is_solvable() const {
    size_t counter = 0;

    if (is_goal()) {
        return true;
    }

    for (size_t i = 0; i < board_size; ++i) {
        for (size_t j = 0; j < board_size; ++j) {
            if (board[i][j]) {

                for (size_t i1 = 0; i1 < i; ++i1) {
                    for (size_t j1 = 0; j1 < board_size; ++j1) {
                        counter += (board[i1][j1] > board[i][j]);
                    }
                }

                for (size_t j1 = 0; j1 < j; ++j1) {
                    counter += (board[i][j1] > board[i][j]);
                }
            }
        }
    }


    if (!(board_size % 2)) {
        for (size_t i = 0; i < board_size; ++i) {
            for (size_t j = 0; j < board_size; ++j) {
                if (!board[i][j]) {
                    counter += 1 + (i * board_size + j) / board_size;
                }
            }
        }
    }

    return !(counter % 2);
}

const std::vector<unsigned> &Board::operator[](size_t index) const {
    return board[index];
}

std::pair<unsigned, unsigned> Board::get_empty_cell() const {
    return empty_cell;
}

const std::vector<std::vector<unsigned>>& Board::get_board() const {
    return board;
}

void Board::find_empty_cell() {
    for (unsigned i = 0; i < board_size; ++i) {
        for (unsigned j = 0; j < board_size; ++j) {
            if (!board[i][j]) {
                empty_cell = {i, j};
                break;
            }
        }
    }
}

std::ostream &operator<<(std::ostream &out, const Board &b) {
    out << b.to_string();
    return out;
}

bool operator==(const Board &a, const Board &b) {
    return a.board == b.board;
}

bool operator!=(const Board &a, const Board &b) {
    return a.board != b.board;
}

bool operator<(const Board &a, const Board &b) {
    return b.hamming() + b.manhattan() < a.hamming() + a.manhattan();
}
