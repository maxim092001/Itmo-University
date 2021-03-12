#pragma once

#include <vector>
#include <string>
#include <cmath>

class Board {
public:
    Board() = default;

    Board(const Board &other) = default;

    Board &operator=(const Board &other) = default;

    explicit Board(unsigned size);

    explicit Board(const std::vector<std::vector<unsigned>> &data);

    size_t size() const;

    bool is_goal() const;

    unsigned hamming() const;

    unsigned manhattan() const;

    std::string to_string() const;

    bool is_solvable() const;

    friend bool operator!=(const Board &a, const Board &b);

    friend bool operator==(const Board &a, const Board &b);

    friend bool operator<(const Board &a, const Board &b);

    friend std::ostream &operator<<(std::ostream &out, const Board &b);

    const std::vector<unsigned> &operator[](size_t index) const;

    std::pair<unsigned, unsigned> get_empty_cell() const;

    Board create_goal() const;

    const std::vector<std::vector<unsigned>> &get_board() const;

private:
    std::vector<std::vector<unsigned>> board;
    std::pair<unsigned, unsigned> empty_cell;
    size_t board_size = 0;

    void find_empty_cell();
};

namespace std {
    template<>
    class hash<Board> : hash<std::string> {

        static constexpr unsigned mod = 1'000'000'007;
        static constexpr unsigned prime = 31;

    public:
        unsigned operator()(const Board &b) const noexcept {
            unsigned res = 0;
            unsigned pow = 1;

            for (size_t i = 0; i < b.size(); ++i) {
                for (size_t j = 0; j < b.size(); ++j) {
                    pow = ((static_cast<unsigned long long>(pow)) * prime) % mod;
                    res = (res + (static_cast<unsigned long long>(pow) * b[i][j] % mod)) % mod;
                }
            }
            return res;
        }
    };
}