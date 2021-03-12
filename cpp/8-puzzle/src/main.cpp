#include <iostream>
#include "solver.h"

using namespace std;

int main()
{
    Board board;
    Solver solver(board);
    std::cout << solver.moves() << std::endl;
}