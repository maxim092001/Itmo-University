#include "genome.h"

#include <iostream>
#include <string>
#include <vector>

int main()
{
    const std::vector<std::string> reads = {"AATCT", "ACGAA", "GCTAC"};
    const std::size_t k = 2;
    std::cout << "K=" << k << ", [";
    bool first = true;
    for (const auto & r : reads) {
        if (!first) {
            std::cout << ", ";
        }
        else {
            first = false;
        }
        std::cout << r;
    }
    std::cout << "]\n" << genome::assembly(k, reads) << std::endl;
    return 0;
}
