#include "allocator.h"
#include "cache.h"

#include <iostream>
#include <string>

namespace {

struct String
{
    std::string data;
    bool marked = false;

    String(const std::string & key)
        : data(key)
    {}

    bool operator == (const std::string & other) const
    { return data == other; }

};

using TestCache = Cache<std::string, String, AllocatorWithPool>;

} // anonymous namespace

int main()
{
    TestCache cache(9, 10 * sizeof(String));
    std::string line;
    while (std::getline(std::cin, line)) {
        auto & s = cache.get<String>(line);
        if (s.marked) {
            std::cout << "known" << std::endl;
        }
        s.marked = true;
    }
    std::cout << "\n" << cache << std::endl;
}
