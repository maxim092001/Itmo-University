#include "requests.h"

#include <iostream>
#include <iomanip>

namespace {

template <class T>
void print_binary(const T & x)
{
    size_t i = 0;
    const auto print8 = [&x, &i] () {
        for (size_t j = 0; i < x.size() && j < 8; ++i, ++j) {
            if (j != 0) {
                std::cout << ' ';
            }
            std::cout << std::hex << std::setfill('0') << std::setw(2) << static_cast<int>(x[i]);
        }
    };
    while (i < x.size()) {
        print8();
        if (i < x.size()) {
            std::cout << "  ";
            print8();
        }
        std::cout << std::endl;
    }
}

} // anonymous namespace

int main()
{
    const auto new_order_msg = create_enter_order_request(
        "ORD10000000001",
        Side::Buy,
        100,
        12.505,
        "ABBs",
        OrdType::Limit,
        TimeInForce::Day,
        Capacity::Principal,
        "uncr",
        "karl26"
    );
    std::cout << "Print:" << std::endl;
    print_binary(new_order_msg);
    return 0;
}
