#include "requests.h"
#include "codec.h"
#include "fields.h"
#include "opt-fields.h"
#include <algorithm>
#include <string>

namespace {

    auto encode_fields(unsigned char *p,
                       const std::string &cl_ord_id,
                       char side,
                       double volume,
                       double price,
                       size_t symbol,
                       const std::string &firm,
                       const std::string &user
    ) {
#define FIELD(name, _, __) \
    p = encode_field_##name(p, name);
#define VAR_FIELD(name, _) \
    p = encode_field_##name(p, name);

#include "fields.inl"

        return p;
    }

    constexpr size_t bitfield_num(const RequestType type) {
        switch (type) {
            case RequestType::EnterOrder:
                return 4;
            default:
                throw std::invalid_argument(std::to_string(static_cast<int>(type)));
        }
    }

    constexpr size_t opt_fields_size() {
        return 0
               #define FIELD(_, __, ___, protocol_type, ____) + protocol_type##_size

               #include "new-enter-order.inl"
               ;
    }

    auto encode_optional_fields(unsigned char *bitfield_start,
                                RequestType type,
                                char time_in_force,
                                char capacity
    ) {
        unsigned char *p = bitfield_start + bitfield_num(type);
#define FIELD(name, bitfield_num, bit, protocol_type, ctype) \
    set_opt_field_bit(bitfield_start, bitfield_num, bit); \
    p = encode_field_##name(p, name);

#include "new-enter-order.inl"

        return p;
    }

    auto add_header(unsigned char *begin, const RequestType type) {
        char res;
        switch (type) {
            case RequestType::EnterOrder:
                res = 'O';
                break;
            default:
                throw std::invalid_argument(std::to_string(static_cast<int>(type)));
        }
        *begin = res;
        return ++begin;
    }

    constexpr size_t type_size(const RequestType type) {
        switch (type) {
            case RequestType::EnterOrder:
                return opt_fields_size() + 42;
            default:
                throw std::invalid_argument(std::to_string(static_cast<int>(type)));
        }
    }

    char convert_side(Side side) {
        switch (side) {
            case Side::Buy:
                return 'B';
            case Side::Sell:
                return 'S';
            default:
                throw std::invalid_argument(std::to_string(static_cast<int>(side)));
        }
    }

    double convert_price(double price, const OrdType ord_type) {
        return ord_type == OrdType::Market ? 214748.3647 : price;
    }

    char convert_time_in_force(TimeInForce time) {
        switch (time) {
            case TimeInForce::Day:
                return '0';
            case TimeInForce::IOC:
                return '3';
            default:
                throw std::invalid_argument(std::to_string(static_cast<int>(time)));
        }
    }

    char convert_capacity(Capacity capacity) {
        switch (capacity) {
            case Capacity::Agency:
                return '1';
            case Capacity::Principal:
                return '2';
            case Capacity::RisklessPrincipal:
                return '7';
            default:
                throw std::invalid_argument(std::to_string(static_cast<int>(capacity)));
        }
    }

}

std::vector<unsigned char> create_enter_order_request(
        const std::string &cl_ord_id,
        Side side,
        double volume,
        double price,
        const std::string &symbol,
        OrdType ord_type,
        TimeInForce time_in_force,
        Capacity capacity,
        const std::string &firm,
        const std::string &user
) {
    std::vector<unsigned char> msg(type_size(RequestType::EnterOrder));
    auto *p = &msg[0];
    p = add_header(p, RequestType::EnterOrder);
    p = encode_fields(p,
                      cl_ord_id,
                      convert_side(side),
                      volume,
                      convert_price(price, ord_type),
                      std::stoll(symbol),
                      firm,
                      user
    );
    encode_optional_fields(p,
                               RequestType::EnterOrder,
                               convert_time_in_force(time_in_force),
                               convert_capacity(capacity)
    );
    return msg;
}