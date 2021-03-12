#pragma once

#include <cstddef>
#include <vector>
#include <string>
#include <array>

enum class RequestType {
    EnterOrder
};

enum class Side {
    Buy,
    Sell
};

enum class OrdType {
    Market,
    Limit
};

enum class TimeInForce {
    Day,
    IOC
};

enum class Capacity {
    Agency,
    Principal,
    RisklessPrincipal
};

std::vector<unsigned char> create_enter_order_request(
  const std::string & cl_ord_id,
  Side side,
  double volume,
  double price,
  const std::string & symbol,
  OrdType ord_type,
  TimeInForce time_in_force,
  Capacity capacity,
  const std::string & firm,
  const std::string & user
);