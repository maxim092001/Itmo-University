#include "requests.h"

#include <gtest/gtest.h>

TEST(EnterOrderTest, simple_size_check)
{
    const auto bmsg = create_enter_order_request(
        "ORD1001",
        Side::Buy,
        100,
        12.505,
        "1",
        OrdType::Limit,
        TimeInForce::IOC,
        Capacity::Principal,
        "swedbank",
        "karlg26"
    );

    EXPECT_EQ(44, bmsg.size());
}

TEST(EnterOrderTest, limit_day_order)
{
    const auto bmsg = create_enter_order_request(
        "ORD10000000001",
        Side::Buy,
        100,
        12.505,
        "1234567",
        OrdType::Limit,
        TimeInForce::Day,
        Capacity::Principal,
        "uncr",
        "karl26"
    );

    ASSERT_EQ(44, bmsg.size());

    auto it = bmsg.begin();
    EXPECT_EQ(0x4F, *it++); // message type

    std::vector<unsigned char> cl_ord_id = {0x4f, 0x52, 0x44, 0x31, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x31};
    EXPECT_TRUE(std::equal(it, it + 14, cl_ord_id.begin())); // client order id
    it = it + 14;

    EXPECT_EQ(0x42, *it++); // Side
    EXPECT_TRUE(0x00==*it++ && 0x00==*it++ && 0x00==*it++ && 0x64==*it++); // volume
    EXPECT_TRUE(0x00==*it++ && 0x12==*it++ && 0xD6==*it++ && 0x87==*it++); // symbol
    EXPECT_TRUE(0x00==*it++ && 0x01==*it++ && 0xE8==*it++ && 0x7A==*it++); // price
    EXPECT_TRUE(0x75==*it++ && 0x6E==*it++ && 0x63==*it++ && 0x72==*it++); // firm
    EXPECT_TRUE(0x6B==*it++ && 0x61==*it++ && 0x72==*it++ && 0x6C==*it++ && 0x32==*it++ && 0x36==*it++); // user
    EXPECT_EQ(0b00001001, *it++); // bitfield 1
    EXPECT_EQ(0b00000000, *it++); // bitfield 2
    EXPECT_EQ(0b00000000, *it++); // bitfield 3
    EXPECT_EQ(0b00000000, *it++); // bitfield 4
    EXPECT_EQ('0', *it++); // time in force
    EXPECT_EQ('2', *it); // capacity
}

TEST(EnterOrderTest, market_ioc_order)
{
    const auto bmsg = create_enter_order_request(
        "ORD10000000001",
        Side::Sell,
        270,
        12.505,
        "1002",
        OrdType::Market,
        TimeInForce::IOC,
        Capacity::RisklessPrincipal,
        "uncr",
        "karl26"
    );

    ASSERT_EQ(44, bmsg.size());

    auto it = bmsg.begin();
    EXPECT_EQ(0x4F, *it++); // message type

    std::vector<unsigned char> cl_ord_id = {0x4f, 0x52, 0x44, 0x31, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x31};
    EXPECT_TRUE(std::equal(it, it + 14, cl_ord_id.begin())); // client order id
    it = it + 14;

    EXPECT_EQ(0x53, *it++); // Side
    EXPECT_TRUE(0x00==*it++ && 0x00==*it++ && 0x01==*it++ && 0x0E==*it++); // volume
    EXPECT_TRUE(0x00==*it++ && 0x00==*it++ && 0x03==*it++ && 0xEA==*it++); // symbol
    EXPECT_TRUE(0x7F==*it++ && 0xFF==*it++ && 0xFF==*it++ && 0xFF==*it++); // price
    EXPECT_TRUE(0x75==*it++ && 0x6E==*it++ && 0x63==*it++ && 0x72==*it++); // firm
    EXPECT_TRUE(0x6B==*it++ && 0x61==*it++ && 0x72==*it++ && 0x6C==*it++ && 0x32==*it++ && 0x36==*it++); // user
    EXPECT_EQ(0b00001001, *it++); // bitfield 1
    EXPECT_EQ(0b00000000, *it++); // bitfield 2
    EXPECT_EQ(0b00000000, *it++); // bitfield 3
    EXPECT_EQ(0b00000000, *it++); // bitfield 4
    EXPECT_EQ('3', *it++); // time in force
    EXPECT_EQ('7', *it); // capacity
}

TEST(EnterOrderTest, short_ord_token)
{
    const auto bmsg = create_enter_order_request(
        "ORD1000001",
        Side::Buy,
        100,
        12.505,
        "100",
        OrdType::Limit,
        TimeInForce::Day,
        Capacity::Principal,
        "uncr",
        "karl26"
    );

    ASSERT_EQ(44, bmsg.size());

    auto it = bmsg.begin();
    EXPECT_EQ(0x4F, *it++); // message type

    std::vector<unsigned char> cl_ord_id = {0x4f, 0x52, 0x44, 0x31, 0x30, 0x30, 0x30, 0x30, 0x30, 0x31, 0x20, 0x20, 0x20, 0x20};
    EXPECT_TRUE(std::equal(it, it + 14, cl_ord_id.begin())); // client order id
    it = it + 14;

    EXPECT_EQ(0x42, *it++); // Side
    EXPECT_TRUE(0x00==*it++ && 0x00==*it++ && 0x00==*it++ && 0x64==*it++); // volume
    EXPECT_TRUE(0x00==*it++ && 0x00==*it++ && 0x00==*it++ && 0x64==*it++); // symbol
    EXPECT_TRUE(0x00==*it++ && 0x01==*it++ && 0xE8==*it++ && 0x7A==*it++); // price
    EXPECT_TRUE(0x75==*it++ && 0x6E==*it++ && 0x63==*it++ && 0x72==*it++); // firm
    EXPECT_TRUE(0x6B==*it++ && 0x61==*it++ && 0x72==*it++ && 0x6C==*it++ && 0x32==*it++ && 0x36==*it++); // user
    EXPECT_EQ(0b00001001, *it++); // bitfield 1
    EXPECT_EQ(0b00000000, *it++); // bitfield 2
    EXPECT_EQ(0b00000000, *it++); // bitfield 3
    EXPECT_EQ(0b00000000, *it++); // bitfield 4
    EXPECT_EQ('0', *it++); // time in force
    EXPECT_EQ('2', *it); // capacity
}

TEST(EnterOrderTest, big_orderbook_num)
{
    const auto bmsg = create_enter_order_request(
        "ORD10000000001",
        Side::Sell,
        270,
        12.505,
        "4294967295",
        OrdType::Market,
        TimeInForce::IOC,
        Capacity::Agency,
        "uncr",
        "karl26"
    );

    ASSERT_EQ(44, bmsg.size());

    auto it = bmsg.begin();
    EXPECT_EQ(0x4F, *it++); // message type

    std::vector<unsigned char> cl_ord_id = {0x4f, 0x52, 0x44, 0x31, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x31};
    EXPECT_TRUE(std::equal(it, it + 14, cl_ord_id.begin())); // client order id
    it = it + 14;

    EXPECT_EQ(0x53, *it++); // Side
    EXPECT_TRUE(0x00==*it++ && 0x00==*it++ && 0x01==*it++ && 0x0E==*it++); // volume
    EXPECT_TRUE(0xFF==*it++ && 0xFF==*it++ && 0xFF==*it++ && 0xFF==*it++); // symbol
    EXPECT_TRUE(0x7F==*it++ && 0xFF==*it++ && 0xFF==*it++ && 0xFF==*it++); // price
    EXPECT_TRUE(0x75==*it++ && 0x6E==*it++ && 0x63==*it++ && 0x72==*it++); // firm
    EXPECT_TRUE(0x6B==*it++ && 0x61==*it++ && 0x72==*it++ && 0x6C==*it++ && 0x32==*it++ && 0x36==*it++); // user
    EXPECT_EQ(0b00001001, *it++); // bitfield 1
    EXPECT_EQ(0b00000000, *it++); // bitfield 1
    EXPECT_EQ(0b00000000, *it++); // bitfield 1
    EXPECT_EQ(0b00000000, *it++); // bitfield 1
    EXPECT_EQ('3', *it++); // time in force
    EXPECT_EQ('1', *it); // capacity
}

TEST(EnterOrderTest, max_allowed_price)
{
    const auto bmsg = create_enter_order_request(
        "ORD10000000001",
        Side::Buy,
        100,
        199999.99,
        "1234567",
        OrdType::Limit,
        TimeInForce::Day,
        Capacity::Principal,
        "uncr",
        "karl26"
    );

    ASSERT_EQ(44, bmsg.size());

    auto it = bmsg.begin();
    EXPECT_EQ(0x4F, *it++); // message type

    std::vector<unsigned char> cl_ord_id = {0x4f, 0x52, 0x44, 0x31, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x31};
    EXPECT_TRUE(std::equal(it, it + 14, cl_ord_id.begin())); // client order id
    it = it + 14;

    EXPECT_EQ(0x42, *it++); // Side
    EXPECT_TRUE(0x00==*it++ && 0x00==*it++ && 0x00==*it++ && 0x64==*it++); // volume
    EXPECT_TRUE(0x00==*it++ && 0x12==*it++ && 0xD6==*it++ && 0x87==*it++); // symbol
    EXPECT_TRUE(0x77==*it++ && 0x35==*it++ && 0x93==*it++ && 0x9C==*it++); // price
    EXPECT_TRUE(0x75==*it++ && 0x6E==*it++ && 0x63==*it++ && 0x72==*it++); // firm
    EXPECT_TRUE(0x6B==*it++ && 0x61==*it++ && 0x72==*it++ && 0x6C==*it++ && 0x32==*it++ && 0x36==*it++); // user
    EXPECT_EQ(0b00001001, *it++); // bitfield 1
    EXPECT_EQ(0b00000000, *it++); // bitfield 2
    EXPECT_EQ(0b00000000, *it++); // bitfield 3
    EXPECT_EQ(0b00000000, *it++); // bitfield 4
    EXPECT_EQ('0', *it++); // time in force
    EXPECT_EQ('2', *it); // capacity
}

TEST(EnterOrderTest, too_long_firm_and_user)
{
    const auto bmsg = create_enter_order_request(
        "ORD10000000001",
        Side::Buy,
        100,
        12.505,
        "1234567",
        OrdType::Limit,
        TimeInForce::Day,
        Capacity::Principal,
        "itiviti",
        "karl.g26"
    );

    ASSERT_EQ(44, bmsg.size());

    auto it = bmsg.begin();
    EXPECT_EQ(0x4F, *it++); // message type

    std::vector<unsigned char> cl_ord_id = {0x4f, 0x52, 0x44, 0x31, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x31};
    EXPECT_TRUE(std::equal(it, it + 14, cl_ord_id.begin())); // client order id
    it = it + 14;

    EXPECT_EQ(0x42, *it++); // Side
    EXPECT_TRUE(0x00==*it++ && 0x00==*it++ && 0x00==*it++ && 0x64==*it++); // volume
    EXPECT_TRUE(0x00==*it++ && 0x12==*it++ && 0xD6==*it++ && 0x87==*it++); // symbol
    EXPECT_TRUE(0x00==*it++ && 0x01==*it++ && 0xE8==*it++ && 0x7A==*it++); // price
    EXPECT_TRUE(0x69==*it++ && 0x74==*it++ && 0x69==*it++ && 0x76==*it++); // firm
    EXPECT_TRUE(0x6B==*it++ && 0x61==*it++ && 0x72==*it++ && 0x6C==*it++ && 0x2E==*it++ && 0x67==*it++); // user
    EXPECT_EQ(0b00001001, *it++); // bitfield 1
    EXPECT_EQ(0b00000000, *it++); // bitfield 2
    EXPECT_EQ(0b00000000, *it++); // bitfield 3
    EXPECT_EQ(0b00000000, *it++); // bitfield 4
    EXPECT_EQ('0', *it++); // time in force
    EXPECT_EQ('2', *it); // capacity
}

TEST(EnterOrderTest, too_short_user)
{
    const auto bmsg = create_enter_order_request(
        "ORD10000000001",
        Side::Sell,
        270,
        12.505,
        "1002",
        OrdType::Market,
        TimeInForce::IOC,
        Capacity::RisklessPrincipal,
        "uncr",
        "rk"
    );

    ASSERT_EQ(44, bmsg.size());

    auto it = bmsg.begin();
    EXPECT_EQ(0x4F, *it++); // message type

    std::vector<unsigned char> cl_ord_id = {0x4f, 0x52, 0x44, 0x31, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x31};
    EXPECT_TRUE(std::equal(it, it + 14, cl_ord_id.begin())); // client order id
    it = it + 14;

    EXPECT_EQ(0x53, *it++); // Side
    EXPECT_TRUE(0x00==*it++ && 0x00==*it++ && 0x01==*it++ && 0x0E==*it++); // volume
    EXPECT_TRUE(0x00==*it++ && 0x00==*it++ && 0x03==*it++ && 0xEA==*it++); // symbol
    EXPECT_TRUE(0x7F==*it++ && 0xFF==*it++ && 0xFF==*it++ && 0xFF==*it++); // price
    EXPECT_TRUE(0x75==*it++ && 0x6E==*it++ && 0x63==*it++ && 0x72==*it++); // firm
    EXPECT_TRUE(0x72==*it++ && 0x6B==*it++ && 0x20==*it++ && 0x20==*it++ && 0x20==*it++ && 0x20==*it++); // user
    EXPECT_EQ(0b00001001, *it++); // bitfield 1
    EXPECT_EQ(0b00000000, *it++); // bitfield 1
    EXPECT_EQ(0b00000000, *it++); // bitfield 1
    EXPECT_EQ(0b00000000, *it++); // bitfield 1
    EXPECT_EQ('3', *it++); // time in force
    EXPECT_EQ('7', *it); // capacity
}

TEST(EnterOrderTest, thin_price)
{
    const auto bmsg = create_enter_order_request(
        "ORD10000000001",
        Side::Buy,
        100,
        1.0001,
        "1234567",
        OrdType::Limit,
        TimeInForce::Day,
        Capacity::Principal,
        "uncr",
        "karl26"
    );

    ASSERT_EQ(44, bmsg.size());

    auto it = bmsg.begin();
    EXPECT_EQ(0x4F, *it++); // message type

    std::vector<unsigned char> cl_ord_id = {0x4f, 0x52, 0x44, 0x31, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x31};
    EXPECT_TRUE(std::equal(it, it + 14, cl_ord_id.begin())); // client order id
    it = it + 14;

    EXPECT_EQ(0x42, *it++); // Side
    EXPECT_TRUE(0x00==*it++ && 0x00==*it++ && 0x00==*it++ && 0x64==*it++); // volume
    EXPECT_TRUE(0x00==*it++ && 0x12==*it++ && 0xD6==*it++ && 0x87==*it++); // symbol
    EXPECT_TRUE(0x00==*it++ && 0x00==*it++ && 0x27==*it++ && 0x11==*it++); // price
    EXPECT_TRUE(0x75==*it++ && 0x6E==*it++ && 0x63==*it++ && 0x72==*it++); // firm
    EXPECT_TRUE(0x6B==*it++ && 0x61==*it++ && 0x72==*it++ && 0x6C==*it++ && 0x32==*it++ && 0x36==*it++); // user
    EXPECT_EQ(0b00001001, *it++); // bitfield 1
    EXPECT_EQ(0b00000000, *it++); // bitfield 2
    EXPECT_EQ(0b00000000, *it++); // bitfield 3
    EXPECT_EQ(0b00000000, *it++); // bitfield 4
    EXPECT_EQ('0', *it++); // time in force
    EXPECT_EQ('2', *it); // capacity
}
