# Кодирование сообщения EnterOrder
## Новый протокол!
В примере на лекции мы познакомились с протоколом BOE, используемом преимущественно на торговых площадках, принадлежащих бирже CBOE.
В этом задании вам предлагается познакомиться с ещё одним протоколом - OUCH, используемом более широко (в разных вариациях), однако
мы сосредоточимся на его варианте, используемом на площадках Nasdaq Nordic (INET), объединяюищх скандинавские биржи.

## Идея
Полностью аналогично рассмотренному на лекции примеру, здесь мы будем кодировать сообщение для посылки нового заказа на биржу.
Основные данные, отправляемые в заказе - те же. Устройство протокола тоже похоже на BOE.

Существенное отличие: числа кодируются в big endian формате.

## Задача
Требуется реализовать кодировщик сообщения EnterOrder (см. [protocol specifications](doc/OUCH_for_Nasdaq_Nordic_4.03.2.pdf))
имеющий следующий интерфейс:
```cpp
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
```

Использование полей:
* cl_ord_id -> Order Token
* side -> Buy/Sell indicator (Buy -> 'B', Sell -> 'S')
* volume -> Quantity
* price -> Price
* symbol -> Order Book (числовой идентификатор инструмента закодирован в symbol как десятичное число)
* ord_type -> неявно кодируется в поле Price: Limit -> Price = price или Market -> Price = 0x7FFFFFFF.
* time_in_force -> Time In Force (Day -> '0', IOC -> '3')
* capacity -> Capacity (Agency -> '1', Principal -> '2', RisklessPrincipal -> '7')
* firm -> Firm
* user -> User
