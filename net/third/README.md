# DHCP-сервер

В этом задании адреса нужно выдавать вам. Для начала активируйте это задание и подключитесь к VPN. Вы попадёте в сеть, в которой ничего нет.

## Часть 1. Выдайте IPv4 (4 балла)

Запустите собственный DHCP-сервер, который будет выдавать клиентам на интерфейсе VPN адреса из сети 10.174.196.0/24. Вы можете реализовать его самостоятельно или взять любой готовый.

Мы будем очень рады, если вы попробуете разобраться с не самым популярным решением.

## Часть 2. Настройте SLAAC (4 балла)

Теперь нужно настроить IPv6 — ваша подсеть fdbb:1599:dd7a:97f6::/64. Но в этой части вам нужно сделать так, чтобы клиенты получали адрес с помощью SLAAC. Для этого нужно отвечать на запросы Router Solicitation каждый раз, когда приходит новый клиент.

Чтобы получить баллы за части 1 и 2, нажмите на кнопку. Наш бот придёт в вашу сеть и попробует получить адреса.
**Тут кнопка**

## Часть 3. MAC-адрес (2 балла)

Дело за малым — настроить логи. Скажите MAC-адрес нашего бота в формате aa:bb:cc:dd:ee:ff. Если вы уже получили баллы, а MAC-адрес не узнали — не беда. Нажмите кнопку «Проверить» ещё раз, и бот снова подключится.

Не забудьте выключить и (если это был сервис) деактивировать сервер — иначе можно случайно сломать себе домашнюю сеть.
