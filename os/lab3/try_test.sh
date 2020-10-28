#!/bin/bash
curDate=$(date '+%d.%m.%y_%H:%M:%S')
(mkdir ~/lab3/test && echo "catalog test was created successfully" >> ~/lab3/report && touch ~/lab3/test/$curDate)
(ping -c 1 http://www.net_nikogo.ru/) || (echo "$curDate No answer from net_nikogo.ru lol" >> ~/lab3/report)

