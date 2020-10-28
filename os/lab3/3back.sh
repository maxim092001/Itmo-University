#!/bin/bash
cpulimit --limit 10 ./generator4.sh &
cpulimit --limit 5 ./generator4.sh &
./generator4.sh &
kill $!
