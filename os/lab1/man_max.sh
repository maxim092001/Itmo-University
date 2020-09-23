#!/bin/bash
man bash | grep -Eio "[[:alnum:]_]{4,}" | sort |  uniq -c | sort -nr -k1 | awk '{print $2}' | head --lines=3
