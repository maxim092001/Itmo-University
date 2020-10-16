#!/bin/bash
ps -ao pid,stime | tail -n+2 | sort -k2 | tail -1 | awk '{print $1}'
