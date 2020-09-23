#!/bin/bash

log=$(sed -e 's/INFO/Information:/g; s/WARNING:/Warning:/g' /var/log/syslog)
echo "$log" | grep "Warning:" >> full.log
echo "$log" | grep "Information:" >> full.log
cat full.log
