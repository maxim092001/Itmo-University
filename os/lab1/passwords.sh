#!/bin/bash
awk -F: '{print $1 " " $3|"sort -n -k2"}' /etc/passwd
