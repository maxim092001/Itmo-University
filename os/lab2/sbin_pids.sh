#!/bin/bash
pids=$(ps -ax)
awk '$5~/^\/sbin\//{print $1, $5}' <<< "$pids"

