#!/bin/bash
dir="/var/log/*.log"
find $dir -print0 | wc --files0-from=- -l
