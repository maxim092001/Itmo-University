#!/bin/bash
grep -Ewo '[[:alnum:]_]+@[[:alnum:]]+\.[[:alnum:]]{2,3}' --binary-files=text -r /etc -h > emails.lst

