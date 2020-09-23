#!/bin/bash
	

echo "Press 1 to run nano"
echo "Press 2 to run vi"
echo "Press 3 to run links"
echo "Press 4 to exit"

read num

case $num in
1)
  nano
  ;;
2)
  vi
  ;;
3)
  links
  ;;
4)
  exit
  ;;
esac
