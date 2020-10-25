#!/bin/bash

binbash=$(grep -n '#!/bin/bash$' $1 | cut -f 1)

sed -in-place -r 's/#.*//' $1

if [[ ! -z $binbash ]]
then
    sed -i '1i#!/bin/bash' $1
fi
