#!/usr/bin/env bash

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# theme

# Four themes: [ 'dark-colorful', 
#      'bright-colorful', 'dark-arrow', 'bright-arrow' ]

theme='bright-deco'

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# include

DIR=$(dirname "$0")
. ${DIR}/gmc.sh
. ${DIR}/vars.sh
. ${DIR}/segments.sh
. ${DIR}/output.sh

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----
# main

# remove all dzen2 instance
pkill dzen2

# execute dzen
generated_output | dzen2 $parameters &

# optional transparency
# https://github.com/wildefyr/transset-df
# sleep 1 && exec `(transset-df .8 -n dzentop >/dev/null 2>&1 &)` &

# you may use xorg-transset instead of transset-df
sleep 1 && exec `(transset .8 -n dzentop >/dev/null 2>&1 &)` &

