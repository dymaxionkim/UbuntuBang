#!/bin/bash

# Pyenv
export PATH="/home/osboxes/.pyenv/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
pyenv activate CalculixCAE;

unset LD_LIBRARY_PATH;
cd /home/osboxes/Calculix/cae;
/home/osboxes/Calculix/cae/cae.sh;
#sleep 100
