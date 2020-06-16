#!/bin/bash

iconv -c -f EUC-KR -t UTF-8 $1 > $1'.utf8'
