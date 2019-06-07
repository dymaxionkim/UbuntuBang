#!/bin/bash
#feh --auto-zoom --geometry 800x600 $1

shopt -s nullglob

if [[ ! -f $1 ]]; then
	echo "$0: first argument is not a file" >&2
	exit 1
fi

file=$(basename -- "$1")
dir=$(dirname -- "$1")
arr=()
shift

cd -- "$dir"

for i in *; do
	[[ -f $i ]] || continue
	arr+=("$i")
	[[ $i == $file ]] && c=$((${#arr[@]} - 1))
done

exec feh --auto-zoom --geometry 640x480 "$@" -- "${arr[@]:c}" "${arr[@]:0:c}" 
