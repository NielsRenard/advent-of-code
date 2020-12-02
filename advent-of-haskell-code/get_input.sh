#!/bin/bash
# based on https://github.com/haskelling/aoc2020/blob/main/get

cd "$(dirname "$0")"

if [ -r .cookie ]; then
  . .cookie
fi

export TZ=EST
thisyear=${1:-$(date +%Y)}
thismonth="$(date +%m)"
thisday=${2:-$(date +%d)}

echo "$thisyear // $thismonth // $thisday"

for day in {1..25}; do
    if [[ "$thisyear" -ne ${1:-$(date +%Y)} || "$thismonth" -ne 12 || "$day" -gt ${2:-$(date +%d)} ]]; then
      echo "Nothing more to fetch"
      exit 0
  fi

  filename="./data/$thisyear/$day".input
  if [ -r "$filename" ]; then
    continue  # make sure we don't fetch the same file twice!
  fi
  echo "getting input for year: $thisyear, day: $day "
  echo "filename: $filename"
  echo "curl -sS -o "$filename" -b "$AOC_COOKIE" https://adventofcode.com/"$thisyear"/day/"$thisday"/input"
  curl -sS -o "$filename" -b "$AOC_COOKIE" https://adventofcode.com/"$thisyear"/day/"$day"/input
done
