#!/bin/bash
# based on https://github.com/haskelling/aoc2020/blob/main/get
# you can call this script with no arguments to download everything, or
# ./get_input.sh 2021 1
# to get a specific date's input

cd "$(dirname "$0")"

# .cookie/
# AOC_COOKIE="session=278238929h37894hfg893fh42378fdj2389879432h8fg3"

if [ -r .cookie ]; then
  . .cookie
fi

export TZ=EST
thisyear=${1:-$(date +%Y)}
month=12
thisday=${2:-$(date +%d)}

echo "trying to fetch for date(s): $thisyear/$month/$thisday"

for day in $(seq ${2:-1} ${2:-25}); do
    if [[ "$thisyear" -ne ${1:-$(date +%Y)} || "$day" -gt 25 ]]; then
      echo "Nothing more to fetch"
      exit 0
  fi

  filename="./data/$thisyear/$day".input
  if [ -r "$filename" ]; then
      echo "$day input data already present, skipping fetch."
    continue  # make sure we don't fetch the same file twice!
  fi
  echo "getting input for year: $thisyear, day: $day "
  #echo "filename: $filename"
  #echo "curl -sS -o "$filename" -b "$AOC_COOKIE" https://adventofcode.com/"$thisyear"/day/"$thisday"/input"
  curl -sS -o "$filename" -b "$AOC_COOKIE" https://adventofcode.com/"$thisyear"/day/"$day"/input
  echo "sleep 2 seconds"
  sleep 2
done
