#!/bin/bash

#This program calculates the core count for each result row and updates the
#results. This is a work around due to a current limitation of
#HdpHBencher/HsBencher.

#Author: Blair Archibald

function usage {
  echo "Usage: processResults.sh <resultsFile>"
  exit -1
}

test -z $1 && usage

resultsFile="$1"

#Find the results column
flags=$(awk -F, '

NR == 1 {
  for (i = 1; i <= NF; i++) {
    if ($i == "RUNTIME_FLAGS") {
      cid = i;
    }
  }
}

NR > 1 {
  if (cid > 0) {
    print $cid
  }
}

' $resultsFile)

#Get the thread count and the processor count.
j=0
declare -a Lines
while read i; do
  procs=$(echo $i | egrep -o "numProcs: [0-9]+" | egrep -o "[0-9]+")
  threads=$(echo $i | egrep -o "numThreads: [0-9]+" | egrep -o "[0-9]+")
  cores=$(($procs * $threads))
  Lines[j]=$cores
  let "j += 1"
done <<< "$flags"

#Could also use sed or awk here. 
line=-1
tmp=$(mktemp)
while read l; do
  #First line we add a new header
  if [[ $line == -1 ]]; then
    echo "$l,NUM_CORES" > $tmp
  else
    echo "$l,${Lines[$line]}" >> $tmp
  fi
  let "line += 1"
done < "$resultsFile"

cp $resultsFile ${resultsFile}.bak
cp $tmp $resultsFile
