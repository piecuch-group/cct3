#!/bin/sh

cct3_energy=$(awk '/CC\(t;3\)  /{print $3}' examples/H8-0.1.out)
check=$(python -c "print(abs($cct3_energy - -4.220587742726) > 1e-10)")

if [ "$check" == "True" ]; then
    echo "Energy past 1e-10 error threshold"
    exit 1
else
    exit 0
fi




