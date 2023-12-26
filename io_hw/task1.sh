#!/bin/bash

for i in {1..2999}; do
    echo "String $i" >> input.txt
done

cat input.txt | ./task1

files_count=$(ls text_copy_*.log | wc -l)

if [ $files_count -eq 3 ]; then
    echo "files were created correctly"
else
    echo "invalid number of files"
fi

lines_count=$(grep -v '^$' text_copy_*.log | wc -l)
echo $lines_count

if [ $lines_count -eq 2999 ]; then
    echo "strings are writed correctly"
else
    echo "invalid numbers of strings"
fi

rm text_copy_*.log
rm input.txt