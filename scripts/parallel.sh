#!/bin/bash

if [ "$#" -ne 2 ]; then
    echo "Usage: $0 log_dir/ command_file" >&2
    echo "" >&2
    echo "Runs each shell command from a file (one per line) in parallel." >&2
    exit 2
fi

LOG_DIR="$1"
COMMAND_FILE="$2"
EXIT_DIR="$(mktemp -d -t parallel.XXXXXXXXXX)"

mkdir -p "$LOG_DIR"

i=0
while read command ; do
    LOG_FILE="$LOG_DIR/$i.log"
    EXIT_FILE="$EXIT_DIR/$i"
    echo "Logging output of '$command' to file: $LOG_FILE"
    (
        sh -c "$command" > "$LOG_FILE" 2>&1
        echo "$?" > "$EXIT_FILE"
    ) &
    i=$((i+1))
done < "$COMMAND_FILE"

wait

failed=0

i=0
while read command ; do
    LOG_FILE="$LOG_DIR/$i.log"
    EXIT_FILE="$EXIT_DIR/$i"
    if [ "$(cat "$EXIT_DIR/$i")" -ne 0 ]; then
        echo "==> $command ($LOG_FILE) <==" >&2
        cat "$LOG_FILE" >&2
        echo "" >&2
        failed=$((failed+1))
    fi
    i=$((i+1))
done < "$COMMAND_FILE"

if [ $failed -gt 0 ]; then
    if [ $failed -eq 1 ]; then
        echo "1 command failed" >&2
    else
        echo "$failed commands failed" >&2
    fi
    exit 1
else
    exit 0
fi
