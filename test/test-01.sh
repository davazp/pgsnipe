#!/bin/bash

PGSNIPE_BIN="../pgsnipe"
PGSNIPE_ARGS="--commit"

if [ ! -x "$PGSNIPE_BIN" ]; then
    echo "You must build pgsnipe before running the tests" 2>&1
    exit -1
fi

PGSNIPE="$PGSNIPE_BIN $PGSNIPE_ARGS"

export PGDATABASE="pgsnipe_test_db"

dropdb --if-exists "$PGDATABASE"
createdb "$PGDATABASE"

$PGSNIPE test-01.sql
$PGSNIPE test-01.sql
