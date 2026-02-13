#!/bin/bash -e
lftp -u "$(basename "$1"),$(pass "$1")" "$(basename "$(dirname "$1")")"
