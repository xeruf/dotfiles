#!/bin/sh -e
find \( -name src -prune -o -name pkg -prune -o -name "*.deb" -o -name "*.xz" -o -name "*.zip" -o -name "*.zst" \) -exec rm -r -Iv {} +
