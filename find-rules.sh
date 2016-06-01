#!/bin/bash

HACKAGE=/Volumes/Data/hackage
HASRULES=dist/build/hasrules/hasrules

cabal build

find $HACKAGE -type f -name "*.hs" -exec bash -c "$HASRULES {} 2> /dev/null && echo {} | cut -sd / -f 5- " \; > has-rules.out
