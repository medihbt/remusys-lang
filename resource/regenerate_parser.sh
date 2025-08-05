#!/bin/bash

# Script to regenerate the parser from grammar.lalrpop
# This should be run whenever the grammar file is modified

curr_dir=$(dirname "$0")
project_rootdir="$(git rev-parse --show-toplevel)"
cd "$project_rootdir/$curr_dir"

echo "Regenerating parser from grammar.lalrpop..."

# Use the lalrpop binary to process the grammar file
lalrpop resource/grammar.lalrpop -o src/

if [ $? -eq 0 ]; then
    echo "Parser successfully generated!"
    echo "Generated file: src/grammar.rs"
else
    echo "Failed to generate parser!"
    exit 1
fi
