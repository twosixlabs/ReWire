#!/bin/bash


# Get the working dir from env
working_dir=$(dirname "$0")

# Move to the specified working directory
cwd=$(pwd)
# echo "cwd: $cwd"
# echo "working_dir: $working_dir"
cd "$working_dir" || exit 1

echo "Building ReWire session..."
isabelle build -b -D ../../../rewire-targets/isabelle/thys ReWire
echo "...Completed."

# Return to prior working directory
cd "$cwd"
