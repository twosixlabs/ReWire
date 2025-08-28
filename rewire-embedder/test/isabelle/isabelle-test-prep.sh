#!/bin/bash

# Accept AFP path as first argument, or use environment variable
AFP_PATH="${1:-$AFP}"

# Get the working dir from env
working_dir=$(dirname "$0")

# Move to the specified working directory
cwd=$(pwd)
# echo "cwd: $cwd"
# echo "working_dir: $working_dir"
cd "$working_dir" || exit 1

echo "Checking for required Isabelle AFP components..."

# Check if AFP_PATH is provided and valid
if [ -z "$AFP_PATH" ]; then
    echo "ERROR: AFP path not provided."
    echo "Usage: $0 <AFP_PATH>"
    echo "   or set AFP environment variable"
    echo ""
    echo "The ReWire session requires AFP components Word_Lib and Jordan_Normal_Form."
    echo "Please install AFP from: https://www.isa-afp.org/download.html"
    exit 1
fi

# Normalize AFP path - add /thys if needed
if [ -d "$AFP_PATH/thys" ]; then
    AFP_PATH="$AFP_PATH/thys"
fi

if [ ! -d "$AFP_PATH" ]; then
    echo "ERROR: AFP path does not exist: $AFP_PATH"
    exit 1
fi

# Verify AFP contains required sessions
# Check if Word_Lib ROOT file exists
if [ -f "$AFP_PATH/Word_Lib/ROOT" ]; then
    echo "AFP components found at: $AFP_PATH"
else
    echo "ERROR: Cannot find Word_Lib session in AFP at: $AFP_PATH"
    echo "Make sure AFP is properly installed and the path points to the 'thys' directory."
    exit 1
fi

echo "Building ReWire session and dependencies..."
isabelle build -b -d ../../targets/isabelle/thys -d "$AFP_PATH" ReWire
if [ $? -ne 0 ]; then
    echo "Failed to build ReWire session. Make sure AFP is properly installed."
    exit 1
fi
echo "...Completed."

# Return to prior working directory
cd "$cwd"
