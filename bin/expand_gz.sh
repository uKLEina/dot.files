#!/bin/bash

# ==============================================================================
#
# Title: GZ File Expander
# Description: This script decompresses all *.gz files from a specified source
#              directory and saves the output to a specified destination directory.
# Author: Gemini
# Date: 2024-06-26
#
# Usage: ./expand_gz.sh <source_directory> <destination_directory>
#
# ==============================================================================

# --- Configuration ---
# Exit immediately if a command exits with a non-zero status.
set -e

# --- Functions ---

# Function to display usage information
usage() {
    echo "Usage: $0 <source_directory> <destination_directory>"
    echo ""
    echo "This script expands all *.gz files from the specified source directory"
    echo "into the specified destination directory."
    echo ""
    echo "Arguments:"
    echo "  <source_directory>        The path to the directory containing the .gz files."
    echo "  <destination_directory>   The path to the directory where files will be extracted."
    exit 1
}

# --- Main Script Logic ---

# Check for the correct number of arguments.
# We expect exactly two arguments: source and destination directories.
if [ "$#" -ne 2 ]; then
    echo "Error: Invalid number of arguments."
    usage
fi

# Assign arguments to variables for clarity.
SOURCE_DIR="$1"
DEST_DIR="$2"

# Check if the source directory exists and is actually a directory.
if [ ! -d "$SOURCE_DIR" ]; then
    echo "Error: Source directory '$SOURCE_DIR' not found or is not a directory."
    exit 1
fi


# Create the destination directory if it does not already exist.
# The '-p' flag ensures that no error is thrown if the directory
# already exists, and it creates parent directories as needed.
echo "--> Checking destination directory: '$DEST_DIR'"
mkdir -p "$DEST_DIR"
echo "--> Destination directory is ready."
echo ""

# Find all files ending in .gz in the source directory.
# We use a 'for' loop to iterate over each file found.
# The 'shopt -s nullglob' command ensures the loop doesn't run at all
# if no *.gz files are found, preventing errors.
shopt -s nullglob
files=("$SOURCE_DIR"/*.gz)

if [ ${#files[@]} -eq 0 ]; then
  echo "--> No *.gz files found in directory '$SOURCE_DIR'. Exiting."
  exit 0
fi

echo "--> Starting decompression process for files in '$SOURCE_DIR'..."
for file in "${files[@]}"; do
    # Ensure that we are only processing actual files.
    if [ -f "$file" ]; then
        # Construct the output filename by removing the '.gz' extension.
        # The 'basename' command is used to strip path and suffix from filename.
        base_name=$(basename "$file" .gz)
        output_file="$DEST_DIR/$base_name"

        echo "    -> Decompressing '$file' to '$output_file'"

        # Decompress using 'gunzip -c'.
        # The '-c' flag tells gunzip to write its output to standard output,
        # which we then redirect ('>') to our target file. This also has the
        # benefit of leaving the original .gz file intact.
        gunzip -c "$file" > "$output_file"
    fi
done
shopt -u nullglob # Revert the nullglob option to its default state.

echo ""
echo "--> All files have been successfully decompressed."
echo "--> Extracted files are located in: '$DEST_DIR'"
echo "--> Process complete."
