# Sigcomm 2021 Example Programs and Patching Scripts

This directory contains example programs from sigcomm 2021 and scripts for patching them to work with the current lucid compiler. These programs are tested to generate compiling p4, but have not been run.

## Scripts

- `mk.sh`: Shell script to build a single project based on the provided arguments.
- `all_mk.sh`: Shell script to build all .dpt files in the current directory.
- `patch_script.py`: Python script to apply adjustments to .dpt files.

## Example Programs

- `./orig`: Directory containing original example programs.
- `./patched`: Directory containing patched example programs.

## Date Patched

The example programs were patched on [PATCH_DATE] and compiled on commit: [COMMIT_HASH](https://github.com/princetonuniversity/lucid/commit/COMMIT_HASH)