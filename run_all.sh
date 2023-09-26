#!/bin/bash

# Define the regular expression pattern you want to search for.
# Replace 'YOUR_PATTERN' with your actual regex pattern.
pattern="YOUR_PATTERN"

# Output file for storing results
results_file="results.txt"

# Loop through each file in files.txt
while IFS= read -r filename; do
  # Capture the start time
  start_time=$(date +%s.%N)
  
  # Run the command and redirect the output to output.txt
  ./dptc "$filename" > output.txt
  
  # Capture the end time
  end_time=$(date +%s.%N)
  
  # Calculate the runtime in seconds
  runtime=$(echo "$end_time - $start_time" | bc)
  
  # Use grep to search for the pattern and store the results
  grep "$pattern" output.txt > temp_results.txt
  
  # Output the results and runtime to results.txt
  echo "File: $filename" >> "$results_file"
  echo "Runtime: $runtime seconds" >> "$results_file"
  cat temp_results.txt >> "$results_file"
  echo "-----------------------------------" >> "$results_file"
  
  # Clean up temporary files
  rm output.txt temp_results.txt
done < files.txt
