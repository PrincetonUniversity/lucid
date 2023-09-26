#!/bin/bash

# Clear previous results
> results.txt

# Loop through each filename in files.txt
while IFS= read -r filename; do
    # Time the dptc command and run it, redirecting output to output.txt
    { time ./dptc "$filename"; } > output.txt 2>&1

    # Extract synthesis time, alphabet size, and states size using grep and regular expressions
    synthesis_time=$(grep -oE "Time spent on synthesis is ([0-9.]+)" output.txt | sed 's/Time spent on synthesis is //')
    alphabet_size=$(grep -oE "Alphabet size ([0-9]+)" output.txt | sed 's/Alphabet size //')
    states_size=$(grep -oE "States size ([0-9]+)" output.txt | sed 's/States size //')

    # Append results to results.txt
    echo "File: $filename" >> results.txt
    echo "Synthesis Time: $synthesis_time" >> results.txt
    echo "Alphabet Size: $alphabet_size" >> results.txt
    echo "States Size: $states_size" >> results.txt
    dptc_time=$(grep "real" output.txt | awk '{print $2}')

    # Append bf-p4c runtime to results.txt
    echo "dptc Runtime for $filename: $dptc_time" >> results.txt

    # Time the bf-p4c command and run it
    { time ~/p4c-9.13.0.x86_64/bin/bf-p4c -g "./lucid_tofino_build/lucid.p4"; } > bf-p4c_time.txt 2>&1

    # Extract the real time from the time command output
    bf_p4c_real_time=$(grep "real" bf-p4c_time.txt | awk '{print $2}')

    # Append bf-p4c runtime to results.txt
    echo "bf-p4c Runtime for $filename: $bf_p4c_real_time" >> results.txt

    stages_count=$(grep -oE "Number of stages in table allocation: ([0-9]+)" "lucid.tofino/pipe/logs/table_summary.log" | sed 's/Number of stages in table allocation: //')

    # Append stages count to results.txt
    echo "Number of Stages: $stages_count" >> results.txt

    PHV_usage=$(grep "Overall PHV Usage" "lucid.tofino/pipe/logs/phv_allocation_summary_0.log" | cut -d "|" -f3)

    echo "PHV Usage: $PHV_usage" >> results.txt

    VLIW_usage=$(grep "Average" "lucid.tofino/pipe/logs/mau.resources.log" | cut -d "|" -f11)

    echo "VLIW usage: $VLIW_usage" >> results.txt

    # Clean up temporary files
    rm -f bf-p4c_time.txt

    # Separate results for each file
    echo "--------------------------" >> results.txt

done < files.txt

# Print a summary
cat results.txt

# Done