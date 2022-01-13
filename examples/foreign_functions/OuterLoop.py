# Call the dpt executable
# Could also put this in a loop if you wanted to run a bunch of times and
# do stuff with the results
import subprocess, pickle

open("BloomFilter_out.txt", "w").close()

cmd = ["../../dpt", "--silent", "BloomFilter.dpt"]
# ret = subprocess.run(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL) # Suppress dpt output
ret = subprocess.run(cmd) # Run with full output
if ret.returncode != 0:
    print("Error!")

outfile = open("BloomFilter_out.txt", "rb")

print("Final results:")
try: # Hacky, but quick way to unpickle the whole file
    while True:
        print(pickle.load(outfile))
except EOFError:
    pass
