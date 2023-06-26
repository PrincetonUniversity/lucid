import subprocess

OUT_FILE = "output.txt"
IN_FILE = "input.txt"

def anonymize():
    interactive_cmd = "docker run --rm -i --mount type=bind,source=/Users/irisshi/Downloads/princeton-intel/lucid/langsec/examples/anonymize/anonymize.dpt,target=/app/inputs/anonymize.dpt --mount type=bind,source=/Users/irisshi/Downloads/princeton-intel/lucid/langsec/examples/anonymize/anonymize.json,target=/app/inputs/anonymize.json jsonch/lucid:lucid /bin/sh -c \"./dpt -i /app/inputs/anonymize.dpt --spec /app/inputs/anonymize.json\""
    in_file = open(IN_FILE, "r")
    with open(OUT_FILE, "w") as out_file:
        try:
            result = subprocess.run(interactive_cmd, shell=True, stdin=in_file, stdout=out_file, timeout=3)
        except subprocess.TimeoutExpired:
            pass
        out_file.close()

anonymize()