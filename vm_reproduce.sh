# compile the benchmark apps from lucid to P4 and report results.

# bring up lucid vm
if vagrant up --provider=virtualbox; 
then 
    # in vm: rebuild lucid compiler
    echo "rebuilding lucid compiler inside of VM..."
    vagrant ssh -c "cd lucid; make"
    # in vm: build the apps
    echo "running sigcomm_apps/reproduce.sh inside of VM..."
    vagrant ssh -c "cd lucid/sigcomm_apps; ./reproduce.sh"
else
    echo "error: could not bring up vm. Did you run artifact_setup.sh?"
fi