#!/bin/bash
# this scripts allows gaston to run the PFEM tests in background
#
# run this script as:
#   echo "/bin/bash ./run_tests.sh" | at now -m

# load required environment
. ~/.profile

./run.py -k 10 tests/waterdrop.py
./run.py -k 10 tests/waterdrop2.py
./run.py -k 10 tests/waterdrop3.py
./run.py -k 10 tests/waterdrop4.py

echo "done."
