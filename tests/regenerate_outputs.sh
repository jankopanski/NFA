mkdir outputs

INPUT_DIR=$(pwd)/inputs
OUTPUT_DIR=$(pwd)/outputs
cd ../src

for input in $(ls $INPUT_DIR)
do
  runhaskell RunAuto.hs $INPUT_DIR/$input > $OUTPUT_DIR/${input/%.in/.out}
done
