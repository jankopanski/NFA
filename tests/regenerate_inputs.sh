mkdir inputs

for i in $(seq 1 10000)
do
  python gen.py > inputs/test$i.in
done
