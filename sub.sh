
echo $1
number1=1
number2=1
for number1 in {1..5} 
do
  for number2 in {1..10} 
  do
    qsub -N $1 -v ARFF=$1,fold=$number1,exec=$number2 script.sh
  done
done


# echo $1
# number1=1
# number2=1
# until [ $number1 -ge 6 ]; do
#   until [ $number2 -ge 11 ]; do
#     qsub -N co -v ARFF=$1,fold=$number1,exec=$number2 script.sh
#     number2=$((number2 + 1))
#   done
#   number1=$((number1 + 1))
# done