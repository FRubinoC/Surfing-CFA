make clean
make
mkdir attacks
mkdir attacks/simpleAttack
mkdir attacks/complexAttack

FILENAME="test1"

while getopts n:f: flag;
do
    case "${flag}" in
        f) FILENAME="${OPTARG}";;
    esac
done

cat test/$FILENAME.txt | ./main.native