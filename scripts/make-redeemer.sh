input=$1

file=redeemer-$input.json 
echo "file:$file"
echo "input:$input"

cabal exec make-redeemer $file $input 