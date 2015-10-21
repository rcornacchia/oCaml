ocamllex wordcount.mll
echo " "
ocamlc -o wordcount wordcount.ml
echo " "
./wordcount < wordcount.mll
echo " "
