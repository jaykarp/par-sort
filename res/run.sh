for i in {1..32}
do
stack exec -- par-sort-exe +RTS -N8 -A600M -RTS -s mergePar -z 18 >> res/mergePar.txt;  
stack exec -- par-sort-exe +RTS -N8 -A600M -RTS -s quickPar -z 18 >> res/quickPar.txt;  
stack exec -- par-sort-exe +RTS -N8 -A600M -RTS -s bitonicPar -z 18 >> res/bitonicPar.txt;  
stack exec -- par-sort-exe +RTS -N8 -A600M -RTS -s hybrid -z 18 >> res/hybrid.txt;  
done
