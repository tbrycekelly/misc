#/bin/bash

echo "Compile"
time gfortran ./assign2-4.f90
echo "Run"
time ./a.out
echo "Clear output direction"
time rm ./IMG/*.png
echo "Make images"
time Rscript Assign2.R
echo "Compile movie"
time convert ./IMG/*.png ./out.mp4
echo "-------DONE!--------"
