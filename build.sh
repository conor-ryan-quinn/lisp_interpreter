#docco source/*.hs
cd source
ghc -package parsec -XExistentialQuantification Main.hs
rm *.hi
rm *.o
./Main
