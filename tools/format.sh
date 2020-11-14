#!/bin/sh

cabal update
cabal install ormolu 
ormolu -o -XImportQualifiedPost -i $(find . -wholename './*.hs')
echo "Done!"