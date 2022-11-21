{ s, ormolu }:

{
  format = s "format" "${ormolu}/bin/ormolu --mode inplace $(git ls-files '*.hs')";
}
