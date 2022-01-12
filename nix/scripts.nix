{ s, brittany, brittany-config }:
{
  format = s "format" "${brittany}/bin/brittany --write-mode=inplace --config-file=${brittany-config} src/**/*.hs";
}
