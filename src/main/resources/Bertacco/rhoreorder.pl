#!/usr/bin/perl
#Convert original matrix files into bare format supported by umts.cpp
while(<>) {
  if(/^((\s+i\d+)+)\s*$/) {
    @colindexes=split(/\s+/, $_);
    shift @colindexes;
    @colindexes = map substr($_,1), @colindexes;
  }
  if(/^i(\d+)((\s+[\d.E+-]+)+)\s*;?\s*$/) {
    $row=$1;
    @vals = split(/\s+/, $2);
    shift @vals;
    $rows = $row if $row>$rows;
    for($i=0; $i<=$#vals; $i++) {
      $matr[$row][$colindexes[$i]]=$vals[$i];
      $cols = $colindexes[$i] if $colindexes[$i]>$cols;
      $maxlen = length($vals[$i]) if length($vals[$i])>$maxlen;
    }
  }
}
$maxlen++;
for($i=1; $i<=$rows; $i++) {
  for($j=1; $j<=$cols; $j++) {
    print substr($matr[$i][$j] . (" " x $maxlen), 0, $maxlen);
  }
  print "\n";
}
