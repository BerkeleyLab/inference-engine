set logscale y 10
set term png size 2600, 1200

set output "input-tensor-histograms.png"
set multiplot layout 3,3
do for [name in "potential-temperature pressure qc qr qs qv temperature"] {
  filename = name . ".plt"
  set title sprintf("Histogram for %s",name)
  plot filename using 1:2 with linespoints title columnheader
}
print "\n Output-tensor histograms have been written to the graphics file 'output-tensor-histograms.png'."

unset multiplot

set output "output-tensor-histograms.png"
set multiplot layout 3,2
do for [name in "dptdt dqcdt dqrdt dqsdt dqvdt"] {
  filename = name . ".plt"
  set title sprintf("Histogram for %s",name)
  plot filename using 1:2 with linespoints title columnheader
}
print " Input-tensor histograms have been written to the graphics file 'input-tensor-histograms.png'.\n"
