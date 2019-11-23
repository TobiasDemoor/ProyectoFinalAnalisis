set title "Tiempo de ejecucion con h variable"
set term png
set out "Tiempohvar.png"
set autoscale
set ylabel "t"
set style data histogram
set style histogram cluster gap 0.0001
set style fill solid
set boxwidth 0.9
set xtics format ""
set grid ytics
plot \
    "tiempos30D.dat" using 1 lc "blue" title 'EULER SIMPLE',\
    "tiempos30D.dat" using 2 lc 'yellow' title 'EULER MEJORADO',\
    "tiempos30D.dat" using 3 lc 'red' title 'RK4',\
    "tiempos30D.dat" using 4 lc 'violet' title 'RKF'