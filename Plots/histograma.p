set title "Tiempos de ejecución con h variable y un transcurso de 30 años"
set autoscale
set term png
set out "./Histogramas/TiemposhvarRK.png"
set ylabel "t"
set yrange [0 to 15]
set style data histogram
set style histogram cluster gap 0.0001
set style fill solid
set boxwidth 0.9
set xtics format ""
set grid ytics
plot \
    "./Plots/tiemposplot.txt" using 3 lc 'red' title 'RK4',\
    "./Plots/tiemposplot.txt" using 4 lc 'violet' title 'RKF'