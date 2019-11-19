set term gif animate
set output 'SistSolar.gif'
set xrange [-31483303.769 to 31483303.769]
set yrange [-31483303.769 to 31483303.769]
set zrange [-31483303.769 to 31483303.769]
set xlabel 'x'
set ylabel 'y'
set zlabel 'z'
set style fill solid 1.0
i = 0
j = 0
while (i < 1849){
    set title sprintf("Minuto %i", j)
    j = j + 15
    i = i + 15
    splot 'fort.11' every ::i::i title 'Marte' with points lt 6 lw 5 lc 'red',\
     'fort.12' every ::i::i title 'Fobos' with points lt 6 lw 3 lc 'black',\
     'fort.13' every ::i::i title 'Deimos' with points lt 6 lw 3 lc 'orange',\
     'fort.12' every ::i-220::i-16 with lines lt 6 lw 1 lc 'black' notitle,\
     'fort.13' every ::i-220::i-16 with lines lt 6 lw 1 lc 'orange' notitle,\
}
set output
 #los rangos deben ser seteados a mano
 #grafica el archivo dando pasos de a 16 lineas en este caso para lograr 120 frames
 #el limite del while es la cantidad de lineas del archivo
 #j es solo para indicar las horas
 #con every ::i::i indicamos que grafique la linea i en el instante j
 #Para graficar las lineas y que queden completas hacemos every ::i-i*(cantidad de frames)::i-(paso de i)
 #Si no, donde está i-i*(cant frames) podemos cambiar i- otro valor para cambiar la longitud de a linea