set term gif animate
set output 'SistSolar.gif'
set xrange [-4658068734472.4395 to 4568068734472.4395]
set yrange [-4658068734472.4395 to 4568068734472.4395]
set zrange [-4458068734472.4395 to 4558068734472.4395]
set xlabel 'x'
set ylabel 'y'
set zlabel 'z'
set style fill solid 1.0
i = 0
j = 0
while (i < 250719){
    set title sprintf("Años: %i", j)
    j = j + 1
    i = i + 1510
    splot 'fort.11' every ::i::i title 'Sol' with points lt 6 lw 3 lc 'yellow',\
     'fort.14' every ::i::i title 'Tierra' with points lt 6 lw 3 lc 'blue',\
     'fort.15' every ::i::i title 'Marte' with points lt 6 lw 3 lc 'red',\
     'fort.16' every ::i::i title 'Jupiter' with points lt 6 lw 3 lc 'brown',\
     'fort.17' every ::i::i title 'Saturno' with points lt 6 lw 3 lc 'yellow',\
     'fort.18' every ::i::i title 'Urano' with points lt 6 lw 3 lc 'blue',\
     'fort.19' every ::i::i title 'Neptuno' with points lt 6 lw 3 lc 'violet',\
     'fort.14' every ::i-166*i::i-1510 with lines lt 6 lw 1 lc 'blue' notitle,\
     'fort.15' every ::i-166*i::i-1510 with lines lt 6 lw 1 lc 'red' notitle,\
     'fort.16' every ::i-166*i::i-1510 with lines lt 6 lw 1 lc 'brown' notitle,\
     'fort.17' every ::i-166*i::i-1510 with lines lt 6 lw 1 lc 'yellow' notitle,\
     'fort.18' every ::i-166*i::i-1510 with lines lt 6 lw 1 lc 'blue' notitle,\
     'fort.19' every ::i-166*i::i-1510 with lines lt 6 lw 1 lc 'violet' notitle,\
}
set output

 #los rangos deben ser seteados a mano
 #grafica el archivo dando pasos de a 1510 lineas en este caso para lograr 120 frames
 #el limite del while es la cantidad de lineas del archivo
 #j es solo para indicar las horas
 #con every ::i::i indicamos que grafique la linea i en el instante j
 #Para graficar las lineas y que queden completas hacemos every ::i-i*(cantidad de frames)::i-(paso de i)
 #Si no, donde está i-i*(cant frames) podemos cambiar i- otro valor para cambiar la longitud de a linea