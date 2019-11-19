set term gif animate
set output 'SistSolar.gif'
set xrange [-258068734472.4395 to 258068734472.4395]
set yrange [-258068734472.4395 to 258068734472.4395]
set zrange [-258068734472.4395 to 258068734472.4395]
set xlabel 'x'
set ylabel 'y'
set zlabel 'z'
set style fill solid 1.0
i = 0
j = 0
while (i < 38844){
    set title sprintf("Dia %i", j)
    j = j + 10
    i = i + 216
    splot 'fort.11' every ::i::i title 'Sol' with points lt 6 lw 8 lc 'yellow',\
     'fort.12' every ::i::i title 'Mercurio' with points lt 6 lw 3 lc 'brown',\
     'fort.13' every ::i::i title 'Venus' with points lt 6 lw 3 lc 'orange',\
     'fort.14' every ::i::i title 'Tierra' with points lt 6 lw 3 lc 'blue',\
     'fort.15' every ::i::i title 'Marte' with points lt 6 lw 3 lc 'red',\
     'fort.12' every ::i-360*i::i-216 with lines lt 6 lw 1 lc 'brown' notitle,\
     'fort.13' every ::i-360*i::i-216 with lines lt 6 lw 1 lc 'orange' notitle,\
     'fort.14' every ::i-360*i::i-216 with lines lt 6 lw 1 lc 'blue' notitle,\
     'fort.15' every ::i-360*i::i-216 with lines lt 6 lw 1 lc 'red' notitle,\
}
set output

 #los rangos deben ser seteados a mano
 #grafica el archivo dando pasos de a 216 lineas en este caso para lograr 120 frames
 #el limite del while es la cantidad de lineas del archivo
 #j es solo para indicar las horas
 #con every ::i::i indicamos que grafique la linea i en el instante j
 #Para graficar las lineas y que queden completas hacemos every ::i-i*(cantidad de frames)::i-(paso de i)
 #Si no, donde está i-i*(cant frames) podemos cambiar i- otro valor para cambiar la longitud de a linea