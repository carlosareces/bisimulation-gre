Este archivo tiene la explicación para ejecutar los experimentos adecuadamente.
Esta carpeta está organizada de la siguiente forma:
* Contiene a la carpeta "input" que contiene los archivos necesarios para correr el experimento. La carpeta "input" contiene:
    * overspecifications.txt: el archivo que contiene con cuales propiedades se puede sobreespecificar.
    * prob_de_uso_X.txt: el archivo con las probabilidades de uso de cada modelo.
    * modelo_mapa_zoom_idX.xml: el archivo con el modelo del mapa.
* La carpeta "output_mapaX" se imprime el output del programa. Se imprimen dos outputs, "formula.txt" y "texto.txt", uno tiene
escritas las fórmulas de una manera más matemática, y el otro tiene las fórmulas escritas en modo de texto.

Para ejecutar el experimento, añadir esta carpeta al directorio de 
/bisimulacion-gre. Luego, en Run Configurations, elegir una Scala Application. En la pestaña Main tendrá como proyecto DlGre y como clase principal dlgre.main. Dentro de la pestaña argumentos, tendrá como Program Arguments la siguiente linea:

positive mapa_zoom_id20/input/modelo_mapa_id20.xml mapa_zoom_id20/input/prob_uso_mapa20.txt rest2,rest3 2000 mapa_zoom_id20/input/overspecifications.txt mapa_zoom_id20/output_mapa20/

Para tratar el output del experimento, es necesario compilar el archivo script.cpp:
$ g++ -o scriptplural scriptplural.cpp
$ ./scriptplural < formula.txt > excelplural.in

Luego de esto, se encuentra el archivo de output con el número de ocurrencias de cada expresión referencial ejecutando 2000 veces el algoritmo.
Este archivo se puede cargar en un archivo de LibreOffice, que en este caso, se llama rankingERs.ods.

