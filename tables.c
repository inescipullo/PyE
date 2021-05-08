#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int contar_lineas (FILE *fp) {
  int count = 0;
  char c;

  // Recorro el archivo caracter por caracter
  for (c = getc(fp); c != EOF; c = getc(fp)) {
    // Y cada vez que me encuentro con un \n aumento el contador de lineas
    if (c == '\n') {
      count++;
    }
  }
  
  // Una vez llegado al end of file, hago que el puntero al archivo vuelva a estar al principio del mismo
  rewind(fp);

  // Y devuelvo el contador
  return count;
}


int main(int argc, char** argv) {
    FILE *entrada = fopen(argv[1], "r+"); // archivo de recorridos
    FILE *tabla = fopen(argv[2], "w+");

    if (entrada == NULL || tabla == NULL) {
        perror("Error en la asignación de los file pointers");
        exit(EXIT_FAILURE);
    }

    int lineas = contar_lineas(entrada);

    char* string_espacio = malloc(sizeof(char)*100);

    fscanf(entrada, "%[^,],", string_espacio);

    int length_espacio = strlen(string_espacio);

    fprintf(tabla, "%d -> long de espacio\n", length_espacio);

    char* basura = malloc(sizeof(char)*200);

    fscanf(entrada, "%[^\n]\n", basura);

    free(basura);

    for (int i = 0; i < lineas; i++) {
        fprintf(tabla, "<tr>\n");
        char* stringy = malloc(sizeof(char)*67);
        fscanf(entrada, "%67[^\n]\n", stringy);
        fprintf(tabla, "<td class=\"estacion\">%s</td>\n", stringy);
        int fabs;
        fscanf(entrada, "%d", &fabs);
        fprintf(tabla, "<td>%d</td>\n", fabs);
        char* sfrel = malloc(sizeof(char)*21);
        fscanf(entrada, "%[^\n]\n", sfrel);
        fprintf(tabla, "<td>%.4f</td>\n", atof(sfrel));
        fprintf(tabla, "</tr>\n");

        free(stringy);
        free(sfrel);
    }


    return 0;

}
