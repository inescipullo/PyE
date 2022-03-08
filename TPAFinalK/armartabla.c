#include <stdio.h>
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


int main() {
    FILE *file = fopen("tablas.txt", "r+");
    FILE *file2 = fopen("tablasparalatex.txt", "w+");
    int lineas = contar_lineas(file);
    int i=0;
    while (i <= lineas) {
        char* line = malloc(sizeof(char)*100);
        fscanf(file, "%[^\n]\n", line);
        int linea, cant, fa;
        float fr;
        sscanf(line,"%d %d %d %f", &linea, &cant, &fa, &fr);
        printf("%d & %d & %.3f \\\\ \n", cant, fa, fr);
        printf("\\hline\n");
        i++;
    }
    fclose(file);
    return 0;
}