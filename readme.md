## Arte generativo: ruido visual

![](ruido_visual.png)

Experimentos con R para generar ruido visual mediante la generación de texto ASCII en la consola.

Publicación en mi blog demostrando el fenómeno: <https://bastianolea.rbind.io/blog/2025-08-27/>


### Scripts
- `ascii_rayas.R`: varios modos de llenar la consola con texto algorítmico que produce patrones visuales interesantes

### Apps
- `/app/`: aplicación Shiny que usa algunos modos de generación de ruido visual en vivo, con algunos controles para personalizar el output. Funciona generando en R un flujo continuo de caracteres con las funciones del script `generadores.R`, que se corta cada cierta cantidad de columnas (`ancho`) igual que en una consola, por lo que cada línea queda desfasada con respecto a la anterior. Cada _x_ milisegundos (`intervalo`) Shiny envía al navegador solamente los caracteres nuevos, a un ritmo de un fragmento de texto cada `tiempo` segundos.
