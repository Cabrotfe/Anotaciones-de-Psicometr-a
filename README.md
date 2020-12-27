Análisis Psicométricos
================

Vamos a ocupar los siguientes paquetes:

``` r
pacman::p_load(tidyverse, mirt, corrplot,assertr,sjPlot, lavaan)
set.seed(1234)
```

Simulación de datos
-------------------

Vamos a hacer análisis de ítems de una prueba que suponemos mide una sola dimensión. Esto es, que hay solo un atributo psicológico que genera el patrón de respuestas observado (o que induce la covariación entre ítems).

Vamos a generar ítems de fácil, media y alta dificultad.

``` r
betas = matrix(c(1,1.3,1.8,2,2.1, # ítems fáciles
          0,0.3,0.5,-0.5,-0.8,  #  ítems medios
          -1,-1.2,-1.5,-1.7,-1.4), ncol = 1) ## ítems difíciles

alfas = matrix(c(1.2,1.2,1.3,1.4,1.1, 
          0.8,0.9,1.1,1.3,1.5,
          0.7,0.6,0.9,1.2,1.5),ncol=1)

guess = matrix(c(0.1,0.1,0.1,0.1,0.1,
          0.15,0.15,0.15,0.15,0.15,
          0.2,0.2,0.2,0.2,0.2),ncol=1)

theta = matrix(rnorm(400, 0.5, 1),ncol=1)
```

``` r
base_datos=simdata(a=alfas,d=betas, guess = guess,itemtype = "3PL", Theta = theta)
base_datos = data.frame(base_datos)
head(base_datos)
```

    ##   Item_1 Item_2 Item_3 Item_4 Item_5 Item_6 Item_7 Item_8 Item_9 Item_10
    ## 1      0      1      0      0      0      0      0      0      0       0
    ## 2      1      1      1      1      1      1      1      1      1       0
    ## 3      1      1      1      1      1      0      0      1      1       1
    ## 4      0      0      1      1      0      0      0      0      0       1
    ## 5      1      0      1      1      1      1      1      0      1       0
    ## 6      1      1      0      1      1      1      0      1      1       1
    ##   Item_11 Item_12 Item_13 Item_14 Item_15
    ## 1       0       1       1       0       0
    ## 2       0       0       0       0       0
    ## 3       1       0       0       1       1
    ## 4       0       0       1       0       1
    ## 5       0       0       0       1       0
    ## 6       1       1       0       1       1

Descriptivos
============

Algunos datos que nos pueden interesar son los descriptivos, correlaciones ítem test y consistencia interna. Para ello, podríamos ocupar la siguiente función:

``` r
sjPlot::sjt.itemanalysis(base_datos)
```

<table style="border-collapse:collapse; border:none;">
<caption style="font-weight: bold; text-align:left;">
Component 1
</caption>
<tr>
<th style="border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm; border-bottom:1px solid black; text-align:left;text-align:left; ">
Row
</th>
<th style="border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm; border-bottom:1px solid black; ">
Missings
</th>
<th style="border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm; border-bottom:1px solid black; ">
Mean
</th>
<th style="border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm; border-bottom:1px solid black; ">
SD
</th>
<th style="border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm; border-bottom:1px solid black; ">
Skew
</th>
<th style="border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm; border-bottom:1px solid black; ">
Item Difficulty
</th>
<th style="border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm; border-bottom:1px solid black; col7">
Item Discrimination
</th>
<th style="border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm; border-bottom:1px solid black; col8">
α if deleted
</th>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left;text-align:left; ">
Item\_1
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">
0.00 %
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">
0.78
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">
0.41
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">
-1.38
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">
0.78
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col7">
0.318
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col8">
0.587
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left;text-align:left; background-color:#f2f2f2; ">
Item\_2
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; ">
0.00 %
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; ">
0.83
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; ">
0.38
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; ">
-1.73
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; ">
0.83
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; col7">
0.368
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; col8">
0.581
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left;text-align:left; ">
Item\_3
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">
0.00 %
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">
0.9
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">
0.3
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">
-2.61
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">
0.9
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col7">
0.281
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col8">
0.596
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left;text-align:left; background-color:#f2f2f2; ">
Item\_4
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; ">
0.00 %
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; ">
0.9
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; ">
0.3
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; ">
-2.66
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; ">
0.9
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; col7">
0.281
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; col8">
0.597
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left;text-align:left; ">
Item\_5
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">
0.00 %
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">
0.92
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">
0.26
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">
-3.22
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">
0.92
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col7">
0.247
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col8">
0.602
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left;text-align:left; background-color:#f2f2f2; ">
Item\_6
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; ">
0.00 %
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; ">
0.64
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; ">
0.48
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; ">
-0.58
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; ">
0.64
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; col7">
0.164
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; col8">
0.612
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left;text-align:left; ">
Item\_7
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">
0.00 %
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">
0.71
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">
0.45
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">
-0.92
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">
0.71
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col7">
0.209
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col8">
0.604
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left;text-align:left; background-color:#f2f2f2; ">
Item\_8
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; ">
0.00 %
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; ">
0.72
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; ">
0.45
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; ">
-1
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; ">
0.72
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; col7">
0.301
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; col8">
0.589
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left;text-align:left; ">
Item\_9
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">
0.00 %
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">
0.62
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">
0.49
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">
-0.48
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">
0.62
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col7">
0.311
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col8">
0.586
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left;text-align:left; background-color:#f2f2f2; ">
Item\_10
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; ">
0.00 %
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; ">
0.56
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; ">
0.5
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; ">
-0.26
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; ">
0.56
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; col7">
0.325
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; col8">
0.583
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left;text-align:left; ">
Item\_11
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">
0.00 %
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">
0.48
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">
0.5
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">
0.09
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">
0.48
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col7">
0.205
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col8">
0.606
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left;text-align:left; background-color:#f2f2f2; ">
Item\_12
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; ">
0.00 %
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; ">
0.44
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; ">
0.5
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; ">
0.26
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; ">
0.44
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; col7">
0.137
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; col8">
0.618
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left;text-align:left; ">
Item\_13
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">
0.00 %
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">
0.44
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">
0.5
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">
0.26
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; ">
0.44
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col7">
0.11
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; col8">
0.623
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left;text-align:left; background-color:#f2f2f2; ">
Item\_14
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; ">
0.00 %
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; ">
0.44
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; ">
0.5
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; ">
0.23
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; ">
0.44
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; col7">
0.176
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; background-color:#f2f2f2; col8">
0.611
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left;text-align:left; border-bottom: double; ">
Item\_15
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; border-bottom: double; ">
0.00 %
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; border-bottom: double; ">
0.51
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; border-bottom: double; ">
0.5
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; border-bottom: double; ">
-0.05
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; border-bottom: double; ">
0.51
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; border-bottom: double; col7">
0.32
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; border-bottom: double; col8">
0.584
</td>
</tr>
<tr>
<td colspan="9" style="font-style:italic; border-top:double black; text-align:right;">
Mean inter-item-correlation=0.104 · Cronbach's α=0.615
</td>
</tr>
</table>
Algo que es útil es ordenar los ítems por dificultad. Para ello, podríamos ocupar el siguiente código:

``` r
a=base_datos %>% summarise(across(.cols=everything(), mean,na.rm=T)) %>% t()
a=sort(a[,1], decreasing = T)
base_datos_sort=base_datos[,c(names(a))]
head(base_datos)
```

    ##   Item_1 Item_2 Item_3 Item_4 Item_5 Item_6 Item_7 Item_8 Item_9 Item_10
    ## 1      0      1      0      0      0      0      0      0      0       0
    ## 2      1      1      1      1      1      1      1      1      1       0
    ## 3      1      1      1      1      1      0      0      1      1       1
    ## 4      0      0      1      1      0      0      0      0      0       1
    ## 5      1      0      1      1      1      1      1      0      1       0
    ## 6      1      1      0      1      1      1      0      1      1       1
    ##   Item_11 Item_12 Item_13 Item_14 Item_15
    ## 1       0       1       1       0       0
    ## 2       0       0       0       0       0
    ## 3       1       0       0       1       1
    ## 4       0       0       1       0       1
    ## 5       0       0       0       1       0
    ## 6       1       1       0       1       1

Podríamos crear una función para ordenar ítems:

``` r
orden_items=function(items){
 it_prop=items %>% summarise(across(.cols=everything(), mean,na.rm=T)) %>% t()
 it_prop=sort(it_prop[,1], decreasing = T)
 items=items[,c(names(it_prop))]
 return(items)
}
```

Covarianzas:
============

También nos pueden interesar las covarianzas. Ello da una idea de la estructura interna del instrumento.

``` r
corrplot(cor(base_datos), method = "ellipse", order="hclust",addrect=2)
```

![](README_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
sjPlot::sjp.corr(base_datos, decimals = 2)
```

    ## Computing correlation using pearson-method with listwise-deletion...

    ## Warning: Removed 120 rows containing missing values (geom_text).

![](README_files/figure-markdown_github/unnamed-chunk-8-1.png)

Teoría clásica de tests
=======================

Lo más importante es la confiabilidad. Esto es la correlación al cuadrado del puntaje observado con el puntaje verdadero. El puntaje verdadero no es estimable porque es la esperanza matemática de aplicaciones suscesivas e idénticas de un test. Decimos que toda cosa tiene teóricamente un puntaje verdadero E(X), pero no podemos establecerlo. Podemos entonces estimar la correlación entre X y T? La respuesta es: sí y no. Se requiere asumir supuestos, en particular de test paralelos, o de tau equivalencia.

Test paralelos nos permite asumir que la correlación entre dos test es igual a la confiabilidad. Tau-equivalencia nos permite asumir que la covarianza entre 2 test es igual a varianza verdadera, sin embargo, la correlación entre tests tau-equivalentes no permite asumir confiabilidad, pues las varianzas X1 y X2 no son necesariamente iguales. La Tau-equivalencia señala que dos o más test tienen la misma cantidad de varianza verdadera pero, esta varianza verdadera puede significar una distinta proporción del test de modo que los tests tienen distinta confiabilidad.

### Modelo de test paralelos:

Podemos ocupar un modelo de variables latentes que de cuenta de los supuestos de test paralelos. Con test paralelos asumimos que los ítems comparten la misma cantidad y proporción de varianza verdadera (igual confiabilidad). Con los índices de ajuste, podemos determinar si el supuesto de test paralelos es razonable o no.

``` r
str_c(colnames(base_datos),"~~e*",colnames(base_datos), collapse = ";")
```

    ## [1] "Item_1~~e*Item_1;Item_2~~e*Item_2;Item_3~~e*Item_3;Item_4~~e*Item_4;Item_5~~e*Item_5;Item_6~~e*Item_6;Item_7~~e*Item_7;Item_8~~e*Item_8;Item_9~~e*Item_9;Item_10~~e*Item_10;Item_11~~e*Item_11;Item_12~~e*Item_12;Item_13~~e*Item_13;Item_14~~e*Item_14;Item_15~~e*Item_15"

``` r
cfa_ctt_paralel = "f1=~b*Item_1+b*Item_2+b*Item_3+b*Item_4+b*Item_5+b*Item_6+b*Item_7+b*Item_8+b*Item_9+b*Item_10+b*Item_11+b*Item_12+b*Item_13+b*Item_14+b*Item_15
Item_1~~e*Item_1;Item_2~~e*Item_2;Item_3~~e*Item_3;
Item_4~~e*Item_4;Item_5~~e*Item_5;Item_6~~e*Item_6;
Item_7~~e*Item_7;Item_8~~e*Item_8;Item_9~~e*Item_9;
Item_10~~e*Item_10;Item_11~~e*Item_11;Item_12~~e*Item_12;
Item_13~~e*Item_13;Item_14~~e*Item_14;Item_15~~e*Item_15"
cfa_ctt_model_paralel = sem(model = cfa_ctt_paralel, estimator = "MLR", data = base_datos)
lavaan::fitted(cfa_ctt_model_paralel)
```

    ## $cov
    ##         Item_1 Item_2 Item_3 Item_4 Item_5 Item_6 Item_7 Item_8 Item_9
    ## Item_1  0.195                                                         
    ## Item_2  0.019  0.195                                                  
    ## Item_3  0.019  0.019  0.195                                           
    ## Item_4  0.019  0.019  0.019  0.195                                    
    ## Item_5  0.019  0.019  0.019  0.019  0.195                             
    ## Item_6  0.019  0.019  0.019  0.019  0.019  0.195                      
    ## Item_7  0.019  0.019  0.019  0.019  0.019  0.019  0.195               
    ## Item_8  0.019  0.019  0.019  0.019  0.019  0.019  0.019  0.195        
    ## Item_9  0.019  0.019  0.019  0.019  0.019  0.019  0.019  0.019  0.195 
    ## Item_10 0.019  0.019  0.019  0.019  0.019  0.019  0.019  0.019  0.019 
    ## Item_11 0.019  0.019  0.019  0.019  0.019  0.019  0.019  0.019  0.019 
    ## Item_12 0.019  0.019  0.019  0.019  0.019  0.019  0.019  0.019  0.019 
    ## Item_13 0.019  0.019  0.019  0.019  0.019  0.019  0.019  0.019  0.019 
    ## Item_14 0.019  0.019  0.019  0.019  0.019  0.019  0.019  0.019  0.019 
    ## Item_15 0.019  0.019  0.019  0.019  0.019  0.019  0.019  0.019  0.019 
    ##         Itm_10 Itm_11 Itm_12 Itm_13 Itm_14 Itm_15
    ## Item_1                                           
    ## Item_2                                           
    ## Item_3                                           
    ## Item_4                                           
    ## Item_5                                           
    ## Item_6                                           
    ## Item_7                                           
    ## Item_8                                           
    ## Item_9                                           
    ## Item_10 0.195                                    
    ## Item_11 0.019  0.195                             
    ## Item_12 0.019  0.019  0.195                      
    ## Item_13 0.019  0.019  0.019  0.195               
    ## Item_14 0.019  0.019  0.019  0.019  0.195        
    ## Item_15 0.019  0.019  0.019  0.019  0.019  0.195

la pregunta es, ¿es esta matriz similar a la matriz original? Podemos asumir que en la población, esta es la matriz que real, ¿en cuánto difiere esa matriz con esta?

Los residuos indican la discrepancia:

``` r
residuals(cfa_ctt_model_paralel)
```

    ## $type
    ## [1] "raw"
    ## 
    ## $cov
    ##         Item_1 Item_2 Item_3 Item_4 Item_5 Item_6 Item_7 Item_8 Item_9
    ## Item_1  -0.026                                                        
    ## Item_2   0.012 -0.052                                                 
    ## Item_3  -0.003  0.004 -0.103                                          
    ## Item_4   0.002 -0.008 -0.001 -0.105                                   
    ## Item_5   0.000 -0.002 -0.011 -0.016 -0.125                            
    ## Item_6   0.001  0.009 -0.013 -0.005 -0.001  0.036                     
    ## Item_7  -0.001  0.006 -0.006 -0.003  0.002 -0.018  0.011              
    ## Item_8   0.002  0.006  0.001  0.009  0.006  0.002  0.007  0.005       
    ## Item_9   0.017  0.010 -0.008  0.006 -0.002  0.004  0.003  0.019  0.042
    ## Item_10  0.023  0.019  0.014  0.013 -0.014 -0.003 -0.010  0.009  0.032
    ## Item_11  0.011 -0.004 -0.005 -0.003 -0.003 -0.014  0.000  0.000  0.009
    ## Item_12 -0.013 -0.004 -0.014 -0.010 -0.016 -0.002 -0.003  0.006 -0.010
    ## Item_13 -0.018  0.014 -0.007 -0.023 -0.014 -0.012 -0.008 -0.009 -0.012
    ## Item_14  0.001 -0.002 -0.001 -0.012 -0.018 -0.012 -0.008 -0.002  0.006
    ## Item_15  0.024  0.020  0.004  0.003 -0.008 -0.002  0.012  0.012  0.032
    ##         Itm_10 Itm_11 Itm_12 Itm_13 Itm_14 Itm_15
    ## Item_1                                           
    ## Item_2                                           
    ## Item_3                                           
    ## Item_4                                           
    ## Item_5                                           
    ## Item_6                                           
    ## Item_7                                           
    ## Item_8                                           
    ## Item_9                                           
    ## Item_10  0.051                                   
    ## Item_11  0.006  0.055                            
    ## Item_12 -0.010  0.009  0.051                     
    ## Item_13  0.030 -0.029 -0.023  0.051              
    ## Item_14  0.011 -0.003 -0.006 -0.009  0.052       
    ## Item_15  0.004  0.017  0.003 -0.007  0.009  0.055

Podemos atender a otra cosa, en este modelo los valores del factor son lineales con los puntajes observados. Es posible estimar, desde los puntajes observados, el puntaje del factor.

``` r
plot(lavPredict(cfa_ctt_model_paralel, method = "Bartlett"), rowSums(base_datos))
```

![](README_files/figure-markdown_github/unnamed-chunk-11-1.png)

Modelo de tau-equivalencia
==========================

Este es el modelo asumido en el cálculo del alpha de cronbach. El alpha hace una descomposición de varianza verdadera y varianza total, asumiendo que la varianza verdadera es igual al promedio de la covarianza observada, multiplicada por el total de elementos de la matriz varianza covarianza. Esto es, que la matriz tiene k^2 elementos var(T).

``` r
tot_covar=sum(var(base_datos)) - sum(diag(var(base_datos))) ## Suma de todas las covarianzas
tot_covar/(15*14) ## esta es la covarianza promedio. Sería T. Se asume, que hay 15^2 Ts en la matriz
```

    ## [1] 0.01878918

``` r
tot_covar*15^2/((15*14)*sum(var(base_datos))) ## así, tenemos el total es Ts de la matriz, dividido por la varianza total. Esto es el alpha
```

    ## [1] 0.6152628

La diagnonal de la matriz tiene varianza verdadera y varianza error. Lo que está fuera de la diagonal es varianza verdadera (covarianzas)

¿Deberíamos asumir que la covarianza entre ítems es igual a la varianza verdadera?

``` r
cfa_ctt_tau = "f1=~b*Item_1+b*Item_2+b*Item_3+b*Item_4+b*Item_5+b*Item_6+b*Item_7+b*Item_8+b*Item_9+b*Item_10+b*Item_11+b*Item_12+b*Item_13+b*Item_14+b*Item_15"
cfa_ctt_model_tau = sem(model = cfa_ctt_tau, estimator = "MLR", data = base_datos)
lavaan::fitted(cfa_ctt_model_tau) ## la pregunta es, ¿es esta matriz similar a la matriz original?
```

    ## $cov
    ##         Item_1 Item_2 Item_3 Item_4 Item_5 Item_6 Item_7 Item_8 Item_9
    ## Item_1  0.162                                                         
    ## Item_2  0.017  0.136                                                  
    ## Item_3  0.017  0.017  0.095                                           
    ## Item_4  0.017  0.017  0.017  0.094                                    
    ## Item_5  0.017  0.017  0.017  0.017  0.077                             
    ## Item_6  0.017  0.017  0.017  0.017  0.017  0.233                      
    ## Item_7  0.017  0.017  0.017  0.017  0.017  0.017  0.205               
    ## Item_8  0.017  0.017  0.017  0.017  0.017  0.017  0.017  0.190        
    ## Item_9  0.017  0.017  0.017  0.017  0.017  0.017  0.017  0.017  0.227 
    ## Item_10 0.017  0.017  0.017  0.017  0.017  0.017  0.017  0.017  0.017 
    ## Item_11 0.017  0.017  0.017  0.017  0.017  0.017  0.017  0.017  0.017 
    ## Item_12 0.017  0.017  0.017  0.017  0.017  0.017  0.017  0.017  0.017 
    ## Item_13 0.017  0.017  0.017  0.017  0.017  0.017  0.017  0.017  0.017 
    ## Item_14 0.017  0.017  0.017  0.017  0.017  0.017  0.017  0.017  0.017 
    ## Item_15 0.017  0.017  0.017  0.017  0.017  0.017  0.017  0.017  0.017 
    ##         Itm_10 Itm_11 Itm_12 Itm_13 Itm_14 Itm_15
    ## Item_1                                           
    ## Item_2                                           
    ## Item_3                                           
    ## Item_4                                           
    ## Item_5                                           
    ## Item_6                                           
    ## Item_7                                           
    ## Item_8                                           
    ## Item_9                                           
    ## Item_10 0.234                                    
    ## Item_11 0.017  0.249                             
    ## Item_12 0.017  0.017  0.254                      
    ## Item_13 0.017  0.017  0.017  0.256               
    ## Item_14 0.017  0.017  0.017  0.017  0.251        
    ## Item_15 0.017  0.017  0.017  0.017  0.017  0.239

Podemos asumir que en la población, esta es la matriz que real, ¿en cuánto difiere esa matriz con esta?

``` r
residuals(cfa_ctt_model_tau)
```

    ## $type
    ## [1] "raw"
    ## 
    ## $cov
    ##         Item_1 Item_2 Item_3 Item_4 Item_5 Item_6 Item_7 Item_8 Item_9
    ## Item_1   0.007                                                        
    ## Item_2   0.014  0.007                                                 
    ## Item_3  -0.001  0.006 -0.003                                          
    ## Item_4   0.004 -0.007  0.000 -0.004                                   
    ## Item_5   0.002  0.000 -0.009 -0.014 -0.007                            
    ## Item_6   0.003  0.011 -0.011 -0.003  0.001 -0.003                     
    ## Item_7   0.001  0.008 -0.004 -0.001  0.004 -0.016  0.001              
    ## Item_8   0.004  0.008  0.003  0.011  0.008  0.004  0.008  0.009       
    ## Item_9   0.018  0.012 -0.006  0.007  0.000  0.005  0.005  0.021  0.009
    ## Item_10  0.025  0.021  0.016  0.015 -0.012 -0.001 -0.008  0.011  0.034
    ## Item_11  0.013 -0.002 -0.003 -0.002 -0.001 -0.012  0.002  0.002  0.011
    ## Item_12 -0.011 -0.002 -0.012 -0.008 -0.014  0.000 -0.001  0.008 -0.008
    ## Item_13 -0.016  0.016 -0.005 -0.021 -0.012 -0.010 -0.006 -0.007 -0.010
    ## Item_14  0.003  0.000  0.001 -0.010 -0.016 -0.010 -0.006  0.000  0.007
    ## Item_15  0.026  0.022  0.006  0.004 -0.006  0.000  0.014  0.014  0.034
    ##         Itm_10 Itm_11 Itm_12 Itm_13 Itm_14 Itm_15
    ## Item_1                                           
    ## Item_2                                           
    ## Item_3                                           
    ## Item_4                                           
    ## Item_5                                           
    ## Item_6                                           
    ## Item_7                                           
    ## Item_8                                           
    ## Item_9                                           
    ## Item_10  0.012                                   
    ## Item_11  0.008  0.001                            
    ## Item_12 -0.008  0.010 -0.008                     
    ## Item_13  0.032 -0.027 -0.021 -0.010              
    ## Item_14  0.013 -0.001 -0.004 -0.007 -0.004       
    ## Item_15  0.006  0.018  0.005 -0.005  0.011  0.011

Respecto a los puntajes observados, acá comienza a versa la indeterminancia de los puntajes del factor, por el hecho de que los ítems entregan distinta cantidad de información para predecir los puntajes del factor.

``` r
plot(lavPredict(cfa_ctt_model_tau, method = "Bartlett"), rowSums(base_datos))
```

![](README_files/figure-markdown_github/unnamed-chunk-15-1.png)

Modelo congenérico
==================

En este caso, dado que los ítems tienen distinto grado de covarianza entre sí, y que hay ítems que tienen muy poca covarianza con los demás ítems, decimos que no todos los ítems se relacionan con igual magnitud con el factor. Hay ítems que se relacionan fuertemente con el factor (la covarianza en general reperesenta una alta proporción de la varianza del ítem) e ítems con baja asociación con el factor.

``` r
congeneric = "f1=~Item_1+Item_2+Item_3+Item_4+Item_5+Item_6+Item_7+Item_8+Item_9+Item_10+Item_11+Item_12+Item_13+Item_14+Item_15"
congeneric_model = sem(model=congeneric, data=base_datos, estimator="MLR")
fitted(congeneric_model)
```

    ## $cov
    ##         Item_1 Item_2 Item_3 Item_4 Item_5 Item_6 Item_7 Item_8 Item_9
    ## Item_1  0.169                                                         
    ## Item_2  0.030  0.143                                                  
    ## Item_3  0.020  0.019  0.092                                           
    ## Item_4  0.020  0.019  0.012  0.090                                    
    ## Item_5  0.014  0.014  0.009  0.009  0.069                             
    ## Item_6  0.019  0.018  0.012  0.012  0.009  0.230                      
    ## Item_7  0.022  0.021  0.014  0.014  0.010  0.013  0.206               
    ## Item_8  0.030  0.030  0.019  0.019  0.014  0.019  0.022  0.199        
    ## Item_9  0.035  0.034  0.022  0.022  0.016  0.021  0.025  0.034  0.236 
    ## Item_10 0.036  0.035  0.023  0.023  0.017  0.022  0.026  0.036  0.041 
    ## Item_11 0.024  0.023  0.015  0.015  0.011  0.014  0.017  0.023  0.026 
    ## Item_12 0.014  0.014  0.009  0.009  0.006  0.008  0.010  0.014  0.016 
    ## Item_13 0.014  0.013  0.009  0.009  0.006  0.008  0.010  0.013  0.015 
    ## Item_14 0.019  0.019  0.012  0.012  0.009  0.012  0.014  0.019  0.022 
    ## Item_15 0.036  0.035  0.023  0.023  0.017  0.022  0.025  0.035  0.040 
    ##         Itm_10 Itm_11 Itm_12 Itm_13 Itm_14 Itm_15
    ## Item_1                                           
    ## Item_2                                           
    ## Item_3                                           
    ## Item_4                                           
    ## Item_5                                           
    ## Item_6                                           
    ## Item_7                                           
    ## Item_8                                           
    ## Item_9                                           
    ## Item_10 0.246                                    
    ## Item_11 0.028  0.249                             
    ## Item_12 0.016  0.011  0.246                      
    ## Item_13 0.016  0.010  0.006  0.246               
    ## Item_14 0.022  0.015  0.009  0.008  0.247        
    ## Item_15 0.042  0.027  0.016  0.016  0.022  0.250

la discrepancia entre la matriz reproducida por el modelo y la matriz original es la siguiente:

``` r
residuals(congeneric_model)
```

    ## $type
    ## [1] "raw"
    ## 
    ## $cov
    ##         Item_1 Item_2 Item_3 Item_4 Item_5 Item_6 Item_7 Item_8 Item_9
    ## Item_1   0.000                                                        
    ## Item_2   0.000  0.000                                                 
    ## Item_3  -0.004  0.003  0.000                                          
    ## Item_4   0.001 -0.009  0.005  0.000                                   
    ## Item_5   0.004  0.003 -0.002 -0.007  0.000                            
    ## Item_6   0.001  0.010 -0.006  0.002  0.009  0.000                     
    ## Item_7  -0.004  0.004 -0.001  0.002  0.011 -0.013  0.000              
    ## Item_8  -0.010 -0.005  0.000  0.008  0.010  0.002  0.004  0.000       
    ## Item_9   0.000 -0.005 -0.011  0.002  0.000  0.001 -0.003  0.003  0.000
    ## Item_10  0.005  0.002  0.010  0.008 -0.012 -0.006 -0.017 -0.008  0.010
    ## Item_11  0.007 -0.008 -0.001  0.000  0.005 -0.010  0.002 -0.004  0.001
    ## Item_12 -0.008  0.001 -0.004  0.000 -0.004  0.008  0.006  0.011 -0.007
    ## Item_13 -0.013  0.019  0.003 -0.013 -0.001 -0.002  0.001 -0.004 -0.009
    ## Item_14  0.001 -0.002  0.006 -0.005 -0.008 -0.005 -0.003 -0.002  0.003
    ## Item_15  0.007  0.003  0.000 -0.002 -0.006 -0.005  0.006 -0.004  0.011
    ##         Itm_10 Itm_11 Itm_12 Itm_13 Itm_14 Itm_15
    ## Item_1                                           
    ## Item_2                                           
    ## Item_3                                           
    ## Item_4                                           
    ## Item_5                                           
    ## Item_6                                           
    ## Item_7                                           
    ## Item_8                                           
    ## Item_9                                           
    ## Item_10  0.000                                   
    ## Item_11 -0.002  0.000                            
    ## Item_12 -0.007  0.017  0.000                     
    ## Item_13  0.033 -0.021 -0.010  0.000              
    ## Item_14  0.008  0.002  0.004  0.002  0.000       
    ## Item_15 -0.019  0.008  0.006 -0.004  0.006  0.000

Respecto a los puntajes observados, acá comienza a versa la indeterminancia de los puntajes del factor, por el hecho de que los ítems entregan distinta cantidad de información para predecir los puntajes del factor.

``` r
plot(lavPredict(congeneric_model, method = "regression"), rowSums(base_datos))
```

![](README_files/figure-markdown_github/unnamed-chunk-18-1.png)
