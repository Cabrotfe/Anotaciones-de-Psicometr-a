

########### Introducción a una página web en github ############

# Este es un archivo anexo, vamos a crear un poco de script:

# este script va a incluir algunas cosas de psicometría:

pacman::p_load(tidyverse, mirt, corrplot,assertr,sjPlot)



# Simulación de datos -----------------------------------------------------

betas = matrix(c(1,1.3,1.8,2,2.1, ## ítems fáciles
                 0,0.3,0.5,-0.5,-0.8, ## ítems medios
                 -1,-1.2,-1.5,-1.7,-1.4), ncol = 1) ## ítems difíciles

alfas = matrix(c(1.2,1.2,1.3,1.4,1.1, 
                 0.8,0.9,1.1,1.3,1.5,
                 0.7,0.6,0.9,1.2,1.5),ncol=1)

guess = matrix(c(0.1,0.1,0.1,0.1,0.1,
                 0.15,0.15,0.15,0.15,0.15,
                 0.2,0.2,0.2,0.2,0.2),ncol=1)

theta = matrix(rnorm(400, 0.5, 1),ncol=1)


base_datos=simdata(a=alfas,d=betas, guess = guess,itemtype = "3PL", Theta = theta)



# Descriptivos ------------------------------------------------------------


base_datos = data.frame(base_datos)



# Ordenar por dificultad --------------------------------------------------



a=base_datos %>% summarise(across(.cols=everything(), mean,na.rm=T)) %>% t()
a=sort(a[,1], decreasing = T)
base_datos_sort=base_datos[,c(names(a))]


# función para ordenar ítems:

orden_items=function(items){
  it_prop=items %>% summarise(across(.cols=everything(), mean,na.rm=T)) %>% t()
  it_prop=sort(it_prop[,1], decreasing = T)
  items=items[,c(names(it_prop))]
  return(items)
}

print(orden_items(base_datos))




# Correlaciones -----------------------------------------------------------


var(base_datos)
cor(base_datos)
corrplot(cor(base_datos), method = "ellipse", order="hclust",addrect=2)
sjPlot::sjp.corr(base_datos, decimals = 2)



# Teoría clásica de tests -------------------------------------------------
# Descriptivos ------------------------------------------------------------


sjPlot::sjt.itemanalysis(base_datos)


# Lo más importante es la confiabilidad. Esto es la correlación al cuadrado del
# puntaje observado con el puntaje verdadero. El puntaje verdadero no es estimable
# porque es la esperanza matemática de aplicaciones suscesivas e idénticas de un test.
# Decimos que toda cosa tiene teóricamente un puntaje verdadero E(X), pero no podemos establecerlo
# Podemos entonces estimar la correlación entre X y T?

# Sí y no. Se requiere asumir supuestos, en particular de test paralelos, tau equivalencia.

# Test paralelos nos permite asumir que la correlación entre dos test es igual a la confiabilidad.
# Tau-equivalencia nos permite asumir que la covarianza entre 2 test es igual a varianza verdadera, 
# sin embargo, la correlación entre tests tau-equivalentes no permite asumir confiabilidad, pues las 
# varianzas X1 y X2 no son necesariamente iguales.

# Así, en cualquier modo,lo que se hace es asumir que la covarianza entre ítems es igual a la varianza verdadera





###############################################################################
################## Modelo de test paralelos:

str_c(colnames(base_datos),"~~e*",colnames(base_datos), collapse = ";")

cfa_ctt_paralel = "f1=~b*Item_1+b*Item_2+b*Item_3+b*Item_4+b*Item_5+b*Item_6+b*Item_7+b*Item_8+b*Item_9+b*Item_10+b*Item_11+b*Item_12+b*Item_13+b*Item_14+b*Item_15
Item_1~~e*Item_1;Item_2~~e*Item_2;Item_3~~e*Item_3;
Item_4~~e*Item_4;Item_5~~e*Item_5;Item_6~~e*Item_6;
Item_7~~e*Item_7;Item_8~~e*Item_8;Item_9~~e*Item_9;
Item_10~~e*Item_10;Item_11~~e*Item_11;Item_12~~e*Item_12;
Item_13~~e*Item_13;Item_14~~e*Item_14;Item_15~~e*Item_15"


cfa_ctt_model_paralel = sem(model = cfa_ctt_paralel, estimator = "MLR", data = base_datos)

lavaan::fitted(cfa_ctt_model_paralel) ## la pregunta es, ¿es esta matriz similar a la matriz original?
## Podemos asumir que en la población, esta es la matriz que real, ¿en cuánto difiere esa matriz con esta?

residuals(cfa_ctt_model_paralel)

summary(cfa_ctt_model_paralel, fit.measures=T,rsquare=T,standardized=T)

### Los ítems se asocian de igual forma con el factor. Al no haber error de medición
### Los puntajes del factor estan determinados por los ítems

plot(lavPredict(cfa_ctt_model_paralel, method = "Bartlett"), rowSums(base_datos))






###########################################################################
# Modelo de tau-equivalencia ----------------------------------------------
## Ejemplo emblemático es el alpha de cronbach:


tot_covar=sum(var(base_datos)) - sum(diag(var(base_datos))) ## Suma de todas las covarianzas
tot_covar/(15*14) ## esta es la covarianza promedio. Sería T. Se asume, que hay 15^2 Ts en la matriz
## La diagnonal de la matriz tiene varianza verdadera y varianza error

tot_covar*15^2/((15*14)*sum(var(base_datos))) ## así, tenemos el total es Ts de la matriz, dividido por la varianza total


### ¿Deberíamos asumir que la covarianza entre ítems es igual a la varianza verdadera?

str_c("b*",colnames(base_datos), collapse = "+")

cfa_ctt_tau = "f1=~b*Item_1+b*Item_2+b*Item_3+b*Item_4+b*Item_5+b*Item_6+b*Item_7+b*Item_8+b*Item_9+b*Item_10+b*Item_11+b*Item_12+b*Item_13+b*Item_14+b*Item_15"

cfa_ctt_model_tau = sem(model = cfa_ctt_tau, estimator = "MLR", data = base_datos)

lavaan::fitted(cfa_ctt_model_tau) ## la pregunta es, ¿es esta matriz similar a la matriz original?
## Podemos asumir que en la población, esta es la matriz que real, ¿en cuánto difiere esa matriz con esta?

residuals(cfa_ctt_model_tau)

summary(cfa_ctt_model_tau, fit.measures=T,rsquare=T,standardized=T)

#### Acá comienza a versa la indeterminancia de los puntajes del factor, por el hecho de que
#### hay error de medición:

plot(lavPredict(cfa_ctt_model_tau, method = "Bartlett"), rowSums(base_datos))





###########################################################################
# Modelo congenérico ------------------------------------------------------

str_c(colnames(base_datos), collapse="+")

congeneric = "f1=~Item_1+Item_2+Item_3+Item_4+Item_5+Item_6+Item_7+Item_8+Item_9+Item_10+Item_11+Item_12+Item_13+Item_14+Item_15"

congeneric_model = sem(model=congeneric, data=base_datos, estimator="MLR")

fitted(congeneric_model)

residuals(congeneric_model)

summary(congeneric_model, standardized=T,fit.measures=T)


plot(lavPredict(congeneric_model, method = "regression"), rowSums(base_datos))



###########################################################################
# Modelos factoriales categóricos -----------------------------------------

###########################################################################
# Test paralelos con variables categóricas --------------------------------



cfa_ctt_model_paralel_cat = sem(model = cfa_ctt_paralel, estimator = "WLSMV",
                                data = base_datos, ordered=names(c(base_datos)))



lavaan::fitted(cfa_ctt_model_paralel_cat) ## la pregunta es, ¿es esta matriz similar a la matriz original?
## Podemos asumir que en la población, esta es la matriz que real, ¿en cuánto difiere esa matriz con esta?

residuals(cfa_ctt_model_paralel_cat)

summary(cfa_ctt_model_paralel_cat, fit.measures=T,rsquare=T,standardized=T)

plot(lavPredict(cfa_ctt_model_paralel_cat, method = "EBM"), rowSums(base_datos))






###########################################################################
# Tau equivalencia categoricos  --------------------------------


## Este modelo es idéntico al modelo de test paralelos, porque la varianza error
## es 1 - b^2*var(f1). Entonces, como beta es igual para todos, todos tienen igual
# varianza error. Acá la varianza error es de la variables auxiliar y*, esta es la
## variable predicha


cfa_ctt_model_tau_cat = sem(model = cfa_ctt_tau, estimator = "WLSMV", data = base_datos, ordered=names(c(base_datos)))

lavaan::fitted(cfa_ctt_model_tau_cat) ## la pregunta es, ¿es esta matriz similar a la matriz original?
## Podemos asumir que en la población, esta es la matriz que real, ¿en cuánto difiere esa matriz con esta?

residuals(cfa_ctt_model_tau_cat)

summary(cfa_ctt_model_tau_cat, fit.measures=T,rsquare=T,standardized=T)

#### Acá comienza a versa la indeterminancia de los puntajes del factor, por el hecho de que
#### hay error de medición:

plot(lavPredict(cfa_ctt_model_tau, method = "Bartlett"), rowSums(base_datos))

########



#############################################################################
# Modelo congenérico ------------------------------------------------------
# Modelos congenerico con variables categóricas:


pacman::p_load(tidyverse, mirt, corrplot,assertr,sjPlot, lavaan)



cfa_1f = "f1=~Item_1+Item_2+Item_3+Item_4+Item_5+Item_6+Item_7+Item_8+Item_9+Item_10+Item_11+Item_12+Item_13+Item_14+Item_15"

cfa_1f_model = sem(model = cfa_1f, estimator = "WLSMV", ordered = names(c(base_datos)),
                   parameterization = "theta", data = base_datos)


summary(cfa_1f_model, fit.measures=T,rsquare=T,standardized=T)


fitted(cfa_1f_model)

plot(lavPredict(cfa_1f_model, method = "EBM"), rowSums(base_datos))



### ¿ podemos tener una medida de probabilidad de respuesta para cada ítem?
## R: Sí.

# Lo que estamos modelando es un valor esperado de y* a partir de valores en el factor
# y* es una variable latente que determina la probabilidad de respuesta para cada ítem


# el valor esperado de y* es:

#E(y*) = int + b*F1,

# la probabilidad de respuesta en cambio es:

#p(y=1)= 1-pnorm(th-E(y*)/var(error)) Es decir, es el área en la distribución normal que está por sobre el umbral
# La distribución normal utilizada asume que sigma = var(error)

## Por ejemplo, con el primer ítem tenemos:

#tresh = -1.208.
#beta = 0.903

f1 = -3:3
1-pnorm(-1.208-0.903*f1)

####
######33 ¿Cómo estamos modelando los datos?
## Estamos modelando una matriz de correlaciones, de modo que podemos
## reproducir la matriz de varianza covarianza con los factor loadings:

parameterestimates(cfa_1f_model, standardized = T)
f1_cfa_pars = parameterestimates(cfa_1f_model, standardized = T) %>% filter(op=="=~") %>% pull(std.all)
f1_cfa_error = parameterestimates(cfa_1f_model, standardized = T) %>% filter(op=="~~") %>% pull(std.all) 

f1_cfa_pars %*% t(f1_cfa_pars)+diag(f1_cfa_error)[1:15,1:15]






