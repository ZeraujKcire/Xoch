
# === LIBRERIAS === (((
sprintf(" --- LIBRERIAS --- ")
suppressPackageStartupMessages(library(lmtest))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(GGally))
suppressPackageStartupMessages(library(car))
suppressPackageStartupMessages(library(agricolae))
suppressPackageStartupMessages(library(qqplotr))
suppressPackageStartupMessages(library(stats))
suppressPackageStartupMessages(library(DescTools))
suppressPackageStartupMessages(library(dplyr))
# )))

# === DATOS === (((
sprintf(" --- DATOS --- ")
datos = read.table('DATOS_COMPLETOS.txt', header = TRUE, stringsAsFactors = TRUE)
datos$Concentracion = as.factor(datos$Concentracion)
attach(datos)
regresion_lineal = lm(mV ~ Sexo * Compuesto * Concentracion,data = datos)
# regresion_lineal$coefficients
# )))

# === CAJAS SIMULTÁNEAS === (((
sprintf(" --- DIAGRAMA DE CAJAS SIMULTÁNEAS --- ")
ggplot(datos) + aes(x=Sexo,y=mV, fill = Sexo) + geom_boxplot(alpha=0.8) + facet_wrap(~Compuesto) 
ggplot(datos) + aes(x=Sexo,y=mV, fill = Sexo) + geom_boxplot(alpha=0.8) + facet_wrap(~Concentracion) 
ggplot(datos) + aes(x=Compuesto,y=mV, fill = Compuesto) + geom_boxplot(alpha=0.8) + facet_wrap(~Sexo) + theme(axis.text.x = element_blank(), legend.position = "bottom")
ggplot(datos) + aes(x=Compuesto,y=mV, fill = Compuesto) + geom_boxplot(alpha=0.8) + facet_wrap(~Concentracion)  + theme(legend.position =  "bottom",text = element_text(size = 12), axis.text.x = element_blank())
ggplot(datos) + aes(x=Concentracion,y=mV, fill = Concentracion) + geom_boxplot(alpha=0.8) + facet_wrap(~Sexo) 
ggplot(datos) + aes(x=Concentracion,y=mV, fill = Concentracion) + geom_boxplot(alpha=0.8) + facet_wrap(~Compuesto) 

# GRAFICO IMPORTANTE :)
ggplot(datos) + aes(x=Compuesto,y=mV, fill = Concentracion) + geom_boxplot(alpha=0.8) + facet_wrap(~Sexo)  + theme(legend.position = "bottom", text = element_text(size = 12), axis.text.x = element_text(angle = 90, hjust = 1))
# )))

# === SUPUESTOS DEL MODELO === (((
sprintf(" --- SUPUESTOS DEL MODELO --- ")

# === NORMALIDAD === (((
sprintf(" --- 1. NORMALIDAD --- ")
# GRÁFICA
resultados  =  aov(regresion_lineal)
anova       =  anova(resultados)
residuos    =  resid(resultados)
# ggplot(data = NULL, mapping=aes(sample=residuos)) + stat_qq_band() + stat_qq_line() + stat_qq_point() + labs(x="Cuantiles Teóricos", y="Cuantiles")
# SHAPIRO
sprintf("Shapiro Test.")
shapiro.test(residuos)
# KOLMOGOROV-SMIRNOFF
sprintf("KS Test")
ks.test(residuos,"pnorm",0, sqrt(anova$Mean[8])) # es el 8vo elemento de la Tabla ANOVA [columna X4].
# )))

# === HOMOCEDASTICIDAD === (((
sprintf(" --- 2. HOMOCEDASTICIDAD --- ")
# BARTLETT
leveneTest(mV ~ interaction(Sexo,Concentracion,Compuesto), data = datos)
bartlett.test(mV ~ interaction(Sexo,Concentracion,Compuesto), data = datos)
# GRÁFICO
# y_estimada       =  fitted(regresion)
# residu_estandar  =  rstudent(regresion)
# graf_1 = ggplot(data = NULL, aes(x= F.Control,y=residuos)) + geom_point() + ggtitle("mV vs Resiuales.")      + labs(x="mV",y="Residuales")     + geom_hline(yintercept=0)
# graf_2 = ggplot(data = NULL, aes(x= F.Control,y=residu_estandar))   + geom_point() + ggtitle("mV vs Resid. Estand.")  + labs(x="mV",y="Resid. Estand.") + geom_hline(yintercept=0)
# grid.arrange(graf_1,graf_2,ncol=2,nrow=1) 
# )))

# === INDEPENDENCIA === (((
sprintf(" --- 3. INDEPENDENCIA --- ")
# DURBIM WATSON
dwtest(regresion_lineal)
# GRÁFICO
# n = dim(datos)[1]-1
# ggplot(data = NULL, aes(x=1:n,y=residu_estandar)) + geom_line() + ggtitle("Orden de Corrida vs Resid. Estand.") + labs(x="Orden de Corrida",y="Resid. Estand.")  + geom_hline(yintercept=0)
# )))

# )))

# === ANOVA === (((
sprintf(" --- ANOVA --- ")
anova
sprintf("SUMA DE CUADRADOS:")
SCT = sum(anova$Sum) # se calcula el total de la Suma de Cuadrados
SCT
sprintf("GRADOS DE LIBERTAD DE SCT:")
sum(anova$Df) # se calculan los grados de libertad de SCT
sprintf("Desv. Estand.")
n = dim(datos)[1]
sqrt(SCT/(n-1))
# )))

# === NUEVA ANOVA === (((
sprintf(" --- NUEVA ANOVA --- ")
sprintf("Se elimina la variable Sexo, porque no es significativa.")
regresion_lineal = lm(mV ~ Compuesto * Concentracion,data = datos)
# regresion_lineal$coefficients
resultados = aov(regresion_lineal)
anova = anova(resultados)
anova
sprintf("SUMA DE CUADRADOS:")
SCT = sum(anova$Sum) # se calcula el total de la Suma de Cuadrados
SCT
sprintf("GRADOS DE LIBERTAD DE SCT:")
sum(anova$Df) # se calculan los grados de libertad de SCT
sprintf("Desv. Estand.")
n = dim(datos)[1]
sqrt(SCT/(n-1))
# )))

# === TUKEY (COMPARACIONES) === (((
# sprintf(" --- TUKEY COMPARACIONES --- ")
# TukeyHSD(resultados)
# )))

# === LSD === (((
sprintf("--- LSD ---")
LSD.test(regresion_lineal , c("Compuesto" , "Concentracion") , alpha=0.05 , console=TRUE)
# )))

# === LSD (con Modelo Completo) === (((
sprintf(" --- LSD (con Modelo Completo) --- ")
regresion_lineal = lm(mV ~ Sexo * Compuesto * Concentracion,data = datos)
LSD.test(regresion_lineal , c("Sexo", "Compuesto" , "Concentracion") , alpha=0.05 , console=TRUE)
# )))

# === TABLA SD TRATAMIENTOS (completo) === (((
sprintf(" --- TABLA SD TRATAMIENTOS (completo) --- ")
tabla_sd = data.frame(matrix(ncol = 5,nrow = 0))
for(sexo in levels(Sexo)) 
	for(comp in levels(Compuesto))
		for (conc in levels(Concentracion))
		{
			nd = filter(datos,Sexo == sexo, Compuesto == comp, Concentracion == conc)
			mean = mean(nd$mV)
			desv_stand = sd(nd$mV)
			tabla_sd = rbind(tabla_sd , c(sexo, comp, conc,mean,desv_stand))
		}

tabla_sd = SetNames(tabla_sd, c("Sexo", "Compuesto", "Concentracion", "Mean", "Sd"))
tabla_sd
# )))

# === TABLA SD TRATAMIENTOS (reducido) === (((
sprintf(" --- TABLA SD TRATAMIENTOS (reducido) --- ")
tabla_sd_reducida = data.frame(matrix(ncol = 4,nrow = 0))
for(comp in levels(Compuesto))
	for (conc in levels(Concentracion))
	{
		nd = filter(datos,Compuesto == comp, Concentracion == conc)
		mean = mean(nd$mV)
		desv_stand = sd(nd$mV)
		tabla_sd_reducida = rbind(tabla_sd_reducida , c(comp, conc,mean,desv_stand))
	}

tabla_sd_reducida = SetNames(tabla_sd_reducida, c("Compuesto", "Concentracion", "Mean", "Sd"))
tabla_sd_reducida
# )))
