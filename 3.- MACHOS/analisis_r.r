
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
datos = read.table('MACHOS.txt', header = TRUE, stringsAsFactors = TRUE)
datos$Concentracion = as.factor(datos$Concentracion)
attach(datos)
regresion_lineal = lm(mV ~ Compuesto * Concentracion,data = datos)
# regresion_lineal$coefficients
# )))

# === CAJAS SIMULTÁNEAS === (((
sprintf(" --- DIAGRAMA DE CAJAS SIMULTÁNEAS --- ")
# ggplot(datos) + aes(x=Compuesto,y=mV, fill = Compuesto) + geom_boxplot(alpha=0.8) + theme(axis.text.x = element_blank(), legend.position = "bottom")
# ggplot(datos) + aes(x=Compuesto,y=mV, fill = Compuesto) + geom_boxplot(alpha=0.8) + facet_wrap(~Concentracion)  + theme(legend.position =  "bottom",text = element_text(size = 12), axis.text.x = element_blank())
# ggplot(datos) + aes(x=Concentracion,y=mV, fill = Concentracion) + geom_boxplot(alpha=0.8)  
# ggplot(datos) + aes(x=Concentracion,y=mV, fill = Concentracion) + geom_boxplot(alpha=0.8) + facet_wrap(~Compuesto) 

# GRAFICO IMPORTANTE :)
# ggplot(datos) + aes(x=Compuesto,y=mV, fill = Concentracion) + scale_fill_grey() + geom_boxplot(alpha=0.8) + theme(legend.position = "bottom", text = element_text(size = 12), axis.text.x = element_text(angle = 90, hjust = 1))
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
# ks.test(residuos,"pnorm",0, sqrt(anova$Mean[8])) # es el 8vo elemento de la Tabla ANOVA [columna Mean].
# )))

# === HOMOCEDASTICIDAD === (((
sprintf(" --- 2. HOMOCEDASTICIDAD --- ")
# BARTLETT
leveneTest(mV ~ interaction(Concentracion,Compuesto), data = datos)
bartlett.test(mV ~ interaction(Concentracion,Compuesto), data = datos)
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

# === TUKEY (COMPARACIONES) === (((
sprintf(" --- TUKEY COMPARACIONES --- ")
TukeyHSD(resultados)
# )))

# === LSD === (((
sprintf("--- LSD ---")
LSD.test(regresion_lineal , c("Compuesto" , "Concentracion") , alpha=0.05 , console=TRUE)
# )))

# === TABLA SD TRATAMIENTOS (machos) === (((
sprintf(" --- TABLA SD TRATAMIENTOS (hembras) --- ")
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

# GRAFICA EN BLANCO Y NEGRO
grafica = data.frame(mean = as.numeric(tabla_sd_reducida$Mean), sd = as.numeric(tabla_sd_reducida$Sd), Compuesto = as.factor(tabla_sd_reducida$Compuesto), Concentracion = as.factor(tabla_sd_reducida$Concentracion))
str(grafica)
ggplot(grafica ,aes(x=Compuesto,fill = Concentracion)) + scale_fill_grey() + geom_boxplot(aes(lower = mean - sd, upper = mean + sd, middle = mean, ymin = mean -3*sd, ymax = mean + 3*sd), stat = "identity") + theme(legend.position = "bottom", text = element_text(size = 12), axis.text.x = element_text(angle = 90, hjust = 1))
# )))

# === TABLA SD COMPUESTOS (machos) === (((
# sprintf(" --- TABLA SD COMPUESTOS (machos) --- ")
# tabla_sd_compuestos = data.frame(matrix(ncol = 3,nrow = 0))
# for(comp in levels(Compuesto))
	# {
		# nd = filter(datos,Compuesto == comp)
		# mean = mean(nd$mV)
		# desv_stand = sd(nd$mV)
		# tabla_sd_compuestos = rbind(tabla_sd_compuestos , c(comp, mean,desv_stand))
	# }
			# 
# tabla_sd_compuestos = SetNames(tabla_sd_compuestos, c("Compuesto", "Mean", "Sd"))
# tabla_sd_compuestos

# GRAFICA DEL AMOR
# grafica = data.frame(mean = as.numeric(tabla_sd_compuestos$Mean), sd = as.numeric(tabla_sd_compuestos$Sd), Compuesto = as.factor(tabla_sd_compuestos$Compuesto))
# str(grafica)
# ggplot(grafica ,aes(x=Compuesto) )+ geom_boxplot(aes(lower = mean - sd, upper = mean + sd, middle = mean, ymin = mean -3*sd, ymax = mean + 3*sd), stat = "identity")
# + theme(legend.position = "bottom", text = element_text(size = 12), axis.text.x = element_text(angle = 90, hjust = 1))
# )))

# === GRAFICA === (((
sprintf(" --- GRAFICA --- ")
columnas = c(2,3,4)
datos_grafica = data.frame(lapply(tabla_sd_reducida[,columnas], function(x) as.numeric(x)) )
datos_grafica$Compuesto = factor(tabla_sd_reducida$Compuesto)

ggplot(datos_grafica, aes(group = Concentracion, x = Compuesto,y = Mean, color = factor(Concentracion)) ) +
	geom_errorbar(aes(y = Mean,ymin = Mean - Sd, ymax = Mean + Sd), width = 0.4) +
 	geom_point() +
	theme_bw() +
	scale_fill_manual(values = c("#b3b3b3", "#1a1a1a")) +
	scale_color_manual(values = c("#b3b3b3", "#1a1a1a")) +
	geom_line(size = 1.1) +
	theme(text = element_text(size = 17)) +
	labs(title = "Promedios MACHOS", y = "Promedio (mV)")

ggsave("MACHOS_PROMEDIOS_BARRAS.png", width = 16, height = 9)
ggsave("MACHOS_PROMEDIOS_BARRAS.pdf", width = 16, height = 9)

ggplot(datos_grafica, aes(group = Concentracion, x = Compuesto,y = Mean, color = factor(Concentracion)) ) +
	geom_ribbon(aes(ymin = Mean - Sd, ymax = Mean + Sd, fill= factor(Concentracion)), alpha = 0.7, colour = NA) +
	scale_fill_manual(values = c("#999999", "#1a1a1a")) +
 	geom_line(size = 0.5) +
	scale_color_manual(values = c("#ffffff", "#000000")) +
 	geom_point() +
	theme_bw() +
	labs(title = "Promedios HEMBRAS", y = "Promedio (mV)")

ggsave("HEMBRAS_PROMEDIOS.png", width = 16, height = 9)
ggsave("HEMBRAS_PROMEDIOS.pdf", width = 16, height = 9)
# )))
