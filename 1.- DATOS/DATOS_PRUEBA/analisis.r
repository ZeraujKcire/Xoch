
# === LIBRERIAS === (((
sprintf(" --- LIBRERIAS --- ")
suppressPackageStartupMessages(library(lmtest))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(GGally))
suppressPackageStartupMessages(library(qqplotr))
# )))

# === DATOS === (((
sprintf(" --- DATOS --- ")
datos = read.table('MAXIMOS.txt', header = TRUE, stringsAsFactors = TRUE)
x = datos$COMPUESTO
y = datos$MAXIMO
# )))

# === GRAFICA === (((
sprintf(" --- GRAFICA --- ")
ggplot(data = datos, aes(x = x,y = y, fill=x)) +
 	ggtitle("Grafico Comparativo de Maximos entre Compuestos") +
 	labs(x = "Compuesto",y = "mV") +
	geom_boxplot(alpha=0.8) +
	theme(legend.position="none") +
	scale_fill_brewer(palette="Dark2")
# )))
