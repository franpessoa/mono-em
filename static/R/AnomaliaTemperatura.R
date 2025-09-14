library(readr)
library(ggplot2)
library(dplyr)

# Lê os dados
data <- readr::read_csv("./AnomaliaTemperatura.csv") |> filter(Year >= 1970)

# Faz a regressão linear
est = lm(Anomaly~Year, data)

data$Pred = predict(est)

# Anomalia em função do tempo, histograma
p1 <- ggplot(data, aes(x = Year, y = Anomaly)) + 
  geom_col() +
  labs(y="Anomalia (°C)", x="Tempo (anos)", fill="Anomalia") +
  scale_color_grey()
ggsave("plot/anom-1-hist.png", plot = p1, dpi=400, unit="px", width=3840, height = 2160)

# Anomalia em função do tempo, pirulito
p2 <- ggplot(data, aes(x = Year, y = Anomaly)) + 
  geom_point() +
  geom_segment( aes(x=Year, xend=Year, y=0, yend=Anomaly)) +
  labs(y="Anomalia (°C)", x="Tempo (anos)", color="Anomalia") +
  scale_color_grey()
ggsave("plot/anom-2-lollipop.png", plot = p2, dpi=400, unit="px", width=3840, height = 2160)


# Anomalia em função do tempo, scatter
p3 <- ggplot(data, aes(x = Year, y = Anomaly)) + 
  geom_point() +
  labs(y="Anomalia (°C)", x="Tempo (anos)", color="Anomalia") +
  scale_color_grey()
ggsave("plot/anom-3-scatter.png", plot = p3, dpi=400, unit="px", width=3840, height = 2160)

# Anomalia em função do tempo, scatter + linha de regressão
p4 <- ggplot(data, aes(x = Year, y = Anomaly)) + 
  geom_point() + 
  geom_smooth(aes(y=Pred, color="Regreessão Linear")) +
  labs(y="Anomalia(°C)", x="Tempo (anos)", color="Curva")+
  scale_color_grey()
ggsave("plot/anom-3-scatter+reg.png", plot = p4, dpi=400, unit="px", width=3840, height = 2160)

# Anomalia em função do tempo, scatter + linha de regressão + linha polinomial
p5 <- ggplot(data, aes(x = Year, y = Anomaly)) + 
  geom_point() +
  geom_smooth(aes(color = "Linear (Grau 1)"), se=F, method="gam", formula = y ~ poly(x, 1)) +
  geom_smooth(aes(color = "Grau 3"),se=F, method="gam", formula = y ~ poly(x, 3)) +
  labs(y="Anomalia(°C)", x="Tempo (anos)", color="Curva") +
  scale_color_grey()

ggsave("plot/anom-5-scatter+reg+poly.png", plot = p5, dpi=400, unit="px", width=3840, height = 2160)

