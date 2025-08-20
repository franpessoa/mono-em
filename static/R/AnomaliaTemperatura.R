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
  labs(y="Anomalia (°C)", x="Tempo (anos)", fill="Anomalia")
ggsave("plot/anom-1-hist.png", plot = p1, dpi=400, unit="px", width=3840, height = 2160)

# Anomalia em função do tempo, pirulito
p2 <- ggplot(data, aes(x = Year, y = Anomaly)) + 
  geom_point() +
  geom_segment( aes(x=Year, xend=Year, y=0, yend=Anomaly)) +
  labs(y="Anomalia (°C)", x="Tempo (anos)", color="Anomalia")
ggsave("plot/anom-2-lollipop.png", plot = p2, dpi=400, unit="px", width=3840, height = 2160)


# Anomalia em função do tempo, scatter
p3 <- ggplot(data, aes(x = Year, y = Anomaly)) + 
  geom_point() +
  labs(y="Anomalia (°C)", x="Tempo (anos)", color="Anomalia")
ggsave("plot/anom-3-scatter.png", plot = p3, dpi=400, unit="px", width=3840, height = 2160)

# Anomalia em função do tempo, scatter + linha de regressão
p4 <- ggplot(data, aes(x = Year, y = Anomaly)) + 
  geom_point() + 
  geom_smooth(aes(y=Pred)) +
  labs(y="Anomalia(°C)", x="Tempo (anos)", color="Anomalia")
ggsave("plot/anom-3-scatter+reg.png", plot = p4, dpi=400, unit="px", width=3840, height = 2160)

# Anomalia em função do tempo, scatter + linha de regressão + linha polinomial
p5 <- ggplot(data, aes(x = Year, y = Anomaly)) + 
  geom_point() +
  geom_smooth(aes(y=Pred)) +
  geom_smooth(color="red", se=F) +
  labs(y="Anomalia(°C)", x="Tempo (anos)", color="Anomalia")

ggsave("plot/anom-5-scatter+reg+poly.png", plot = p5, dpi=400, unit="px", width=3840, height = 2160)
