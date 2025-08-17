library(readr)
library(ggplot2)
library(dplyr)

data <- readr::read_csv("Downloads/data.csv") |> filter(Year > 1960)

ggplot(data, aes(x = Year, y = Anomaly)) + geom_col()

est = lm(Anomaly~Year, data)
summary(est)

data$Pred = predict(est)
ggplot(data, aes(x = Year, y = Anomaly)) + 
  geom_col() +
  labs(y="Anomalia", x="Tempo", fill="Anomalia") 

ggplot(data, aes(x = Year, y = Anomaly)) + 
  geom_point() +
  geom_segment( aes(x=Year, xend=Year, y=0, yend=Anomaly)) +
  labs(y="Anomalia", x="Tempo", color="Anomalia")

ggplot(data, aes(x = Year, y = Anomaly)) + 
  geom_point() +
  labs(y="Anomalia", x="Tempo", color="Anomalia")

ggplot(data, aes(x = Year, y = Anomaly)) + 
  geom_point() + 
  geom_smooth(aes(y=Pred)) +
  labs(y="Anomalia", x="Tempo", color="Anomalia")