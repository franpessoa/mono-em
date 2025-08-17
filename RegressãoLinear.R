library(ggplot2)
library(plotly)
library(reshape2)

set.seed(1234)

n <- 1000

m_v1 <- 0
sd_v1 <- 0.2
v1 <- rnorm(n, mean=m_v1, sd=sd_v1) + 5

m_v2 <- 0
sd_v2 <- 0.2
v2 <- rnorm(n, mean=m_v2, sd=sd_v2) + 3

df <- data.frame(v1, v2)

m_res1_noise <- 0
sd_res1_noise <- 0.3
res1 <- 2 * v1 + rnorm(n, m_res1_noise, sd_res1_noise) + 10
df$res1 = res1

m_res2_noise <- 0
sd_res2_noise <- 0.3
res2 <- v1 - (2 * v2) + rnorm(n, m_res2_noise, sd_res2_noise)
df$res2 = res2

est1 <- lm(res1~v1, data=df)
summary(est1)

df$pred1 = predict(est1)

ggplot(df, aes(x = v1, y = res1)) +
  geom_point()

ggplot(df, aes(x = v1, y = res1)) +
  geom_point() + 
  geom_smooth(aes(y = pred1))

est2 <- lm(res2~v1 + v2, data=df)
summary(est2)

df$pred2 = predict(est2)

axis_x <- seq(min(df$v1), max(df$v1), 0.02)
axis_y <- seq(min(df$v2), max(df$v2), 0.02)
pred_surf <- expand.grid(v1=axis_x, v2=axis_y, KEEP.OUT.ATTRS = F)
pred_surf$pred2 <- predict.lm(est2, newdata = pred_surf)
pred_surf <- acast(pred_surf, v2 ~v1,value.var="pred2")

p3d <- plot_ly(df, x = ~v1, y = ~v2, z = ~res2, type="scatter3d", marker=list(size=1.5, color="black", alpha=0.65)) |>
  add_surface(
    x = seq(min(df$v1), max(df$v1), 0.02),
    y = seq(min(df$v2), max(df$v2), 0.02),
    z = pred_surf,
    opacity = 0.5,
    name="pred_sfc",
    type="mesh3d"
  ) |>
  layout(title = "3D Scatter Plot",
         scene = list(xaxis = list(title = "X Axis"),
                      yaxis = list(title = "Y Axis"),
                      zaxis = list(title = "Z Axis"),
                      aspectmode="cube"))
p3d
