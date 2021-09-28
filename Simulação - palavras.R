library(readxl)
library(tidyverse)
library(sjPlot)
library(plotly)

palavras_simulacao <- read_excel("palavras_simulacao.xlsx")

analyze_X = function(X) {
  data_X = palavras_simulacao %>%
    mutate(isX = class == X) %>%
    select(word, class_prev, class_after, isX)
  
  model_X_prev = glm(isX ~ class_prev, data = data_X, family = 'binomial')
  plot = plot_model(model_X_prev, type = 'eff')
  plot[[1]][["labels"]][["title"]] = paste('Predicted value of is', X, sep = '')
  
  print(ggplotly(plot[[1]]$plot_env$p))
  print(summary(model_X_prev))
  print(format(
    plot[[1]][["plot_env"]][["p"]][["data"]], 
    scientific = F))
  
  model_X_after = glm(isX ~ class_after, data = data_X, family = 'binomial')
  plot = plot_model(model_X_after, type = 'eff')
  plot[[1]][["labels"]][["title"]] = paste('Predicted value of is', X, sep = '')
  
  print(ggplotly(plot[[1]]$plot_env$p))
  print(summary(model_X_after))
  print(format(
    plot[[1]][["plot_env"]][["p"]][["data"]], 
    scientific = F))
}

# N
analyze_X('N')

# Adj
analyze_X('Adj')

# V
analyze_X('V')

# Adv
analyze_X('Adv')

# N summary
data_N %>%
  group_by(class_prev) %>%
  summarize(
    isN = mean(isN),
    N = n(),
    SE = sqrt(isN * (1 - isN) / N),
    lower = isN - qnorm(.975) * SE,
    upper = isN + qnorm(.975) * SE)

# N ~ Adj Simulation
simulation = function(Num) {
  isN = mean(sample(c(1,0), Num, T, c(.62, .38)))
  SE = sqrt(isN * (1-isN) / Num)
  lower = isN - qnorm(.975) * SE
  upper = isN + qnorm(.975) * SE
  c(lower, upper)
}

p_sim = as.data.frame(t(sapply(seq(100, 10000, 100), simulation))) %>%
  ggplot(aes(x = seq(100, 10000, 100))) + 
    geom_line(aes(y = V1), color = 'blue') + 
    geom_line(aes(y = V2), color = 'blue') + 
    geom_line(aes(y = V2 - V1), color = 'red')
ggplotly(p_sim)

# N ~ N Simulation
simulation = function(Num) {
  isN = mean(sample(c(1,0), Num, T, c(.05, .95)))
  SE = sqrt(isN * (1-isN) / Num)
  lower = isN - qnorm(.975) * SE
  upper = isN + qnorm(.975) * SE
  c(lower, upper)
}

p_sim = as.data.frame(t(sapply(seq(100, 10000, 100), simulation))) %>%
  ggplot(aes(x = seq(100, 10000, 100))) + 
  geom_line(aes(y = V1), color = 'blue') + 
  geom_line(aes(y = V2), color = 'blue') + 
  geom_line(aes(y = V2 - V1), color = 'red')
ggplotly(p_sim)
