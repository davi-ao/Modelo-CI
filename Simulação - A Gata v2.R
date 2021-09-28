# SIMULÇÃO CONSIDERADO A CONSTRUÇÃO DE MACROPROPOSIÇÕES POR REPETIÇÃO
# PRIMEIRO CICLO
A1 = matrix(c(
  #P1 P2  
  1, 1), nrow = 1) # Ativação inicial
colnames(A1) = c("P1", "P2") # Renomear colunas

W1 = matrix(c(1, 1, 1, 1), nrow = length(A1)) # Pesos iniciais
colnames(W1) = colnames(A1) # Renomear colunas
rownames(W1) = colnames(A1) # Renomear linhas

t = 1
while(any(abs(A1 - (A1 %*% W1)/max(A1 %*% W1)) > .001)) {
  A1 = (A1 %*% W1)/max(A1 %*% W1) # Estabilização (t = 1 iteração)
  t = t + 1
}

# Memória de Longo Prazo
M = matrix(rep(rep(0, ncol(A1)), ncol(A1)), nrow = ncol(A1))

for (i in 1:nrow(W1)) {
  for (j in 1:ncol(W1))
    M[i, j] = ifelse(i <= nrow(W1) & j <= ncol(W1), (W1[i, j] * A1[i] * A1[j]), 0)
}

colnames(M) = colnames(A1) # Renomear colunas
rownames(M) = colnames(A1) # Renomear linhas

# SEGUNDO CICLO
A2 = matrix(c(A1, 1, 1, 1, 1, 1, 1), nrow = 1) # Ativação inicial
colnames(A2) = c(colnames(A1), "P3", "P4", "P5", "P6", "P7", "P8") # Renomear colunas

W2 = matrix(c(
  #P1 P2 P3 P4 P5 P6 P7 P8
  1, 1,-1, 1, 0, 0, 0, 0, 
  1, 1, 1, 0, 0, 1, 0, 0,
  -1, 1, 1, 0, 1, 0, 0, 1,
  1, 0, 0, 1, 1, 0, 0, 0,
  0, 0, 1, 1, 1, 1, 0, 0,
  0, 1, 0, 0, 1, 1, 1, 0,
  0, 0, 0, 0, 0, 1, 1, 1,
  0, 0, 1, 0, 0, 0, 1, 1), 
  nrow = length(A2)) # Pesos iniciais
colnames(W2) = colnames(A2) # Renomear colunas
rownames(W2) = colnames(A2) # Renomear linhas

M = matrix( # Expandir Memória de longo prazo
  c(M[1,], rep(0, 6), M[2,], rep(0,6), rep(0, 8*6)), 
  nrow = length(A2)
)
colnames(M) = colnames(A2) # Renomear colunas
rownames(M) = colnames(A2) # Renomear linhas

W2 = W2 + M/sum(M) # Reforço de ligações por repetição

t = 1
while(any(abs(A2 - (A2 %*% W2)/max(A2 %*% W2)) > .001)) {
  A2 = (A2 %*% W2)/max(A2 %*% W2) # Estabilização (t = 18 iterações)
  t = t + 1
}

# Memória de Longo Prazo
M = matrix(rep(rep(0, ncol(A2)), ncol(A2)), nrow = ncol(A2))

for (i in 1:nrow(W2)) {
  for (j in 1:ncol(W2))
    M[i, j] = ifelse(i <= nrow(W1) & j <= ncol(W1), (W1[i, j] * A1[i] * A1[j]), 0) +
              ifelse(i <= nrow(W2) & j <= ncol(W2), (W2[i, j] * A2[i] * A2[j]), 0)
}

colnames(M) = colnames(A2) # Renomear colunas
rownames(M) = colnames(A2) # Renomear linhas

# TERCEIRO CICLO
A3 = matrix(c(A2, 1, 1, 1, 1), nrow = 1) # Ativação inicial
colnames(A3) = c(colnames(A2), "P9", "P10", "P11", "P12") # Renomear colunas

W3 = matrix(c(
  #P1  P2  P3  P4  P5  P6  P7  P8  P9 P10 P11 P12  
  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  1,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,
  0,  0,  1,  0,  0,  0,  0,  0,  1,  0,  0,  0,
  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,
  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,
  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,
  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,
  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,
  0,  0,  1,  0,  0,  0,  0,  0,  1,  1,  0,  0,
  0,  1,  0,  0,  0,  0,  0,  0,  1,  1,  1,  1,
  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  1,  1,
  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  1,  1), 
  nrow = length(A3)) # Pesos iniciais
colnames(W3) = colnames(A3) # Renomear colunas
rownames(W3) = colnames(A3) # Renomear linhas

M = matrix( # Expandir Memória de longo prazo
  c(M[1,], rep(0,4), 
    M[2,], rep(0,4), 
    M[3,], rep(0,4), 
    M[4,], rep(0,4), 
    M[5,], rep(0,4), 
    M[6,], rep(0,4), 
    M[7,], rep(0,4), 
    M[8,], rep(0,4), 
    rep(0, 12*4)), 
  nrow = length(A3)
)
colnames(M) = colnames(A3) # Renomear colunas
rownames(M) = colnames(A3) # Renomear linhas

W3 = W3 + M/sum(M) # Reforço de ligações por repetição

t = 1
while(any(abs(A3 - (A3 %*% W3)/max(A3 %*% W3)) > .001)) {
  A3 = (A3 %*% W3)/max(A3 %*% W3) # Estabilização (t = 10 iterações)
  t = t + 1
}

# Memória de Longo Prazo
M = matrix(rep(rep(0, ncol(A3)), ncol(A3)), nrow = ncol(A3))

for (i in 1:nrow(W3)) {
  for (j in 1:ncol(W3))
    M[i, j] = ifelse(i <= nrow(W1) & j <= ncol(W1), (W1[i, j] * A1[i] * A1[j]), 0) +
              ifelse(i <= nrow(W2) & j <= ncol(W2), (W2[i, j] * A2[i] * A2[j]), 0) +
              ifelse(i <= nrow(W3) & j <= ncol(W3), (W3[i, j] * A3[i] * A3[j]), 0)
}

colnames(M) = colnames(A3) # Renomear colunas
rownames(M) = colnames(A3) # Renomear linhas

# QUARTO CICLO
A4 = matrix(c(A3, 1, 1, 1), nrow = 1) # Ativação inicial
colnames(A4) = c(colnames(A3), "P13", "P14", "P15") # Renomear colunas

W4 = matrix(c(
  #P1  P2  P3  P4  P5  P6  P7  P8  P9 P10 P11 P12 P13 P14 P15
  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  1,
  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,
  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  1,
  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,
  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,
  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  1,  0,
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  1,  1,
  0,  0,  1,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  1,  1), 
  nrow = length(A4)) # Pesos iniciais
colnames(W4) = colnames(A4) # Renomear colunas
rownames(W4) = colnames(A4) # Renomear linhas

M = matrix( # Expandir Memória de longo prazo
  c(M[1,], rep(0,3), 
    M[2,], rep(0,3), 
    M[3,], rep(0,3), 
    M[4,], rep(0,3), 
    M[5,], rep(0,3), 
    M[6,], rep(0,3), 
    M[7,], rep(0,3), 
    M[8,], rep(0,3),
    M[9,], rep(0,3),
    M[10,], rep(0,3),
    M[11,], rep(0,3),
    M[12,], rep(0,3),
    rep(0, 15*3)), 
  nrow = length(A4)
)
colnames(M) = colnames(A4) # Renomear colunas
rownames(M) = colnames(A4) # Renomear linhas

W4 = W4 + M/sum(M) # Reforço de ligações por repetição

t = 1
while(any(abs(A4 - (A4 %*% W4)/max(A4 %*% W4)) > .001)) {
  A4 = (A4 %*% W4)/max(A4 %*% W4) # Estabilização (t = 10 iterações)
  t = t + 1
}

# Memória de Longo Prazo
M = matrix(rep(rep(0, ncol(A4)), ncol(A4)), nrow = ncol(A4))

for (i in 1:nrow(W4)) {
  for (j in 1:ncol(W4))
    M[i, j] = ifelse(i <= nrow(W1) & j <= ncol(W1), (W1[i, j] * A1[i] * A1[j]), 0) +
      ifelse(i <= nrow(W2) & j <= ncol(W2), (W2[i, j] * A2[i] * A2[j]), 0) +
      ifelse(i <= nrow(W3) & j <= ncol(W3), (W3[i, j] * A3[i] * A3[j]), 0) +
      ifelse(i <= nrow(W4) & j <= ncol(W4), (W4[i, j] * A4[i] * A4[j]), 0)
}

colnames(M) = colnames(A4) # Renomear colunas
rownames(M) = colnames(A4) # Renomear linhas

# QUINTO CICLO
A5 = matrix(c(A4, 1, 1, 1, 1, 1, 1, 1), nrow = 1) # Ativação inicial
colnames(A5) = c(colnames(A4), "P16", "P17", "P18", "P19", "P20", "P21", "P22") # Renomear colunas

W5 = matrix(c(
  #P1  P2  P3  P4  P5  P6  P7  P8  P9 P10 P11 P12 P13 P14 P15 P16 P17 P18 P19 P20 P21 P22  
  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  1,  1,  1,  1,  0,  0,
  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,
  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  1,  0,  0,  0,  1,  1,
  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  1,  0,  0,  0,  0,  0,
  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  1,  1,
  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  1,  1,
  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  1,  1,
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  1,  1,  1,  1,  1), 
  nrow = length(A5)) # Pesos iniciais
colnames(W5) = colnames(A5) # Renomear colunas
rownames(W5) = colnames(A5) # Renomear linhas

M = matrix( # Expandir Memória de longo prazo
  c(M[1,], rep(0,7), 
    M[2,], rep(0,7), 
    M[3,], rep(0,7), 
    M[4,], rep(0,7), 
    M[5,], rep(0,7), 
    M[6,], rep(0,7), 
    M[7,], rep(0,7), 
    M[8,], rep(0,7),
    M[9,], rep(0,7),
    M[10,], rep(0,7),
    M[11,], rep(0,7),
    M[12,], rep(0,7),
    M[13,], rep(0,7),
    M[14,], rep(0,7),
    M[15,], rep(0,7),
    rep(0, 22*7)), 
  nrow = length(A5)
)
colnames(M) = colnames(A5) # Renomear colunas
rownames(M) = colnames(A5) # Renomear linhas

W5 = W5 + M/sum(M) # Reforço de ligações por repetição

t = 1
while(any(abs(A5 - (A5 %*% W5)/max(A5 %*% W5)) > .001)) {
  A5 = (A5 %*% W5)/max(A5 %*% W5) # Estabilização (t = 8 iterações)
  t = t + 1
}

# Memória de Longo Prazo
M = matrix(rep(rep(0, ncol(A5)), ncol(A5)), nrow = ncol(A5))

for (i in 1:nrow(W5)) {
  for (j in 1:ncol(W5))
    M[i, j] = ifelse(i <= nrow(W1) & j <= ncol(W1), (W1[i, j] * A1[i] * A1[j]), 0) +
      ifelse(i <= nrow(W2) & j <= ncol(W2), (W2[i, j] * A2[i] * A2[j]), 0) +
      ifelse(i <= nrow(W3) & j <= ncol(W3), (W3[i, j] * A3[i] * A3[j]), 0) +
      ifelse(i <= nrow(W4) & j <= ncol(W4), (W4[i, j] * A4[i] * A4[j]), 0) +
      ifelse(i <= nrow(W5) & j <= ncol(W5), (W5[i, j] * A5[i] * A5[j]), 0)
}

colnames(M) = colnames(A5) # Renomear colunas
rownames(M) = colnames(A5) # Renomear linhas

# Gráfico
library(tidyverse)
library(ggthemes)
theme_set(theme_bw() + theme_economist_white())

plot = data.frame(proposition = paste0("P", 1:22), activation = rowSums(M)) %>%
  ggplot(
    aes(
      x = reorder(proposition, -activation), 
      y = activation, 
      color = activation, 
      size = activation,
      label = proposition
    )
  ) +
  geom_text() +
  scale_color_gradient(low = '#5C258D', high = '#4389A2') +
  scale_y_continuous(trans = "log2") +
  xlab('') + 
  ylab('Ativação') +
  theme(legend.position = 'none', axis.text.x = element_blank())
plot

ggsave('estimativa_ativacao.png', plot, width = 18, height = 13.5, units = 'cm')
