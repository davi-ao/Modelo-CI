#######################################################################
# MODELO CONSTRUÇÃO-INTEGRAÇÃO                                        #
#######################################################################

# Proposições

# Período a - Ciclo 1               Período b - Ciclo 2
# P1 DESCRIMINALIZAR [ABORTO]       P9 MODIFICA [NECESSÁRIO, P1]
# P2 MODIFICA [NECESSÁRIO, P1]      P10 PORQUE [P9, 11]
# P3 NEGAR [P2]                     P11 NEGAR [P12]
# P4 PORQUE [P3, P5]                P12 MODIFICA [POSSIBILIDADE, P13]
# P5 SE [P6, P7]                    P13 INTERFERIR [GOVERNO, P14]
# P6 DAR [DEUS, VIDA]               P14 MODIFICA [PESSOAIS, DECISÕES]
# P7 MODIFICA [POSSIBILIDADE, P8]   P8 TIRAR [DEUS, VIDA]

library(tidyverse)
library(igraph)

C = matrix(c(
  #P1  P2  P3  P4  P5  P6  P7  P8  P9  P10 P11 P12 P13 P14
   1,  1,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0, #P1
   1,  1,  1,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0, #P2
   0,  1,  1,  1,  0,  0,  0,  0, -1,  0,  0,  0,  0,  0, #P3
   0,  0,  1,  1,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0, #P4
   0,  0,  0,  1,  1,  1,  1,  0,  0,  0,  0,  0,  0,  0, #P5
   0,  0,  0,  0,  1,  1,  0,  1,  0,  0,  0,  0,  0,  0, #P6
   0,  0,  0,  0,  1,  0,  1,  1,  0,  0,  0,  1,  0,  0, #P7
   0,  0,  0,  0,  0,  1,  1,  1,  0,  0,  0,  0,  0,  0, #P8
   1,  1, -1,  0,  0,  0,  0,  0,  1,  1,  0,  0,  0,  0, #P9
   0,  0,  0,  0,  0,  0,  0,  0,  1,  1,  1,  0,  0,  0, #P10
   0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  1,  1,  0,  0, #P11
   0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  1,  1,  1,  0, #P12
   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  1,  1, #P13
   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  1  #P14
   ), nrow = 14) # Conexões

colnames(C) = paste("P", 1:14, sep = "") # Renomear colunas
rownames(C) = colnames(C) # Renomear linhas

# Conferir se matriz é simétrica
all(C == t(C))

# PRIMEIRO CICLO - Leitor A: contra o aborto
A1 = matrix(c(1, 1, 1, 1, 1, 1, 1, 1), nrow = 1) # Ativação inicial do ciclo 1
colnames(A1) = colnames(C[,1:length(A1)]) # Renomear linhas

t = 1
while(
  any(abs(
    A1 - (A1 %*% C[1:length(A1),1:length(A1)])/
    max(A1 %*% C[1:length(A1),1:length(A1)])) > .001
  )) {
  A1 = (
    A1 %*% C[1:length(A1),1:length(A1)])/
    max(A1 %*% C[1:length(A1),1:length(A1)]) # Estabilização (t = 23 iterações)
  t = t + 1
}

# SEGUNDO CICLO  - Leitor A: contra o aborto
A2 = matrix(c(A1, -1, 1, 1, 1, 1, 1), nrow = 1) # Ativação inicial do ciclo 2
colnames(A2) = colnames(C[,1:length(A2)]) # Renomear colunas

t = 1
while(
  any(abs(
    A2 - (A2 %*% C[1:length(A2),1:length(A2)])/
    max(A2 %*% C[1:length(A2),1:length(A2)])) > .001
  )) {
  A2 = (
    A2 %*% C[1:length(A2),1:length(A2)])/
    max(A2 %*% C[1:length(A2),1:length(A2)]) # Estabilização (t = 21 iterações)
  t = t + 1
}

# Memória de Longo Prazo - Leitor A: contra o aborto
M_A = matrix(rep(rep(0, ncol(A2)), ncol(A2)), nrow = ncol(A2))

for (i in 1:nrow(C)) {
  for (j in 1:ncol(C))
    M_A[i, j] = ifelse(i <= nrow(C) & j <= ncol(C), (C[i, j] * A2[i] * A2[j]), 0)
}

colnames(M_A) = colnames(C) # Renomear colunas
rownames(M_A) = colnames(C) # Renomear linhas

# -----------------------------------------------------------------------------

# PRIMEIRO CICLO - Leitor B: a favor do aborto
B1 = matrix(c(1, 1, -1, 1, 1, 1, 1, 1), nrow = 1) # Ativação inicial do ciclo 1
colnames(B1) = colnames(C[,1:length(B1)]) # Renomear linhas

t = 1
while(
  any(abs(
    B1 - (B1 %*% C[1:length(B1),1:length(B1)])/
    max(B1 %*% C[1:length(B1),1:length(B1)])) > .001
  )) {
  B1 = (
    B1 %*% C[1:length(B1),1:length(B1)])/
    max(B1 %*% C[1:length(B1),1:length(B1)]) # Estabilização (t = 23 iterações)
  t = t + 1
}

# SEGUNDO CICLO  - Leitor A: contra o aborto
B2 = matrix(c(B1, 1, 1, 1, 1, 1, 1), nrow = 1) # Ativação inicial do ciclo 2
colnames(B2) = colnames(C[,1:length(B2)]) # Renomear colunas

t = 1
while(
  any(abs(
    B2 - (B2 %*% C[1:length(B2),1:length(B2)])/
    max(B2 %*% C[1:length(B2),1:length(B2)])) > .001
  )) {
  B2 = (
    B2 %*% C[1:length(B2),1:length(B2)])/
    max(B2 %*% C[1:length(B2),1:length(B2)]) # Estabilização (t = 21 iterações)
  t = t + 1
}

# Memória de Longo Prazo - Leitor A: contra o aborto
M_B = matrix(rep(rep(0, ncol(B2)), ncol(B2)), nrow = ncol(B2))

for (i in 1:nrow(C)) {
  for (j in 1:ncol(C))
    M_B[i, j] = ifelse(i <= nrow(C) & j <= ncol(C), (C[i, j] * B2[i] * B2[j]), 0)
}

colnames(M_B) = colnames(C) # Renomear colunas
rownames(M_B) = colnames(C) # Renomear linhas

# ------------------------------------------------------------------------------

# Grafos
# Referência: https://kateto.net/network-visualization

# Leitor A
G_A = graph_from_adjacency_matrix(M_A, weighted = T) %>%
  simplify(remove.multiple = F, remove.loops = T)

deg = G_A %>% degree(mode = 'all')

V(G_A)$size = deg * 5
E(G_A)$width = E(G_A)$weight * 5

graph_attr(G_A, "layout") = layout_with_lgl

plot(G_A, 
     edge.arrow.size = 0,
     vertex.color = '#FF968A',
     vertex.label.family = 'sans',
     vertex.label.color = '#333333',
     vertex.frame.color = '#FFFFFF')

# Leitor B
G_B = graph_from_adjacency_matrix(M_B, weighted = T) %>%
  simplify(remove.multiple = F, remove.loops = T)

deg = G_B %>% degree(mode = 'all')

V(G_B)$size = deg * 5
E(G_B)$width = E(G_B)$weight * 5

graph_attr(G_B, "layout") = layout_with_lgl

plot(G_B, 
     edge.arrow.size = 0,
     vertex.color = '#FF968A',
     vertex.label.family = 'sans',
     vertex.label.color = '#333333',
     vertex.frame.color = '#FFFFFF')
