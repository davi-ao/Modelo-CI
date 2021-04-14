################################
# MODELO CONSTRUÇÃO-INTEGRAÇÃO #
################################

# Proposições

# Período a - Ciclo 1               Período b - Ciclo 2
# P1 DESCRIMINALIZAR [ABORTO]       P9 MODIFICA [NECESSÁRIO, P1]
# P2 MODIFICA [NECESSÁRIO, P1]      P10 PORQUE [P9, 11]
# P3 NEGAR [P2]                     P11 NEGAR [P12]
# P4 PORQUE [P3, P5]                P12 MODIFICA [POSSIBILIDADE, P13]
# P5 SE [P6, P7]                    P13 INTERFERIR [GOVERNO, P14]
# P6 DAR [DEUS, VIDA]               P14 MODIFICA [PESSOAIS, DECISÕES]
# P7 MODIFICA [POSSIBILIDADE, P8]   
# P8 TIRAR [DEUS, VIDA]

# Leitor A: contra a descriminalização do aborto, P1 a P8 mais forte
# Leitor B: a favor da descriminalização do aborto, P9 a P14 mais forte

library(tidyverse)
library(igraph)

# Leitor A: contra a descriminalização
C_A = matrix(c(
  #P1  P2  P3  P4  P5  P6  P7  P8  P9  P10 P11 P12 P13 P14
 1.5,1.5,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0, #P1
 1.5,1.5,1.5,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0, #P2
   0,1.5,1.5,1.5,  0,  0,  0,  0, -1,  0,  0,  0,  0,  0, #P3
   0,  0,1.5,1.5,1.5,  0,  0,  0,  0,  0,  0,  0,  0,  0, #P4
   0,  0,  0,1.5,1.5,1.5,1.5,  0,  0,  0,  0,  0,  0,  0, #P5
   0,  0,  0,  0,1.5,1.5,  0,1.5,  0,  0,  0,  0,  0,  0, #P6
   0,  0,  0,  0,1.5,  0,1.5,1.5,  0,  0,  0,  1,  0,  0, #P7
   0,  0,  0,  0,  0,1.5,1.5,1.5,  0,  0,  0,  0,  0,  0, #P8
   1,  1, -1,  0,  0,  0,  0,  0,  1,  1,  0,  0,  0,  0, #P9
   0,  0,  0,  0,  0,  0,  0,  0,  1,  1,  1,  0,  0,  0, #P10
   0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  1,  1,  0,  0, #P11
   0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  1,  1,  1,  0, #P12
   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  1,  1, #P13
   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  1  #P14
   ), nrow = 14) # Conexões

colnames(C_A) = paste("P", 1:14, sep = "") # Renomear colunas
rownames(C_A) = colnames(C_A) # Renomear linhas

# Conferir se matriz é simétrica
all(C_A == t(C_A))

# PRIMEIRO CICLO
A1 = matrix(c(1, 1, 1, 1, 1, 1, 1, 1), nrow = 1) # Ativação inicial do ciclo 1
colnames(A1) = colnames(C_A[,1:length(A1)]) # Renomear linhas

t = 1
while(
  any(abs(
    A1 - (A1 %*% C_A[colnames(A1), colnames(A1)])/
    max(A1 %*% C_A[colnames(A1), colnames(A1)])) > .001
  )) {
  A1 = (
    A1 %*% C_A[colnames(A1), colnames(A1)])/
    max(A1 %*% C_A[colnames(A1), colnames(A1)]) # Estabilização (t = 23 iterações)
  t = t + 1
}

# SEGUNDO CICLO
A2 = matrix(c(A1, 1, 1, 1, 1, 1, 1), nrow = 1) # Ativação inicial do ciclo 2
colnames(A2) = colnames(C_A[,1:length(A2)]) # Renomear colunas

# Selecionar apenas as proposições ativadas no ciclo
A2 = A2[, c('P9', 'P10', 'P11', 'P12', 'P13', 'P14', 'P1', 'P2', 'P3', 'P7')] %>%
  t() %>% as.matrix()

t = 1
while(
  any(abs(
    A2 - (A2 %*% C_A[colnames(A2), colnames(A2)])/
    max(A2 %*% C_A[colnames(A2), colnames(A2)])) > .001
  )) {
  A2 = (
    A2 %*% C_A[colnames(A2), colnames(A2)])/
    max(A2 %*% C_A[colnames(A2), colnames(A2)]) # Estabilização (t = 25 iterações)
  t = t + 1
}

# Memória de Longo Prazo - Leitor A
M_A = matrix(rep(rep(0, ncol(C_A)), nrow(C_A)), nrow = ncol(C_A))
colnames(M_A) = colnames(C_A) # Renomear colunas
rownames(M_A) = rownames(C_A) # Renomear linhas

# Primeiro ciclo
for (i in rownames(C_A)) {
  for (j in colnames(C_A)) {
    M_A[i, j] = ifelse(
      i %in% colnames(A1) & j %in% colnames(A1), 
      M_A[i, j] + (C_A[i, j] * A1[,i] * A1[,j]), 
      0)
  }
}

# Segundo ciclo
for (i in rownames(C_A)) {
  for (j in colnames(C_A)){
    M_A[i, j] = ifelse(
      i %in% colnames(A2) & j %in% colnames(A2), 
      M_A[i, j] + (C_A[i, j] * A2[,i] * A2[,j]), 
      0)
  }
}

# -----------------------------------------------------------------------------

# Leitor B: a favor da descriminalização aborto

C_B = matrix(c(
  #P1 P2   P3  P4  P5  P6  P7  P8   P9  P10 P11 P12 P13 P14
  1,  1,   0,  0,  0,  0,  0,  0, 1.5,  0,  0,  0,  0,  0, #P1
  1,  1,   1,  0,  0,  0,  0,  0, 1.5,  0,  0,  0,  0,  0, #P2
  0,  1,   1,  1,  0,  0,  0,  0,-1.5,  0,  0,  0,  0,  0, #P3
  0,  0,   1,  1,  1,  0,  0,  0,   0,  0,  0,  0,  0,  0, #P4
  0,  0,   0,  1,  1,  1,  1,  0,   0,  0,  0,  0,  0,  0, #P5
  0,  0,   0,  0,  1,  1,  0,  1,   0,  0,  0,  0,  0,  0, #P6
  0,  0,   0,  0,  1,  0,  1,  1,   0,  0,  0,1.5,  0,  0, #P7
  0,  0,   0,  0,  0,  1,  1,  1,   0,  0,  0,  0,  0,  0, #P8
1.5,1.5,-1.5,  0,  0,  0,  0,  0, 1.5,1.5,  0,  0,  0,  0, #P9
  0,  0,   0,  0,  0,  0,  0,  0, 1.5,1.5,1.5,  0,  0,  0, #P10
  0,  0,   0,  0,  0,  0,  0,  0,   0,1.5,1.5,1.5,  0,  0, #P11
  0,  0,   0,  0,  0,  0,1.5,  0,   0,  0,1.5,1.5,1.5,  0, #P12
  0,  0,   0,  0,  0,  0,  0,  0,   0,  0,  0,1.5,1.5,1.5, #P13
  0,  0,   0,  0,  0,  0,  0,  0,   0,  0,  0,  0,1.5,1.5  #P14
), nrow = 14) # Conexões

colnames(C_B) = paste("P", 1:14, sep = "") # Renomear colunas
rownames(C_B) = colnames(C_B) # Renomear linhas

# Conferir se matriz é simétrica
all(C_B == t(C_B))


# PRIMEIRO CICLO
B1 = matrix(c(1, 1, 1, 1, 1, 1, 1, 1), nrow = 1) # Ativação inicial do ciclo 1
colnames(B1) = colnames(C_B[,1:length(B1)]) # Renomear linhas

t = 1
while(
  any(abs(
    B1 - (B1 %*% C_B[colnames(B1), colnames(B1)])/
    max(B1 %*% C_B[colnames(B1), colnames(B1)])) > .001
  )) {
  B1 = (
    B1 %*% C_B[colnames(B1), colnames(B1)])/
    max(B1 %*% C_B[colnames(B1), colnames(B1)]) # Estabilização (t = 23 iterações)
  t = t + 1
}

# SEGUNDO CICLO
B2 = matrix(c(B1, 1, 1, 1, 1, 1, 1), nrow = 1) # Ativação inicial do ciclo 2
colnames(B2) = colnames(C_B[,1:length(B2)]) # Renomear colunas

# Selecionar apenas as proposições ativadas no ciclo
B2 = B2[, c('P9', 'P10', 'P11', 'P12', 'P13', 'P14', 'P1', 'P2', 'P3', 'P7')] %>%
  t() %>% as.matrix()

t = 1
while(
  any(abs(
    B2 - (B2 %*% C_B[colnames(B2), colnames(B2)])/
    max(B2 %*% C_B[colnames(B2), colnames(B2)])) > .001
  )) {
  B2 = (
    B2 %*% C_B[colnames(B2), colnames(B2)])/
    max(B2 %*% C_B[colnames(B2), colnames(B2)]) # Estabilização (t = 40 iterações)
  t = t + 1
}

# Memória de Longo Prazo - Leitor A
M_B = matrix(rep(rep(0, ncol(C_B)), nrow(C_B)), nrow = ncol(C_B))
colnames(M_B) = colnames(C_B) # Renomear colunas
rownames(M_B) = rownames(C_B) # Renomear linhas

# Primeiro ciclo
for (i in rownames(C_B)) {
  for (j in colnames(C_B)) {
    M_B[i, j] = ifelse(
      i %in% colnames(B1) & j %in% colnames(B1), 
      M_B[i, j] + (C_B[i, j] * B1[,i] * B1[,j]), 
      0)
  }
}

# Segundo ciclo
for (i in rownames(C_B)) {
  for (j in colnames(C_B)){
    M_B[i, j] = ifelse(
      i %in% colnames(B2) & j %in% colnames(B2), 
      M_B[i, j] + (C_B[i, j] * B2[,i] * B2[,j]), 
      0)
  }
}

# ------------------------------------------------------------------------------

# Grafos
# Referência: https://kateto.net/network-visualization

# Leitor A: contra a descriminalização
G_A = graph_from_adjacency_matrix(M_A, weighted = T) %>%
  simplify(remove.multiple = F, remove.loops = T)

V(G_A)$size = sqrt(pi * strength(G_A)) * 6
E(G_A)$width = E(G_A)$weight * 5
E(G_A)$lty = ifelse(E(G_A)$weight > 0, 1, 3)

graph_attr(G_A, "layout") = layout_with_graphopt(G_A, charge = .1)

plot(G_A, 
     edge.arrow.size = 0,
     vertex.color = '#FF968A',
     vertex.label.family = 'sans',
     vertex.label.color = '#333333',
     vertex.frame.color = '#FFFFFF',
     main = 'Leitor A',
     submain = '(contra a descriminalização)')

# Leitor B: a favor da descriminalização
G_B = graph_from_adjacency_matrix(M_B, weighted = T) %>%
  simplify(remove.multiple = F, remove.loops = T)

V(G_B)$size = sqrt(pi * strength(G_B)) * 6
E(G_B)$width = E(G_B)$weight * 5
E(G_B)$lty = ifelse(E(G_B)$weight > 0, 1, 3)

graph_attr(G_B, "layout") = layout_with_graphopt(G_B, charge = .1)

plot(G_B, 
     edge.arrow.size = 0,
     vertex.color = '#FF968A',
     vertex.label.family = 'sans',
     vertex.label.color = '#333333',
     vertex.frame.color = '#FFFFFF',
     main = 'Leitor B',
     submain = '(a favor da descriminalização)')
