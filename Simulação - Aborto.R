#######################################################################
# MODELO CONSTRUÇÃO-INTEGRAÇÃO                                        #
#######################################################################

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
A2 = matrix(c(A1, .1, 1, 1, 1, 1, 1), nrow = 1) # Ativação inicial do ciclo 2
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

# Memória de Longo Prazo
M_A = matrix(rep(rep(0, ncol(A2)), ncol(A2)), nrow = ncol(A2))

for (i in 1:nrow(C)) {
  for (j in 1:ncol(C))
    M_A[i, j] = ifelse(i <= nrow(C) & j <= ncol(C), (C[i, j] * A2[i] * A2[j]), 0)
}

colnames(M_A) = colnames(C) # Renomear colunas
rownames(M_A) = colnames(C) # Renomear linhas

# Grafo
# Referência: https://kateto.net/network-visualization

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
