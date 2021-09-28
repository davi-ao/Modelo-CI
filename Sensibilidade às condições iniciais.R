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

# Leitor a favor da descriminalização do aborto, P9 a P14 mais forte

library(tidyverse)
library(igraph)
library(ggraph)
library(ggrepel)
library(ggthemes)

theme_set(theme_bw() + theme_hc())

# CONDIÇÃO 1

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
activation = tibble(ciclo = NULL, 
                    t = NULL, 
                    P1 = NULL,
                    P2 = NULL,
                    P3 = NULL,
                    P4 = NULL,
                    P5 = NULL,
                    P6 = NULL,
                    P7 = NULL,
                    P8 = NULL,
                    P9 = NULL,
                    P10 = NULL,
                    P11 = NULL,
                    P12 = NULL,
                    P13 = NULL,
                    P14 = NULL,)

t = 1
while(
  any(abs(
    B1 - (B1 %*% C_B[colnames(B1), colnames(B1)])/
    max(B1 %*% C_B[colnames(B1), colnames(B1)])) > .001
  )) {
  B1 = (
    B1 %*% C_B[colnames(B1), colnames(B1)])/
    max(B1 %*% C_B[colnames(B1), colnames(B1)]) # Estabilização (t = 23 iterações)
  
  activation = activation %>% bind_rows(tibble(ciclo = 1,
                                               t = t, 
                                               P1 = B1[1],
                                               P2 = B1[2],
                                               P3 = B1[3],
                                               P4 = B1[4],
                                               P5 = B1[5],
                                               P6 = B1[6],
                                               P7 = B1[7],
                                               P8 = B1[8],
                                               P9 = 0,
                                               P10 = 0,
                                               P11 = 0,
                                               P12 = 0,
                                               P13 = 0,
                                               P14 = 0))
  
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
  
  activation = activation %>% bind_rows(tibble(ciclo = 2,
                                               t = t, 
                                               P1 = B2[7],
                                               P2 = B2[8],
                                               P3 = B2[9],
                                               P4 = 0,
                                               P5 = 0,
                                               P6 = 0,
                                               P7 = B2[10],
                                               P8 = 0,
                                               P9 = B2[1],
                                               P10 = B2[2],
                                               P11 = B2[3],
                                               P12 = B2[4],
                                               P13 = B2[5],
                                               P14 = B2[6]))
  
  t = t + 1
}

g_con1 = activation %>%
  mutate(Tempo = row_number()) %>%
  select(-c(ciclo, t)) %>%
  pivot_longer(starts_with('P'), names_to = 'Proposição', values_to = 'Ativação') %>%
  ggplot(aes(Tempo, 
             Ativação, 
             color = Proposição, 
             label = ifelse(Tempo == max(Tempo), 
                            paste(Proposição, 
                                  '(', 
                                  round(Ativação, digits = 2), 
                                  ')'), 
                            ''))) +
  geom_line() +
  geom_text_repel(max.overlaps = 20, size = 2) +
  theme(legend.position = 'none') +
  ylim(c(-0.7, 1.1)) +
  xlim(c(0, 65))

# ------------------------------------------------------------------------------
# CONDIÇÃO 2
# Conexões P1-P3, P9-P14 e P11-P12 mudada para -1.5

C_B = matrix(c(
   #P1 P2   P3  P4  P5  P6  P7  P8   P9  P10  P11  P12 P13  P14
   1,  1,-1.5,  0,  0,  0,  0,  0, 1.5,  0,   0,   0,  0,   0, #P1
   1,  1,   1,  0,  0,  0,  0,  0, 1.5,  0,   0,   0,  0,   0, #P2
-1.5,  1,   1,  1,  0,  0,  0,  0,-1.5,  0,   0,   0,  0,   0, #P3
   0,  0,   1,  1,  1,  0,  0,  0,   0,  0,   0,   0,  0,   0, #P4
   0,  0,   0,  1,  1,  1,  1,  0,   0,  0,   0,   0,  0,   0, #P5
   0,  0,   0,  0,  1,  1,  0,  1,   0,  0,   0,   0,  0,   0, #P6
   0,  0,   0,  0,  1,  0,  1,  1,   0,  0,   0, 1.5,  0,   0, #P7
   0,  0,   0,  0,  0,  1,  1,  1,   0,  0,   0,   0,  0,   0, #P8
 1.5,1.5,-1.5,  0,  0,  0,  0,  0, 1.5,1.5,   0,   0,  0,-1.5, #P9
   0,  0,   0,  0,  0,  0,  0,  0, 1.5,1.5, 1.5,   0,  0,   0, #P10
   0,  0,   0,  0,  0,  0,  0,  0,   0,1.5, 1.5,-1.5,  0,   0, #P11
   0,  0,   0,  0,  0,  0,1.5,  0,   0,  0,-1.5, 1.5,1.5,   0, #P12
   0,  0,   0,  0,  0,  0,  0,  0,   0,  0,   0, 1.5,1.5, 1.5, #P13
   0,  0,   0,  0,  0,  0,  0,  0,-1.5,  0,   0,   0,1.5, 1.5  #P14
), nrow = 14) # Conexões

colnames(C_B) = paste("P", 1:14, sep = "") # Renomear colunas
rownames(C_B) = colnames(C_B) # Renomear linhas

# Conferir se matriz é simétrica
all(C_B == t(C_B))


# PRIMEIRO CICLO
B1 = matrix(c(1, 1, 1, 1, 1, 1, 1, 1), nrow = 1) # Ativação inicial do ciclo 1
colnames(B1) = colnames(C_B[,1:length(B1)]) # Renomear linhas
activation = tibble(ciclo = NULL, 
                    t = NULL, 
                    P1 = NULL,
                    P2 = NULL,
                    P3 = NULL,
                    P4 = NULL,
                    P5 = NULL,
                    P6 = NULL,
                    P7 = NULL,
                    P8 = NULL,
                    P9 = NULL,
                    P10 = NULL,
                    P11 = NULL,
                    P12 = NULL,
                    P13 = NULL,
                    P14 = NULL,)

t = 1
while(
  any(abs(
    B1 - (B1 %*% C_B[colnames(B1), colnames(B1)])/
    max(B1 %*% C_B[colnames(B1), colnames(B1)])) > .001
  )) {
  B1 = (
    B1 %*% C_B[colnames(B1), colnames(B1)])/
    max(B1 %*% C_B[colnames(B1), colnames(B1)]) # Estabilização (t = 23 iterações)
  
  activation = activation %>% bind_rows(tibble(ciclo = 1,
                                               t = t, 
                                               P1 = B1[1],
                                               P2 = B1[2],
                                               P3 = B1[3],
                                               P4 = B1[4],
                                               P5 = B1[5],
                                               P6 = B1[6],
                                               P7 = B1[7],
                                               P8 = B1[8],
                                               P9 = 0,
                                               P10 = 0,
                                               P11 = 0,
                                               P12 = 0,
                                               P13 = 0,
                                               P14 = 0))
  
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
  
  activation = activation %>% bind_rows(tibble(ciclo = 2,
                                               t = t, 
                                               P1 = B2[7],
                                               P2 = B2[8],
                                               P3 = B2[9],
                                               P4 = 0,
                                               P5 = 0,
                                               P6 = 0,
                                               P7 = B2[10],
                                               P8 = 0,
                                               P9 = B2[1],
                                               P10 = B2[2],
                                               P11 = B2[3],
                                               P12 = B2[4],
                                               P13 = B2[5],
                                               P14 = B2[6]))
  
  t = t + 1
}

g_con2 = activation %>%
  mutate(Tempo = row_number()) %>%
  select(-c(ciclo, t)) %>%
  pivot_longer(starts_with('P'), names_to = 'Proposição', values_to = 'Ativação') %>%
  ggplot(aes(Tempo, 
             Ativação, 
             color = Proposição, 
             label = ifelse(Tempo == max(Tempo), 
                            paste(Proposição, 
                                  '(', 
                                  round(Ativação, digits = 2), 
                                  ')'), 
                            ''))) +
  geom_line() +
  geom_text_repel(max.overlaps = 20, size = 2) +
  theme(legend.position = 'none') +
  ylim(c(-0.7, 1.1)) +
  xlim(c(0, 65))

ggsave('gráfico_condição_1.png', 
       plot = g_con1, 
       device = 'png', 
       width = 16, 
       height = 9, 
       units = 'cm')

ggsave('gráfico_condição_2.png', 
       plot = g_con2, 
       device = 'png', 
       width = 16, 
       height = 9, 
       units = 'cm')

#-------------------------------------------------------------------------------
# Animação

get_frame = function(p1p3, p9p14, p11p12) {
  C_B = matrix(c(
    #P1 P2   P3  P4  P5  P6  P7  P8    P9  P10    P11    P12 P13   P14
    1,  1,p1p3,  0,  0,  0,  0,  0,  1.5,  0,     0,     0,  0,    0, #P1
    1,  1,   1,  0,  0,  0,  0,  0,  1.5,  0,     0,     0,  0,    0, #P2
 p1p3,  1,   1,  1,  0,  0,  0,  0, -1.5,  0,     0,     0,  0,    0, #P3
    0,  0,   1,  1,  1,  0,  0,  0,    0,  0,     0,     0,  0,    0, #P4
    0,  0,   0,  1,  1,  1,  1,  0,    0,  0,     0,     0,  0,    0, #P5
    0,  0,   0,  0,  1,  1,  0,  1,    0,  0,     0,     0,  0,    0, #P6
    0,  0,   0,  0,  1,  0,  1,  1,    0,  0,     0,   1.5,  0,    0, #P7
    0,  0,   0,  0,  0,  1,  1,  1,    0,  0,     0,     0,  0,    0, #P8
  1.5,1.5,-1.5,  0,  0,  0,  0,  0,  1.5,1.5,     0,     0,  0,p9p14, #P9
    0,  0,   0,  0,  0,  0,  0,  0,  1.5,1.5,   1.5,     0,  0,    0, #P10
    0,  0,   0,  0,  0,  0,  0,  0,    0,1.5,   1.5,p11p12,  0,    0, #P11
    0,  0,   0,  0,  0,  0,1.5,  0,    0,  0,p11p12,   1.5,1.5,    0, #P12
    0,  0,   0,  0,  0,  0,  0,  0,    0,  0,     0,   1.5,1.5,  1.5, #P13
    0,  0,   0,  0,  0,  0,  0,  0,p9p14,  0,     0,     0,1.5,  1.5  #P14
  ), nrow = 14) # Conexões
  
  colnames(C_B) = paste("P", 1:14, sep = "") # Renomear colunas
  rownames(C_B) = colnames(C_B) # Renomear linhas
  
  # Conferir se matriz é simétrica
  if (!all(C_B == t(C_B))) {
    print('Matriz não é simétrica')
  } else {
    # PRIMEIRO CICLO
    B1 = matrix(c(1, 1, 1, 1, 1, 1, 1, 1), nrow = 1) # Ativação inicial do ciclo 1
    colnames(B1) = colnames(C_B[,1:length(B1)]) # Renomear linhas
    activation = tibble(ciclo = NULL, 
                        t = NULL, 
                        P1 = NULL,
                        P2 = NULL,
                        P3 = NULL,
                        P4 = NULL,
                        P5 = NULL,
                        P6 = NULL,
                        P7 = NULL,
                        P8 = NULL,
                        P9 = NULL,
                        P10 = NULL,
                        P11 = NULL,
                        P12 = NULL,
                        P13 = NULL,
                        P14 = NULL,)
    
    t = 1
    while(
      any(abs(
        B1 - (B1 %*% C_B[colnames(B1), colnames(B1)])/
        max(B1 %*% C_B[colnames(B1), colnames(B1)])) > .001
      )) {
      B1 = (
        B1 %*% C_B[colnames(B1), colnames(B1)])/
        max(B1 %*% C_B[colnames(B1), colnames(B1)]) # Estabilização (t = 23 iterações)
      
      activation = activation %>% bind_rows(tibble(ciclo = 1,
                                                   t = t, 
                                                   P1 = B1[1],
                                                   P2 = B1[2],
                                                   P3 = B1[3],
                                                   P4 = B1[4],
                                                   P5 = B1[5],
                                                   P6 = B1[6],
                                                   P7 = B1[7],
                                                   P8 = B1[8],
                                                   P9 = 0,
                                                   P10 = 0,
                                                   P11 = 0,
                                                   P12 = 0,
                                                   P13 = 0,
                                                   P14 = 0))
      
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
      
      activation = activation %>% bind_rows(tibble(ciclo = 2,
                                                   t = t, 
                                                   P1 = B2[7],
                                                   P2 = B2[8],
                                                   P3 = B2[9],
                                                   P4 = 0,
                                                   P5 = 0,
                                                   P6 = 0,
                                                   P7 = B2[10],
                                                   P8 = 0,
                                                   P9 = B2[1],
                                                   P10 = B2[2],
                                                   P11 = B2[3],
                                                   P12 = B2[4],
                                                   P13 = B2[5],
                                                   P14 = B2[6]))
      
      t = t + 1
    }
  }
  
  g_con = activation %>%
    mutate(Tempo = row_number()) %>%
    select(-c(ciclo, t)) %>%
    pivot_longer(starts_with('P'), names_to = 'Proposição', values_to = 'Ativação') %>%
    ggplot(aes(Tempo, 
               Ativação, 
               color = Proposição, 
               label = ifelse(Tempo == max(Tempo), 
                              paste(Proposição, 
                                    '(', 
                                    round(Ativação, digits = 2), 
                                    ')'), 
                              ''))) +
    geom_line() +
    geom_text_repel(max.overlaps = 50, size = 1.8) +
    theme(legend.position = 'none') +
    ylim(c(-1, 1.1)) +
    xlim(c(0, 100)) + 
    ggtitle(paste('P1-P3: ', 
                  p1p3, 
                  '    ', 
                  'P19-P14: ', 
                  p9p14, 
                  '    ', 
                  'P11-P12: ', 
                  p11p12))
  
  g_con
}

x = 1
#seq(-1.5, 1.5, .5)
for (p1p3 in seq(-1.5, 1.5, .5)) {
  y = 1
  
  for (p9p14 in seq(-1.5, 1.5, .5)) {
    z = 1
    
    for (p11p12 in seq(-1.5, 1.5, .5)) {
      ggsave(paste0('animação/frame_', x, y, z, '.png'), 
             plot = get_frame(p1p3, p9p14, p11p12), 
             device = 'png', 
             width = 16, 
             height = 9, 
             units = 'cm')
      
      z = z + 1
    }
    
    y = y + 1
  }
  
  x = x + 1
}

