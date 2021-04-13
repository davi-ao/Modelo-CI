library(tidyverse)
library(plotly)

visualize_matrix = function(M, x_label = 'Rows', y_label = 'Columns') {
  p = plot_ly(z = M, type = "surface") %>%
    layout(
#      title = "Matrix visualization as landscape-like activation",
      scene = list(
        xaxis = list(
          title = paste('X: ', x_label),
          range = c(0, ncol(M) - 1),
          nticks = 5,
          tickfont = list(size = 18),
          titlefont = list(size = 30)
        ),
        yaxis = list(
          title = paste('Y:', y_label), 
          range = c(0, nrow(M) - 1),
          nticks = 5,
          tickfont = list(size = 18),
          titlefont = list(size = 30)
        ),
        zaxis = list(
          title = "Activation", 
          range = c(0, max(M)),
          tickfont = list(size = 18),
          titlefont = list(size = 30)
        )
      ),
      showlegend = F
    ) %>%
    add_trace(
      hovertemplate = paste(
        'X%{x},',
        'Y%{y}: ',
        '<b>%{z}</b>',
        '<extra></extra>'
      )
    ) %>%
    layout(showlegend = F)
  p
}

# Example
visualize_matrix(matrix(c(
    1,  .6,  .3,  .5,  .1, 
   .7,   1,  .6,  .1,  .4,
    1,  .0,  .8,   1,  .4,
    0,  .6,  .7,  .9,  .2,
    0,   0,  .3,  .7,  .6
  ), 5), 'Cycles', 'Propositions')
