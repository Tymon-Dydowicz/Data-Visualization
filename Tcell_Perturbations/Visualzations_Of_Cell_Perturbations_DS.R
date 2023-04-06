library(ggplot2)
library(dplyr)
library(tidyr)
library(ggforce)
library(cowplot)
library(RColorBrewer)
library(plotly)
kullbackLeiblerDivergence <- function(progenitor, effector, terminal, cycling, other){
  ideal <- c(0.95, 0, 0, 0.05, 0)
  tested <- c(progenitor, effector, terminal, cycling, other)
  totalDist <- 0
  for (i in 1:5){
    if(tested[i] != 0 & ideal[i] != 0){
      totalDist = totalDist - tested[i]*log2(ideal[i])     
    }
  }
  print(1/totalDist^exp(-1))
}

# You need to load the file here, use your custom path
perturbations <- read.csv('Path', sep = ',')
perturbations %>%
  mutate(distance= purrr::pmap_dbl(list(progenitor, effector, terminal.exhausted, cycling, other), kullbackLeiblerDivergence)) ->
  perturbations

perturbations %>%
  gather(key = attribute, value = 'value', -condition, -distance) %>% 
  arrange(condition) ->
  perturbations_long

perturbations_long %>%
  subset(attribute %in% c("progenitor", "effector", "terminal.exhausted", "cycling", "other")) ->
  perturbations_subset

perturbations_frequency <- log10(perturbations[, 2:6] * perturbations[, 7] + 1) 
perturbations_frequency <- cbind(perturbations_frequency, perturbations[, c("condition", "cell.count", "distance")])
perturbations_frequency$memory <- perturbations$cycling


#_____________________________________________________________________________________________________________________
# BARCHARTS REPRESENTING DISTRIBUTION
#_____________________________________________________________________________________________________________________

perturbations_subset$attribute <- factor(perturbations_subset$attribute, levels = c("other", "cycling", "terminal.exhausted", "effector", "progenitor"))

ggplot(perturbations_subset, aes(fill = attribute, x = value, y = reorder(condition, distance))) +
  geom_bar(position = 'stack', stat = 'identity', width = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_brewer(type = 'qual', palette = 6) +
  geom_text(aes(label = ifelse(perturbations_subset[,4] == 'cycling' & perturbations_subset$memory < 0.05, "*", "")),
            size = 10,
            vjust = 0.75,
            hjust = 0.1) +
  ggtitle("Distribution of T cells type after change in specific gene") + 
  labs(fill = "States of T cells", 
       caption = "The graph represents the distribution of states
                  of T cells after perturbation of different genes.
                  Number of cells on which the given change was tested
                  was logarithmized. The results have been sorted
                  with accordance to Kullback-Leibler divergence
                  to the ideal distribution (0.95, 0, 0, 0.05, 0).
                  The more similar the result the higher it apears
                  on the graph
                  * - Given perturbation resulted in not sufficient amount
                  of cycling type T cells",
       y = expression(atop("Name of perturbed gene", "Sorted by the similarity to the ideal distribution")),
       x = "Logarithm of the number of cells") +
  theme(plot.title = element_text(hjust = 0.5))

#_____________________________________________________________________________________________________________________
# GRID OF RING CHARTS
#_____________________________________________________________________________________________________________________\



ggplot(perturbations_subset, aes(fill = attribute, x = value*distance, y = reorder(condition, distance))) +
  geom_bar(position = 'stack', stat = 'identity', width = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_brewer(type = 'qual', palette = 6)
  



plots <- list()
for (i in range(1:length(perturbations$condition))){
  plot <- ggplot(perturbations_subset[i, ], aes(x = '', y = value, fill = condition)) +
    geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = i/10, r = (i+1)/10), color = "white", size = 1)
  plot
  plots[[i]] <- plot
}

grid <- plot_grid(plotlist = plots, nrow = 10, ncol = 10)
grid













