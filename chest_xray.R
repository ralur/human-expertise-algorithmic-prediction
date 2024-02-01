wd <- ''
if (wd == '') {
  stop('must set working directory to the root of the repository')
}

setwd(wd)

library(tidyverse)
library(glue)
library(data.table)
library(cluster)
library(Cairo)
library(boot)
library(mltools)

source('./helpers.R')

# hyperparams

conf.level <- .95
bootstrap.replicates <- 1000
save.plots <- TRUE
pathologies <- c('Cardiomegaly', 'Edema', 'Consolidation', 'Atelectasis', 'Pleural Effusion')


# benchmark radiologists
r1 <- read_csv('./xray-data/radiologists/benchmark_radiologists/bc4.csv') %>% mutate(radiologist_id = 1)
r2 <- read_csv('./xray-data/radiologists/benchmark_radiologists/bc6.csv') %>% mutate(radiologist_id = 2)
r3 <- read_csv('./xray-data/radiologists/benchmark_radiologists/bc8.csv') %>% mutate(radiologist_id = 3)

# ground truth
y <- read_csv('./xray-data/groundtruth.csv')

# predictive models
model_files <- list.files('./xray-data/example_models/decisions/standard/')


# compute accuracy and distinguishability statistics for each pathology
pathology.fig.data <- lapply(pathologies, function(pathology) {
  set.seed(0)
  
  pathology.df <- data.frame(pathology = pathology, Study = y$Study)
  
  for (model_file in model_files) {
    model <- read_csv(glue('./xray-data/example_models/decisions/standard/{model_file}'))
    if (any(model$Study != pathology.df$Study)) {
      stop('Studies do not match')
    }
    model_str <- sub("_decisions.csv", "", model_file)
    
    pathology.df[, model_str] <- model[, pathology]
  }
  
  
  chebyshev_dist <- dist(pathology.df[, 3:ncol(pathology.df)], method = "maximum")
  result <- pam(chebyshev_dist, 2)
  pathology.df <- pathology.df %>% mutate(cluster = result$clustering - 1)
  
  if (any(r1$Study != pathology.df$Study) || any(r2$Study != pathology.df$Study) || any(r3$Study != pathology.df$Study) || any(y$Study != pathology.df$Study)) {
    stop('Studies do not match')
  }
  
  combined <- pathology.df %>% mutate(
    radiologist.1 = r1 %>% pull(!!pathology),
    radiologist.2 = r2 %>% pull(!!pathology),
    radiologist.3 = r3 %>% pull(!!pathology),
    y = y %>% pull(!!pathology)
  )
  
  model_names <- map_chr(model_files, function(m) sub("_decisions.csv", "", m))
  colnames <- c(model_names, c('radiologist.1', 'radiologist.2', 'radiologist.3'))
  
  accuracy <- combined %>%
    compute_stats.cols(colnames, bootstrap.replicates) %>%
    rename('name' = 'type', 'accuracy' = 'coef') %>%
    mutate(type = map_chr(name, function(x) if (str_detect(x, 'radiologist')) 'radiologist' else 'algorithmic')) %>%
    arrange(type, accuracy) %>%
    mutate(name = map_chr(name,
                          function(x) switch(x, 'radiologist.1' = 'radiologist 1',
                                             'radiologist.2' = 'radiologist 2',
                                             'radiologist.3' = 'radiologist 3',
                                             x)))
  
  
  
  levels.pred = accuracy$name %>% unique
  human.levels.pred <- levels.pred[str_detect(levels.pred, 'radiologist')]
  algo.levels.pred <- levels.pred[!str_detect(levels.pred, 'radiologist')]
  levels.pred <- c(sort(algo.levels.pred), sort(human.levels.pred))
  
  accuracy$predictor <- factor(accuracy$name, levels = levels.pred)
  accuracy$type <- factor(accuracy$type, levels = c('algorithmic', 'radiologist'))
  
  distinguishability <- combined %>%
    mutate(random.baseline = sample(y)) %>%
    group_by(cluster) %>%
    nest() %>%
    mutate(stats = map(data, function(d) compute_stats.cols(d, c(colnames, 'random.baseline'), bootstrap.replicates)), n = map_dbl(data, nrow)) %>%
    unnest(stats) %>%
    select(coef, ci.lower, ci.upper, n, type, cluster) %>%
    mutate(type = map_chr(type,
                          function(x) switch(x, 'radiologist.1' = 'radiologist 1',
                                             'radiologist.2' = 'radiologist 2',
                                             'radiologist.3' = 'radiologist 3',
                                             'random.baseline' = 'random baseline',
                                             x)))
  
  
  cluster_sums <- combined %>%
    select(-c(pathology, Study, radiologist.1, radiologist.2, radiologist.3, y)) %>%
    group_by(cluster) %>%
    summarize_all(sum) %>%
    arrange(cluster) %>%
    select(-cluster)
  
  
  cluster_max_sd <- combined %>%
    select(-c(pathology, Study, y, contains('radiologist'))) %>%
    group_by(cluster) %>% summarize_all(var) %>% select(-cluster) %>% apply(1, function(x) sqrt(max(x)))
  
  cluster_mean <- combined %>%
    select(-c(pathology, Study, y, contains('radiologist'))) %>%
    group_by(cluster) %>% summarize_all(mean) %>% select(-cluster) %>% apply(1, mean)
  
  cluster_size <- combined %>% group_by(cluster) %>% summarize(n = n()) %>% pull(n)
  cluster_truth <- combined %>% group_by(cluster) %>% summarize(m = mean(y)) %>% pull(m)
  
  cluster_names <- pmap_chr(list(cluster_max_sd, cluster_mean, cluster_size, cluster_truth), function(s, m, n, t) {
    glue('\u03B1={round(s, 2)}, \u03BC={round(m, 2)}, n={n}')
    
  })
  
  
  levels = distinguishability$type %>% unique
  human.levels <- levels[str_detect(levels, 'radiologist')]
  algo.levels <- levels[!(str_detect(levels, 'radiologist') | str_detect(levels, 'random'))]
  levels <- c('random baseline', sort(algo.levels), sort(human.levels))
  
  distinguishability <- distinguishability %>% arrange(desc(cluster), coef)
  distinguishability$predictor <- factor(distinguishability$type, levels = levels)
  distinguishability$cluster <- map_chr(distinguishability$cluster, function(x) cluster_names[x+1])
  distinguishability$cluster <- factor(distinguishability$cluster)
  

  
  
  list(accuracy = accuracy, distinguishability = distinguishability)
})

# produces the accuracy figures (e.g., figure 1)
create_accuracy_figure <- function(data) {
  data %>%
    ggplot(aes(x = type, group = predictor)) +
    geom_boxplot(stat = "identity", color = "#00000030",
                 aes(ymin = ci.lower, lower = ci.lower, fill = predictor,
                     group = predictor,
                     ymax = ci.upper, upper = ci.upper, middle = accuracy),
                 width = 0.9) +
    geom_point(position = position_dodge(width = 0.9), 
               aes(fill = predictor, y = accuracy), shape = 21, size = 4) +
    theme_minimal(base_size = 20) +
    xlab('Prediction Type') +
    ylab('Correlation Coefficient') +
    theme(axis.text.x = element_text(size = 30),
          axis.text.y = element_text(size = 30),
          legend.text = element_text(size = 30),
          legend.key.size = unit(1.2, "cm"),
          axis.title.x = element_text(size = 40, face = "bold"),
          legend.title = element_text(size = 35),
          axis.title.y = element_text(size = 40, face = "bold"))
}

# produces the distinguishability figures (e.g., figure 2)
create_distinguishability_figure <- function(data) {
  data %>%
    ggplot(aes(cluster, group = predictor)) +
    geom_boxplot(stat = "identity", color = "#00000030",
                 aes(ymin = ci.lower, lower = ci.lower, fill = predictor,
                     group = interaction(cluster, predictor),
                     ymax = ci.upper, upper = ci.upper, middle = coef),
                 width = .9) +
    geom_point(position = position_dodge(width = .9), 
               aes(fill = predictor, y = coef), shape = 21, size = 4) +
    theme_minimal(base_size = 20) +
    ylab('Correlation Coefficient') +
    xlab('Model Prediction Bins') +
    theme(axis.text.x = element_text(size = 30),
          axis.text.y = element_text(size = 30),
          legend.text = element_text(size = 30),
          legend.key.size = unit(1.2, "cm"),
          axis.title.x = element_text(size = 40, face = "bold", vjust = -1),
          legend.title = element_text(size = 35),
          axis.title.y = element_text(size = 40, face = "bold"))
  
}


# create and save the figures
for (i in 1:5) {
  print(glue('Creating figures for {pathologies[i]}'))
  fig.4 <- create_accuracy_figure(pathology.fig.data[[i]]$accuracy)
  fig.5 <- create_distinguishability_figure(pathology.fig.data[[i]]$distinguishability)
  
  if (save.plots) {
    ggsave(glue('{wd}/plots/chexpert_{pathologies[i]}_accuracy.pdf'), fig.4, width = 20, height = 8, device = cairo_pdf)
    ggsave(glue('{wd}/plots/chexpert_{pathologies[i]}_distinguishability.pdf'), fig.5, width = 20, height = 8, device = cairo_pdf)  
  }
  
}

