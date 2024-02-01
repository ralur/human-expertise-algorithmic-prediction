wd <- ''
if (wd == '') {
  stop('must set working directory to the root of the repository')
}

setwd(wd)

# libraries
library(tidyverse)
library(cluster)
library(boot)
library(glue)
library(mltools)
library(data.table)

source('./helpers.R')

# hyperparams
set.seed(0)
bootstrap.replicates <- 1000

# load data
h <- read_csv('./visual-prediction-data/study_2/responses.csv')
m <- read_csv('./visual-prediction-data/study_2/machine_predictions_[random_forest].csv')

input.data <- h %>%
  select(-TurkerID) %>% pivot_wider(names_from = Condition, values_from = Esc) %>%
  drop_na %>%
  select(file_id = Img, human.pred.c = control, human.pred.t4 = training4, human.pred.t8 = training8, human.pred.t12 = training12) %>%
  left_join(m %>% select(-pred_esc), by = 'file_id') %>%
  rename('y' = 'true_esc') %>%
  mutate(across(-file_id, function(x) as.numeric(x == 'Y')))
  

model_files <- list.files('./visual-prediction-data/study_2/', pattern = "machine_predictions*", full.names = TRUE)

model_dfs <- lapply(model_files, process_file.etr)
models <- reduce(model_dfs, left_join, by = "file_id") %>% as_tibble

models.data <- models[,2:ncol(models)] %>% mutate(across(everything(), function(x) map_dbl(x, function(y) if (y == 'Y') 1 else 0)))
models[2:ncol(models)] <- models.data

# combine data into tidy df
tidy <- models %>% inner_join(input.data, by = 'file_id') %>% select(-dummy_most_freq)
n.models <- length(model_dfs)

# compute clusters
tidy_models <- tidy %>% select(c('gradient_boosting', 'linear_svm', 'logistic_regression', 'naive bayes', 'random_forest'))
chebyshev_dist <- dist(tidy_models, method = "maximum")
clusters <- pam(chebyshev_dist, 2)
clustered <- tidy_models %>% mutate(cluster = clusters$clustering)

cluster_max_sd <- clustered %>%
  group_by(cluster) %>% summarize_all(var) %>% select(-cluster) %>% apply(1, function(x) sqrt(max(x)))

cluster_mean <- clustered %>%
  group_by(cluster) %>% summarize_all(mean) %>% select(-cluster) %>% apply(1, mean)

cluster_size <- clustered %>% group_by(cluster) %>% summarize(n = n()) %>% pull(n)

cluster_names <- pmap_chr(list(cluster_max_sd, cluster_mean, cluster_size), function(s, m, n) {
  glue('\u03B1={round(s, 2)}, \u03BC={round(m, 2)}, n={n}')
  
})

clustered <- tidy %>% mutate(cluster = clustered$cluster)

# compute accuracy statistics for each model
cluster_stats <- clustered %>%
  group_by(cluster) %>%
  nest() %>%
  mutate(stats = map(data, function(d) compute_stats.etr(d, bootstrap.replicates)), n = map_dbl(data, nrow)) %>%
  unnest(stats)


# Select and rename for each model type
out.gradient_boosting <- cluster_stats %>% 
  select(cluster, gradient_boosting_coef, gradient_boosting_ci_lower, gradient_boosting_ci_upper, n) %>% 
  mutate(type = 'gradient_boosting', bin = cluster)

out.linear_svm <- cluster_stats %>% 
  select(cluster, linear_svm_coef, linear_svm_ci_lower, linear_svm_ci_upper, n) %>% 
  mutate(type = 'linear_svm', bin = cluster)

out.random_forest <- cluster_stats %>% 
  select(cluster, random_forest_coef, random_forest_ci_lower, random_forest_ci_upper, n) %>% 
  mutate(type = 'random_forest', bin = cluster)

out.logistic_regression <- cluster_stats %>% 
  select(cluster, logistic_regression_coef, logistic_regression_ci_lower, logistic_regression_ci_upper, n) %>% 
  mutate(type = 'logistic_regression', bin = cluster)

out.naive_bayes <- cluster_stats %>% 
  select(cluster, naive_bayes_coef, naive_bayes_ci_lower, naive_bayes_ci_upper, n) %>% 
  mutate(type = 'naive_bayes', bin = cluster)

out.human.c <- cluster_stats %>% 
  select(cluster, human_pred.c_coef, human_pred.c_ci_lower, human_pred.c_ci_upper, n) %>% 
  mutate(type = 'human (control)', bin = cluster)

out.human.4 <- cluster_stats %>% 
  select(cluster, human_pred.4_coef, human_pred.4_ci_lower, human_pred.4_ci_upper, n) %>% 
  mutate(type = 'human (4 examples)', bin = cluster)

out.human.8 <- cluster_stats %>% 
  select(cluster, human_pred.8_coef, human_pred.8_ci_lower, human_pred.8_ci_upper, n) %>% 
  mutate(type = 'human (8 examples)', bin = cluster)

out.human.12 <- cluster_stats %>% 
  select(cluster, human_pred.12_coef, human_pred.12_ci_lower, human_pred.12_ci_upper, n) %>% 
  mutate(type = 'human (12 examples)', bin = cluster)

# Renaming columns with corrected order for 'bin' and 'type'
colnames(out.gradient_boosting) <- c('group', 'coef', 'ci.lower', 'ci.upper', 'n', 'type', 'bin')
colnames(out.linear_svm) <- c('group', 'coef', 'ci.lower', 'ci.upper', 'n', 'type', 'bin')
colnames(out.random_forest) <- c('group', 'coef', 'ci.lower', 'ci.upper', 'n', 'type', 'bin')
colnames(out.logistic_regression) <- c('group', 'coef', 'ci.lower', 'ci.upper', 'n', 'type', 'bin')
colnames(out.naive_bayes) <- c('group', 'coef', 'ci.lower', 'ci.upper', 'n', 'type', 'bin')
colnames(out.human.c) <- c('group', 'coef', 'ci.lower', 'ci.upper', 'n', 'type', 'bin')
colnames(out.human.4) <- c('group', 'coef', 'ci.lower', 'ci.upper', 'n', 'type', 'bin')
colnames(out.human.8) <- c('group', 'coef', 'ci.lower', 'ci.upper', 'n', 'type', 'bin')
colnames(out.human.12) <- c('group', 'coef', 'ci.lower', 'ci.upper', 'n', 'type', 'bin')

# Combine all into one tibble
combined_data <- bind_rows(out.gradient_boosting, out.linear_svm, out.random_forest, 
                           out.logistic_regression, out.naive_bayes, out.human.c, out.human.4, out.human.8, out.human.12) %>%
  mutate(group = as.factor(group))

mc_clusters <- read_csv('./mc_predictions.csv')
mc_clusters <- mc_clusters %>%
  left_join(clustered, by = 'file_id') %>%
  select(file_id, mc_pred, contains('human.pred'), y, cluster = bin) %>%
  drop_na

# add baseline permutation of labels
mc_clusters$human.pred.baseline <- sample(mc_clusters$y)

# compute stats within level sets of multicalibrated predictor
mc_stats <- mc_clusters %>%
  group_by(cluster) %>%
  nest() %>%
  mutate(stats = map(data, function(d) compute_stats.etr.mc(d, bootstrap.replicates)), n = map_dbl(data, nrow)) %>%
  unnest(stats)

# wrangle data into form that can be easily visualized
out.mc <- mc_stats %>% 
  select(cluster, mc_coef, mc_ci_lower, mc_ci_upper, n) %>% 
  mutate(type = 'multicalibrated predictor', bin = cluster)

out.human.c.mc <- mc_stats %>% 
  select(cluster, human_pred.c_coef, human_pred.c_ci_lower, human_pred.c_ci_upper, n) %>% 
  mutate(type = 'human (control)', bin = cluster)

out.human.4.mc <- mc_stats %>% 
  select(cluster, human_pred.4_coef, human_pred.4_ci_lower, human_pred.4_ci_upper, n) %>% 
  mutate(type = 'human (4 examples)', bin = cluster)

out.human.8.mc <- mc_stats %>% 
  select(cluster, human_pred.8_coef, human_pred.8_ci_lower, human_pred.8_ci_upper, n) %>% 
  mutate(type = 'human (8 examples)', bin = cluster)

out.human.12.mc <- mc_stats %>% 
  select(cluster, human_pred.12_coef, human_pred.12_ci_lower, human_pred.12_ci_upper, n) %>% 
  mutate(type = 'human (12 examples)', bin = cluster)

out.human.baseline.mc <- mc_stats %>% 
  select(cluster, human_pred.baseline_coef, human_pred.baseline_ci_lower, human_pred.baseline_ci_upper, n) %>% 
  mutate(type = 'random baseline', bin = cluster)

colnames(out.mc) <- c('group', 'coef', 'ci.lower', 'ci.upper', 'n', 'type', 'bin')
colnames(out.human.c.mc) <- c('group', 'coef', 'ci.lower', 'ci.upper', 'n', 'type', 'bin')
colnames(out.human.4.mc) <- c('group', 'coef', 'ci.lower', 'ci.upper', 'n', 'type', 'bin')
colnames(out.human.8.mc) <- c('group', 'coef', 'ci.lower', 'ci.upper', 'n', 'type', 'bin')
colnames(out.human.12.mc) <- c('group', 'coef', 'ci.lower', 'ci.upper', 'n', 'type', 'bin')
colnames(out.human.baseline.mc) <- c('group', 'coef', 'ci.lower', 'ci.upper', 'n', 'type', 'bin')

combined_data.mc <- bind_rows(out.mc, out.human.c.mc, out.human.4.mc, out.human.8.mc, out.human.12.mc, out.human.baseline.mc) %>%
  mutate(bin = as.factor(bin), group = as.factor(group))

## Figure 1 ##

colnames <- tidy %>% select(-c(file_id, y)) %>% colnames
accuracy <- tidy %>%
  compute_stats.cols(colnames, bootstrap.replicates) %>%
  rename('name' = 'type', 'accuracy' = 'coef') %>%
  mutate(type = map_chr(name, function(x) if (str_detect(x, 'human')) 'human subjects' else 'algorithmic')) %>%
    arrange(type, accuracy) %>%
    mutate(name = map_chr(name,
                          function(x) switch(x, 'human.pred.c' = 'human (control)',
                                 'human.pred.t4' = 'human (4 examples)',
                                 'human.pred.t8' = 'human (8 examples)', 'human.pred.t12' = 'human (12 examples)',
                                 #'dummy_most_freq' = 'dummy (most frequent)',
                                 'gradient_boosting' = 'gradient boosting',
                                 'random_forest' = 'random forest',
                                 'logistic_regression' = 'logistic regression',
                                 'linear_svm' = 'linear svm',
                                 x)))

accuracy$predictor <- factor(accuracy$name, levels = unique(accuracy$name))

fig.1 <- accuracy %>%
  filter(name != 'dummy (most frequent)') %>%
  ggplot(aes(x = reorder(type, accuracy, FUN = mean), group = predictor)) +
  geom_boxplot(stat = "identity", color = "#00000030",
               aes(ymin = ci.lower, lower = ci.lower, fill = predictor,
                   group = predictor,
                   ymax = ci.upper, upper = ci.upper, middle = accuracy),
               width = 0.8) +
  geom_point(position = position_dodge(width = 0.8), 
             aes(fill = predictor, y = accuracy), shape = 21, size = 4) +
  theme_minimal(base_size = 20) +
  xlab('Prediction Type') +
  ylab('Correlation Coefficient') +
  theme(axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        legend.text = element_text(size = 30),
        legend.key.size = unit(1.2, "cm"),
        axis.title.x = element_text(size = 40, face = "bold", vjust = -1),
        legend.title = element_text(size = 35),
        axis.title.y = element_text(size = 40, face = "bold"))


ggsave('./plots/etr_accuracy.pdf', fig.1, width = 20, height = 8, device = cairo_pdf)


## Figure 2 ##

levels = combined_data$type %>% unique
algo.levels <- levels[!(str_detect(levels, 'human'))]
levels <- c(sort(algo.levels), c('human (control)', 'human (4 examples)', 'human (8 examples)', 'human (12 examples)'))

combined_data <- combined_data %>% arrange(desc(group), coef)
combined_data$predictor <- factor(combined_data$type, levels = levels)
combined_data$bin <- map_chr(combined_data$bin, function(x) cluster_names[x])
combined_data$bin <- factor(combined_data$bin)


fig.2 <- combined_data %>%
  ggplot(aes(bin, group = predictor)) +
  geom_boxplot(stat = "identity", color = "#00000030",
               aes(ymin = ci.lower, lower = ci.lower, fill = predictor,
                   group = interaction(bin, predictor),
                   ymax = ci.upper, upper = ci.upper, middle = coef),
               width = 0.8) +
  geom_point(position = position_dodge(width = 0.8), 
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


ggsave('./plots/etr_distinguishability.pdf', fig.2, width = 20, height = 8, device = cairo_pdf)
## Figure 3 ##

levels.mc = c('random baseline', 'multicalibrated predictor', 'human (control)', 'human (4 examples)', 'human (8 examples)', 'human (12 examples)')

combined_data.mc <- combined_data.mc %>% arrange(desc(group), coef)
combined_data.mc$predictor <- factor(combined_data.mc$type, levels = levels.mc)
combined_data.mc$bin <- map_chr(combined_data.mc$bin, function(x) if (x == 0) 'Positive' else 'Negative')
combined_data.mc$bin <- factor(combined_data.mc$bin)


fig.3 <- combined_data.mc %>%
  ggplot(aes(bin, group = predictor)) +
  geom_boxplot(stat = "identity", color = "#00000030",
               aes(ymin = ci.lower, lower = ci.lower, fill = predictor,
                   group = interaction(bin, predictor),
                   ymax = ci.upper, upper = ci.upper, middle = coef),
               width = 0.8) +
  geom_point(position = position_dodge(width = 0.8), 
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

ggsave('./plots/mc_distinguishability.pdf', fig.3, width = 20, height = 8, device = cairo_pdf)


