library(tidyverse)

output_path <- "/Volumes/fsolt/swiid_kfold/output/by_fold"
output_files <- list.files(output_path)

kfold_output <- map_dfr(output_files, function(output_file) {
  rio::import(file.path(output_path, output_file)) %>% 
    mutate(fold = str_extract(output_file, "\\d{1,3}$"),
           cy = paste(country, year),
           cy_color = if_else(problem == 1, "#354995", "#5E5E5E"),
           point_diff = mean - gini_b %>% round(5))
}) %>% 
  group_by(country) %>% 
  mutate(prob_perc = mean(problem, na.rm = TRUE),
         t_diff = point_diff/se_diff) %>% 
  ungroup() %>% 
  arrange(point_diff) %>% 
  add_count(country)

kfold_output %>% 
  group_by(country) %>%
  select(country, year, fold, problem, prob_perc, 
         gini_b, gini_b_se, mean, sd,
         point_diff, se_diff, t_diff) %>%
  arrange(-problem, -prob_perc, country, -t_diff) %>%
  View()

1 - mean(kfold_output %>% filter(n > 1) %>% pull(problem), na.rm = TRUE)
mean(abs(kfold_output %>% filter(n > 1) %>% pull(point_diff)) < .01, na.rm = TRUE)
mean(abs(kfold_output %>% filter(n > 1) %>% pull(point_diff)) < .02, na.rm = TRUE)

ggplot(kfold_output %>%
         filter(n > 1)) +
  geom_hline(yintercept=0, linetype=2, colour="gray60") +
  geom_pointrange(fatten = 1,
                  aes(x = forcats::fct_reorder(cy, point_diff), 
                      y=point_diff*100, 
                      ymin=point_diff*100 - 1.96*100*se_diff,
                      ymax = point_diff*100 + 1.96*100*se_diff,
                      colour = cy_color,
                      alpha = problem)) +
  theme_bw() + 
  theme(legend.position="none") +
  scale_colour_manual(values=c("#354995", "#5E5E5E")) +
  scale_alpha_discrete(range = c(0.4, 1)) +
  labs(x = "", y = latex2exp::TeX("SWIID By-Observation \\textit{k}-fold Prediction minus LIS")) + 
  theme(panel.grid.minor=element_blank()) +
  scale_y_continuous(breaks=c(-10, -5, -2, -1,  0, 1, 2, 5, 10)) +
  scale_x_discrete(breaks = NULL) 

ggsave(file = "paper/figures/kfold_obs.pdf", width=8.5, height=5.25)
save(kfold_output, file = "data/kfold_output.rda")


# by country
output_path <- "/Volumes/fsolt/swiid_kfold/output/by_country"
output_files <- list.files(output_path)

kfold_output_by_country <- map_dfr(output_files, function(output_file) {
  rio::import(file.path(output_path, output_file)) %>% 
    mutate(fold = str_extract(output_file, "\\d{1,3}$"),
           cy = paste(country, year),
           cy_color = if_else(problem == 1, "#354995", "#5E5E5E"),
           point_diff = mean - gini_b %>% round(5))
}) %>% 
  group_by(country) %>% 
  mutate(prob_perc = mean(problem, na.rm = TRUE),
         t_diff = point_diff/se_diff) %>% 
  ungroup() %>% 
  arrange(point_diff) %>% 
  add_count(country)

kfold_output_by_country %>% 
  group_by(country) %>%
  select(country, year, fold, problem, prob_perc, 
         gini_b, gini_b_se, mean, sd,
         point_diff, se_diff, t_diff) %>%
  arrange(-problem, -prob_perc, country, -t_diff) %>%
  View()

1 - mean(kfold_output_by_country %>% pull(problem), na.rm = TRUE)
mean(abs(kfold_output_by_country %>% pull(point_diff)) < .01, na.rm = TRUE)
mean(abs(kfold_output_by_country %>% pull(point_diff)) < .02, na.rm = TRUE)

ggplot(kfold_output_by_country) +
  geom_hline(yintercept=0, linetype=2, colour="gray60") +
  geom_pointrange(fatten = 1,
                  aes(x = forcats::fct_reorder(cy, point_diff), 
                      y=point_diff*100, 
                      ymin=point_diff*100 - 1.96*100*se_diff,
                      ymax = point_diff*100 + 1.96*100*se_diff,
                      colour = cy_color,
                      alpha = problem)) +
  theme_bw() + 
  theme(legend.position="none") +
  scale_colour_manual(values=c("#354995", "#5E5E5E")) +
  scale_alpha_discrete(range = c(0.4, 1)) +
  labs(x = "", y = latex2exp::TeX("SWIID By-Country \\textit{k}-fold Prediction minus LIS")) + 
  theme(panel.grid.minor=element_blank()) +
  scale_y_continuous(breaks=c(-20, -15, -10, -5, -2, -1,  0, 1, 2, 5, 10)) +
  scale_x_discrete(breaks = NULL) 

ggsave(file = "paper/figures/kfold_country.pdf", width=8.5, height=5.25)
save(kfold_output_by_country, file = "data/kfold_output_by_country.rda")
