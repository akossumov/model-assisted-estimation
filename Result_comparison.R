#result15 <- read.csv("/home/jiayin99/links/scratch/MATH525/logs/N_500_n_150_ptrue_30_pnoise_15.csv")

library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)
library(tidyr)
# folder path
path <- "~/Desktop/Sampling_Project/Math525_simu2/"   
files <- list.files(path, pattern = "N_500.*\\.csv$", full.names = TRUE)

data_all <- map_dfr(files, read.csv)

data_long <- data_all %>%
  pivot_longer(
    cols = starts_with("HT") | starts_with("ma"),
    names_to = "method",
    values_to = "estimate"
  ) %>% 
  filter(!is.na(estimate))

mse_df <- data_long %>%
  mutate(sq_error = (estimate - mu_true)^2) %>%
  group_by(p_noise, method) %>%
  summarise(
    mu_true_val = first(mu_true),
    mean_est = mean(estimate, na.rm = TRUE),
    bias = mean_est - mu_true_val,
    bias2 = bias^2,
    variance = var(estimate, na.rm = TRUE),
    mse = mean((estimate - mu_true_val)^2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    design = case_when(
      str_detect(method, "SRSWOR") ~ "SRSWOR",
      str_detect(method, "Bern") ~ "Bernoulli"
    ),
    method_label = case_when(
      method %in% c("HT_SRSWOR", "HT_Bern") ~ "HT",
      method %in% c("ma_SRSWOR_OLS", "ma_Bern_OLS") ~ "OLS",
      method %in% c("ma_SRSWOR_LASSO", "ma_Bern_LASSO") ~ "LASSO",
      method %in% c("ma_SRSWOR_NET", "ma_Bern_NET") ~ "Elastic Net",
      method %in% c("ma_SRSWOR_RF", "ma_Bern_RF") ~ "Random Forest",
      method %in% c("ma_SRSWOR_GBM", "ma_Bern_GBM") ~ "GBM",
      TRUE ~ method
    ),
    dimension_ratio = (p_noise + 30) / 150
  ) %>%
  filter(!(design == "Bernoulli" & method == "ma_Bern_OLS" & p_noise > 40))


mse_df_plot <- mse_df %>%
  mutate(
    method_label = case_when(
      method == "HT_SRSWOR" ~ "Horvitz–Thompson",
      method == "ma_SRSWOR_OLS" ~ "Linear Regression",
      method == "ma_SRSWOR_LASSO" ~ "LASSO",
      method == "ma_SRSWOR_NET" ~ "Elastic Net",
      method == "ma_SRSWOR_RF" ~ "Random Forest",
      method == "ma_SRSWOR_GBM" ~ "GBM",
      
      method == "HT_Bern" ~ "Horvitz–Thompson",
      method == "ma_Bern_OLS" ~ "Linear Regression",
      method == "ma_Bern_LASSO" ~ "LASSO",
      method == "ma_Bern_NET" ~ "Elastic Net",
      method == "ma_Bern_RF" ~ "Random Forest",
      method == "ma_Bern_GBM" ~ "GBM",
      
      TRUE ~ method
    )
  ) %>% filter(design !="Bernoulli" | method !="ma_Bern_OLS" |p_noise != 135) %>% 
  mutate(
    method_label = factor(
      method_label,
      levels = c("Horvitz–Thompson",
                 "Linear Regression",
                 "LASSO",
                 "Elastic Net",
                 "Random Forest",
                 "GBM")
    )
  )

#mse_df_plot %>%  filter(design =="Bernoulli" & method =="ma_Bern_OLS" )

ggplot(mse_df_plot , 
       aes(x = (p_noise)/0.3/150, y = mse, color = method_label)) +
  geom_line(size = 1) +
  geom_point() +
  facet_wrap(~design, scales = "free_y") +
  theme_bw() +
  labs(
    title = "",
    x = "Dimension (p / n)",
    y = "Mean Squared Error",
    color = "Estimator"
  ) + scale_color_brewer(palette = "Dark2") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )


p_bias2 <- ggplot(
  mse_df_plot,
  aes(x = (p_noise)/0.3/150, y = abs(bias2), color = method_label)
) +
  geom_line(linewidth = 1) +
  geom_point() +
  facet_wrap(~ design, scales = "free_y") +
  theme_bw() +
  labs(
    title = expression(""),
    x = "Dimension Ratio (p / n)",
    y = expression("Bias^2"),
    color = "Estimator"
  ) + scale_color_brewer(palette = "Dark2") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

p_bias2

p_var <- ggplot(
  mse_df_plot,
  aes(x = (p_noise)/0.3/150, y = variance, color = method_label)
) +
  geom_line(linewidth = 1) +
  geom_point() +
  facet_wrap(~ design, scales = "free_y") +
  theme_bw() +
  labs(
    title = "",
    x = "Dimension Ratio (p / n)",
    y = "Variance",
    color = "Estimator"
  ) + scale_color_brewer(palette = "Dark2") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

p_var


mse_wide <- mse_df %>%
  select(p_noise, design, method_label, mse) %>%
  mutate(model = paste(design, method_label, sep = "_")) %>%
  select(-design, -method_label) %>%
  pivot_wider(
    names_from = model,
    values_from = mse
  ) %>%
  arrange(p_noise)

mse_wide


###### AK: Histograms and boxplots

#plot_file <- list.files(path, pattern = "N_500_n_150_ptrue_210_pnoise_90.csv$", full.names = TRUE)
plot_file <- list.files(path, pattern = "N_500_n_150_ptrue_42_pnoise_18.csv$", full.names = TRUE)
plot_data <- map_dfr(plot_file, read.csv)

### Simple Random Sampling Without Replacement

cols_SRSWOR <- c("HT_SRSWOR", "ma_SRSWOR_OLS", "ma_SRSWOR_RF", "ma_SRSWOR_GBM", "ma_SRSWOR_LASSO", "ma_SRSWOR_NET")

mu_true <- unique(plot_data$mu_true)

library(tidyr)
library(dplyr)

plot_data_SRSWOR <- plot_data %>%
  select(all_of(cols_SRSWOR)) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "value")

library(ggplot2)

ggplot(plot_data_SRSWOR, aes(x = value)) +
  geom_histogram(bins = 30, fill = "#4C72B0", color = "white", alpha = 0.8) +
  geom_vline(xintercept = mu_true, color = "red", linetype = "dashed") +
  annotate("text",
           x = mu_true,
           y = 0.6,
           label = "μ",
           parse = TRUE,
           vjust = 1.5,
           hjust = -0.2,
           color = "red",
           size = 5) +
  facet_wrap(~variable, scales = "free") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Histograms of estimators under SRSWOR with p/n = 0.4",
    x = NULL,
    y = "Count"
  ) +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

### Bernoulli sampling

cols_Bern <- c("HT_Bern", "ma_Bern_OLS", "ma_Bern_RF", "ma_Bern_GBM", "ma_Bern_LASSO", "ma_Bern_NET")

plot_data_Bern <- plot_data %>%
  select(all_of(cols_Bern)) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "value")


ggplot(plot_data_Bern, aes(x = value)) +
  geom_histogram(bins = 30, fill = "#4C72B0", color = "white", alpha = 0.8) +
  geom_vline(xintercept = mu_true, color = "red", linetype = "dashed") +
  annotate("text",
           x = mu_true,
           y = 0.6,
           label = "μ",
           parse = TRUE,
           vjust = 1.5,
           hjust = -0.2,
           color = "red",
           size = 5) +
  facet_wrap(~variable, scales = "free") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Histograms of estimators under Bernoulli sampling with p/n = 0.4",
    x = NULL,
    y = "Count"
  ) +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )


### Boxplot for SRSWOR

ggplot(plot_data_SRSWOR, aes(x = variable, y = value)) +
  geom_boxplot(fill = "#4C72B0", alpha = 0.8, outlier.color = "red") +
  stat_summary(fun = mean,
               geom = "point",
               shape = 18,
               size = 3,
               color = "black") +
  geom_hline(yintercept = mu_true, color = "red", linetype = "dashed") +
  annotate("text",
           x = 0.6,
           y = mu_true,
           label = "μ",
           parse = TRUE,
           hjust = 1.2,
           vjust = -0.5,
           color = "red") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Boxplots of estimators under SRSWOR with p/n = 0.4",
    x = NULL,
    y = "Value"
  ) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

### Boxplot for Bernoulli

ggplot(plot_data_Bern, aes(x = variable, y = value)) +
  geom_boxplot(fill = "#4C72B0", alpha = 0.8, outlier.color = "red") +
  stat_summary(fun = mean,
               geom = "point",
               shape = 18,
               size = 3,
               color = "black") +
  geom_hline(yintercept = mu_true, color = "red", linetype = "dashed") +
  annotate("text",
           x = 0.6,
           y = mu_true,
           label = "μ",
           parse = TRUE,
           hjust = 1.2,
           vjust = -0.5,
           color = "red") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Boxplots of estimators under Bernoulli sampling with p/n = 0.4",
    x = NULL,
    y = "Value"
  ) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )


