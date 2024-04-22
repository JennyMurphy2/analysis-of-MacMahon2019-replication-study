library(rstatix)
library(afex)
library(car)
library(broom)
library(emmeans)
library(stringr)
library(lmerTest)
library(tidyverse)
library(MOTE)

# Import and prepare data 

data <- read_csv("replication_data.csv") 

# Prep data -----------

# Convert to long data set

long_data <- data %>%
  select(id, congruent_beep_time, noncongruent_beep_time) %>%
  pivot_longer(cols = c("congruent_beep_time", "noncongruent_beep_time"),
               names_to = "condition",
               values_to = "beep_time") %>%
  drop_na()

long_data$condition <- as.factor(long_data$condition)


# Convert minutes and seconds into seconds for the analysis


sep_data <- long_data %>%
  separate_wider_delim(beep_time, delim = ".", names = c("mins", "secs"),
                       too_few = "align_start",
                       too_many = "merge"
  )

sep_data$mins <- as.numeric(sep_data$mins)
sep_data$secs <- as.numeric(sep_data$secs)

seconds_data <- sep_data %>%
  mutate(
    across(secs, replace_na, 0)
  ) %>%
  mutate(beep_seconds = ((mins*60)+secs))

# Descriptives ------------

desc <- seconds_data %>% 
  group_by(condition) %>%
  summarize(count = n (),
            overall_mean = mean(beep_seconds,na.rm=TRUE),
            overall_sd = sd(beep_seconds, na.rm = TRUE))
desc

# Repeated Measures ANOVA -----

##afex::aov_4(continuous_var ~ group_var + (RM_var|id_var)

anova_results <- afex::aov_4(beep_seconds ~ (condition|id), 
                                   data = seconds_data,
                                   anova_table = list(es = "pes")) # partial eta squared
anova_results

## Post hoc ------------

data_emm <- anova_results %>% 
  emmeans::emmeans(~ condition, model = "multivariate")
data_emm

posthocresults <- pairs(data_emm, adjust = "Bon") %>% 
  broom::tidy(conf.int = T)
posthocresults

## Resolving assumptions --------

### Normality test 

seconds_data %>% 
  dplyr::group_by(condition) %>% 
  rstatix::shapiro_test(beep_seconds) # shapiro-wilk test on individual groups

norm <- performance::check_normality(anova_results)
plot(norm)
plot(norm, type = "qq")

### Outliers check

seconds_data %>%
  group_by(condition) %>%
  identify_outliers(beep_seconds)

## Plots

## violin

seconds_data %>% 
  ggplot(aes(condition, beep_seconds)) +  
  geom_violin(fill = "gray") +
  geom_boxplot(width = .07,
               fill = "white") +
  geom_jitter(position = position_jitter(0.21)) +
  stat_summary(fun = mean,
               geom = "point",
               shape = 18,
               color = "red",
               size = 5) +
  theme_bw()

## Individual qq plots 

seconds_data %>% 
  ggplot(aes(sample = beep_seconds)) +    
  geom_qq() +                               
  stat_qq_line() +                          
  facet_wrap(~ condition,                   # Panel by group
             labeller = label_both) +    
  theme_bw()

# Original values ------

orig_values <- data.frame(
  f_val = 21.76,
  df1 = 1,
  df2 = 11,
  eta = 0.67
) 

# Replication test -----

pes_rep = anova_results$anova_table$pes
df_rep = anova_results$anova_table$`den Df`
pes_ori = orig_values$eta
df_ori = orig_values$df2

rho_ori = 2*sqrt(pes_ori)-1
rho_rep = 2*sqrt(pes_rep)-1

rep_test = TOSTER::compare_cor(r1 = rho_ori,
                               df1 = df_ori,
                               r2 = rho_rep,
                               df2 = df_rep,
                               alternative = "greater")
rep_test

# Calculating CI for pes

# Replication
pes_rep_ci <- eta.F(
  dfm = anova_results$anova_table$`num Df`,
  dfe = anova_results$anova_table$`den Df`,
  Fvalue = anova_results$anova_table$F,
  a = 0.05) %>%
  as.data.frame() %>%
  select(eta, etalow, etahigh) %>%
  mutate(study_id = c("Replication study")) # add identifier
pes_rep_ci

# original 
pes_ori_ci <- eta.F(
  dfm = orig_values$df1,
  dfe = orig_values$df2,
  Fvalue = orig_values$f_val,
  a = 0.05) %>%
  as.data.frame() %>%
  select(eta, etalow, etahigh) %>%
  mutate(study_id = c("Original study")) # add identifier
pes_ori_ci

