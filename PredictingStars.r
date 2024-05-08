# load libraries
library(tidyverse)
library(broom)
library(dplyr)
library(cowplot)
library(gridExtra)
library(grid)
library(GGally)
library(car)
library(mltools)
library(leaps) 

# read data from github repo
github_url <- "https://raw.githubusercontent.com/alim0118/stat-301-project/main/repositories.csv"

repo_data <- read.csv(github_url, header=TRUE)
head(repo_data)
nrow(repo_data) # dataset size before wrangling: 215029

# find number of unique values in each feature
sapply(repo_data, n_distinct)

missing_counts <- repo_data |> 
    summarize_all(~ sum(str_trim(.) %in% c("", '[]')))

missing_prop <- missing_counts / nrow(repo_data)

# show only features with missing proportions > 10%
missing_prop |> select_if(~. > 0.1)

repo_data <- repo_data |>
    select(-Name, -Description, -URL, -Created.At, -Updated.At, -Homepage, -Language, -License, -Topics, -Default.Branch)
head(repo_data)

set.seed(2024)

repo_sample <- sample_n(repo_data, 3000, replace = FALSE)
head(repo_sample)
nrow(repo_sample)

repo_sample <- repo_sample |> filter_all(all_vars(!(str_trim(.) %in% c("", NA))))
nrow(repo_sample)

summary(repo_sample$Stars)

options(repr.plot.width = 10, repr.plot.height =8)

# distribution of number of stars in log
num_stars_hist <- repo_sample |>
    ggplot(aes(x = log(Stars))) +
    geom_histogram(bins=25) +
    labs(x = "Number of Log Stars", title = "G1 - Distribution of Log Stars for GitHub Repositories") +
    theme(
        text = element_text(size = 15),
        plot.title = element_text(face = "bold")
    )
num_stars_hist

p1 <- repo_sample |>
    ggplot(aes(x=Has.Issues, y=log(Stars))) +
    geom_boxplot() +
    labs(x="Has Issues", y = "Number of Log Stars") +
    theme(text = element_text(size = 15))
p2 <- repo_sample |>
    ggplot(aes(x=Has.Projects, y=log(Stars))) +
    geom_boxplot() +
    labs(x="Has Projects", y = "Number of Log Stars") +
    theme(text = element_text(size = 15))
p3 <- repo_sample |>
    ggplot(aes(x=Has.Downloads, y=log(Stars))) +
    geom_boxplot() +
    labs(x="Has Downloads", y = "Number of Log Stars") +
    theme(text = element_text(size = 15))
p4 <- repo_sample |>
    ggplot(aes(x=Has.Wiki, y=log(Stars))) +
    geom_boxplot() +
    labs(x="Has Wiki", y = "Number of Log Stars") +
    theme(text = element_text(size = 15))
p5 <- repo_sample |>
    ggplot(aes(x=Has.Pages, y=log(Stars))) +
    geom_boxplot() +
    labs(x="Has Pages", y = "Number of Log Stars") +
    theme(text = element_text(size = 15))
p6 <- repo_sample |>
    ggplot(aes(x=Has.Discussions, y=log(Stars))) +
    geom_boxplot() +
    labs(x="Has Discussions", y = "Number of Log Stars") +
    theme(text = element_text(size = 15))
p7 <- repo_sample |>
    ggplot(aes(x=Is.Fork, y=log(Stars))) +
    geom_boxplot() +
    labs(x="Is Fork", y = "Number of Log Stars") +
    theme(text = element_text(size = 15))
p8 <- repo_sample |>
    ggplot(aes(x=Is.Archived, y=log(Stars))) +
    geom_boxplot() +
    labs(x="Is Archived", y = "Number of Log Stars") +
    theme(text = element_text(size = 15))
p9 <- repo_sample |>
    ggplot(aes(x=Is.Template, y=log(Stars))) +
    geom_boxplot() +
    labs(x="Is Template", y = "Number of Log Stars") +
    theme(text = element_text(size = 15))

eda_boxplots <- grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, nrow=3, ncol=3,
                            top = textGrob("G2 - Number of Log Stars to each Categorical Variable", gp=gpar(fontsize=20, font=2)))
eda_boxplots

repo_sample <- repo_sample |> select(-Is.Fork)

repo_pair_plots <- repo_sample |>
    ggpairs(columns = 1:5) +
    labs(title = "G3 - Pair Plot of Continuous features to Response variable") +
    theme(
        text = element_text(size = 15),
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),  
        axis.text.y = element_text(angle = 0, hjust = 1)    
    )
repo_pair_plots

corr_matrix <- repo_sample %>%
   select(Size, Forks, Issues, Watchers) %>% 
   cor() %>%
   as.data.frame() %>%
   rownames_to_column("var1") %>%
   pivot_longer(-var1, names_to = "var2", values_to = "corr")

plot_corr_matrix <- corr_matrix %>%
   ggplot(aes(var1, var2)) +
   geom_tile(aes(fill = corr), color = "white") +
   scale_fill_distiller("Correlation Coefficient \n",
     palette =  "YlOrRd",
     direction = 1, limits = c(-1,1)
   ) +
   labs(x = "", y = "", title="G4 - Heat Map between Input Variables") +
   theme_minimal() +
   theme(
       text = element_text(size = 15),
       plot.title = element_text(face = "bold"),
       axis.text.x = element_text(
       angle = 45, vjust = 1,
       size = 18, hjust = 1
     ),
     axis.text.y = element_text(
       vjust = 1,
       size = 18, hjust = 1
     ),
     legend.title = element_text(size = 18, face = "bold"),
     legend.text = element_text(size = 18),
     legend.key.size = unit(2, "cm")
   ) +
   coord_fixed() +
   geom_text(aes(var1, var2, label = round(corr, 2)), color = "black", size = 6)
plot_corr_matrix

# change categorical variables to factor
repo_sample[6:13] <- lapply(repo_sample[6:13], as.factor)

# check variables have been converted
sapply(repo_sample, is.factor)

# assess multicollinearity

# fit full model
full_mod <- lm(Stars~., data=repo_sample)

full_mod_results <- tidy(full_mod) %>% mutate_if(is.numeric, round, 2)
full_mod_results

# calculate VIF
VIF_full_mod <- vif(full_mod)
round(VIF_full_mod, 3)

red_mod <- repo_sample %>% select(-Watchers) %>% lm(Stars~., data = .)

red_mod_results <- tidy(red_mod) %>% mutate_if(is.numeric, round, 2)
red_mod_results

# recheck VIF for reduced model
VIF_red_mod <- vif(red_mod)
round(VIF_red_mod, 3)

# remove variable with largest VIF to solve multicollinearity issue
repo_sample <- repo_sample %>% select(-Watchers)

# remove outliers 

# create function that finds outliers using IQR method
find_outliers <- function(x) {
    q1 <- quantile(x, prob=0.25)
    q3 <- quantile(x, prob=0.75)
    IQR = q3-q1
    outliers <- x < (q1 - (1.5*IQR)) | x > (q3 + (1.5*IQR))
    return(outliers)
}

# create function that removes outliers 
remove_outliers <- function(data, num_cols) {
    for(c in num_cols) {
        outliers <- find_outliers(data[[c]])
        data <- data[!outliers, ]
        
    }
    print("Outliers removed")
    return(data)
}

repo_sample <- remove_outliers(repo_sample, names(repo_sample)[sapply(repo_sample, is.numeric)]) 
nrow(repo_sample)

set.seed(2024)

repo_sample$ID <- rownames(repo_sample)
training_set <- sample_n(repo_sample, size = nrow(repo_sample) * 0.70,
  replace = FALSE
)

test_set <- anti_join(repo_sample,
  training_set,
  by = "ID"
)

training_set <- training_set  %>% select(-"ID")
test_set <- test_set %>% select(-"ID")

head(training_set)
nrow(training_set) # 1322

head(test_set)
nrow(test_set) # 567

# build full model with all 11 inputs against the response variable
full_OLS_mod <- lm(Stars~., data=training_set)
summary(full_OLS_mod)

# predict on test sset
pred_full_OLS <- predict(full_OLS_mod, newdata=test_set)
head(pred_full_OLS)

options(repr.plot.width = 7, repr.plot.height =5)

# residual plot of full model
plot(fitted(full_OLS_mod), resid(full_OLS_mod),
     xlab = "Fitted Values", ylab = "Residual",
     main = "Fitted vs Residual for Full Model")
abline(0,0)

qqnorm(resid(full_OLS_mod)) 
qqline(resid(full_OLS_mod))

# forward selection
forward_sel <- regsubsets(
  x=Stars~., data = training_set,
  nvmax = 11,
  method = "forward"
)
forward_sel

forward_sel_summary <- summary(forward_sel)

forward_sel_summary <- tibble(
   n_input_variables = 1:11,
   Cp = forward_sel_summary$cp
)
forward_sel_summary

# select variables
cp_min_forward = which.min(forward_sel_summary$Cp) 
coef_names_fw <- gsub("(True)$", "", names(coef(forward_sel, cp_min_forward)))
selected_var_forward <- coef_names_fw[-1]
selected_var_forward

training_subset_forward <- training_set %>% select(all_of(selected_var_forward), Stars)

summary(forward_sel)

# backward selection
backward_sel <- regsubsets(
  x=Stars~., data = training_set,
  nvmax = 11,
  method = "backward"
)
backward_sel

backward_sel_summary <- summary(backward_sel)

backward_sel_summary <- tibble(
   n_input_variables = 1:11,
   Cp = backward_sel_summary$cp
)
backward_sel_summary

# select variables
cp_min_backward = which.min(backward_sel_summary$Cp) 
coef_names_bw <- gsub("(True)$", "", names(coef(backward_sel, cp_min_backward)))
selected_var_backward <- coef_names_bw[-1]
selected_var_backward

training_subset_backward <- training_set %>% select(all_of(selected_var_backward), Stars)

summary(backward_sel)

# use variables selected by forward selection to build predictive model
red_forward_OLS <- lm(Stars ~ .,
  training_subset_forward
)

summary(red_forward_OLS)

# use variables selected by backward selection to build predictive model
red_backward_OLS <- lm(Stars ~ .,
  training_subset_backward
)

summary(red_backward_OLS)

plot(fitted(red_forward_OLS), resid(red_forward_OLS),
                      xlab = "Fitted Values", ylab = "Residual",
                      main = "Fitted vs Residual for Forward Regression Model")
abline(0,0)

qqnorm(resid(red_forward_OLS))
qqline(resid(red_forward_OLS))

# predict on test set using forward model
pred_red_forward_OLS <- predict(red_forward_OLS, newdata=test_set)

# predict on test set using backward model
pred_red_backward_OLS <- predict(red_backward_OLS, newdata=test_set)

# compute RMSE of full OLS model
RMSE_models <- tibble(
  Model = "OLS Full Regression",
  RMSE = rmse(pred_full_OLS, test_set$Stars)
)
RMSE_models

# compute RMSE of reduced models 
RMSE_models <- rbind(
  RMSE_models,
  tibble(
    Model = c("OLS Reduced Forward Regression", "OLS Reduced Backward Regression"),
    RMSE = c(rmse(pred_red_forward_OLS, test_set$Stars), rmse(pred_red_backward_OLS, test_set$Stars))
    )
  )
RMSE_models
