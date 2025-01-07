# run test for three datasets : PSID, Musiclab, & Sinatra 
library(dplyr)
library(ggplot2)
library(cowplot)
library(haven)
library(kableExtra)
library(tidyr)



rm(list = ls())

# where the datasets are:
setwd("")


#===================================================================================================
# Load musiclab formatted dataset:
load("newDfV1.saved")
ml1 <- newDf[which(newDf$world == 9),]
load("newDf.saved")
ml2 <- newDf[which(newDf$world == 9),]
rm(newDf)


# first convert data into was it downloaded by the user or not 
t <- as.matrix(ml1[,1:48])

# Subtract each row from the row above
t[2:nrow(t), ] <- t[2:nrow(t), ] - t[1:(nrow(t) - 1), ]
t <- as.data.frame(t)
t$userId <- ml1$userId
vars <- names(t)[1:48]

t <- t %>% 
  group_by(userId) %>% 
  mutate_at(vars(matches(vars)), max) %>% 
  slice(1)

t <- as.matrix(t[,1:48])

column_differences <- abs(t[2:nrow(t), ] - t[1:(nrow(t) - 1), ])

# Count the number of non-zero differences for each column
non_zero_diff_count_empi <- sum(colSums(column_differences))

sum_runs <- c()
number_sim <- 10000
for (i in 1:number_sim){
  shuffled_t <- t[sample(1:nrow(t), nrow(t), replace = FALSE), ]
  column_differences <- abs(shuffled_t[2:nrow(shuffled_t), ] - shuffled_t[1:(nrow(shuffled_t) - 1), ])
  non_zero_diff_count_shuffled <- sum(colSums(column_differences))
  sum_runs <- c(sum_runs, non_zero_diff_count_shuffled)
}

sum_runs_v1 <- sum_runs
non_zero_diff_count_empi_v1 <- non_zero_diff_count_empi

# second world:
# first convert data into was it downloaded by the user or not 
t <- as.matrix(ml2[,1:48])
# Subtract each row from the row above
t[2:nrow(t), ] <- t[2:nrow(t), ] - t[1:(nrow(t) - 1), ]

t <- as.data.frame(t)
t$userId <- ml2$userId
vars <- names(t)[1:48]

t <- t %>% 
  group_by(userId) %>% 
  mutate_at(vars(matches(vars)),max) %>% 
  slice(1)

t <- as.matrix(t[,1:48])
column_differences <- abs(t[2:nrow(t), ] - t[1:(nrow(t) - 1), ])

# Count the number of non-zero differences for each column
non_zero_diff_count_empi <- sum(colSums(column_differences))

sum_runs <- c()
number_sim <- 10000
for (i in 1:number_sim){
  shuffled_t <- t[sample(1:nrow(t), nrow(t), replace = FALSE), ]
  column_differences <- abs(shuffled_t[2:nrow(shuffled_t), ] - shuffled_t[1:(nrow(shuffled_t) - 1), ])
  non_zero_diff_count_shuffled <- sum(colSums(column_differences))
  sum_runs <- c(sum_runs, non_zero_diff_count_shuffled)
}

sum_runs <- sum_runs_v1 + sum_runs
non_zero_diff_count_empi <- non_zero_diff_count_empi + non_zero_diff_count_empi_v1

# Graph:
bottom_2_5_percent <- unname(quantile(sum_runs, probs = 0.025))
density_data <- density(sum_runs)
density_df <- data.frame(x = density_data$x, y = density_data$y)
density_shaded <- subset(density_df, x <= bottom_2_5_percent)
density_shaded_above <- subset(density_df, x > bottom_2_5_percent)


g <- ggplot() +
  geom_area(data = density_shaded, aes(x = x, y = y), fill = "#2f4b7c", alpha = 0.7) + # Shaded area
  geom_area(data = density_shaded_above, aes(x = x, y = y), fill = "#fff4c2", alpha = 0.7) + # Shaded area
  geom_line(data = density_df, aes(x = x, y = y), size = 1) + # Density curve
  geom_vline(xintercept = non_zero_diff_count_empi,
             linetype = "dashed",
             color = "#cc0000", size = 1) + # Red vertical line
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(title = "Music Lab",
       x = "Runs",
       y = "Density") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

g

# save graph:
ggsave("", # name graph
       plot=g, device = "jpeg",
       width = 12, height = 12, units = "cm")
#===================================================================================================


#===================================================================================================
# Scientists data

your_path<-""

small<-read_dta(paste0(your_path,"/scientists.dta"))

# Number papers in sequential order:
small<-small%>%
  group_by(authorId)%>%
  mutate(t=1,
         k=cumsum(t))

# log transform (add + 1 for papers with no citations in 10 years)
small<-small%>%
  mutate(logCit=log(cit10+1))

# compute q per researcher:
small<-small%>%
  group_by(authorId)%>%
  mutate(q=mean(logCit))

# compute p: deviation from author q
small$p<-small$q-small$logCit

small <- small %>% 
  group_by(authorId) %>% 
  mutate(med = unname(quantile(logCit, probs = 0.5)),
         number_pub = n()) %>% 
  filter(number_pub >= 50, 
         k <= 50) %>% 
  mutate(value = ifelse(logCit >= med, 1, 0))

t <- small[,c("authorId", "value")]

t <- matrix(t$value, nrow = 50, byrow = FALSE)

# each column is a scientist, each row in a publication (whether it was more than his median)
# take the row minus the previous row to see whether there was a run:
column_differences <- abs(t[2:nrow(t), ] - t[1:(nrow(t) - 1), ])

# Count the number of non-zero differences for each column and sum all
non_zero_diff_count_empi <- sum(colSums(column_differences))

sum_runs <- c()
number_sim <- 10000
for (i in 1:number_sim){
  shuffled_t <- matrix(nrow = nrow(t), ncol = ncol(t), 0)
  for (j in 1:ncol(t)){
    shuffled_t[ , j] <- t[sample(1:nrow(t), nrow(t), replace = FALSE), j]
  }
  column_differences <- abs(shuffled_t[2:nrow(shuffled_t), ] - shuffled_t[1:(nrow(shuffled_t) - 1), ])
  non_zero_diff_count_shuffled <- sum(colSums(column_differences))
  sum_runs <- c(sum_runs, non_zero_diff_count_shuffled)
}

bottom_2_5_percent <- unname(quantile(sum_runs, probs = 0.025))
density_data <- density(sum_runs)
density_df <- data.frame(x = density_data$x, y = density_data$y)
density_shaded <- subset(density_df, x <= bottom_2_5_percent)
density_shaded_above <- subset(density_df, x > bottom_2_5_percent)


g2 <- ggplot() +
  geom_area(data = density_shaded, aes(x = x, y = y), fill = "#2f4b7c", alpha = 0.7) + # Shaded area
  geom_area(data = density_shaded_above, aes(x = x, y = y), fill = "#fff4c2", alpha = 0.7) + # Shaded area
  geom_line(data = density_df, aes(x = x, y = y), size = 1) + # Density curve
  geom_vline(xintercept = non_zero_diff_count_empi,
             linetype = "dashed",
             color = "#cc0000", size = 1) + # Red vertical line + # Red vertical line
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(title = "Scientists",
       x = "Runs",
       y = "Density") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


g2

ggsave(".jpeg",
       plot=g2, device = "jpeg",
       width = 20, height = 10, units = "cm")
#===================================================================================================




#===================================================================================================
# Load PSID dataset:
# PSID data need to be downloaded from the PSID website https://psidonline.isr.umich.edu
# and then pre-processed using Philip Lersch's stata code: https://osf.io/uey5h/ 
data <- read_dta('psid.dta')

# Select relevant columns
data <- data %>% select(-women, -immigrant, -birthyearb, -birthyear, -cohort)

# Identify dependent variables
dependent_vars <- setdiff(names(data), c("age18", "pid", "svyyear", "age", "parent"))



#===================================================================================================
perform_runs_test <- function(data, variable, number_sim) {
  small1 <- data %>%
    select(pid, age, all_of(variable)) %>%
    filter(!is.na(.data[[variable]]) & .data[[variable]] < 8) 
  
  small1 <- small1 %>%
    group_by(pid) %>%
    mutate(n_obs = n()) %>%
    filter(n_obs > 2) %>%
    mutate(value = get(variable)) %>% 
    ungroup()  
  
  number_sub_matrices <- length(unique(small1$n_obs))
  values_sub_matrices <- unique(small1$n_obs)
  
  sum_runs_tot <- matrix(nrow = number_sub_matrices, ncol = number_sim, 0)
  empirical_runs <- c()
  l <- 1
  
  for (k in values_sub_matrices){
    t1 <- small1 %>% 
      filter(n_obs == k) %>%
      select(pid, value) 
    
    t1 <- matrix(t1$value, nrow = k, byrow = FALSE)
    # each column is an individual, each row in a attitude value
    
    # take the row minus the previous row to see whether there was a run:
    column_differences <- abs(t1[2:nrow(t1), ] - t1[1:(nrow(t1) - 1), ])
    empi <- sum(colSums(column_differences))
    empirical_runs <- c(empirical_runs, empi)
    
    sum_runs <- c()
    for (i in 1:number_sim){
      shuffled_t1 <- matrix(nrow = nrow(t1), ncol = ncol(t1), 0)
      for (j in 1:ncol(t1)){
        shuffled_t1[ , j] <- t1[sample(1:nrow(t1), nrow(t1), replace = FALSE), j]
      }
      column_differences <- abs(shuffled_t1[2:nrow(shuffled_t1), ] - shuffled_t1[1:(nrow(shuffled_t1) - 1), ])
      non_zero_diff_count_shuffled <- sum(colSums(column_differences))
      sum_runs <- c(sum_runs, non_zero_diff_count_shuffled)
    }
    sum_runs_tot[l,] <- sum_runs
    l <- l + 1
  }
  sum_runs_tot <- colSums(sum_runs_tot)
  empirical_runs <- sum(empirical_runs)
  list(empirical = empirical_runs, simulated = sum_runs_tot)
}
#===================================================================================================





#===================================================================================================
# Function to compute descriptive stats one variable
compute_descriptive_stats <- function(data, variable) {
  small <- data %>%
    select(pid, age, all_of(variable)) %>%
    filter(!is.na(.data[[variable]]) & .data[[variable]] < 8)

    big_n <- length(unique(small$pid))
    small <- small %>%
      group_by(pid) %>%
      mutate(n_obs = n()) %>%
      filter(n_obs > 2) %>%
      mutate(med = median(get(variable)), 
             average = mean(get(variable)),
             changed = ifelse(get(variable) != average, 1, 0)) %>% # checks whether the person changed his/her attitude
      ungroup()
    
    changers_data <- small %>% 
      group_by(pid) %>%
      summarise(changed = min(changed))
    
    tot_changers <- sum(changers_data$changed)
    
    full_changers <- small %>% 
      group_by(pid) %>%
      summarise(n = n(),
                n_diff = length(unique(get(variable)))) %>%
      filter(n == n_diff) %>%
      nrow()

  if (nrow(small) == 0) return(NULL)  # Skip if no data

  # compute descriptive statistics:
  t <- small %>%
    summarise(N = n(),
              mean = mean(get(variable)),
              sd = sd(get(variable)),
              min = min(get(variable)),
              max = max(get(variable)),
              ind = length(unique(pid)),
              big_n = big_n,
              var = variable, 
              tot_changers = tot_changers - full_changers)
  return(t)
}
#===================================================================================================



#===================================================================================================
# Loop through variables: for descriptive stats
stats_des <- c()
for (j in seq_along(dependent_vars)) {
  variable <- dependent_vars[j]
  result <- compute_descriptive_stats(data, variable)
  print(result)
  stats_des <- bind_rows(stats_des, result)

}

load("p_values_optimized_with_power.saved")

stats_des <- left_join(stats_des, store_stats, by = c("var" = "variable"))

stats_des <- stats_des %>% 
  select(var, big_n, ind, tot_changers, N, mean, sd, min, max, p_value)

# Create a latex table
latex_table <- kable(stats_des, format = "latex", booktabs = TRUE)

# Save 
writeLines(latex_table, "table.tex")
#===================================================================================================



#===================================================================================================
# Graph appendix, relationship between p-value and 'real' sample size:
g3 <- ggplot(stats_des, aes(x = tot_changers, y = p_value)) +
  geom_point(size = 2,  alpha = 0.8, shape = 21, fill = "blue", stroke = 0.5) +  
  labs(
    x = "Effective sample size",  
    y = "P-value"                 
  ) +
  theme_bw() + 
  theme(
    axis.text = element_text(size = 12),                
    panel.grid.major = element_line(size = 0.5),
    panel.grid.minor = element_blank()                 
  ) 

g3

ggsave(".jpeg",
       plot=g3, device = "jpeg",
       width = 8.5, height = 8.5, units = "cm")
#===================================================================================================



#===================================================================================================
# runs test without transforming variable into dichotomous variable
# Set simulation parameters
number_sim <- 10000
empirical_stats <- numeric(length(dependent_vars))
distributions <- matrix(0, nrow = number_sim, ncol = length(dependent_vars))
variables <- character(0)

# Loop through variables
for (j in seq_along(dependent_vars)) {
  variable <- dependent_vars[j]
  result <- perform_runs_test(data, variable, number_sim)
  
  if (!is.null(result)) {
    empirical_stats[j] <- result$empirical
    distributions[, j] <- result$simulated
    variables <- c(variables, variable)
    print(paste("Processed:", variable, Sys.time()))
  }
}

# Save results
save(distributions, file = "distributions_optimized_non_transformed.saved")
save(empirical_stats, file = "empirical_stats_optimized_non_transformed.saved")
#===================================================================================================



#===================================================================================================
# find the position of empirical stats in the distribution of simulated stats
# so take each column in distributions and compare it to the same column in empirical stats
# just give the position of the empirical stats in the distribution

store_stats <- matrix(nrow = length(variables), ncol = 6) # one col for pvalues, other for different values of power

for (j in seq_along(variables)) {
  # find the exact position of the empirical value in the distribution
  #reverse the order, so that the lowest value is the first one
  distributions[,j] <- sort(distributions[,j], decreasing = FALSE)
  # now for each column tell me the row in which the empirical would be in the distribution
  store_stats[j, 1] <- sum(distributions[,j] <= empirical_stats[j]) / number_sim
  
  for (percent_change in c(1, 2, 3, 4, 5)){
    # For each test compute the power for different values of the effect:
    bottom_2_5_percent <- unname(quantile(unname(distributions[,j]), probs = 0.025))
    distribution_h1_bottom <- distributions[,j] * (1 - (percent_change / 100))
    distribution_h1_bottom <- sort(distribution_h1_bottom, decreasing = TRUE)
    
    # now for each column tell where the empirical value would be in the distribution
    store_stats[j, percent_change + 1] <- sum(distribution_h1_bottom <= bottom_2_5_percent) / length(distribution_h1_bottom)
  }
}

store_stats <- as.data.frame(store_stats)
names(store_stats) <- c("p_value", "power_001", "power_002",
                        'power_003', "power_004", "power_005")

store_stats$variable <- variables

store_stats <- store_stats %>% 
  mutate(p_value = ifelse(p_value <= 0.5, p_value * 2, 2 * (1 - p_value)))

# Save p-values
save(store_stats, file = "p_values_optimized_with_power.saved")

write.csv(store_stats, "store_stats.csv")
#===================================================================================================



#===================================================================================================
load("p_values_optimized_with_power.saved")
load("distributions_optimized_binary_with_power.saved")
load("empirical_stats_optimized_binary_with_power.saved")

distributions <- as.data.frame(t(distributions))
distributions$variable <- store_stats$variable
distributions <- distributions[!grepl("cds", distributions$variable), ]
distributions <- distributions %>% 
  select(-variable)
distributions <- t(as.matrix(distributions))

sim_distribution <- rowSums(distributions)

# get rid of 0s in empirical_stats 
empirical_stats <- empirical_stats[empirical_stats != 0]
empirical_distribution <- data.frame(empirical_stats, variable = store_stats$variable)
empirical_distribution <- empirical_distribution[!grepl("cds", empirical_distribution$variable), ]
sum_empirical <- sum(empirical_distribution$empirical_stats)


bottom_2_5_percent <- unname(quantile(sim_distribution, probs = 0.025))
density_data <- density(sim_distribution)
density_df <- data.frame(x = density_data$x, y = density_data$y)
density_shaded <- subset(density_df, x <= bottom_2_5_percent)
density_shaded_above <- subset(density_df, x > bottom_2_5_percent)

g4 <- ggplot() +
  geom_area(data = density_shaded, aes(x = x, y = y), fill = "#2f4b7c", alpha = 0.7) + 
  geom_area(data = density_shaded_above, aes(x = x, y = y), fill = "#fff4c2", alpha = 0.7) + 
  geom_line(data = density_df, aes(x = x, y = y), size = 1) + 
  geom_vline(xintercept = sum_empirical,
             linetype = "dashed",
             color = "#cc0000", size = 1) + 
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(title = "Attitudes",
       x = "Runs",
       y = "Density") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


g4

tot<-plot_grid(g2, g4, g,
               labels=c("A", "B", "C"),  
               ncol = 3, nrow = 1)

tot

ggsave(".jpeg",
       plot=tot, device = "jpeg",
       width = 21, height = 7, units = "cm")

