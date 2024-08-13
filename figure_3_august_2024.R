# This script produces Figure 3, which compares empirical data with Q-model and Q-model twin predictions.
# It estimates parameters for the Q-model using empirical citation data of scientists with 50 or more publications.

# Load necessary libraries and check if they are installed, otherwise install and load them
required_packages <- c("data.table", "viridis", "ggplot2", "scales", "MASS", "grid", "dplyr", "haven", "reshape2")

install_and_load <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
  library(package, character.only = TRUE)
}

lapply(required_packages, install_and_load)

# Set the path for the data file
your_path <- ""

# Load and filter the dataset to include only scientists with 50 or more publications
small <- read_dta(paste0(your_path, "/scientists.dta"))

small <- small %>%
  group_by(authorId) %>%
  mutate(t = 1,
         paperNumber = cumsum(t),
         N = n()) %>%
  filter(N > 49)

# Number of scientists in the filtered dataset
cat("Number of scientists:", length(unique(small$authorId)), "\n")  # 2908 scientists

# Organize data so that all researchers have exactly 50 publications
small <- small %>%
  group_by(authorId) %>%
  mutate(t = 1,
         k = cumsum(t)) %>%
  filter(k <= 50)

# Log transform of citations plus one to avoid log(0)
small <- small %>%
  mutate(logCit = log(cit10 + 1))

# Compute average log citation (q) for each researcher
small <- small %>%
  group_by(authorId) %>%
  mutate(q = mean(logCit))

# Compute p as the deviation from researcher-specific q
small$p <- small$q - small$logCit

# Fitting lognormal distribution with Maximum Likelihood Estimation (MLE)
empirical_data <- small %>%
  filter(cit10 > 0) %>%  # Exclude papers with 0 citations for lognormal fitting
  pull(cit10)

fit_mle <- fitdistr(empirical_data, "lognormal")
mu_mle <- unname(fit_mle$estimate['meanlog'])
sigma_mle <- unname(fit_mle$estimate['sdlog'])

cat("MLE Estimates - Meanlog:", mu_mle, "Sdlog:", sigma_mle, "\n")

# Summarize citation counts
all_pubs <- small %>%
  filter(cit10 > 0) %>%
  group_by(cit = cit10) %>%
  summarize(n = n()) %>%
  mutate(n = n / sum(n))

# Function to create logarithmically spaced points
create_log_points <- function(base, min_exp, max_exp, num_points) {
  exponents <- seq(min_exp, max_exp, length.out = num_points)
  points <- base^exponents
  return(points)
}

# Define logarithmic points
log_points <- round(create_log_points(10, 1, 3, 30))
log_points <- c(1:9, log_points)

# Match publication counts to the closest logarithmic points
all_pubs <- all_pubs %>%
  rowwise() %>%
  mutate(closest_log_point = log_points[which.min(abs(log_points - cit))]) %>%
  ungroup() %>%
  group_by(closest_log_point) %>%
  summarize(total_n = mean(n), .groups = 'drop')

# Lognormal density function for plotting
lognorm_density <- function(x, meanlog, sdlog) {
  dlnorm(x, meanlog, sdlog)
}

# Compute between-subject variance and other parameters
bet_var <- var(small$q)
var_log_normal <- sigma_mle^2
var_x <- var_log_normal - bet_var

param1q <- mu_mle
param2q <- bet_var
param3q <- var_x

# Q-model twin parameters
param1q_twin <- mu_mle
param2q_twin <- var_x
param3q_twin <- var_x / bet_var

# Define breaks for log scale in the plot
breaks_log10 <- function(x) {
  low <- floor(log10(min(x)))
  high <- ceiling(log10(max(x)))
  10^(seq.int(low, high))
}

# Annotation text for the plot
annotation_text <- bquote(
  atop(bold("Q-model parameters:") ~ mu[T] == .(sprintf("%.2f", param1q)) * "," ~
         sigma[T]^2 == .(sprintf("%.2f", param2q)) * "," ~
         sigma[X]^2 == .(sprintf("%.2f", param3q)), 
       bold("Q-model twin parameters:") ~ a == .(sprintf("%.2f", param1q_twin)) * "," ~
         b == .(sprintf("%.2f", param2q_twin)) * "," ~
         c == .(sprintf("%.2f", param3q_twin)))
)

# Create the annotation grob for the plot
annotation_grob <- grobTree(
  textGrob(annotation_text, x = 0.95, y = 0.95, hjust = 1, vjust = 1, gp = gpar(col = "black", fontsize = 8))
)

# Create and display the plot
g <- ggplot() +
  geom_point(data = all_pubs, 
             aes(x = closest_log_point, y = total_n, color = "Empirical data", fill = "Empirical data"),
             size = 3, shape = 21) + 
  stat_function(fun = lognorm_density, 
                args = list(meanlog = mu_mle, sdlog = sigma_mle), 
                aes(color = "Q-model"), linetype = "twodash", size = 1) + 
  stat_function(fun = lognorm_density, 
                args = list(meanlog = mu_mle, sdlog = sigma_mle), 
                aes(color = "Q-model twin"), linetype = "dotted", size = 1) +
  annotation_logticks() +
  scale_x_log10(breaks = breaks_log10,
                labels = trans_format(log10, math_format(10^.x))) +
  scale_y_continuous(limits = c(0, 0.045)) +
  scale_color_manual(
    values = c("Empirical data" = "black", "Q-model" = "red", "Q-model twin" = "blue"),
    guide = guide_legend(override.aes = list(
      shape = c(21, NA, NA), 
      fill = c("grey80", NA, NA),
      linetype = c("blank", "twodash", "dotted"),
      color = c("black", "red", "blue")))
  ) +
  scale_fill_manual(values = c("Empirical data" = "grey80"), guide = "none") +
  theme_classic() + 
  theme(axis.title = element_text(size = 14)) +
  labs(x = expression(c[10]), y = bquote(P(c[10])), color = NULL) +
  theme(legend.position = "bottom") +
  annotation_custom(annotation_grob)

print(g)

# Save the plot as a JPEG file
ggsave("/fit_q_model_and_twin_distribution.jpeg",
       plot = g, device = "jpeg",
       width = 13, height = 10, units = "cm")
