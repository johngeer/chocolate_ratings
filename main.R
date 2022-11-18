library(xgboost)
library(tidyverse)

clean_chocolate_data <- function(choc_df) {
  choc_df %>%
    rename_at(vars(everything()), ~ c("company", "species", "id", "review_date", "cocoa_percent", "company_location", "rating", "bean", "broad_bean")) %>%
    mutate(
      cocoa_percent = str_replace_all(cocoa_percent, "%", "") %>% as.numeric(),
      company_location = str_replace_all(company_location, "Amsterdam", "Netherlands")
    )
}

plot_rating_dist <- function(choc_df) {
  # Plot the distribution of ratings across years
  p <- ggplot(data = choc_df, aes(x = rating)) +
    geom_density(fill = "#7B3F00") +
    facet_grid(rows = vars(review_date), scales = "free_y") +
    labs(title = "Distribution of Chocolate Ratings", subtitle = "By Year") +
    theme_light() +
    theme(
      # axis.title.y=element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )

  ggsave("images/rating_dist_by_year.png", p, width = 600, height = 600, unit = "px", dpi = 100)
}

plot_rating_dist_company <- function(choc_df) {
  # Plot the distribution of ratings across years
  company_df <- choc_df %>%
    group_by(company) %>%
    summarize(n = n(), mean = mean(rating)) %>%
    arrange(desc(n)) %>%
    head(18) %>%
    arrange(desc(mean))

  plot_df <- choc_df %>%
    filter(company %in% company_df$company) %>%
    mutate(company = factor(company, levels = company_df$company))

  p <- ggplot(data = plot_df, aes(x = rating)) +
    geom_density(fill = "#7B3F00", adjust = 2) +
    facet_wrap(vars(company), scales = "free_y", ncol = 3) +
    labs(title = "Distribution of Chocolate Ratings", subtitle = "By Company") +
    theme_light() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )

  ggsave("images/rating_dist_by_company.png", p, width = 600, height = 600, unit = "px", dpi = 100)
}

plot_rating_by_cocoa_perc <- function(choc_df) {
  plot_df <- choc_df

  p <- ggplot(data = plot_df, aes(x = cocoa_percent, y = rating)) +
    geom_hex(bins = 10) +
    scale_fill_continuous(type = "viridis") +
    labs(title = "Chocolate Ratings by Cocoa Percentage") +
    theme_bw()

  ggsave("images/rating_by_cocoa_percent.png", p, width = 600, height = 600, unit = "px", dpi = 100)
}

top_chocolates <- function(choc_df, n) {
  # Get the most recent, highest rated ones
  choc_df %>%
    arrange(desc(rating), desc(review_date)) %>%
    slice(1:n)
}

feature_importance <- function(given_df) {
  # Explore what seems to matter
  options(na.action = "na.pass")
  model_matrix <- model.matrix(rating ~ ., data = given_df, na.action = "na.pass")
  model <- xgboost(model_matrix, label = given_df$rating, objective = "reg:squarederror", nrounds = 100, max.depth = 4, eta = 1, nthread = 2)

  importance <- xgb.importance(feature_names = colnames(model_matrix), model = model)
  head(importance, 100)
}

main <- function() {
  choc_df <- read_csv("data/flavors_of_cacao.csv") %>%
    clean_chocolate_data()

  # Explore which variables may matter
  feature_importance(choc_df)

  # Put together some plots
  plot_rating_by_cocoa_perc(choc_df)
  plot_rating_dist(choc_df)
  plot_rating_dist_company(choc_df)

  # Show the top 10 best rated chocolates (more recent years preferred)
  top_chocolates(choc_df, 10) %>%
    print()
}

main()
