library(tidyverse)

clean_chocolate_data = function(choc_df) {
  choc_df %>%
    rename_at(vars(everything()), ~c("company","species","id","review_date","cocoa_percent","company_location","rating","bean","broad_bean")) %>%
    mutate(
      cocoa_percent = str_replace_all(cocoa_percent, "%", "") %>% as.numeric(),
      company_location = str_replace_all(company_location, "Amsterdam", "Netherlands")
           )
}

plot_rating_dist = function(choc_df) {
  # Plot the distribution of ratings across years
  p = ggplot(data = choc_df, aes(x = rating)) +
    geom_density(fill = "#7B3F00") +
    facet_grid(rows = vars(review_date), scales="free_y") +
    labs(title = "Distribution of Chocolate Ratings", subtitle = "By Year") +
    theme_light() +
    theme(
          # axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())

  ggsave("images/rating_dist_by_year.png", p, width=600, height=600, unit="px", dpi=100)
}

top_chocolates = function(choc_df, n) {
  # Get the most recent, highest rated ones
  choc_df %>%
    arrange(desc(rating), desc(review_date)) %>%
    slice(1:n)
}

main = function() {
  choc_df = read_csv('data/flavors_of_cacao.csv') %>%
    clean_chocolate_data()

  top_chocolates(choc_df, 10) %>%
    print()

  plot_rating_dist(choc_df)

}

main()
