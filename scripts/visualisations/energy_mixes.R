library(readxl)
library(ggplot2)
library(tidyverse)



data_gen_mix = read_excel("data/raw/visualisations/2023_electricity_generation_mixes.xlsx")

str(data_gen_mix)

data_gen_mix <- data_gen_mix %>%
  mutate(Country = factor(Country, levels = sort(unique(Country))))

colors <- c(
  "Coal"        = "salmon4",
  "Natural gas" = "#FF9900",
  "Oil"         = "royalblue4",
  "Hydro"       = "steelblue1",
  "Geothermal"  = "yellow4",
  "Wind"        = "palegreen",
  "Solar"       = "khaki1",
  "Biofuels"    = "springgreen4",
  "Nuclear"     = "purple4"
)

ordered_sources <- c("Coal", "Oil", "Natural gas", "Hydro", "Solar", "Wind", "Geothermal", "Biofuels", "Nuclear")

source_cols <- setdiff(names(data_gen_mix), c("Country", "Total"))

bar_height <- 0.8
threshold <- 5

df_long <- data_gen_mix %>%
  pivot_longer(cols = all_of(source_cols), names_to = "Source", values_to = "GWh") %>%
  mutate(GWh = replace_na(GWh, 0)) %>%
  mutate(Source = factor(Source, levels = ordered_sources)) %>%
  group_by(Country) %>%
  mutate(
    pct_raw = ifelse(Total > 0, GWh / Total * 100, 0),
    pct = if(sum(pct_raw, na.rm = TRUE) > 0) pct_raw / sum(pct_raw, na.rm = TRUE) * 100 else 0
  ) %>%
  ungroup()

plots <- list()

for(ct in levels(df_long$Country)) {
  dat0 <- df_long %>% filter(Country == ct)
  dat_plot <- dat0 %>% filter(pct > 0) %>% arrange(Source)

  dat_plot <- dat_plot %>%
    mutate(
      x_start   = lag(cumsum(pct), default = 0),
      x_end     = x_start + pct,
      x_center  = x_start + pct/2,
      pct_label = sprintf("%.1f", pct)
    )
  
  bar_ymin <- 0.5 - bar_height/2
  bar_ymax <- 0.5 + bar_height/2
  
  dat_labels <- dat_plot %>% filter(pct >= threshold)
  
  p <- ggplot()
  
  # Inside rectangles
  p <- p + geom_rect(
    data = dat_plot,
    aes(xmin = x_start, xmax = x_end, ymin = bar_ymin, ymax = bar_ymax, fill = Source),
    color = "black",
    size = 0.3
  )
  
  if(nrow(dat_labels) > 0) {
    p <- p + geom_text(
      data = dat_labels,
      aes(x = x_center, y = bar_ymin, label = paste0(Source, "\n", pct_label, "%")),
      vjust = 1.2,
      size = 8.3
    )
  }
  
  # Outside rectangle
  p <- p + geom_rect(
    aes(xmin = 0, xmax = 100, ymin = bar_ymin, ymax = bar_ymax),
    fill = NA,
    color = "black",
    size = 0.5,
    inherit.aes = FALSE
  )
  
  p <- p +
    scale_fill_manual(values = colors) +
    labs(title = ct) +

    # To leave some range for the margins
    scale_x_continuous(expand = c(0.025, 0)) +

    coord_cartesian(ylim = c(-0.4, 1)) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, vjust = -1.6, size = 30, face = "bold"),
      legend.position = "none",
    )
  
  plots[[as.character(ct)]] <- p
}

print(plots[[1]])  # Change this number according to the desired country (1 to 26 included).

#ggsave("Austria.jpg")