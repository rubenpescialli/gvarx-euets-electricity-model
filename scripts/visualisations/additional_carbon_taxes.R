library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)



df <- read_excel("data/raw/visualisations/additional_carbon_taxes.xlsx")

df_long <- df %>%
  pivot_longer(cols = c(`2018`, `2021`, `2025`), 
               names_to = "Year", 
               values_to = "Pct")

df_long <- df_long %>% mutate(Country = factor(Country, levels = unique(df$Country)))

transparency_value = 0.4

df_long <- df_long %>%
  mutate(
    Year = factor(Year, levels = c("2018","2021","2025")),
    Includes_power_sector = factor(Includes_power_sector, levels = c("YES","NO","NOT PRESENT")),
    alpha_val = ifelse(Includes_power_sector == "YES", 1, transparency_value)
  )

cols_year <- c(
  "2018" = "brown",
  "2021" = "forestgreen",
  "2025" = "steelblue"
)

pd <- position_dodge(width = 0.78)
bar_width <- 0.78

ggplot() +
  
  geom_col(
    data = df_long,
    mapping = aes(x = Country, y = Pct, fill = Year, alpha = alpha_val, group = Year),
    position = pd,
    width = bar_width,
    colour = NA,
    show.legend = TRUE
  ) +
  
  geom_col(
    data = filter(df_long, Includes_power_sector == "YES"),
    mapping = aes(x = Country, y = Pct, group = Year),
    position = pd,
    width = bar_width,
    fill = NA,
    colour = "black",
    size = 0.3,
    show.legend = FALSE
  ) +
  
  scale_fill_manual(values = cols_year, name = "") +
  
  scale_alpha_identity(guide = FALSE) +
  
  scale_y_continuous(breaks = seq(0, 150, by = 20), expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 150)) +
  
  labs(title = "Prices of the additional carbon taxes", x = "", y = "Carbon price (€/tCO2)") +
  
  theme_grey(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    
    legend.text = element_text(size = 14),
    legend.key.spacing.y = unit(7, "pt"),
    
    legend.justification = c(0.5, 0.8), 
    
    axis.text.x = element_text(hjust = 0.5, size = 15),
    
    axis.text.y = element_text(hjust = 0.5, size = 15),
    axis.title.y = element_text(size = 17)
  )

# ggsave("Additional carbon taxes.jpeg",
#        width = 4444,
#        height = 1670,
#        units = "px")