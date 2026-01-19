library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)



df1 <- read_excel("data/raw/visualisations/competition_levels.xlsx", sheet = 1)

df1 <- df1[df1$Country != "AT" &
           df1$Country != "NL" &
           df1$Country != "NO", ]

df2 <- read_excel("data/raw/visualisations/competition_levels.xlsx", sheet = 2)

df2 <- df2[df2$Country != "NO", ]

df_long1 <- df1 %>%
  pivot_longer(cols = -Country, names_to = "Year", values_to = "Pct")

df_long1 <- df_long1 %>%
  mutate(Country = factor(Country, levels = unique(df1$Country)))

cols_plot1 <- c(
  "2018" = "brown",
  "2023" = "steelblue"
)

df_long2 <- df2 %>%
  pivot_longer(cols = -Country, names_to = "Year", values_to = "Pct")

df_long2 <- df_long2 %>%
  mutate(Country = factor(Country, levels = unique(df2$Country)))

cols_plot2 <- c(
  "2018" = "brown",
  "2023" = "steelblue"
)

ggplot(df_long1, aes(x = Country, y = Pct, fill = Year)) +
  
  geom_col(position = position_dodge(width = 0.8),  width = 0.75, colour = "black", size = 0.3) +
  
  scale_fill_manual(values = cols_plot1,
                    name = "") +
  
  scale_y_continuous(breaks = seq(0, 110, by = 20), 
                     expand = c(0, 0)) +
  
  coord_cartesian(ylim = c(0, 105)) +
  
  labs(title = "Market shares of the largest producer",
       x = "", y = "Market share (%)") +
  
  theme_grey(base_size = 12) +
  
  theme(plot.title = element_text(hjust = 0.5, size = 28),
        
        legend.text = element_text(size = 22),
        legend.key.spacing.y = unit(10, "pt"),
        
        axis.text.x = element_text(hjust = 0.5, size = 22),
        
        axis.text.y = element_text(hjust = 0.5, size = 22),
        axis.title.y = element_text(size = 22))

# ggsave("market_shares_of_the_largest_producer.jpeg")

ggplot(df_long2, aes(x = Country, y = Pct, fill = Year)) +
  
  geom_col(position = position_dodge(width = 0.8),  width = 0.75, colour = "black", size = 0.3) +
  
  scale_fill_manual(values = cols_plot2,
                    name = "") +
  
  scale_y_continuous(breaks = seq(0, 110, by = 20), 
                     expand = c(0, 0)) +
  
  coord_cartesian(ylim = c(0, 105)) +
  
  labs(title = "Cumulative market share of the main electricity generation companies",
       x = "", y = "Market share (%)") +
  
  theme_grey(base_size = 12) +
  
  theme(plot.title = element_text(hjust = 0.5, size = 28),
        
        legend.text = element_text(size = 22),
        legend.key.spacing.y = unit(10, "pt"),
        
        axis.text.x = element_text(hjust = 0.5, size = 22),
        
        axis.text.y = element_text(hjust = 0.5, size = 22),
        axis.title.y = element_text(size = 22))

# ggsave("cumulative_market_shares_of_the_main_producers.jpeg")