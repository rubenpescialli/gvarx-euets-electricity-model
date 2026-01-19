library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)



df <- read_excel("data/raw/visualisations/marginal_generation_technologies.xlsx")

df_long <- df %>%
  pivot_longer(cols = -Country, names_to = "Technology", values_to = "Pct")

df_long <- df_long %>%
  mutate(Country = factor(Country, levels = unique(df$Country)))

plot1_data <- df_long %>% filter(Technology %in% c("Natural gas", "Coal", "Oil"))
plot1_data <- plot1_data %>% mutate(Pct = Pct * 100)

plot2_data <- df_long %>% filter(Technology %in% c("Hydro", "Nuclear", "Biofuels"))
plot2_data <- plot2_data %>% mutate(Pct = Pct * 100)

cols_plot1 <- c(
  "Natural gas" = "#FF9900",
  "Coal"        = "salmon4",
  "Oil"         = "royalblue4"
)

order1 <- c("Natural gas", "Oil", "Coal")
plot1_data$Technology <- factor(plot1_data$Technology, levels = order1)
totals1 <- plot1_data %>%
  group_by(Country) %>%
  summarise(total_pct = sum(Pct, na.rm = TRUE), .groups = "drop")

cols_plot2 <- c(
  "Hydro"    = "steelblue1",
  "Nuclear"  = "purple4",
  "Biofuels"  = "springgreen4"
)

order2 <- c("Biofuels", "Nuclear", "Hydro")
plot2_data$Technology <- factor(plot2_data$Technology, levels = order2)
totals2 <- plot2_data %>%
  group_by(Country) %>%
  summarise(total_pct = sum(Pct, na.rm = TRUE), .groups = "drop")

ggplot(plot1_data, aes(x = Country, y = Pct, fill = Technology)) +
  
  geom_col(position = "stack",  width = 0.75, colour = "black", size = 0.3) +
  
  geom_text(
    data = totals1,
    aes(x = Country, y = total_pct, label = paste0(round(total_pct, 0), "%")),
    inherit.aes = FALSE,
    vjust = -0.5, 
    size = 6,
  ) +
  
  scale_fill_manual(values = cols_plot1,
                    name = "") +
  
  scale_y_continuous(breaks = seq(0, 110, by = 20), 
                     expand = c(0, 0)) +
  
  coord_cartesian(ylim = c(0, 110)) +
  
  labs(title = "Marginal shares: fossil fuel based generation",
       x = "", y = "Marginal share (%)") +
  
  theme_grey(base_size = 12) +
  
  theme(plot.title = element_text(hjust = 0.3, size = 28),
        
        legend.text = element_text(size = 22),
        legend.key.spacing.y = unit(7, "pt"),
    
        axis.text.x = element_text(hjust = 0.5, size = 22),
        
        axis.text.y = element_text(hjust = 0.5, size = 22),
        axis.title.y = element_text(size = 26))
        
# ggsave("marginal_shares_fossil_fuels.jpeg")

ggplot(plot2_data, aes(x = Country, y = Pct, fill = Technology)) +
  
  geom_col(position = "stack",  width = 0.75, colour = "black", size = 0.3) +
  
  geom_text(
    data = totals2,
    aes(x = Country, y = total_pct, label = paste0(round(total_pct, 0), "%")),
    inherit.aes = FALSE,
    vjust = -0.5, 
    size = 6,
  ) +
  scale_fill_manual(values = cols_plot2,
                    name = "") +
  
  scale_y_continuous(breaks = seq(0, 110, by = 20), 
                     expand = c(0, 0)) +
  
  coord_cartesian(ylim = c(0, 110)) +
  
  labs(title = "Marginal shares: non-fossil fuel generation",
       x = "", y = "Marginal share (%)") +
  
  theme_grey(base_size = 12) +
  
  theme(plot.title = element_text(hjust = 0.35, size = 28),
        
        legend.text = element_text(size = 22),
        legend.key.spacing.y = unit(7, "pt"),
        
        axis.text.x = element_text(hjust = 0.5, size = 22),
        
        axis.text.y = element_text(hjust = 0.5, size = 22),
        axis.title.y = element_text(size = 26),
        
        plot.margin = margin(t = 5.5, b = 5.5, l = 6, r = 26))

# ggsave("marginal_shares_non_fossil_fuels.jpeg")