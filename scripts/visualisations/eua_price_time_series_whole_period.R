library(readxl)
library(ggplot2)



data_plot_EUA_price_series = read_excel("data/raw/visualisations/eua_price_time_series_whole_period.xlsx")

str(data_plot_EUA_price_series)

data_plot_EUA_price_series$Date <- as.Date(data_plot_EUA_price_series$Date)

names(data_plot_EUA_price_series)[names(data_plot_EUA_price_series) == "ICE EUA Yearly Energy Future c1 - SETT. PRICE"] <- "EUA price"

data_plot_EUA_price_series <- subset(data_plot_EUA_price_series, Date <= as.Date("2025-03-19"))

vertical_lines <- data.frame(
  phase = c("End of phase I", "End of phase II", "End of phase III"),
  date = as.Date(c("2007-12-31", "2012-12-31", "2020-12-31"))
)

yr_min <- as.numeric(format(min(data_plot_EUA_price_series$Date, na.rm=TRUE), "%Y"))
yr_max <- as.numeric(format(max(data_plot_EUA_price_series$Date, na.rm=TRUE), "%Y"))

rects <- data.frame(
  xmin = as.Date(c("2005-01-01", "2008-01-01", "2013-01-01", "2021-01-01")),
  xmax = as.Date(c("2007-12-31", "2012-12-31", "2020-12-31", "2025-03-19")),
  Phase = c("Phase I", "Phase II", "Phase III", "Phase IV")
)

y_min <- min(data_plot_EUA_price_series$`EUA price`, na.rm=TRUE)
y_max <- max(data_plot_EUA_price_series$`EUA price`, na.rm=TRUE)

breaks_major <- seq(floor(y_min/10)*10, ceiling(y_max/10)*10, by = 10)

EUA_price_series_plot <- ggplot(data_plot_EUA_price_series, aes(x = Date, y = `EUA price`)) +
  # this shades the background according to each phase.
  geom_rect(data = rects,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = Phase),
            alpha = 0.3, inherit.aes = FALSE) +
  scale_fill_manual(values = c("Phase I"="grey80","Phase II"="white","Phase III"="grey80", "Phase IV"="white")) +
  
  geom_line(color = "steelblue", linewidth = 0.8) +
  # vertical lines:
  geom_vline(data = vertical_lines, aes(xintercept = as.numeric(date)),
             color = "tomato", linetype = "solid", linewidth = 0.5) +
  # axis labels:
  labs(x = NULL, y = "EUR/EUA") +
  # x axis scale with dates intervals:
  scale_x_date(
    # breaks 6 months:
    breaks = seq(from = as.Date(paste0(yr_min, "-01-01")),
                 to   = as.Date(paste0(yr_max, "-01-01")),
                 by   = "6 months"),
    date_labels = "%m-%Y"
  ) +
  scale_y_continuous(
    breaks = breaks_major
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),     # I don't want a vertical grid.
    panel.grid.minor = element_blank(),       # I take away the minor grid.
    panel.grid.major.y = element_line(color = "gray80", linewidth = 0.25),
    axis.text.x = element_text(angle = 90, hjust = 1),  # I rotated x axis labels.
    legend.position = "none" 
  )

print(EUA_price_series_plot)

# ggsave("EUA price - whole time series.jpg")