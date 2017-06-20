# You will need packages ggplot2 and scales for this script to work
library(ggplot2)
library(scales)

ch <- ggplot2::economics # we will use the dataset economics from the package ggplot2

#we divide the unemployed with the total population to get the ratio as the y axis
new.plot <- ggplot(ch,
                   aes(x = ch$date,
                       y = ch$unemploy/ch$pop))

# We will produce the graph with x axis ticks every 2 years
new.plot + (scale_x_date(breaks = date_breaks("2 years"),
                         name = "Year",
                         labels = date_format("%b '%y"))) +

  # and rotate it at 90 degrees
  theme(axis.text.x = element_text(angle = 30,
                                   hjust = 1)) +

  # now we format our y axis
  scale_y_continuous(name = "Unemployed as % of US population",
                     labels = percent,
                     limits = c(0, 0.06),
                     breaks = seq(0, 0.06, 0.01)) +

 # here we add the title and subtitle to our graph
  ggtitle("Unemployment in the USA",
          subtitle = "Data from the Federal Reserve Bank of St. Louis from 1967 to 2015") +
  geom_point(alpha = 1)  +

  # here we draw the line of the graph
  geom_smooth(se = F,
              color = "darkcyan",
              size = 1.2,
              method = 'loess') +

  # lets change some visual aspects of the theme
  theme(panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "gray"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        axis.line = element_line(color = "black"))