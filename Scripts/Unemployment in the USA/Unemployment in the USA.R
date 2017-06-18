# You will need packages ggplot2 and scales for this script to work
library(ggplot2)
attach(economics)
library(scales)

ch <- economics # we will use the dataset economics from the package ggplot2

#we divide the unemployed with the total population to get the ratio as the y axis
new.plot <- ggplot(ch, 
                   aes(x=ch$date,
                       y=ch$unemploy/ch$pop,
                       color = ch$unemploy/ch$pop))

# We will produce the graph with a x axis tick of each year
new.plot + (scale_x_date(breaks=date_breaks("2 years"), 
                         name="Year", 
                         labels=date_format("%b %y"))) + 
  # then we will give it a format of month and year and rotate it at 90 degrees
  theme(axis.text.x=element_text(angle=90,
                                 hjust=1)) +
  # we rotate the y axis 90 degrees as well
  theme(axis.text.y=element_text(angle=90,
                                 hjust=1)) + 
  # now we add the labels in our axes
  scale_y_continuous(name="Unemployed as % of US population", 
                     labels = percent, 
                     limits = c(0, 0.06), 
                     breaks = seq(0, 0.06, 0.01)) +
 # here we add the title and subtitle to our graph
  ggtitle("Unemployment in the USA", 
          subtitle = "Data from the Federal Reserve Bank of St. Louis from 1967 to 2015") +  
  geom_point(alpha = 1) + 
  # lets make the points have a gradient color according to their value
  scale_color_gradient(low = "black",
                       high = "tomato",
                       breaks = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06),
                       labels = c("0%", "1%", "2%", "3%", "4%", "5%", "6%"), 
                       guide = "colorbar",
                       limits = c(0,0.06)) +
  # here we will adjust the color legend
  guides(color = guide_colorbar(barwidth = 1, 
                                barheight = 15)) +
  # and we draw the line of the graph
  labs(color = "Unemployment") +
  geom_smooth(se = F, 
              color = "darkcyan", 
              size = 1.2, 
              method = 'loess') +
  # lets change some aspects of the theme
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent",color = NA),
        axis.line = element_line(color = "black"))
