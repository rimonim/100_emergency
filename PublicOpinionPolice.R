library(tidyverse)
# Graphics that can deal with Hebrew text
library(ragg)
locale("he")

d = tibble(
  year = 2003:2021,
  calls = c(NA, NA, NA,
            NA, NA, NA, 10087191, mean(8411990,8463645),
            8081250, mean(8462931, 8455278, 8455278), mean(9208890, 9218107, 9215914), mean(8230516, 8228476), 7452941,
            6907081, 7062772, 8287527, 8815017, 9283051,
            9265746),
  calls_110 = c(NA, NA, NA,
                NA, NA, NA, NA, NA,
                NA, NA, NA, NA, 375498,
                857826, 1600345, 1825573, 1952490, 2459646,
                2726681),
  answered = c(NA, NA, NA,
               NA, NA, NA, mean(940892, 1470314), mean(1029933, 1625261),
               1781898, 1849291, mean(2111214, 1978657), mean(2286854, 2072764), mean(2153435, 2165956),
               2298233, 2340883, 2563693, 2606551, 2820671,
               2848259),
  percent_trust_jews = c(67.5, 66.9, 56.5,
                         42.9, 40.3, 31.3, 40.3, 44.6, 
                         59.1, 60.6, 61.9, 44.4, 42.2, 
                         41.9, 42, 52.3, 44.4, mean(41, 44.3),
                         mean(41.9, 33.5)),
  percent_trust_arabs = c(56.5, 60.8, 53.5,
                          48.9, 39.6, 33, 17.8, 22.9, 
                          38.9, 61.9, 43.5, 59.5, 43.8, 
                          27.3, 29.4, 17.9, 38.3, mean(32.9, 25.8),
                         mean(13.3, 22.1)),
  population = c(6689700, 6809000, 6930100,
                 7053700, 7180100, 7308800, 7485600, 7623600, 
                 7765800, 7910500, 8059500, 8215700, 8380100, 
                 8546000, 8713300, 8882800, 9054000, 9215100, 
                 9364000)
)

d %>% 
  ggplot(aes(year)) +
  geom_area(aes(y = calls/population, fill = "100שיחות נכנסות למוקד "), alpha = .8) +
  geom_area(aes(y = answered/population, fill = "שיחות שקיבלו מענה משטרתי"), alpha = .8) +
  geom_line(aes(y = percent_trust_jews/75, color = "יש אמון במשטרה) ?יהודים("), linewidth = 2) +
  geom_line(aes(y = percent_trust_arabs/75, color = "יש אמון במשטרה) ?ערבים("), linewidth = 2) +
  theme_minimal() +
  labs(x = "שנה") + 
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("lightgrey", "darkgrey")) +
  scale_x_continuous(limits = c(2009, 2021)) +
  scale_y_continuous("מספר אירועים ביחס לאוכלוסיה", 
                     sec.axis = sec_axis(~ .*75, name = "אחוז המדווחים על אמון במשטרה"))
