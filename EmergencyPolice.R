# בס"ד

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# REQUIRED PACKAGES
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Quick and Easy Graphical Analysis
  library(flexplot)
# Data manipulation
  library(tidyverse)
# Web scraping
  library(rvest)
# Graphics that can deal with Hebrew text
  library(ragg)
  locale("he")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# IMPORT DATA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Incoming calls by region, city, day of week (Jan 1 - March 21, 2020)
day.place.1 <- read_csv("/Users/louisteitelbaum/Documents/moked_janfebmar.csv",
                        locale = locale(date_names = "he", encoding = "UTF-8"))

# Incoming calls by region, city, day of week (March 21 - April 30, 2020)
day.place.2 <- read_csv("/Users/louisteitelbaum/Documents/moked_marapr.csv",
                        locale = locale(date_names = "he", encoding = "UTF-8"))

# General Population Demographics
demographics <- read_csv("/Users/louisteitelbaum/Documents/demographics/demographics.csv")
demographics <- demographics[-(1:11), c(2, 8, 11)]
names(demographics) <- c("ir", "pop", "arabs")
demographics$pop[demographics$pop == "-"] <- 0
demographics$arabs[demographics$arabs == "-"] <- 0
demographics$pop <- as.numeric(gsub(",", "", demographics$pop))
demographics$arabs <- as.numeric(gsub(",", "", demographics$arabs))

# Incidents (unclear what this means exactly) by city, type, subtype, year, and quarter (2015-2017)
type.place.year <- read_csv("/Users/louisteitelbaum/Documents/moked100.csv")
type.place.year <- type.place.year[-(1:6), c(1:3, 5:6, 11, 16)]
names(type.place.year) <- c("ir", "type", "subtype", "total", "2015", "2016", "2017Q1-2")
type.place.year <- type.place.year %>%
  mutate(across(4:7, ~replace(., . == "$", "1"))) %>%
  mutate(across(4:7, ~replace(., . == "-", "0"))) %>%
  type_convert()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# WEB SCRAPING
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Number of synagogues by city in Israel
shuls_link <- url("https://www.kipa.co.il/%D7%91%D7%AA%D7%99-%D7%9B%D7%A0%D7%A1%D7%AA/%D7%91%D7%AA%D7%99-%D7%9B%D7%A0%D7%A1%D7%AA-%D7%9C%D7%A4%D7%99-%D7%A2%D7%99%D7%A8/", "rb")
shuls_page <- read_html(shuls_link)
shul_cities <- shuls_page %>% html_elements(".mikve-list a") %>% html_text()
shul_city_links <- shuls_page %>% html_elements(".mikve-list a") %>% html_attr("href")

get_shuls <- function(city_link){
  city_page <- read_html(city_link)
  nshuls <- city_page %>% html_elements("thead+ tbody .clickable-row td:nth-child(1)") %>% length()
  npages <- city_page %>% html_elements(".pager a") %>% html_text()
  npages <- max(na.omit(as.numeric(head(npages, -1))))
  if(npages > 0){
    last_page <- read_html(paste(city_link, "page/", npages, "/", sep = ""))
    nshuls_last <- last_page %>% html_elements("thead+ tbody .clickable-row td:nth-child(1)") %>% length()
    nshuls <- nshuls + 20*(npages-2) + nshuls_last
  }
  nshuls
}
    # I tried this with sapply and got an error 410 somewhere along the line. This loop is my way of sidestepping that error.
shuls <- tibble(ir = shul_cities,
                shuls = rep(NA, length(shul_city_links)))
for (city in 1:length(shul_cities)) {
  try(
    shuls$shuls[city] <- get_shuls(shul_city_links[city])
  )
}

# Number of mikvaot by city in Israel
mikvaot_link <- "https://www.kipa.co.il/%D7%9E%D7%A7%D7%95%D7%95%D7%90%D7%95%D7%AA/%D7%9E%D7%A7%D7%95%D7%95%D7%90%D7%95%D7%AA-%D7%9C%D7%A4%D7%99-%D7%A2%D7%99%D7%A8/"
mikvaot_page <- read_html(mikvaot_link)
mikva_cities <- mikvaot_page %>% html_elements(".synagog a") %>% html_text()
mikva_city_links <- mikvaot_page %>% html_elements(".synagog a") %>% html_attr("href")

get_mikvaot <- function(city_link){
  city_page <- read_html(city_link)
  nmikvaot <- city_page %>% html_elements("td:nth-child(1)") %>% length()
  npages <- city_page %>% html_elements(".pager a") %>% html_text()
  npages <- max(na.omit(as.numeric(head(npages, -1))))
  if(npages > 0){
    last_page <- read_html(paste(city_link, "page/", npages, "/", sep = ""))
    nmikvaot_last <- last_page %>% html_elements("thead+ tbody td:nth-child(1)") %>% length()
    nmikvaot <- nmikvaot + 20*(npages-2) + nmikvaot_last
  }
  nmikvaot
}
mikvaot <- tibble(ir = mikva_cities,
                  mikvaot = as.vector(sapply(mikva_city_links, FUN = get_mikvaot)))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# WRANGLE DATA (Incoming calls by region, city, day of week)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Variable names into English
names(day.place.1) <- c("mahoz", "hodesh", "ir", "yom", "n") -> names(day.place.2)

# Only cities for which we have info on all weekdays for each month
day.place.1 <- day.place.1 %>%
  group_by(mahoz, hodesh, ir) %>%
  filter(n() == 7) %>%
  ungroup()

day.place.2 <- day.place.2 %>%
  group_by(mahoz, hodesh, ir) %>%
  filter(n() == 7) %>%
  ungroup()

# Combine day.place.1 and day.place.2
day.place <- bind_rows(day.place.1, day.place.2)
rm(list = c("day.place.1", "day.place.2"))

# Remove summing rows
day.place <- day.place[day.place$mahoz != 'סה"כ' & day.place$mahoz != "Total", ]
day.place <- day.place[day.place$hodesh != "Total" & day.place$ir != "Total" & day.place$yom != "Total", ]

# Rename + Factorize days of the week
day.place <- day.place %>%
  mutate(yom = factor(yom, 
                       levels = c(unique(day.place$yom)[1], 
                                  unique(day.place$yom)[6], 
                                  unique(day.place$yom)[7], 
                                  unique(day.place$yom)[2],
                                  unique(day.place$yom)[5], 
                                  unique(day.place$yom)[4],
                                  unique(day.place$yom)[3]), 
                       labels = c("שבת", "ו", "ה", "ד", "ג", "ב", "א")))

# One case per weekday per city
day.place <- day.place %>%
  group_by(mahoz, ir, yom) %>%
  summarise(n = sum(n)) %>%
  ungroup()

day.place <- day.place[day.place$ir != "לא ידוע", ]

# Add variables to see proportion of calls coming in on each day
day.place <- day.place %>%
  left_join(demographics) %>%
  group_by(ir) %>%
  mutate(daypercent = 100*n/sum(n),
         callspercap = n/pop) %>%
  ungroup()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# VISUALIZE 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Giant faceted bar chart to see weekday patterns in each city
day.place %>%
  left_join(demographics) %>%
  filter(pop > 5000) %>%
  ggplot(aes(yom, daypercent, fill = (yom == "שבת") )) +
  geom_bar(stat = "identity") +
  facet_wrap(~ir) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "יום",
       y = "אחוז פניות",
       title = "2020 מינואר לאפריל 100 פניות למוקד ")

day.place[day.place$ir %in% unique(day.place$ir)[sample(1:56, 25)], ] %>%
  ggplot(aes(yom, daypercent, fill = (yom == "שבת") )) +
  geom_bar(stat = "identity") +
  facet_wrap(~ir) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "יום",
       y = "אחוז פניות",
       title = "2020 מינואר לאפריל 100 פניות למוקד ")

# Same but binned by estimated religious population
day.place %>%
  left_join(shuls) %>%
  filter(is.na(shuls) == F, is.na(pop) == F) %>%
  group_by(shuls/pop > 55/100000) %>%
  mutate(daypercent = 100*n/sum(n)) %>%
  ungroup() %>%
  ggplot(aes(yom, daypercent, fill = (yom == "שבת") )) +
  geom_bar(stat = "identity") +
  facet_wrap(~(shuls/pop > 55/100000)) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "יום",
       y = "אחוז פניות",
       title = "2020 מינואר לאפריל 100 פניות למוקד ")

# Are mikvaot and shuls a good indicator of religious Jewish population?
shuls %>%
  left_join(mikvaot) %>%
  left_join(demographics) %>%
  mutate(
    shulspercap = shuls/pop,
    mikvaotpercap = mikvaot/pop
  ) %>% 
  summarise(cor = cor(shulspercap, mikvaotpercap, use = "pairwise.complete.obs"),
            shuls2pop = mean(shuls/pop, na.rm = T))

shuls %>%
  left_join(demographics) %>%
  filter(ir != "ירושלים") %>%
  ggplot(aes(pop, shuls)) +
    geom_point(aes(color = (shuls/pop > 55/100000))) +
    geom_smooth(method = "lm", color = "black", alpha = 0, size = 1) +
    theme_minimal() + 
    theme(legend.position = "none") +
    scale_x_continuous(breaks = c(100000, 200000), labels = c("100,000", "200,000")) + 
    labs(x = "אוכלוסייה",
         y = "מספר בתי כנסת בעיר")

# Does more religious Jewish population predict a lower proportion of 100 calls on shabbat?
shab.place <- day.place %>%
  left_join(demographics) %>%
  filter(pop > 1000) %>%
  left_join(shuls) %>%
  left_join(mikvaot) %>%
  mutate(
    shulspercap = shuls/pop,
    mikvaotpercap = mikvaot/pop,
    peoplepershul = pop/shuls,
    peoplepermikva = pop/mikvaot,
    arabpercent = 100*arabs/pop
  ) %>%
  filter(yom == "שבת")

flexplot(shuls ~ pop, data = shab.place[shab.place$ir != "ירושלים",], method = "lm")
cor(shab.place$mikvapercap, shab.place$shulpercap, method = "pearson")

# Sanity-check: do our two data sets correlate? (one is 2015-2017 and one is 2020)

type.place.year %>%
  filter(type == "Total") %>%
  right_join(day.place) %>%
  group_by(ir) %>%
  summarise(n = sum(n), total = mean(total)) %>%
  ungroup() %>%
  summarise(cor = cor(n, total, use = "pairwise.complete.obs"))
      # r = 0.95 OK that checks out

# What types are included in the second dataset?
  unique(type.place.year$type[type.place.year$type != "Total"])
  # "תנועה"
  # "איכות חיים"
  # "סדר ציבורי"
  # "אלימות"
  # "סמים ואלכוהול"
  unique(type.place.year$subtype[type.place.year$subtype != "Total"])
  # "הסגת גבול - חניה פרטית" 
  # "חסימת מעבר"             
  # "רכב חונה באיסור"
  # "גרימת רעש מבית"
  # "גרימת רעש מבית תפילה"   
  # "גרימת רעש ממקום ציבורי"
  # "גרימת רעש מעסק"
  # "אדם/חנ"ש/נרקומן משתולל"
  # "סכסוך - שכנים"
  # "סכסוך שכנים עסקי"
  # "שכרות"                  

type.place.year %>%
  filter(type != "Total", subtype == "Total", ir %in% c("ירושלים", "תל אביב -יפו", "חיפה")) %>%
  ggplot(aes(type, total)) +
    geom_bar(stat = "identity") + 
    facet_wrap(~ir)

