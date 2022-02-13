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

# American data from Charleston, Detroit, New Orleans, and Dallas 
# (sampled for faster computation - the dataset is 6,347,478 rows long):   
set.seed(100)
american <- read_csv("/Users/louisteitelbaum/Documents/all_calls.csv")[sample(1:6347478, 20000), -(1:26)]

seattle <- read_csv("/Users/louisteitelbaum/Downloads/Seattle.csv.zip")[sample(1:4206691, 20000),]
charleston <- read_csv("/Users/louisteitelbaum/Downloads/Charleston.csv.zip")[sample(1:708701, 20000),]

  # Summaries (from Data Clinic)
calltypes.sum <- read_csv("/Users/louisteitelbaum/Documents/CallTypes/CallTypes-Table 1.csv")
disposition.sum <- read_csv("/Users/louisteitelbaum/Documents/CallTypes/Disposition-Table 1.csv")

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

## For the sake of reasonable priors, let's look at some patterns in the American 911 data
  #1. What percentage of 911 calls are citizen-initiated?
american %>% 
  filter(city %in% c("NewOrleans")) %>%
  group_by(self_initiated, city) %>% 
  summarise(freq = n()/nrow(.))
      #> No (Citizen-initiated):  0.443
      #> Yes (Police-initiated):  0.328
      #> Other:                   0.228

  #2. What percentage of citizen-initiated calls got a response on the ground?
american %>%
  filter(city == "NewOrleans", self_initiated %in% c("No", "other")) %>%
  mutate(activeresponse = recode(disposition, 
                                 "Unfounded" = F, 
                                 "Report Generated" = F, 
                                 "Repeat" = F, 
                                 "Non-Arrest Activity" = T, 
                                 "Enforcement Activity" = T)) %>%
  group_by(activeresponse)%>%
  summarise(freq = n()/nrow(.))

seattle %>%
  filter(self_initiated %in% c("No", "other")) %>%
  mutate(activeresponse = recode(disposition, 
                                 "Unfounded" = F, 
                                 "Report Generated" = F, 
                                 "Repeat" = F, 
                                 "Non-Arrest Activity" = T, 
                                 "Enforcement Activity" = T,
                                 "Warning Issued" = T,
                                 "Cancellation/Withdrawn" = F,
                                 "False Complaint" = F,
                                 "Citation Issued" = T,
                                 "Location Error" = F,
                                 "Arrest Issued" = T,
                                 "Linked Incident" = T,
                                 "Detaining Individual" = T)) %>%
  group_by(activeresponse)%>%
  summarise(freq = n()/nrow(.))

  #3. Breakdown of outcomes of incoming calls
american %>%
  filter(is.na(disposition) == F, disposition != "Unknown", self_initiated %in% c("No", "other")) %>%
  group_by(disposition) %>%
  summarise(percent = 100*n()/nrow(.)) %>%
  arrange(desc(percent))

american %>%
  filter(is.na(disposition) == F, disposition != "Unknown", self_initiated %in% c("No", "other")) %>%
  group_by(disposition) %>%
  summarise(percent = 100*n()/nrow(.)) %>%
  arrange(desc(percent)) %>%
  ungroup() %>%
  mutate(disposition = factor(disposition, levels = disposition)) %>%
  ggplot(aes(disposition, percent, fill = disposition)) +
    geom_bar(stat = "identity") +
    scale_fill_brewer(palette = "Spectral") +
    theme_minimal() +
    labs(title = "Outcome of 911 Non-Police-Initiated Calls", subtitle = "New Orleans and Dallas", x = "", y = "Percent of Total Cases") +
    theme(plot.title = element_text(hjust = .5), plot.subtitle = element_text(hjust = .5), legend.position = "none")

  #4. Breakdown of incident types
american %>%
  filter(is.na(call_type) == F, self_initiated %in% c("No", "other")) %>%
  group_by(call_type) %>%
  summarise(percent = 100*n()/nrow(.)) %>%
  arrange(desc(percent)) %>%
  ungroup() %>%
  mutate(call_type = factor(call_type, levels = call_type)) %>%
  ggplot(aes(call_type, percent, fill = call_type)) +
  geom_bar(stat = "identity") +
  guides(x = guide_axis(angle = 75)) +
  theme_minimal() +
  labs(title = "Incident Types of 911 Calls", subtitle = "New Orleans and Detroit", x = "", y = "Percent of Total Cases") +
  theme(plot.title = element_text(hjust = .5), plot.subtitle = element_text(hjust = .5), legend.position = "none")

seattle %>%
  filter(is.na(call_type) == F, self_initiated %in% c("No", "other")) %>%
  group_by(call_type) %>%
  summarise(percent = 100*n()/nrow(.)) %>%
  arrange(desc(percent)) %>%
  ungroup() %>%
  mutate(call_type = factor(call_type, levels = call_type)) %>%
  ggplot(aes(call_type, percent, fill = call_type)) +
  geom_bar(stat = "identity") +
  guides(x = guide_axis(angle = 75)) +
  theme_minimal() +
  labs(title = "Incident Types of 911 Calls", subtitle = "Seattle", x = "", y = "Percent of Total Cases") +
  theme(plot.title = element_text(hjust = .5), plot.subtitle = element_text(hjust = .5), legend.position = "none")

    # What are the behavioral health problems?
View(calltypes.sum %>%
  filter(Category == "Behavioral Health") %>%
  group_by(City) %>%
  summarise(n = sum(Count), 
            subcat = `Field 1` ,
            perc = 100*Count/sum(Count)))

  #5. Maybe we can learn more from the overlap
american %>%
  mutate(call_type = str_wrap(call_type, width = 18)) %>%
  filter(is.na(disposition) == F, 
         is.na(call_type) == F,
         disposition != "Unknown", 
         self_initiated %in% c("No", "other")) %>%
  group_by(disposition)%>% summarise(perc = 100*n()/nrow(.))
ggplot(aes(x = "", fill = disposition)) +
    facet_wrap(~call_type, nrow = 4) +
    geom_bar(position = "fill") +
    scale_fill_brewer(name = "Outcome", palette = "Paired") +
    coord_polar("y") +
    theme_void()

seattle %>%
  mutate(call_type = str_wrap(call_type, width = 18)) %>%
  filter(is.na(disposition) == F, 
         is.na(call_type) == F,
         disposition != "Unknown", 
         self_initiated %in% c("No", "other")) %>%
  group_by(disposition)%>% summarise(perc = 100*n()/nrow(.))
  ggplot(aes(x = "", fill = disposition)) +
  facet_wrap(~call_type, nrow = 4) +
  geom_bar(position = "fill") +
  coord_polar("y") +
  theme_void() +
  labs(title = "Incident Outcomes by Type in Seattle") +
  theme(plot.title = element_text(hjust = .5) , legend.title = element_blank())

  #6. How often do initial IDs get changed?
      ## Seattle raw data from https://data.seattle.gov/Public-Safety/Call-Data/33kz-ixgy
seattle_raw <- read_csv("/Users/louisteitelbaum/Downloads/Call_Data_Seattle.csv")[sample(1:4842110, 20000),]

seattle_raw %>%
  group_by(`Initial Call Type`, `Final Call Type`) %>%
  summarise() %>% 
  group_by(`Initial Call Type`) %>%
  summarise(ntypes = n()) %>%
  ggplot(aes(ntypes)) +
    geom_histogram(binwidth = 1) + 
    labs(title = "Number of Unique Final Call Types Per Initial Call Type", 
         subtitle = "Seattle",
         x = "Number of Unique Final Call Types",
         y = "Count of Initial Call Types") +
    theme_minimal()

seattle_raw %>%
  group_by(`Initial Call Type`, `Final Call Type`) %>%
  summarise() %>% 
  group_by(`Final Call Type`) %>%
  summarise(ntypes = n()) %>%
  ggplot(aes(ntypes)) +
  geom_histogram(binwidth = 1) + 
  labs(title = "Number of Unique Initial Call Types Per Final Call Type", 
       subtitle = "Seattle",
       x = "Number of Unique Initial Call Types",
       y = "Count of Final Call Types") +
  theme_minimal()

### IRAELI DATA

# Weekday patterns from 2020 shnaton
weekdays <- tibble(type = c("תנועה", 
                            "איכות חיים", 
                            "אלימות", 
                            "חירום וסכנות לציבור", 
                            "בטחון", 
                            "פעילות משטרתית", 
                            "רכוש", 
                            "סדר ציבורי", 
                            "סמים ואלכוהול", 
                            "מין", 
                            "עבירות ברשת", 
                            "היסטורי", 
                            "פח'ע"),
                   א = c(106065, 38568, 43801, 57382, 20330, 83383, 44352, 5345, 6002, 1577, 127, 18, 6276),
                   ב = c(106998, 34000, 43729, 56949, 20217, 84631, 42905, 5681, 5903, 1478, 79, 13, 5245),
                   ג = c(111376, 36151, 44429, 57119, 20393, 83880, 44268, 5730, 5821, 1449, 113, 17, 5096),
                   ד = c(113958, 36685, 44582, 59439, 19669, 86445, 44976, 5272, 5994, 1467, 101, 22, 2031),
                   ה = c(122195, 49062, 46911, 60205, 19652, 85625, 46262, 6653, 5740, 1506, 110, 23, 5132),
                   ו = c(100986, 58084, 44784, 56144, 14522, 66167, 40251, 5955, 5099, 1265, 77, 12, 3158),
                   שבת = c(61737, 52357, 38406, 47621, 12583, 58115, 28104, 7552, 4471, 1075, 59, 10, 2439))
weekdays <- weekdays %>%
  pivot_longer(2:8, names_to = "day", values_to = "n") %>%
  mutate(day = factor(day, levels = c("שבת", "ו", "ה", "ד", "ג", "ב", "א")))
options(scipen=10000)

ggplot(weekdays, aes(day, n, fill = type)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "יום",
       y = "מספר אירועים")

weekdays %>%
  mutate(shabbat = recode(if_else(day == "שבת", "Y", "N"), "Y" = "שבת", "N" = "ימות השבוע")) %>%
  group_by(shabbat, type) %>%
  summarise(n = sum(n)) %>%
  group_by(shabbat) %>%
  summarise(type = type, prop = n/sum(n)) %>%
  ggplot(aes(x = "", prop, fill = type)) +
    geom_bar(stat = "identity", color = "white") +
    facet_wrap(~shabbat) +
    theme_minimal() +
    theme(axis.text.y = element_blank(), axis.title = element_blank(), legend.title = element_blank())

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
  summarise(cor = cor(shulspercap, mikvaotpercap, use = "pairwise.complete.obs"))

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
require(MASS)

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
  ) %>%
  filter(yom == "שבת")

mikvamod <- lm(daypercent~peoplepermikva, data = shab.place)
visualize(mikvamod)
summary(mikvamod)
robustmikvamod <- rlm(daypercent~peoplepermikva, data = shab.place)
summary(robustmikvamod)
compare.fits(daypercent~peoplepermikva, data = shab.place, mikvamod, robustmikvamod)


flexplot(daypercent~peoplepermikva, data = shab.place[shab.place$ir != "ירושלים",])

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

