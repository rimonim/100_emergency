library(tidyverse)
library(rvest)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# WEB SCRAPING
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# I downloaded the reel pages in advance since the Internet Archive is slow and likes rerouting
# https://web.archive.org/web/20150417090925/http://www.911dispatch.com/tape-library/
raw_html_list <- list.files("data/911_reels/", full.names = TRUE)
get_metadata <- function(reel_html){
  reel_page <- read_html(reel_html)
  event_number <- html_elements(reel_page, xpath = '//div[2]/ul/li') %>% length()
  event_list <- paste0("//div[2]/ul/li[", 1:event_number, "]/")
  bind_rows(
    lapply(event_list, function(xpath){
      link <- html_elements(reel_page, xpath = paste0(xpath, "a")) %>% as.character() %>% str_extract('(?<=href=").+(?=")')
      title <- html_elements(reel_page, xpath = paste0(xpath, "a")) %>% as.character() %>% str_extract('(?<=>).+(?=</a>)')
      description <- html_elements(reel_page, xpath = paste0(xpath, "text()")) %>% as.character()
      tibble(link = link[1], title = title[1], description = description[1])
    })
  )
}
recordings_metadata <- bind_rows(lapply(raw_html_list, get_metadata))
recordings_metadata$link <- str_remove_all(recordings_metadata$link, '" target="_blank')
recordings_metadata$link <- str_remove_all(recordings_metadata$link, '" target="blank')
recordings_metadata <- recordings_metadata %>% mutate(id = 1:nrow(.),
                                                      year = NA,
                                                      state = NA,
                                                      civilian_initiated = NA,
                                                      deaths = NA,
                                                      potential_death = NA,
                                                      false_alarm = NA)

# DOWNLOAD MP3s
for (i in 1:nrow(recordings_metadata)) {
  if (str_detect(recordings_metadata$link[i], ".mp3")){
    destfile <- paste0("data/911_recordings/call_", recordings_metadata$id[i], ".mp3")
    try(
    download.file(recordings_metadata$link[i], 
                  destfile,
                  mode="wb")
    )
  }
}

# list successfully downloaded ids
downloaded_ids <- list.files("data/911_recordings/") %>% 
  str_remove("call_") %>% 
  str_remove(".mp3") %>% 
  as.integer()

recordings_metadata <- recordings_metadata %>% 
  mutate(file_downloaded = id %in% downloaded_ids)

# Write CSV
write_csv(recordings_metadata, "data/911_reels.csv", na = "")

