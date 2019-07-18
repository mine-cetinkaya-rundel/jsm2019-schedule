# load packages -----------------------------------------------------
library(tidyverse)
library(rvest)
library(glue)

# read data ---------------------------------------------------------
jsm2019_html <- read_html("raw-data/jsm2019.htm")
jsm_html <- jsm2019_html

# read pd data ---------------------------------------------------------
jsm2019_pd_html <- read_html("raw-data/jsm2019-pd.htm")
jsm_pd_html <- jsm2019_pd_html

# scrape session schedule -------------------------------------------

dates_times <- jsm_html %>%
  html_nodes("td tr:nth-child(1) td:nth-child(2) b") %>%
  html_text() %>%
  trim() %>%
  str_replace_all("\n", " ") %>%
  str_trim()

locations <- jsm_html %>%
  html_nodes("td~ td+ td b") %>%
  html_text()

sessions_types <- jsm_html %>%
  html_nodes("tr:nth-child(2) b") %>%
  html_text()

ids <- jsm_html %>%
  html_nodes("#bottom br+ a") %>%
  html_text() %>%
  str_remove("!") %>%
  str_remove("!$") %>%
  str_remove("\\*") %>%
  str_trim()

urls <- jsm_html %>%
  html_nodes("#bottom br+ a") %>%
  html_attr("href")

sponsors <- jsm_html %>%
  html_nodes("tr:nth-child(3) td") %>%
  html_text() %>%
  str_trim() %>%
  str_replace_all("  ,|   ,| ,", ",")

jsm_sessions_raw <- tibble(
  date_time = dates_times,
  location = locations,
  id = ids,
  session_type = sessions_types,
  url = urls,
  sponsor = sponsors
)

# get professional development URLs --------------------------------------------

pd_sessions <- jsm_pd_html %>%
  html_nodes("tr:nth-child(2) b") %>%
  html_text() %>%
  str_remove(" — (.*)")

pd_urls <- jsm_pd_html %>%
  html_nodes("tr+ tr a") %>%
  html_attr("href")

pd <- tibble(
  session = pd_sessions,
  url = pd_urls
)

# jsm_sessions -----------------------------------------------------------------

jsm_sessions <- jsm_sessions_raw %>%
  # separate columns
  separate(date_time, into = c("day", "date", "time"), sep = ", ") %>%
  separate(time, into = c("beg_time", "end_time"), sep = " - ", remove = FALSE) %>%
  separate(session_type, into = c("session", "type"), sep = " \u2014 ") %>%
  # manual fixes
  mutate(
    # has fee
    has_fee = str_detect(session, "(ADDED FEE)"),
    # reduce type levels
    type = case_when(
      str_detect(type, "Roundtable")               ~ "Roundtable",
      str_detect(type, "Professional Development") ~ "Professional Development",
      str_detect(type, "Other")                    ~ "Other",
      TRUE                                         ~ type
    ),
    # compose URLs
    url = case_when(
      type == "Professional Development"           ~ NA_character_,
      type == "Roundtable"                         ~ "https://ww2.amstat.org/meetings/jsm/2019/onlineprogram/index.cfm",
      TRUE                                         ~ url
    ),
    # civilian to military time
    beg_time_round = format(strptime(beg_time, "%I:%M %p"), format = "%H:%M:%S") %>% str_remove("\\:.+") %>% as.numeric(),
    end_time_round = format(strptime(end_time, "%I:%M %p"), format = "%H:%M:%S") %>% str_remove("\\:.+") %>% as.numeric(),
    end_time_round = ifelse(str_detect(end_time, "\\:[1-5]"), end_time_round+1, end_time_round),
    # for convenience, fix dance party end time
    end_time_round = ifelse(id == "218966", 23, end_time_round)
  ) %>%
  # fix prof dev URLs
  left_join(pd, by = "session") %>%
  mutate(url = ifelse(is.na(url.x) , url.y, url.x)) %>%
  # select columns
  select(day, date, time, beg_time, end_time, location, id, session, type, url, sponsor, has_fee, beg_time_round, end_time_round)

write_csv(jsm_sessions, path = "app-data/jsm2019_sessions.csv")

# scrape talk info -------------------------------------------------

titles <- jsm_html %>%
  html_nodes("tr+ tr td+ td a") %>%
  html_text()

urls <- jsm_html %>%
  html_nodes("tr+ tr td+ td a") %>%
  html_attr("href")

jsm_talks_raw <- tibble(
  title = titles,
  url = urls
)

jsm_talks <- jsm_talks_raw %>%
  mutate(has_fee = str_detect(title, "(ADDED FEE)"))

write_csv(jsm_talks, "app-data/jsm2019_talks.csv")

