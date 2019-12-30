library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(magrittr)
library(polite)
library(furrr)


page_zero <- "https://en.wikipedia.org/wiki/Category:Lists_of_members_by_band"

bandmembers <- 
  page_zero %>%
  # honestly don't know if it makes sense
  bow() %>% 
  scrape() %>% 
  html_nodes(xpath = "//div[@class = 'mw-category-group']/ul/li/a") %>% 
  html_attrs() %>%
  transpose() %>%
  .[c("href", "title")] %>% 
  as_tibble() %>% 
  mutate_all(unlist) %>% 
  mutate(name = str_remove_all(title, "^List of | members$")) %>% 
  select(name, href)

get_timeline_code <- function(href, wait = 0) {
  Sys.sleep(wait)
  timeline_href <- 
    href %>% 
    wiki_url() %>%  
    read_html() %>% 
    # could be 2 such nodes - looks like duplicated
    html_node(xpath = "//a[@title = 'Edit section: Timeline']") %>% 
    html_attr("href")
  
  if (length(timeline_href) == 0) return(NA_character_)
  
  timeline_code <- 
    timeline_href %>% 
    wiki_url() %>%
    read_html() %>% 
    html_node(xpath = "//textarea") %>% 
    html_text()
  
  if (!is.character(timeline_code) & length(timeline_code) != 1) return(NA_character_)
  return(timeline_code)
}


# (!) takes some time
bandmembers <-
  bandmembers %>% 
  mutate(timeline_code = map_chr(href, possibly(get_timeline_code, otherwise = "ERROR"), wait = .2))

library(tidyr)
bandmembers0 <-
  bandmembers %>% 
  rename(band = name) %>% 
  filter(timeline_code != "ERROR") %>% 
  mutate(plot_data = map(timeline_code, possibly(get_plot_data, otherwise = NULL))) %>% 
  unnest() 

# mentioned in most bands
bandmembers0 %>% 
  select(band, name) %>% 
  unique() %>%
  group_by(name) %>% 
  summarise(
    bands = paste(band, collapse = ", "),
    n     = n()
  ) %>% 
  arrange(desc(n))

# most distinct members
bandmembers0 %>% 
  select(band, name) %>% 
  unique() %>% 
  count(band, sort = TRUE)

# longest duration
band_dur <- 
  bandmembers0 %>% 
  group_by(band) %>% 
  summarise(
    from = min(from, na.rm = TRUE),
    till = max(till, na.rm = TRUE)
  ) %>% 
  mutate(dur = till - from) %>% 
  arrange(desc(dur))

# shortest duration
band_dur %>% 
  arrange(dur)

# washboard ???
# TODO unify roles
bandmembers0 %>% 
  count(role, sort = TRUE) %>% 
  View

# most intersecting members

# longest gap
#bandmembers0 %>% 
  
# most variable dude
bandmembers0 %>% 
  select(name, role) %>% 
  unique() %>% 
  count(name, sort = TRUE)

bandmembers0 %>% 
  filter(name == "Glenn Danzig") %>% 
  View

# most changed position
bandmembers0 %>% 
  count(band, role, sort = TRUE)

# COMBAK remove _short variables?
bandmembers0 %>% 
  select(name, name_short) %>% 
  unique() %>% 
  count(name, sort = TRUE)

bandmembers0 %>% 
  filter(name == "Bob Daisley")


bandmembers0 %>% 
  select(role, role_short) %>% 
  unique() %>% 
  count(role, sort = TRUE)

bandmembers0 %>% 
  filter(role == "Keyboards") %>% 
  select(role, role_short) %>% 
  unique()



#########################
# (!)
# graph of shared members
# member of most groups - liked highlight his lines
# most/least changed/...
# most common/uncommon positions
# biggest break among members/groups -> top 5 breaks - minicharts tooltips

# preprocessing (unification also) goes to the first part

# (?)
# https://en.wikipedia.org/wiki/List_of_The_Jackson_5_band_members nothing here
