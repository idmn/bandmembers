library(rvest)
library(dplyr)
library(purrr)
library(stringr)

page_zero <- "https://en.wikipedia.org/wiki/Category:Lists_of_members_by_band"
wiki_root <- "https://en.wikipedia.org/"

bands <- page_zero %>% 
    read_html() %>% 
    html_nodes(xpath = "//div[@class = 'mw-category-group']/ul/li/a") %>% 
    html_attrs() %>%
    transpose() %>%
    .[c("href", "title")] %>% 
    as_data_frame() %>% 
    mutate_all(unlist) %>% 
    mutate(name = str_remove_all(title, "^List of | members$")) %>% 
    select(name, href)

href <- bands$href[[1]]

get_timeline <- function(href, pause = 0) {
    # just not to irritate wikipedia to much
    Sys.sleep(pause)
    
    timeline_href <- href %>% 
        str_c(wiki_root, .) %>% 
        read_html() %>% 
        html_nodes(xpath = "//a[@title = 'Edit section: Timeline']") %>% 
        html_attr("href")
    
    timeline_code <- read_html(str_c(wiki_root, timeline_href)) %>% 
        html_node(xpath = "//textarea") %>% 
        html_text()
    
    # parse timeline_code
}


# timeline plot script
# need to parse it
scr <- timeline_href %>% 
    str_c(wiki_root, .) %>%
    read_html() %>% 
    html_node(xpath = "//textarea") %>% 
    html_text()

scr_s <- scr %>%
    str_split("\n") %>% 
    .[[1]] 

tmp <- scr %>%
    # split assignment blocks - statements with single equality sign
    str_split("\n(?=\\s*\\w+\\s*=)") %>% 
    .[[1]] %>% 
    str_subset("\\w+\\s*=\\s*\\w+") %>% 
    # separate what's before and after the = sign
    str_split("\\s*=\\s*")

# convert to list
tmp_l <- map(tmp, 2)
names(tmp_l) <- map(tmp, 1)

# complicated case
tt <- tmp_l[[13]]
cat(tt)

tt_s <- tt %>%
    str_split("\\s+") %>% 
    .[[1]] %>% 
    str_subset(":") %>% 
    str_split(":")

tt_l <- map(tt_s, 2)
names(tt_l) <- map(tt_s, 1)

# process tt_s



# (!)
# graph of shared members
# most/least changed/...
