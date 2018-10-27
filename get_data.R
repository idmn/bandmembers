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