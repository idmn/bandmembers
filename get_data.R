library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(magrittr)
library(polite)
library(furrr)


page_zero <- "https://en.wikipedia.org/wiki/Category:Lists_of_members_by_band"


# wiki_url(c("blablabla", page_zero))
wiki_url <- function(x) {
    wiki_root <- "https://en.wikipedia.org"
    loc <- str_sub(x, 1, nchar(wiki_root)) != wiki_root
    x[loc] <- paste(wiki_root, x[loc], sep = "/")
    x
}


bandmembers <- page_zero %>%
    # honestly don't know if it makes sense
    bow() %>% 
    scrape() %>% 
    html_nodes(xpath = "//div[@class = 'mw-category-group']/ul/li/a") %>% 
    html_attrs() %>%
    transpose() %>%
    .[c("href", "title")] %>% 
    as_data_frame() %>% 
    mutate_all(unlist) %>% 
    mutate(name = str_remove_all(title, "^List of | members$")) %>% 
    select(name, href)


get_timeline_code <- function(href, wait = 0) {
    Sys.sleep(wait)
    timeline_href <- href %>% 
        wiki_url() %>%  
        read_html() %>% 
        # could be 2 such nodes - looks like duplicated
        html_node(xpath = "//a[@title = 'Edit section: Timeline']") %>% 
        html_attr("href")
    
    if (length(timeline_href) == 0) return(NA_character_)
    
    timeline_code <- timeline_href %>% 
        wiki_url() %>%
        read_html() %>% 
        html_node(xpath = "//textarea") %>% 
        html_text()
    
    if (!is.character(timeline_code) & length(timeline_code) != 1) return(NA_character_)
    return(timeline_code)
}


# (!) takes some time
bandmembers <- bandmembers %>% 
    mutate(timeline_code = map_chr(href, possibly(get_timeline_code, otherwise = "ERROR"), wait = .2))



    


# will be used in the get_timeline function
parse_code_block <- function(s) {
    item_starters <- c("id", "bar", "at")
    
    sl <- str_split(s, "\n")[[1]] %>% 
        str_trim() %>% 
        discard(~. %in% c("", "}}")) %>% 
        str_split("\\s+(?=[[:alnum:]]+:)") %>% 
        map(~str_split(., ":", 2)) %>%
        map(~set_names(map(., 2), nm = map(.,1)))
    
    sdf <- data_frame(line = sl) %>% 
        mutate(first = map_chr(line, ~names(.)[[1]])) %>% 
        mutate(item = (first %in% item_starters)) %>%
        # group is >= 1 global parameter defition lines (item == FALSE)
        #   followed by >= 1 item definitions (item == TRUE)
        # following is just a lazy smart-ass way to encode it
        mutate(group = cumsum(c(0, diff(item)) == -1))
    
    # collapse global params in each group
    # locf where missing
    sdf_g <- sdf %>%
        filter(!item) %>% 
        group_by(group) %>% 
        summarise(g_line = list(flatten(line)))
    
    sdf_g$g_line <- sdf_g$g_line %>% 
        bind_rows() %>% 
        mutate_all(funs(zoo::na.locf)) %>% 
        transpose()
    
    sdf_i <- sdf %>%
        filter(item) %>% 
        select(group, i_line = line)
    
    full_join(sdf_i, sdf_g, by = "group") %>%
        # first look at the global param
        # which could be overwritten in item definition
        mutate(line = map2(g_line, i_line, c)) %>% 
        pull(line) %>% 
        bind_rows() %>%
        # remove redundant outer ""
        mutate_all(function(x) {
            pos <- str_sub(x, 1, 1) == "\"" & str_sub(x, -1, -1) == "\""
            x[pos] <- str_sub(x[pos], 2, -2)
            x
        }) %>% 
        mutate_all(str_trim)
}


parse_timeline_code <- function(x) {
    code_blocks_0 <- x %>%
        # split assignment blocks - statements with single equality sign
        str_split("\n(?=\\s*\\w+\\s*=)") %>% 
        .[[1]] %>% 
        str_subset("\\w+\\s*=\\s*\\w+") %>% 
        # separate what's before and after the = sign
        str_split("\\s*=\\s*")
    
    code_blocks <- map(code_blocks_0, 2)
    names(code_blocks) <- map(code_blocks_0, 1)
    
    code_dfs <- map(code_blocks, possibly(parse_code_block, NULL))
    where_null <- map_lgl(code_dfs, is.null)
    code_dfs[where_null] <- code_blocks[where_null]
    code_dfs
}








#########################
# (!)
# graph of shared members
# most/least changed/...
# most common/uncommon positions
# biggest break among members/groups

# (?)
# https://en.wikipedia.org/wiki/List_of_The_Jackson_5_band_members nothing here
