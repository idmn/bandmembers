# will be used in the get_timeline function
parse_code_block <- function(s) {
  item_starters <- c("id", "bar", "at")
  
  sl <- 
    str_split(s, "\n")[[1]] %>% 
    str_trim() %>% 
    discard(~. %in% c("", "}}")) %>% 
    str_split("\\s+(?=[[:alnum:]]+:)") %>% 
    map(~str_split(., ":", 2)) %>%
    map(~set_names(map(., 2), nm = map(.,1)))
  
  sdf <- 
    data_frame(line = sl) %>% 
    mutate(first = map_chr(line, ~names(.)[[1]])) %>% 
    mutate(item = (first %in% item_starters)) %>%
    # group is >= 1 global parameter defition lines (item == FALSE)
    #   followed by >= 1 item definitions (item == TRUE)
    # following is just a lazy smart-ass way to encode it
    mutate(group = cumsum(c(0, diff(item)) == -1))
  
  # collapse global params in each group
  # locf where missing
  sdf_g <- 
    sdf %>%
    filter(!item) %>% 
    group_by(group) %>% 
    summarise(g_line = list(flatten(line)))
  
  sdf_g$g_line <- 
    sdf_g$g_line %>% 
    bind_rows() %>% 
    mutate_all(zoo::na.locf) %>% 
    transpose()
  
  sdf_i <-
    sdf %>%
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

# TODO optimise - this works slow
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
