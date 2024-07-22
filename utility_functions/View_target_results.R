
View_state <- function(data, state) {
  tmp <- filter(data, State == state)
  View(tmp)
  glimpse(tmp)
}

View_old_new_STdata <- function(state, fp) {
  tar_load(reformatted_data)
  tar_load(datafp)
  bee_old <- filter(reformatted_data, State == state)
  statedat <- loadSTdata(file.path(datafp, state))
  bee_new <- eval(parse(text = gsub(paste0("/", state, "/"), "statedat$", fp)))
  
  bee_hive <- list(ingested_dat = bee_old, new_dat = bee_new)
  imap(bee_hive, ~{
    eval(parse(text = paste0("View(bee_hive[['", .y, "']])")))
    })
  return(bee_hive)
}
