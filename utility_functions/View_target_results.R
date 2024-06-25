
View_state <- function(data, state) {
  tmp <- filter(data, State == state)
  View(tmp)
  glimpse(tmp)
}