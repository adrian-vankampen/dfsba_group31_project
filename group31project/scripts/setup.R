#############################################
## The following loads the needed packages ##
#############################################

# load the required packages
packages <- c(
  "here", # for the project's organization
  "tidyverse", "lubridate", "readr", "naniar",# for wrangling
  "ggrepel", "gghighlight", "patchwork", "maps", "scales", # for plotting
  "knitr", "kableExtra", "bookdown", "rmarkdown" # for the report
)
purrr::walk(packages, library, character.only = TRUE)

######################################################
## The following sets a few option for nice reports ##
######################################################

# general options
options(
  digits = 3,
  str = strOptions(strict.width = "cut"),
  width = 69,
  tibble.width = 69,
  cli.unicode = FALSE
)

# ggplot options
theme_set(theme_light())

# knitr options
opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  # cache = TRUE,
  fig.retina = 0.8, # figures are either vectors or 300 dpi diagrams
  dpi = 300,
  out.width = "70%",
  fig.align = "center",
  fig.width = 6,
  fig.asp = 0.618,
  fig.show = "hold",
  message = FALSE,
  echo = FALSE,
  warning = FALSE
  )

# creating function for a nice kable
kable_maker <- function(a_tibble, ...) {
  a_tibble %>%
    kable(longtable = TRUE,align='l',...) %>%
    kable_styling(bootstrap_options = c("striped", "hover")) %>%
    `if`(nrow(a_tibble) > 5, (.) %>% scroll_box(height = "260px"), .) %>% 
    `if`(ncol(a_tibble) > 20, (.) %>% scroll_box(height = "120px"), .)
}

# using the kable_maker to display the first 5 rows for exercise 3
kable_head <- function(a_tibble,...){
  a_tibble %>% 
  head(5) %>% kable_maker(...)
}