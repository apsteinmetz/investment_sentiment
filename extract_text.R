# download pdfs from a list of urls
library(tidyverse)
library(rvest)
library(pdftools)
library(tidytext)

# scan directory of pdfs
pdfs_am <- list.files(path = "data/pdf/AM", pattern = "*.pdf", full.names = TRUE)
pdfs_us <- list.files(path = "data/pdf/US", pattern = "*.pdf", full.names = TRUE)
pdfs_eu <- list.files(path = "data/pdf/EU", pattern = "*.pdf", full.names = TRUE)
pdfs_pe <- list.files(path = "data/pdf/PE", pattern = "*.pdf", full.names = TRUE)


extract_words <- function(pdf_file) {
  # extract text from pdfs
  pdftools::pdf_text(pdfs_am[1]) |>
    str_split("\n") |>
    unlist() |>
    str_trim() |>
    str_subset("^[A-Z]") |>
    # tokenize text
    str_split(" ") |>
    unlist() |>
    # remove punctuation
    str_remove_all("[[:punct:]]") |>
    # remove numbers
    str_remove_all("[[:digit:]]") |>
    as_tibble() |>
    filter(value != "") |>
    filter(!(value  %in% stop_words$word))
  
}


all_words <- pdfs_am |> map_df(extract_words)

