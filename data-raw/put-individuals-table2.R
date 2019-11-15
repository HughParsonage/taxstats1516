library(readxl)
library(magrittr)
library(hutils)
library(data.table)
library(knitr)

URL_Table2 <-
  "https://data.gov.au/dataset/d170213c-4391-4d10-ac24-b0c11768da3f/resource/ca3d7752-e689-47fa-a746-20cfee9f2630/download/taxstats2016individual02lodgmentmethodgendertaxablestatusstateage.xlsx"

destfile <- "data-raw/Table2.xlsx"

if (!file.exists("DESCRIPTION") || !dir.exists(basename(destfile))) {
  message("Using non-interactive mode => xlsx file will not be cached")
  destfile <- tempfile(fileext = ".xlsx")
}

res <-
  download.file(URL_Table2,
                destfile = destfile,
                mode = "wb")
if (res) {
  if (HTTP_ERROR <- httr::http_error(URL_Table2)) {
    stop(HTTP_ERROR)
  } else {
    stop("Unable to download \n\t",
         URL_Table2)
  }
}

individuals_table2_201516_raw <-
  read_excel(destfile,
             sheet = "Individuals Table 2A",
             skip = 2) %>%
  as.data.table

individuals_table2_201516 <-
  individuals_table2_201516_raw %>%
  melt.data.table(id.vars = c("Lodgment method",
                              "Gender",
                              "Taxable status",
                              "State / Territory1",
                              "Age range2"),
                  variable.factor = FALSE,
                  variable.name = "raw_variable") %>%
  .[, c("variable", "suffix") := tstrsplit(raw_variable, split = "\r\n")] %>%
  .[, c("variable", "suffix") := lapply(.SD, trimws), .SDcols = c("variable", "suffix")] %>%
  setnames(c("Lodgment method",
             "Gender",
             "Taxable status",
             "State / Territory1",
             "Age range2"),
           c("Lodgment_method",
             "Gender",
             "TaxableStatus",
             "State",
             "Age_range")) %>%
  .[]


number_of_taxpayers <-
  individuals_table2_201516 %>%
  .[variable %ein% "Number of individuals" & TaxableStatus %ein% "Taxable"] %>%
  .[suffix %ein% "no."] %>%
  .[, .(number_of_taxpayers = sum(value)), keyby = "Age_range"] %>%
  .[, Age_range := sub("^[^0-9]+", "", Age_range)] %>%
  .[, c("min_age", "max_age") := tstrsplit(sub("^18$",
                                               "0 - 18",
                                               sub(" and over",
                                                   " - 100",
                                                   Age_range)),
                                           split = " - ",
                                           fixed = TRUE)] %>%
  .[]

persons_by_age <-
  Census2016.DataPack::STE__Age.min %>%
  .[, .(persons = sum(persons)),
    keyby = .(Age_range = cut(Age.min,
                              breaks = c(-Inf, number_of_taxpayers[, unique(max_age)]),
                              labels = number_of_taxpayers[, unique(Age_range)]))] %>%
  .[]

format_numbers <- function(x) {
  if (!is.numeric(x)) {
    return(x)
  }
  if (min(x) > -1 && max(x) < 1) {
    rx <- round(x * 100, 1)

    return(paste0(formatC(rx, width = 3, flag = "#", digits = 1, format = "f",
                          drop0trailing = FALSE),
                  "%"))
  }
  if (max(abs(x)) > 1e3) {
    return(prettyNum(round(x), big.mark = ","))
  }
  x
}
stopifnot(identical(format_numbers(c(0.17, 0.115)),
                    c("17.0%", "11.5%")))

number_of_taxpayers[persons_by_age, on = "Age_range"] %>%
  .[, "%_taxpayers" := number_of_taxpayers / persons] %>%
  .[, "%_taxpayers_at_least_this_old" := rev(cumsum(rev(number_of_taxpayers)) / cumsum(rev(persons)))] %>%
  .[] %>%
  .[, c("min_age", "max_age") := NULL] %>%
  .[Age_range %ein% "18", Age_range := "<18"] %>%
  .[Age_range %ein% "75 and over", Age_range := "75+"] %T>%
  .[, stopifnot(sum(number_of_taxpayers) %between% c(10e6, 11e6),
                sum(persons) %between% c(23e6, 24e6))] %>%
  .[, lapply(.SD, format_numbers)] %>%
  kable(align = "rrrrr")

individuals_table2_201516 %>%
  .[, AgeGroup := sub(" - ", "-", Age_range, fixed = TRUE)] %>%
  .[, AgeGroup := sub("^[^0-9]+", "", AgeGroup)] %>%
  .[, AgeGroup := sub("75 and over", "75+", AgeGroup)] %>%
  .[AgeGroup %ein% "18", AgeGroup := "<18"] %>%
  copy %>%
  .[, Age_range := NULL] %>%
  .[, raw_variable := NULL] %>%
  setkey(Lodgment_method, Gender, TaxableStatus, State, AgeGroup, variable, suffix) %>%
  set_cols_first(key(.)) %>%
  .[]


