library(shiny)
library(bslib)
library(DT)
library(htmltools)
library(gtools)

table_pembanding_df <- data.frame(
  Sintaks = c("sangat diutamakan", "lebih diutamakan menuju sangat diutamakan", "lebih diutamakan",
              "diutamakan menuju lebih diutamakan", "diutamakan", "cukup diutamakan menuju diutamakan",
              "cukup diutamakan", "setara menuju cukup diutamakan", "setara"),
  Nilai = c(9:1)
)
