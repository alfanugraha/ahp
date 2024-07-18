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

RI <- function(n){
  if (n==2) return (100)
  if (n==3) return (0.5247)
  if (n==4) return (0.8816)
  if (n==5) return (1.1086)
  if (n==6) return (1.2479)
  if (n==7) return (1.3417)
  if (n==8) return (1.4057)
  if (n==9) return (1.4499)
  if (n==10) return (1.4854)
  if (n==11) return (1.5140)
  if (n==12) return (1.5365)
  if (n==13) return (1.5551)
  if (n==14) return (1.5713)
  if (n==15) return (1.5838)
  return ((1.7699*n-4.3513)/(n-1.0)) # formula for 16+
}

calculateVE <- function(x, n){
  prod(x) ^ (1/n)
}

calculateVP <- function(x){
  x / sum(x)
}

