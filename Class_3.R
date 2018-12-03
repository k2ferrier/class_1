## Class 3, Missing data filtering and other biological data processing
library(pbda)
library(tidyverse)
library (broom)

missing_ex 

# Filtering out missing data
y <- missing_ex %>%
  filter(!is.na(id))

# Filtering out the empty string

x <- c("a", "b", "c", "")
x[x != ""]
x != ""

# Processing Bio data
yeast_prot_prop
yeast_prot_prop$ORF[1]
brauer_gene_exp

# How many different nutrient limitations are there?
unique_nutr <- brauer_gene_exp$nutrient %>% unique()
unique_nutr

# Overall, are growth rate and gene expression correlated?
gr_vs_ge <- cor.test(brauer_gene_exp$rate, brauer_gene_exp$expression) # exp vs rate is the same
gr_vs_ge
tidy(gr_vs_ge)

# Nesting and unnesting data
mtcars %>% group_by(cyl) %>% nest()
mtcars %>% group_by(cyl) %>% nest() %>% unnest()

# Creates separate tables of nutrient&rate and nutrient&expression and calculate correlation, then unnest those tables
res <- brauer_gene_exp %>% 
  select(nutrient, rate, expression) %>% 
  group_by(nutrient) %>% 
  nest()

res %>%
  mutate(
    correlation = map(
      data,
      ~ tidy(cor.test(.$rate, .$expression))
      )
    ) %>%
  select(nutrient, correlation) %>%
  unnest()
# map applies some function to some data
# same thing as above, but simpler:
#res %>%
#  mutate(
#    correlation = map(
#      data, run_cor)
#  ) %>%
#  select(nutrient, correlation) %>%
#  unnest()

run_cor <- function(x) {
  tidy(cor.test(x$rate, x$expression))
}
run_lm <- function(x) {
  tidy(lm(rate ~ expression, data = x))
}

res %>%
  mutate(
    correlation = map(data, run_cor),
    model = map(data, run_lm)
  ) %>%
  select(nutrient, correlation) %>%
  unnest()

#Building a linear model
run_lm <- function(x) {
  tidy(lm(rate ~ expression, data = x))
}

r <- brauer_gene_exp %>%
  select(systematic_name, nutrient, rate, expression) %>%
  filter(systematic_name == "YBR093C") %>%
  group_by(systematic_name, nutrient) %>%
  nest() %>%
  mutate(model = map(data, run_lm))
r

# Plotting rate by expression, facet by nutrient
data_pho5 <- brauer_gene_exp %>%
  select(systematic_name, nutrient, rate, expression) %>%
  filter(systematic_name == "YBR093C")

ggplot(data_pho5, aes(x = rate, y = expression, color = nutrient)) +
  geom_point() +
  facet_grid(~ nutrient)

#Filtering for the slope terms 
run_lm <- function(x) {
  tidy(lm(rate ~ expression, data = x))
}

b <- brauer_gene_exp %>%
  select(systematic_name, nutrient, rate, expression) %>%
  filter(systematic_name == "YBR093C") %>%
  group_by(systematic_name, nutrient) %>%
  nest() %>%
  mutate(model = map(data, run_lm)) %>%
  select(nutrient, model) %>%
  unnest()
b
