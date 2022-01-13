library(tidyverse)
boston_cocktails <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv")
library(stringr)

View(boston_cocktails)

#Capitalize the first letter of both words in a two word string
Caps <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


#result<- sapply(name, Caps)

#write out result to working directory(desktop)
write.csv(result,file = "result.csv")
 
boston_cocktails %>%
  mutate( 
    name = str_remove(name, "\\?"),
    measure=str_replace(measure, " ?1/2", ".5"),
         measure=str_replace(measure, " ?1/4", ".25"),
         measure=str_replace(measure, " ?3/4", ".75"),
         measure_n= parse_number(measure),
         measure_n=ifelse(str_detect(measure, "dash$"),measure_n/50, measure_n)
  ) %>%
add_count(ingredient,sort = TRUE)
 


boston_cocktails %>%
  mutate(ING = case_when(
   str_detect(ingredient, "Lime") ~ "Juice",
    str_detect(ingredient, "lemon") ~ "juice",
    str_detect(ingredient, "Juice of Orange") ~ "Juice",
    str_detect(ingredient, "Apple schnapps") ~ "Juice"
    ,TRUE ~ingredient  
  )  )


 boston_cocktails %>%
   mutate(category_new=case_when(
     str_detect(category,"Cocktail Classics")~ "Cocktail",
     str_detect(category,"Cordials") ~ "Cocktail"
     
     
   ))



cocktails_parsed <- boston_cocktails %>%
  mutate(
    ingredient = Caps(ingredient),
    ingredient = str_replace_all(ingredient, "-", " "),
    ingredient = str_remove(ingredient, " liqueur$"),
    ingredient = str_remove(ingredient, " (if desired)$"),
    ingredient = case_when(
      str_detect(ingredient, "bitters") ~ "bitters",
      str_detect(ingredient, "lemon") ~ "lemon juice",
      str_detect(ingredient, "lime") ~ "lime juice",
     # str_detect(ingredient, "grapefruit") ~ "grapefruit juice",
     # str_detect(ingredient, "orange") ~ "orange juice",
      TRUE ~ ingredient
    ),
    measure = case_when(
      str_detect(ingredient, "bitters") ~ str_replace(measure, "oz$", "dash"),
      TRUE ~ measure
    ),
    measure = str_replace(measure, " ?1/2", ".5"),
    measure = str_replace(measure, " ?3/4", ".75"),
    measure = str_replace(measure, " ?1/4", ".25"),
    measure_number = parse_number(measure),
    measure_number = if_else(str_detect(measure, "dash$"),
                             measure_number / 50,
                             measure_number)) %>%
  add_count(ingredient) %>%
  filter(n > 15) %>%
  select(-n) %>%
  distinct(row_id, ingredient, .keep_all = TRUE) %>%
  na.omit()


cocktails_df <- cocktails_parsed %>%
  select(-ingredient_number, -row_id, -measure) %>%
  pivot_wider(names_from = ingredient, values_from = measure_number, values_fill = 0) %>%
  janitor::clean_names() %>%
  na.omit()


#Principal component analysis
#This dataset is especially delightful because we get to use recipes with recipes. 😍 
#Let’s load the tidymodels metapackage and implement principal component analysis with a recipe.
library(tidymodels)

pca_rec <- recipe(~., data = cocktails_df) %>%
  update_role(name, category, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

pca_prep <- prep(pca_rec)

pca_prep

tidied_pca <- tidy(pca_prep, 2)

tidied_pca %>%
  filter(component %in% paste0("PC", 1:5)) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL)

#The biggest difference in PC1 is powdered sugar vs. simple syrup; 
#recipes are not likely to have both, which makes sense! Let’s zoom in on the first four components, 
#and understand which cocktail ingredients contribute in the positive and negative directions.


library(tidytext)

tidied_pca %>%
  filter(component %in% paste0("PC", 1:4)) %>%
  group_by(component) %>%
  top_n(8, abs(value)) %>%
  ungroup() %>%
  mutate(terms = reorder_within(terms, abs(value), component)) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  scale_y_reordered() +
  labs(
    x = "Absolute value of contribution",
    y = NULL, fill = "Positive?"
  )

#So PC1 is about powdered sugar + egg + gin drinks vs. simple syrup + lime + tequila drinks. This is the component that explains the most variation in drinks. PC2 is mostly about vermouth, both sweet and dry.

# How are the cocktails distributed in the plane of the first two components?

juice(pca_prep) %>%
  ggplot(aes(PC1, PC2, label = name)) +
  geom_point(aes(color = category), alpha = 0.7, size = 2) +
  geom_text(check_overlap = TRUE, hjust = "inward", family = "IBMPlexSans") +
  labs(color = NULL)

#You can change out PC2 for PC4, for example, to instead see where drinks with more grenadine are.


#UMAP
#One of the benefits of the tidymodels ecosystem is the flexibility and ease of trying different approaches for the same kind of task. For example, we can switch out PCA for
#UMAP, an entirely different algorithm for dimensionality reduction based on ideas from topological data analysis. The
#embed package provides recipe steps for ways to create embeddings including UMAP. Let’s switch out the PCA step for the UMAP step.

library(embed)

umap_rec <- recipe(~., data = cocktails_df) %>%
  update_role(name, category, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_umap(all_predictors())

umap_prep <- prep(umap_rec)

umap_prep

#Now we can example how the cocktails are distributed in the plane of the first two UMAP components.

juice(umap_prep) %>%
  ggplot(aes(umap_1, umap_2, label = name)) +
  geom_point(aes(color = category), alpha = 0.7, size = 2) +
  geom_text(check_overlap = TRUE, hjust = "inward", family = "IBMPlexSans") +
  labs(color = NULL)

#Really interesting, but also different! This is because UMAP is so different from PCA, 
#although they are both approaching this question of how to project a set of features, 
#like ingredients in cocktail recipes, into a smaller space Share


