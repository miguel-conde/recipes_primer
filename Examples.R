if (!require(recipes)) {
  if (packageVersion("devtools") < 1.6) {
    install.packages("devtools")
  }
  devtools::install_github("topepo/recipes")
  require(recipes)
}

library(recipes)
library(mlbench)
data(Sonar)
sonar_rec <- recipe(Class ~ ., data = Sonar) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())


###############################################
# simple example:
data(biomass)

# split data
biomass_tr <- biomass[biomass$dataset == "Training",]
biomass_te <- biomass[biomass$dataset == "Testing",]

# When only predictors and outcomes, a simplified formula can be used.
rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
              data = biomass_tr)
rec

# Now add preprocessing steps to the recipe.
sp_signed <- rec %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_spatialsign(all_predictors())
sp_signed

# now estimate required parameters
sp_signed_trained <- prepare(sp_signed, training = biomass_tr)

sp_signed_trained


# apply the preprocessing to a data set
test_set_values <- bake(sp_signed_trained, newdata = biomass_te)
test_set_values

# or use pipes for the entire workflow:
rec <- biomass_tr %>%
  recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_spatialsign(all_predictors())

###############################################
# multivariate example

# no need for `cbind(carbon, hydrogen)` for left-hand side
multi_y <- recipe(carbon + hydrogen ~ oxygen + nitrogen + sulfur,
                  data = biomass_tr)
multi_y <- multi_y %>%
  step_center(all_outcomes()) %>%
  step_scale(all_predictors())

multi_y_trained <- prepare(multi_y, training = biomass_tr)

results <- bake(multi_y_trained, biomass_te)

###############################################
# Creating a recipe manually with different roles

rec <- recipe(biomass_tr) %>%
  add_role(carbon, hydrogen, oxygen, nitrogen, sulfur,
           new_role = "predictor") %>%
  add_role(HHV, new_role = "outcome") %>%
  add_role(sample, new_role = "id variable") %>%
  add_role(dataset, new_role = "splitting indicator")
rec

###############################################
# Dummy variables
data(okc)
okc <- okc[complete.cases(okc),]

rec <- recipe(~ diet + age + height, data = okc)

dummies <- rec %>% step_dummy(diet)
dummies <- prepare(dummies, training = okc)

dummy_data <- bake(dummies, newdata = okc)

unique(okc$diet)
       
grep("^diet", names(dummy_data), value = TRUE)
