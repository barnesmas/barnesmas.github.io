library(tidyverse)
library(janitor)
library(zoo)
library(modelr)
library(stringr)
library(growthcurver)
library(nlstools)
library(growthcurver)
library(betareg)
library(plotly)

theme_set(theme_classic())

### testing out my models before I figure out how to put them in the shiny app


# loading the data
dinosaur <- readxl::read_xlsx("./dino_bone_growth_app/Dinosaur Age vs Femur Length.xlsx")|> 
  janitor::clean_names() |> 
  rename(data_set = x1) |> 
  fill(data_set, .direction = "down") |> 
  drop_na() %>% 
  mutate(data_set = print(data_set))

# splitting the data into list of unique data sets, 
  # admitedelly it is a supid way to do it.
data_sets <- group_split(dinosaur, data_set)
data_set_names <- map(data_sets, "data_set") %>% 
  map(unique)
data_set_names2 <- make_clean_names(data_set_names)
data_sets <- group_split(dinosaur, data_set, keep = FALSE)
  names(data_sets) <- data_set_names2

  
  
  #### lets look at jsut  a singular data set for now, and model it with different models,  since that is what our shiny app is going to do
df1 <- data_sets$tyrannosaurus_1
# first lets plot the data

df1 %>% ggplot(mapping = aes(x = bone_dimension, y = age)) +
  geom_point()


# creating a linear model

model.linear <- glm(data = df1, formula = age ~ bone_dimension)

df1 %>% ggplot(mapping = aes(x = bone_dimension, y = age)) +
  geom_point() +
  geom_function(fun = function(x) {
    predict(model.linear, newdata = data.frame(bone_dimension = x))
  },
  colour = "red")

# creating a polynomial 2 model

model.quad <- glm(data = df1, formula = age ~ poly(bone_dimension, 2))

df1 %>% ggplot(mapping = aes(x = bone_dimension, y = age)) +
  geom_point() +
  geom_function(fun = function(x) {
    predict(model.quad, newdata = data.frame(bone_dimension = x))
  },
  colour = "red")


# creating a polynomial 3 model

model.cube <- glm(data = df1, formula = age ~ poly(bone_dimension, 3))

df1 %>% ggplot(mapping = aes(x = bone_dimension, y = age)) +
  geom_point() +
  geom_function(fun = function(x) {
    predict(model.cube, newdata = data.frame(bone_dimension = x))
  },
  colour = "red")


# exponential growth model

a = .5
b = .5
model.exp <- nls(age ~ a * exp(b * bone_dimension), data = df1, start = list( a=a, b=b))

df1 %>% ggplot(mapping = aes(x = bone_dimension, y = age)) +
  geom_point() +
  geom_function(fun = function(x) {
    predict(model.exp, newdata = data.frame(bone_dimension = x))
  },
  colour = "red")

# logarithmic growth model

a = 2
b = 0
model.log <- nls(age ~ b + a * log(bone_dimension), data = df1, start = list( a=a, b=b))

df1 %>% ggplot(mapping = aes(x = bone_dimension, y = age)) +
  geom_point() +
  geom_function(fun = function(x) {
    predict(model.log, newdata = data.frame(bone_dimension = x))
  },
  colour = "red")




# log transformed linear

model.logT = glm(data = df1, formula = log(age) ~ bone_dimension)

df1 %>% ggplot(mapping = aes(x = bone_dimension, y = age)) +
  geom_point() +
  geom_function(fun = function(x) {
    exp(predict(model.logT, newdata = data.frame(bone_dimension = x)))
  },
  colour = "red")


### logistic model

L = max(df1$bone_dimension)+10
k = .1
x0= median(df1$age)
model.logistic = nls(age ~
                       - log(L/bone_dimension -1) / k + x0
                     , data = df1, start = list( k = k, x0 = x0, L = L))



df1 %>% ggplot(mapping = aes(x = bone_dimension, y = age)) +
  geom_point() +
  geom_function(fun = function(x) {
    predict(model.logistic, newdata = data.frame(bone_dimension = x))
  },
  colour = "red") 

# creating a Gompertz model

c = 1 # growth rate
b = median(df1$age)
a = max(df1$bone_dimension) +1
model.gompertz <- nls(age ~ 1/c * (b - log(log(a/(bone_dimension)))), data = df1, start = list(a =a, b =b, c=c), model = TRUE)

df1 %>% ggplot(mapping = aes(x = bone_dimension, y = age)) +
  geom_point() +
  geom_function(fun = function(x) {
    predict(model.gompertz, newdata = data.frame(bone_dimension = x))
  },
  colour = "red") 

model.gompertz
model.gompertz$fitted

test <- nlsResiduals(model.gompertz)
test2 <- test$resi1 %>% as.data.frame()
test2$`Fitted values`
nlsResiduals(model.gompertz)$resi1

gompertz

predict(model.gompertz, newdata = df1$bone_dimension) - df1$age

check_model(model.gompertz)


