---
title: "AE Application"
author: "Philip Waggoner"
---

## Basics

NOTE: Here again, much of this is taken from my newest book under contract with *Cambridge University Press*, so please don't share the code beyond this class. Thanks!

Let’s stick with the 2019 ANES data to see if we can pick up on partisan differences across a battery of feeling thermometers (e.g., rate your feelings on a scale from 0-100 towards Barack Obama, or Donald Trump, etc., where 0 is extremely cold and 100 is extremely warm). Recall, our naive expectation is that some level of partisan differences should underlie these responses.

Load data and libraries.

```{r}
# Load libraries
library(tidyverse)
library(here)
library(amerika)
library(tictoc)
library(h2o) # ML engine for fitting the AE
library(bit64) # speeds up some h2o computation

# read in ANES 2019
anes <- read_rds(here("Data", "anes.rds")) 
```

With the data loaded, let's jump in and set things up to be compliant with h2o syntax. First, the party feature should be factor for coloring. Then, initialize an h2o session, and read the ANES data frame as an h2o data frame. 

Next, split the data into train, test, validaiton for a supervised task at the end. 

Finally, separate inputs from output, the latter of which is the party feature that we are calling "response", even though we aren't predicting anything with an AE. But we will use this as a response feature in the following supervised task, hence the name here. 

```{r}
# fitting
set.seed(1234)

anes$democrat <- factor(anes$democrat)

# initializing the h2o cluster; have to do this to work with the h2o engine
my_h2o <- h2o.init()

# h2o df
anes_h2o <- anes %>% 
  as.h2o()

# train, val, test
split_frame <- h2o.splitFrame(anes_h2o, 
                              ratios = c(0.6, 0.2), 
                              seed = 1234)   

split_frame %>% 
  str()

train <- split_frame[[1]]
validation <- split_frame[[2]]
test <- split_frame[[3]]

# Store response and predictors separately (per h2o syntax)
response <- "democrat"

predictors <- setdiff(colnames(train), response)
```

Now, we can build a simple, "vanilla" autoencoder. It's simple, because there is a single hidden layer (thus, "shallow"), with 16 nodes in the hidden layer. So it's also undercomplete, given that we have 35 input features we are working with. With the h2o engine, computation is super fast! See the notes below on the value of using ML engines for larger computational tasks like this and others, especially for deep learning.  

```{r}
# vanilla AE
{
  tic()
autoencoder <- h2o.deeplearning(x = predictors, # input layer
                                training_frame = train, # which data frame we are using for this task
                                autoencoder = TRUE,  # makes the generic h2o.deeplearning() function an AE (else, it is a normal deep learning model)
                                hidden = c(16), # number of hidden layers, and how many nodes in each. e.g., a 2 hidden layer model with 8 nodes in each would read: c(8, 8); this has a single hidden layer with 16 nodes/neurons
                                epochs = 100,  # number of times to see the full input data 
                                activation = "Tanh") # recall, non-linear activation
  toc()
} # ~ 4.5 seconds on my machine

# save model, if desired
#h2o.saveModel(autoencoder, 
#              path = "autoencoder", 
#              force = TRUE)

# load the (saved) model directly, if desired
#autoencoder <- h2o.loadModel(".../file/path/here")
```

Now, with the AE built and fit to the input data, let's extract the codings/features via `h2o.deepfeatures()`.

```{r}
# feature extraction
codings_train <- h2o.deepfeatures(autoencoder, 
                                  data = train, 
                                  layer = 1) %>% # "layer" is referring to the specific hidden layer where the codes are stored, and thus which we want to use as "features"
  as.data.frame() %>% # bring back to normal DF, instead of h2o DF for plotting
  mutate(democrat = as.vector(train[ , 36])) # retain the 36th feature, which is our party ID feature
```

The output is read as, e.g., `DF.L1.C1` - "data frame, layer number, column number".

Of importance, as the `layer` argument in the above chunk is referring to the specific hidden layer where the codes are stored, this is especially useful when there are multiple hidden layers, which are used for feature extraction *from different layers* in the network

Can take a look at these scores, e.g., the first 10. 

```{r}
# Numeric inspection of the "scores"
codings_train %>% 
  head(10)
```

Can also visually inspect. Substantively our goal here is checking to see whether our AE has detected the party labels or not over the first two deep features. Thus, we plot the deep features against each other, and then color the points by political party affiliation. We might expect separation in the projection space if D's are truly different than non-D's. 

```{r}
{ # note: wrapping in {} to run this full chunk at once
p1 <- ggplot(codings_train, aes(x = DF.L1.C1, 
                                y = DF.L1.C2, 
                                color = factor(democrat))) +
  geom_point(alpha = 0.6) + 
  stat_ellipse() +
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Party",
                     breaks=c("0", "1"),
                     labels=c("Non-Democrat", "Democrat")) +
  labs(title = "Deep Features 1 & 2",
       color = "Democrat") + 
  theme_minimal()

# (3 and 4)
p2 <- ggplot(codings_train, aes(x = DF.L1.C3, 
                                y = DF.L1.C4, 
                                color = factor(democrat))) +
  geom_point(alpha = 0.6) + 
  stat_ellipse() +
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Party",
                     breaks=c("0", "1"),
                     labels=c("Non-Democrat", "Democrat")) +
  labs(title = "Deep Features 3 & 4",
       color = "Democrat") + 
  theme_minimal()

# 5 & 6
p3 <- ggplot(codings_train, aes(x = DF.L1.C5, 
                                y = DF.L1.C6, 
                                color = factor(democrat))) +
  geom_point(alpha = 0.6) + 
  stat_ellipse() +
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Party",
                     breaks=c("0", "1"),
                     labels=c("Non-Democrat", "Democrat")) +
  labs(title = "Deep Features 5 & 6",
       color = "Democrat") + 
  theme_minimal()

# 7 & 8
p4 <- ggplot(codings_train, aes(x = DF.L1.C7, 
                                y = DF.L1.C8, 
                                color = factor(democrat))) +
  geom_point(alpha = 0.6) + 
  stat_ellipse() +
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Party",
                     breaks=c("0", "1"),
                     labels=c("Non-Democrat", "Democrat")) +
  labs(title = "Deep Features 7 & 8",
       color = "Democrat") + 
  theme_minimal()

# 9 & 10
p5 <- ggplot(codings_train, aes(x = DF.L1.C9, 
                                y = DF.L1.C10, 
                                color = factor(democrat))) +
  geom_point(alpha = 0.6) + 
  stat_ellipse() +
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Party",
                     breaks=c("0", "1"),
                     labels=c("Non-Democrat", "Democrat")) +
  labs(title = "Deep Features 9 & 10",
       color = "Democrat") + 
  theme_minimal()

# 11 & 12
p6 <- ggplot(codings_train, aes(x = DF.L1.C11, 
                                y = DF.L1.C12, 
                                color = factor(democrat))) +
  geom_point(alpha = 0.6) + 
  stat_ellipse() +
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Party",
                     breaks=c("0", "1"),
                     labels=c("Non-Democrat", "Democrat")) +
  labs(title = "Deep Features 11 & 12",
       color = "Democrat") + 
  theme_minimal()

# 13 & 14
p7 <- ggplot(codings_train, aes(x = DF.L1.C13, 
                                y = DF.L1.C14, 
                                color = factor(democrat))) +
  geom_point(alpha = 0.6) + 
  stat_ellipse() +
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Party",
                     breaks=c("0", "1"),
                     labels=c("Non-Democrat", "Democrat")) +
  labs(title = "Deep Features 13 & 14",
       color = "Democrat") + 
  theme_minimal()

# 15 & 16
p8 <- ggplot(codings_train, aes(x = DF.L1.C15, 
                                y = DF.L1.C16, 
                                color = factor(democrat))) +
  geom_point(alpha = 0.6) + 
  stat_ellipse() +
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Party",
                     breaks=c("0", "1"),
                     labels=c("Non-Democrat", "Democrat")) +
  labs(title = "Deep Features 15 & 16",
       color = "Democrat") + 
  theme_minimal()

# view together
library(patchwork)

(p1 + p2 + p3 + p4) / 
  (p5 + p6 + p7 + p8)
}
```

Great, we have built an AE and plotted the deep features, with color by PID and definitely picked up similar structure as with the previous methods (clearly groups at either extreme, and blending near the middle, reflecting American political dynamics, at a high, intuitive level). 

## Reconstruction Error

Let's take a look at reconstruction error on the training set via `h2o.anomaly()`.

```{r}
h2o.anomaly(autoencoder, train) %>% 
  as.data.frame() %>% 
  ggplot(aes(Reconstruction.MSE)) +
  geom_histogram() + 
  theme_minimal()
```

This shows us a count of error recorded after each pass through the network. In short, our reconstruction error here is consistently extremely *low*, which means we are doing a great job at capturing and reconstructing the original input space. Substantively, this suggests there is a strong structure to the input space that is relatively easily detectable. 

You could see this with any set of data you have - test or val; just substitute if you wish. Importantly, if you used a grid search to tune the AE, as you should in a real, full implementation, you just use the `getModel()` function from h2o to extract the best model, and then continue on with examining error and everything else you’d want to do.

## Supervised Task

Let's go deeper (pun).

This time, instead of just *color*, let's *predict* party ID (a supervised task), and then explore feature importance.

First, let's build a new model to predict party affiliation as a function of the deep features. That is, with the deep features from the previous autoencoder, we will train a "deep" neural net with 2 hidden layers and 8 nodes in each. 

```{r}
# first, feature extraction
codings_val <- h2o.deepfeatures(object = autoencoder, 
                                data = validation,
                                layer = 1) %>%
  as.data.frame() %>%
  mutate(democrat = as.factor(as.vector(validation[ , 36]))) %>%
  as.h2o()

deep_features <- setdiff(colnames(codings_val), response)

# fit the "deep" neural net (2 hidden layers, each with 8 nodes, giving 16 total)
deep_net <- h2o.deeplearning(y = response,
                             x = deep_features,
                             training_frame = codings_val,
                             activation = "Tanh",
                             hidden = c(8, 8), 
                             epochs = 100)
```

With the model built, let's first see how well we did at this classification/prediction task by inspecting a confusion matrix of the results. 

```{r}
# first, create yet another new set of deep features (using a different data set; test)
test_3 <- h2o.deepfeatures(object = autoencoder, 
                           data = test, 
                           layer = 1)

test_pred <- h2o.predict(deep_net, test_3, type = "response") %>%
  as.data.frame() %>%
  mutate(truth = as.vector(test[, 36]))

# Now, summarize predictions as confusion matrix
print(h2o.predict(deep_net, test_3) %>%
        as.data.frame() %>%
        mutate(truth = as.vector(test[, 36])) %>%
        group_by(truth, predict) %>%
        summarise(n = n()) %>%
        mutate(freq = n / sum(n)))
```

Not too bad. Now, let's explore feature importance. 

```{r}
# calculate first
fimp <- as.data.frame(h2o.varimp(deep_net)) %>% 
  arrange(desc(relative_importance))

# viz relative importance
fimp %>% 
  ggplot(aes(x = relative_importance, 
             y = reorder(variable, -relative_importance))) +
  geom_point(color = "dark red", 
             fill = "dark red", 
             alpha = 0.5,
             size = 2) +
  labs(title = "Relative Feature Importance",
       subtitle = "Deep Neural Network (2 hidden layers with 16 total neurons)",
       x = "Relative Importance",
       y = "Feature") + 
  theme_minimal()
```

Let's take a closer look at the most important deep features cross the training and validation sets (note: mine may be different from yours given randomization involved in data splitting at the beginning; if so, just update the deep feature accordingly). Here, we're expecting substantively similar patterns across each set. Substantively, this means we should expect to see the clearest separation between PID across these two deep features in *both* datasets.

```{r}
codings_val2 <- h2o.deepfeatures(object = autoencoder, 
                                 data = validation, 
                                 layer = 1) %>%
  as.data.frame() %>%
  mutate(democrat = as.factor(as.vector(validation[ , 36]))) # note, respecifying to NOT be an h20 object as before

# training plot
tr <- ggplot(codings_train, aes(x = DF.L1.C9, # change if yours are different
                                y = DF.L1.C3, # change if yours are different
                               color = factor(democrat))) +
  geom_point(alpha = 0.6) + 
  stat_ellipse() +
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Party",
                     breaks=c("0", "1"),
                     labels=c("Non-Democrat", "Democrat")) +
  labs(title = "Training Set",
       color = "Democrat") + 
  theme_minimal()

# validation plot
val <- ggplot(codings_val2, aes(x = DF.L1.C9, # change if yours are different
                                y = DF.L1.C3, # change if yours are different
                          color = factor(democrat))) +
  geom_point(alpha = 0.6) + 
  stat_ellipse() +
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Party",
                     breaks=c("0", "1"),
                     labels=c("Non-Democrat", "Democrat")) +
  labs(title = "Validation Set",
       color = "Democrat") + 
  theme_minimal()

# now side by side
(tr + val)
```

We certainly see distinction between parties across these most important features, but not a ton, as the deep features weren't the most important by a huge margin; only subtly "most important". This is in line with the mixed findings with these data we've encountered over the past several methods. Importantly, though, we see a similar pattern across both datasets, as expected.

A final note: For bigger data applications, you might consider using parallel processing like we've done for a few examples in past classes. This will speed up computation a bit. 

Then, when finished, shut down h2o cluster. It will ask you to make sure you want to; which you do, `Y`. 

```{r}
h2o.shutdown()
```

## Some notes on using external machine learning engines for building *scalable* ML pipelines 

Essentially, ML engines are massive sets of tools and computational power. They typically include repositories of source code for tons of model frameworks and architectures (far more than any single package could reasonably contain, though `tidymodels` and caret are making an attempt...).

Then, via an API (application programming interface), these external engines provide access to their store of models to be able to run and build good code for ML applications from anywhere in the world, and even at scale, depending the scale of course.

Perhaps the biggest, most widely used ML engine is Tensorflow (TF). What is this? In their words... "An entire ecosystem to help you solve challenging, real-world problems with machine learning." Ok, what exactly is a machine learning *ecosystem of techniques to solve problems*? Pretty much what it sounds like. That is, there is a store of model architectures that are developed by TF staff, and then made accessible via a variety of tools and avenues
    
Perhaps the simplest and most common way to locally access TF is via a high-level API, known as Keras. Keras was originally developed in Python, and Python is still the predominant approach and context for use of TF, I would argue. But Francois Chollet and JJ Allaire (inventor of RStudio) made an R port to access all of the functions and flexibility from Python's Keras, but locally in R. They even wrote a few books on these in an explicit Deep Learning context: *Deep learning with Python* and *Deep Learning with R*. This functionally means you can do, access, and work with the exact same structure in both R and/or Python, thanks to the work of these guys (and others involved in the open source development parts). So at this point we have: Tensorflow --> Keras --> R/Python
  
But, for bigger applications, you can leverage TF's power via a distributed computing or cloud computing contexts (the API is called: "Distribution Strategy"), which offer more flexibility, but are a bit more complex to use. But, Keras typically does *more than enough* for small-medium scale applications, such as those we encounter most in the social sciences.

Another widely used and increasingly growing ML engine is h2o, as we've used above in today's application of autoencoders. In their words... "H2O.ai is leading the movement to democratize AI for Everyone." In short, h2o also offers a massive store of ML architectures and coding infrastructure for a range of users from beginning ML developers to some pretty complex applications.
  
They have a range of free and not-free services that serve their goal of "democratizing AI". Here are some of their key products:

  - *h2o* (main feature): Use the programming language you already know like R, Python and others to build models in h2o
  
  - *Sparkling Water* (combining h2o and Spark): e.g., make a query using Spark SQL, feed the results into H2O to build a model and make predictions, and then use the results again in Spark; ALLOWS FOR big data and scalable ML applications
  
  - *h2o Flow*, a graphical notebook-based interactive user interface that does not require any coding
  
h2o makes their core pipeline available via their API to run *locally* (as with Keras and TF). But they also have a dashboard system for cloud computing, and even large scale computing abilities, which cost and are typically for pretty massive ML applications.

With all of these and the many more ML engines out there, the common goal is to make ML and AI research and work more accessible, which drastically pushes the boundaries of what we could think or do locally, compared even to just a few years ago.

Some links in case you're interested: 

  - <https://keras.rstudio.com/>
  - <https://tensorflow.rstudio.com/guide/keras/>
  - <https://www.h2o.ai/wp-content/uploads/2018/01/RBooklet.pdf>
  - <https://docs.h2o.ai/h2o/latest-stable/h2o-docs/faq/r.html>
  - <https://tensorflow.rstudio.com/tools/cloudml/getting_started/>
  - <https://www.amazon.com/Deep-Learning-R-Francois-Chollet/dp/161729554X/ref=sr_1_5?dchild=1&keywords=deep+learning+with+r&qid=1617037526&sr=8-5>
  - <https://www.amazon.com/Deep-Learning-Python-Francois-Chollet/dp/1617294438/ref=sr_1_1?dchild=1&keywords=deep+learning+with+python&qid=1617037534&sr=8-1>
  
