---
title: "SOM Application"
author: "Philip Waggoner"
---

## Basics

NOTE: As before, much of this is taken from my newest book under contract with *Cambridge University Press*, so please don't share the code beyond this class. Thanks!

Let’s work with the 2019 ANES data again to see if we can pick up on partisan differences across a battery of feeling thermometers (e.g., rate your feelings on a scale from 0-100 towards Barack Obama, or Donald Trump, etc., where 0 is extremely cold and 100 is extremely warm). Recall, our naive expectation is that some level of partisan differences should underlie these responses. Let's explore with self-organizing maps (SOM).

Load data and libraries.

```{r}
# Load libraries
library(tidyverse)
library(here)
library(amerika)
library(tictoc)
library(kohonen)

# read in cleaned and preprocessed 2019 ANES Pilot Data
anes <- read_rds(here("Data", "anes.rds"))
```

For SOM, recall we need to set the pre-determined number of neurons/nodes and search over the space and standardize the input features, though technically for this exercise you don't need to standardize, as they're all on the same scale. But for best practices, let's do so anyway.
 
```{r}
# begin
set.seed(1234)

# standardize FTs
anes_scaled <- anes[ ,1:35] %>% 
  scale()

# create the structure of the output layer; specify the dimensions of the grid to search
search_grid <- somgrid(xdim = 10, 
                       ydim = 10, 
                       topo = "rectangular",
                       neighbourhood.fct = "gaussian") 
```

Now, we can fit the SOM based on the grid previously set up.

```{r}
{
  tic()
som_fit <- som(anes_scaled, # scaled input data (minus party ID)
               grid = search_grid, # grid we set up
               alpha = c(0.1, 0.001), # learning rate; (default vals decline from .05 to .01)
               radius = 1, # neighborhood size;
               rlen = 500, # epochs - number of times the complete data is presented to alg
               dist.fcts = "euclidean", # distance function
               mode = "batch") # alg type - batch, online, etc.
  toc()
} # ~12 seconds
```

Now, plot the training progress, which is the average deviations from weight vectors/codes. That is, how much the distance varies across the full output layer at a given fit of the all, taking progressively smaller learning steps, including all "clusters."

```{r}
# plot
som_fit$changes %>% 
  as_tibble() %>% 
  dplyr::mutate(changes = V1,
         iteration = seq(1:length(changes))) %>% 
  ggplot(aes(iteration, changes)) +
  geom_line() +
  labs(x = "Training Iteration",
       y = "Mean Distance to Closest Node") +
  theme_minimal()
```

We will come back to clustering in Week 9, but for now, a common approach to diagnosing the fit of a SOM is to fit a clustering algorithm to the codes. This clues us in to whether we likely found structure in the data as we might expect, or whether we didn't. In a word, *corroboration*.

Codes, which are sometimes called weight vectors (called via `$codes`), represent the representation of each feature in each node/neuron. Each row is a neuron and each column is a feature. Cell entries can be read *like* correlation coefficients, though are technically on a different scale and not bounded between -1 and 1. Still, they intuitively tell us the same thing as feature loadings in PCA. 

The intuition with the following code using clustering algorithms on top of the output from a SOM is to explore whether codes are naturally grouped along a substantive dimension. For our case, of course, this is along a partisan dimension. If so, then we should be able to observe grouping that mirrors party affiliation.

So don't worry about the details of the clustering algorithms for now re: k-means, fuzzy C-means, or hierarchical clustering. Just take a look at the code and description, and reach out if unclear. But note, we will cover these and other clustering techniques in week 9.

First, for ease of plotting, store the point colors used throughout in two objects: point colors and neuron colors, with the POINTS being darker shades corresponding with the individual observations and the NEURONS/NODES being lighter shades corresponding with the output layer (the background color). 
			
```{r}
point_colors <- c(amerika_palettes$Republican[2], 
                  amerika_palettes$Democrat[2])

neuron_colors <- c(amerika_palettes$Republican[3], 
                   amerika_palettes$Democrat[3])
```

Now, let’s start with a k-means algorithm to the codes data, searching for two clusters given the dichotomous party affiliation feature. 

```{r}
## k-means
kmeans_clusters <- som_fit$codes[[1]] %>% 
  kmeans(., centers = 2)

# Then, derive cluster labels (1 or 2) for the clusters.
class_assign_km <- map_dbl(kmeans_clusters$cluster, ~{
  if(. == 1) 2
  else 1
}
)
```

Finally, plot accordingly varying color by party affiliation. Again, the expectation is that the majority of observations (points) should correspond to the color of the nodes (background color) found from the given clustering algorithm. If points are scattered and not clearly coordinating, then this would suggest there is not clear separation in the output layer.

So, let's see what we get...

```{r}
plot(som_fit, 
     type = "mapping", 
     pch = 21, 
     bg = point_colors[as.factor(anes$democrat)],
     shape = "straight",
     bgcol = neuron_colors[as.integer(class_assign_km)],
     main = "2 clusters via k-means"); add.cluster.boundaries(x = som_fit, clustering = class_assign_km, lwd = 5, lty = 5)
```

Not too bad. A lot of similarity, suggesting we are finding true structure to some degree. 

Now, let's try with a different approach: fuzzy C-means clustering (a soft-partitional, non-probabilistic clustering algorithm).

```{r}
## FCM
fcm_clusters <- som_fit$codes[[1]] %>% 
  ppclust::fcm(., centers = 2)

class_assign_fcm <- map_dbl(fcm_clusters$cluster, ~{
  if(. == 1) 2
  else 1
}
)

plot(som_fit, 
     type = "mapping", 
     pch = 21, 
     bg = point_colors[as.factor(anes$democrat)],
     shape = "straight",
     bgcol = neuron_colors[as.integer(class_assign_fcm)],
     main = "2 clusters via FCM"); add.cluster.boundaries(x = som_fit, clustering = class_assign_fcm, 
                                                      lwd = 5, lty = 5)

```

Similar to k-means, which is a good sign. 

Finally, now with a very different approach to clustering: agglomerative hierarchical clustering. 

```{r}
## HAC
hac_clusters <- som_fit$codes[[1]] %>% 
  dist() %>% 
  hclust() %>% 
  cutree(., k = 2)

class_assign <- map_dbl(hac_clusters, ~{
  if(. == 1) 2
  else 1
}
)

plot(som_fit, type = "mapping", pch = 21, 
     bg = point_colors[as.factor(anes$democrat)],
     shape = "straight",
     bgcol = neuron_colors[as.integer(class_assign)],
     main = "2 clusters via HAC"); add.cluster.boundaries(x = som_fit, 
                                                  clustering = class_assign, 
                                                  lwd = 5, 
                                                  lty = 5)

```

Similar again. This suggests that the SOM was picking up on partisan differences to a strong degree, with structure corroborated by three clustering algorithms. The structure wasn't "perfectly" separated in the output, but the natural data space also isn't perfectly separated. Thus, this passes the "smell test" too, to a large degree. 

## A Different View of Codes

Now, let's look at the codes a different way. Again, codes give a sense of the specific features that are attracted to similar nodes, such that we are able to pick up on grouping across features in the SOM grid space. 

For example, like features (e.g., feelings toward Barack Obama and Elizabeth Warren) might heavily characterize ("weight") a node, compared to other features (e.g., feelings toward Donald Trump). 

The result is a picture of the relationships between both features and nodes across the full input space. Think of these like features loadings in PCA.

Let's proceed on a feature-by-feature basis, and tie the results back into a substantive understanding and exploration of the American political preferences. Again, we can think of weight vectors/codes like we might correlations between features and neurons. 

Features that are more similar will trend toward neurons in a positive direction, suggesting similar grouping and thus latent structure across those features. This is compared to features that are more different from each other, which will trend negatively. 

For example, consider the direction of the codes between.

```{r}
# trump and obama 
som_fit$codes %>% 
  as.data.frame() %>% 
  ggplot(aes(Trump, Obama)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "SOM Codes for Feelings toward Trump and Obama") +
  theme_minimal()

# sanders and obama 
som_fit$codes %>% 
  as.data.frame() %>% 
  ggplot(aes(Sanders, Obama)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "SOM Codes for Feelings toward Sanders and Obama") +
  theme_minimal()
```

Though these sorts of base, naive expectations are easier for feelings toward *people* based on what we know about partisan politics and ideological preferences in American politics, let's explore whether the same is true for naive expectations across *institutions*, like we might expect negative across UN and the NRA, but positive across ICE and the NRA. 

```{r}
# UN and NRA
som_fit$codes %>% 
  as.data.frame() %>% 
  ggplot(aes(UN, NRA)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "SOM Codes for Feelings toward the UN and NRA") +
  theme_minimal()

# UN and NRA
som_fit$codes %>% 
  as.data.frame() %>% 
  ggplot(aes(ICE, NRA)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "SOM Codes for Feelings toward ICE and the NRA") +
  theme_minimal()
```

Indeed, these are the patterns we see, suggesting the SOM is picking up this latent, partisan structure in these data in line with substantive/domain expectations.
