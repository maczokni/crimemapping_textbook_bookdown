library(MASS)
library(nlme)

### set number of individuals
n <- 200

### average intercept and slope
beta0 <- 1.0
beta1 <- 6.0

### true autocorrelation
ar.val <- .4

### true error SD, intercept SD, slope SD, and intercept-slope cor
sigma <- 1.5
tau0  <- 2.5
tau1  <- 2.0
tau01 <- 0.3

### maximum number of possible observations
m <- 10

### simulate number of observations for each individual
p <- round(runif(n,4,m))

### simulate observation moments (assume everybody has 1st obs)
obs <- unlist(sapply(p, function(x) c(1, sort(sample(2:m, x-1, replace=FALSE)))))

### set up data frame
dat <- data.frame(id=rep(1:n, times=p), obs=obs)

### simulate (correlated) random effects for intercepts and slopes
mu  <- c(0,0)
S   <- matrix(c(1, tau01, tau01, 1), nrow=2)
tau <- c(tau0, tau1)
S   <- diag(tau) %*% S %*% diag(tau)
U   <- mvrnorm(n, mu=mu, Sigma=S)

### simulate AR(1) errors and then the actual outcomes
dat$eij <- unlist(sapply(p, function(x) arima.sim(model=list(ar=ar.val), n=x) * sqrt(1-ar.val^2) * sigma))
dat$yij <- (beta0 + rep(U[,1], times=p)) + (beta1 + rep(U[,2], times=p)) * log(dat$obs) + dat$eij


df <- data.frame(Circulatory=c(32,26,19,16,14,13,11,11),
                 Mental=c(11,11,18,24,23,24,26,23),
                 Musculoskeletal=c(17,18,13,16,12,18,20,26),
                 Cancer=c(10,15,15,14,16,16,14,14))

df$year <- seq(1975,2010,by=5)
library(tidyr)
gathered <- gather(df, cause, percentage, -year)
crossed <- crossing(gathered, highlight = unique(gathered$cause))
?facet_wrap


```{r, eval=FALSE, echo=FALSE}
spa_df <- bur_per_ward
st_geometry(spa_df) <- NULL
class(spa_df)
spa_df <- dplyr::select(spa_df, -date_month)
```

```{r}
ggplot(spa_df, aes(x=Month, y=n, group = wd16nm)) + 
  geom_line() + guides(colour=FALSE) + xlab("Observation Time Point") +
  ylab("Y") 
```




```{r, eval=FALSE, echo=FALSE}
ggplot(spa_df, aes(x=Month, y=n, group=wd16nm)) + 
  geom_line() + guides(colour=FALSE) + xlab("Observation Time Point") + 
  geom_smooth(aes(group=1), se=FALSE, colour="red", size =2)
```

```{r}
library(tidyr)

crossed <- crossing(spa_df, ward = unique(spa_df$wd16nm))

ggplot(spa_df, aes(Month, n, group = wd16nm)) +
  geom_line(show.legend = FALSE) +
  scale_color_manual(values = c("gray")) +
  geom_line(aes(subset(spa_df, wd16nm=="Fallowfield"), Month, n),color="darkblue" ) +   theme_minimal()
```
