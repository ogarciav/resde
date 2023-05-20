# resde  -  Estimation in Reducible Stochastic Differential Equations

An R package for maximum likelihood parameter estimation in reducible stochastic differential equation models.
Discrete, possibly noisy observations, not necessarily evenly
spaced in time.
Can fit multiple individuals/units with global and local
parameters, by fixed-effects or mixed-effects methods.

Current stable _resde_ version 1.1 on CRAN:  <https://cran.r-project.org/package=resde>

![unitran](grex.png)

The picture indicates growth curves that can be modelled with reducible SDEs, see [here](https://ogarciav.github.io/grex/).

### Installation

Get the stable version from CRAN:  `install.packages("resde")`\
or with the RStudio menus: *Tools > Install Packages... > resde* 

### Demo

Fit the Richards model  dH^c = b(a^c - H^c) dt + s dW
to the heights of the first tree in `Loblolly`. Assume H(0) = 0, and no
observation error.

```r
library(resde)
tree <- subset(Loblolly, Seed == Seed[1])
m <- sdemodel(~x^c, beta0=~b*a^c, beta1=~-b, mum=0)
sdefit(m, x="height", t="age", data=tree, start=c(a=70, b=0.1, c=0.5))
```

### Vignette

[Fitting Reducible SDE Models.](https://cran.r-project.org/web/packages/resde/vignettes/resde-vignette.pdf)

### References

García, O. (2019). "Estimating reducible stochastic differential equations by
conversion to a least-squares problem". *Computational Statistics 34*(1): 23-46. [doi: 10.1007/s00180-018-0837-4](https://doi.org/10.1007/s00180-018-0837-4)
