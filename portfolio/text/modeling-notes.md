# Separate models

## Variability

1. For a given permit holder: NA (each permit holder has one CV value)

2. Across permit holders: 

- increased diversity = lower variability (somewhat quadratically for those including salmon in their strategy)
- longer fishing seasons = lower variability
- benefit of diversifying is much greater for those with longer fishing seasons 

3. Across strategies: 

- strategies with higher mean diversity may have very slightly higher CV (but this isn't as clear across most strategy groupings)
- strategies with higher mean diversity have more benefit to diversifying if salmon is included in the strategy 

## Revenue

1. For a given permit holder: 

- increased diversity = lower revenue
- longer boats make more money (obviously)
- longer seasons = more money (obviously)
- no major interaction effects for these 3 predictors

2. Across permit holders:

- permit holders with higher mean diversity on average make slightly more money
- the variability around this relationship becomes tighter at higher levels of mean diversity
- not much change in the cost of diversifying across permit holders with different mean diversity levels

3. Across strategies:

- strategies with higher mean diversity on average make more money, especially if salmon are amongst the strategy 
- for some groupings, strategies with higher mean diversity have less of a diversifying cost (especially for salmon only, salmon plus crab, and salmon plus herring)

# Log differenced models

1. For a given permit holder averaged across the permit holders:

Revenue:
- increases in diversity are on average correlated with decreases in revenue
- increases in days fished are on average correlated with increases in revenue (obviously)

Variability:
- increases in diversity are on average not correlated with changes in variability (if anything variability increases with increases in diversity)
- increases in days fished are correlated with decreases in variability
- if both days fished and diversity increase together then the variability increases more than expected (interaction; biggest effect `0.3` with scaled predictors)

2. Across strategies:

- strategies with higher mean diversity on average have lower variability (very clear)
- benefit of diversity within strategies does not systematically vary by mean diversity of that strategy
- diversifying is less costly (in terms of decreases in revenue) for strategies with higher mean diversity (especially for strategies involving salmon or halibut/sablefish); alternatively, diversifying is less costly for strategies with higher mean revenue (except for groundfish where we see the opposite trend); (in general higher mean revenue is correlated with higher mean diversity; perhaps we want both as group level predictors?)

# Log revenue models

## Revenue

- Same conclusions as for individual model with the exception that it is hard to also estimate a random slope by permit holder and therefore we don't know how the cost of diversifying behaves across permit holders.

## Variability

1. For a given permit holder: NA

2. Across permit holders:
- the fixed effect coefficient gives us an average estimate across permit holders and within permit holders
- hard to estimate variability per permit holder as a random intercept
- fixed effect coefficients tell us: we don't detect a change in variability with diversity, more fishing days = less variability, more fishing days combined with more diversity = less variability than expected (interaction)

3. Across strategies:

- nothing super clear, will need to break out by strategy groupings
