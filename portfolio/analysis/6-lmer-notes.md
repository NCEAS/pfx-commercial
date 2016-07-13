# Model estimates 

- permit holder random intercepts (for both revenue and CV) have much more spread than random intercepts for strategies 
- for a given permit holder: diversity is on average costly but reduces CV
- for a given permit holder: fishing more days means more revenue and less CV
- for a given permit holder: the variance reduction of diversity is enhanced if more days are finished (interaction)
- for a given permit holder: fishing more days may on average negate some or all of the cost of diversifying (interaction) 

- across permit holders: we see the quadratic relationship with mean species diversity and the expected variability around the mean
- permit holder random intercepts for revenue account for a lot more of the variance then the strategy random intercepts 

- across strategies: I don't see anything of note across strategies in terms of mean species diversity (but there might be other interesting stories here)
- across strategies: strategies with higher mean revenue might have more variability as well 

# Model diagnostics 

- fitted versus residuals looking good for revenue 
- fitted versus residuals looks a bit tighter at high fitted values for revenue variability 
- species diversity versus residuals for revenue: there is a very obvious tightening of the residuals towards higher levels of species diversity for most strategies; we should make sure our second models are capturing this 
- species diversity versus residuals for revenue variability: I don't see anything nonlinear; maybe a little tighter at high levels of species diversity (as we were seeing in the original portfolio plots)
- days versus residuals looks good for revenue 
- days versus residuals looks a bit heteroscedastic in different directions for different strategies (e.g. see halibut and sable fish versus herring)
- length versus residuals for revenue (not currently included in the model) doesn't look too bad with the exception of miscellaneous groundfish which is curved upwards; there might be a slight upward curvature for other strategies as well
- length versus residuals looks fine for variability 
