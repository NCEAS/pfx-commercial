
dat = readRDS(file="salmon/data-generated/salmon.rds")

dat$salm = dat$coho+dat$chnk
dat$salm.prev = dat$coho.prev+dat$chnk.prev

dat$totsalm = dat$salm + dat$chum + dat$sock + dat$pink
dat$totsalm.prev = dat$salm.prev + dat$chum.prev + dat$sock.prev + dat$pink.prev

dat$salm = dat$salm/dat$totsalm
dat$sock = dat$sock/dat$totsalm
dat$pink = dat$pink/dat$totsalm
dat$chum = dat$chum/dat$totsalm
dat$chnk = dat$chnk/dat$totsalm
dat$coho = dat$coho/dat$totsalm
dat$salm.prev = dat$salm.prev/dat$totsalm.prev
dat$sock.prev = dat$sock.prev/dat$totsalm.prev
dat$pink.prev = dat$pink.prev/dat$totsalm.prev
dat$chum.prev = dat$chum.prev/dat$totsalm.prev
dat$chnk.prev = dat$chnk.prev/dat$totsalm.prev
dat$coho.prev = dat$coho.prev/dat$totsalm.prev

dat = dat[-which(is.na(dat$salm+dat$sock+dat$pink+dat$chum)),]
dat$logdiff = log(dat$revenue/dat$revenue.prev)

dat$sumP2 = abs(dat$salm - dat$salm.prev) +
  abs(dat$pink-dat$pink.prev) +
  abs(dat$chum-dat$chum.prev) +
  abs(dat$sock-dat$sock.prev)
dat = dat[which(dat$sumP2 < 0.3),]

dat = dat[which(dat$strategy_permit=="S01E"),]

pamk_s01e <- fpc::pamk(dat[,c("chum","pink","sock","salm")])
dat$group = c("Pink-chum","Pink")[pamk_s01e$pamobject$clustering]

# prep data for stan
N = nrow(dat) # number of rows of data
#X_ij = diffdat[,c("days.change")] #"specDiv.change","specdiv.prev",
#if(class(X_ij)=="numeric") X_ij = matrix(X_ij,ncol=1)
J = ncol(X_ij) # number of covariates on mean
X_sigma_ik = dat[,c("days.change")] #"specDiv.change","specdiv.prev",
if(class(X_sigma_ik)=="numeric") X_sigma_ik = matrix(X_sigma_ik,ncol=1)
K = ncol(X_sigma_ik) # number of covariates on variance

offset = dat$revenue.prev
y_i = dat$revenue
n_strategy = length(unique(dat$group))
strategy_i = as.numeric(as.factor(dat$group))
n_yr = length(unique(dat$year))
year_i = dat$year - min(dat$year) + 1

cov = dat$days.change

# create list for STAN
stan_data = list("N"=N, "y_i"=y_i, "offset"=offset, "n_strategy"=n_strategy,
  "n_str_yr"=n_str_yr,"str_yr_i"=str_yr_i, "n_yr"=n_yr, "year_i"=year_i,
  "cov"=cov)
stan_pars = c("mu_yr", "sd_yr", "b_offset", "g_offset", "b_cov", "g_cov")

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# estimate model. This model is modified from the simulation model by (1) including indices to allow NAs in the inputted data, and (2) including estimated year effects (intercepts)
mod = stan(file = 'salmon/analysis/portfolio-offset-simple.stan',data = stan_data,
  verbose = TRUE, chains = 3, thin = 1, warmup = 1000, iter = 2000, pars = stan_pars)

# groups here are 80% pink vs 97% pink
hist(extract(mod)[["b_offset"]], 40, col="grey", xlab="Effect of specializing on pink (mean)")
hist(extract(mod)[["g_offset"]], 40, col="grey", xlab="Effect of specializing on pink (sd)")

df = data.frame("year"=1:29, "mean" = apply(extract(mod)[["mu_yr"]], 2, mean), "low" = apply(extract(mod)[["mu_yr"]], 2, quantile, 0.025),
  "hi" = apply(extract(mod)[["mu_yr"]], 2, quantile, 0.975))
ggplot(data=df, aes(year, mean)) + geom_ribbon(aes(ymin=low,ymax=hi))

save.image("salmon/analysis/model-simple.Rdata")


