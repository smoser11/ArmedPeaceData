library(tidyverse)
library(rstan)
library(beepr)

load("data/ineq.rda")

seed <- 324
iter <- 4000
warmup <- iter - 1000
chains <- 3
cores <- chains
adapt_delta <- .9

baseline_series <- "LIS market sqrt"
baseline_wd <- str_split(baseline_series, "\\s")[[1]] %>% nth(-2)
baseline_es <- str_split(baseline_series, "\\s")[[1]] %>% last()

x0 <- ineq2_m %>%  
  filter(k_bl_obs > 0) %>%                    # use only data for countries with some baseline obs
  mutate(kcode = as.integer(factor(country, levels = unique(country))),
         rcode = as.integer(factor(region, levels = unique(region))),
         scode = as.integer(factor(series, levels = unique(series))),
         wecode = as.integer(factor(wdes, levels = unique(wdes))),
         kwecode = as.integer(factor(100*kcode+wecode)),
         wcode = as.integer(factor(welfare_def) %>% forcats::fct_relevel(baseline_wd)),
         ecode = as.integer(factor(equiv_scale) %>% forcats::fct_relevel(baseline_es)))  # redo codes for filtered sample

kt <- x0 %>%  
  transmute(kcode = kcode,
            yrspan = (lastyr - firstyr) + 1) %>% 
  distinct(kcode, yrspan) %>% 
  slice(rep(1:n(), yrspan)) %>% 
  group_by(kcode) %>% 
  mutate(tcode = 1:n()) %>% 
  ungroup() %>% 
  mutate(ktcode = 1:n())

x <- x0 %>% 
  left_join(kt, by = c("kcode", "tcode"))

kn <- x %>% 
  group_by(kcode) %>% 
  summarize(kt1 = min(ktcode),
            yrspan = first(yrspan)) %>% 
  ungroup()

x_countries <- unique(x$country)
x_wdes <- x %>%
  select(country, wdes) %>% 
  unite("k_weldef_eqsc", c("country", "wdes")) %>% 
  pull(k_weldef_eqsc) %>% 
  unique()

rho_we <- rho_we_m %>% 
  filter(country %in% x_countries) %>% 
  unite("k_w_e", c("country", "wdes"), remove = FALSE) %>% 
  filter(k_w_e %in% x_wdes) %>% 
  select(-matches("code"), -k_w_e) %>% 
  left_join(x %>% 
              select("country", "year", "wdes", matches("code")) %>%
              distinct(),
            by = c("country", "year", "wdes")) 


# Format data for Stan
source_data <- list(  K = max(x$kcode),
                      T = max(x$tcode),
                      KT = nrow(kt),
                      R = max(x$rcode),
                      S = max(x$scode),
                      WE = max(x$wecode),
                      KWE = max(rho_we$kwecode),
                      W = max(x$wcode),
                      E = max(x$ecode),
                      
                      N = length(x$gini_m),
                      N_ibl = length(x$ibl[x$ibl == TRUE]),
                      N_bl = length(x$gini_b[!is.na(x$gini_b)]),
                      N_obl = length(x$s_bl_obs[x$s_bl_obs>0]),
                      
                      kk = x$kcode,
                      tt = x$tcode,
                      kktt = x$ktcode,
                      ktt = kt$tcode,
                      ktk = kt$kcode,
                      kn = kn$yrspan,
                      kt1 = kn$kt1,
                      rr = x$rcode,
                      ss = x$scode,
                      wen = x$wecode,
                      kwen = x$kwecode,
                      gini_m = x$gini_m,
                      gini_m_se = x$gini_m_se,
                      gini_b = x$gini_b[!is.na(x$gini_b)],
                      gini_b_se = x$gini_b_se[!is.na(x$gini_b_se)],
                      
                      M = length(rho_we$rho),
                      kkm = rho_we$kcode,      
                      rrm = rho_we$rcode,
                      ttm	= rho_we$tcode,
                      wem = rho_we$wecode,
                      kwem = rho_we$kwecode,
                      rho_we = rho_we$rho,
                      rho_we_se = rho_we$rho_se
)

# Stan
start <- proc.time()
out1 <- stan(file = "R/estimate_swiid/lis_mkt.stan",
             data = source_data,
             seed = seed,
             iter = iter,
             warmup = warmup,
             cores = cores,
             chains = chains,
             control = list(max_treedepth = 20,
                            adapt_delta = adapt_delta))
runtime <- proc.time() - start
runtime

lapply(get_sampler_params(out1, inc_warmup = FALSE),
       summary, digits = 2)

save(x, out1, file = str_c("data/lis_mkt_", iter/1000, "k_", 
                        str_replace(Sys.time(), " ", "_") %>% str_replace("2017-", ""), ".rda"))

beep() # chime


# Plots
source("R/plot_tscs.R")
plot_tscs(x, out1, save_pdf = "paper/figures/ts_lis_mkt.pdf")
plot_tscs(x, out1)

shinystan::launch_shinystan(out1)

