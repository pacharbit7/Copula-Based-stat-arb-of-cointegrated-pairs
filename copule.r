library('MASS')
#library(copula) # unused
library(VineCopula)
options(warn=-1) # we desactivate the warnings because we estimate the laws on our spreads are not optimisable

#### Formation Period ####

### AIC of marginal distribution ###

compute_aic_distr = function(distr, nb_param, spread) 
{
  aic_value = tryCatch({ 
    
    LL_fit = fitdistr(spread, distr)$loglik 
    return(2*nb_param - 2*LL_fit) 
    
  }, error = function(e)
    {
    return(Inf) # if optim fail we return Inf
  })
  
  return(aic_value)
}

### Transform spread Uniform ###

transform_to_uniform = function(spread) # spread from formation period list without transformation
{
  distr = c("t","cauchy","normal") # law of distribution to fit spread
  nb_param_distr = c(1,2,2)
  
  aics = numeric(length(distr))
  for (i in seq_along(distr)) 
  {
    aics[i] = compute_aic_distr(distr[i], nb_param_distr[i], spread) # we compute AIC for every law 
  }
  
  best_distr = distr[which.min(aics)]
  distr_spread = fitdistr(spread, best_distr) # we estimate parameter of the distribution that minimise AIC
  
  if(best_distr == "t")
  {
    uniform_spread = pt((spread-distr_spread$estimate["m"])/distr_spread$estimate["s"], df = distr_spread$estimate["df"])
  }
  if(best_distr == "cauchy")
  {
    uniform_spread = pcauchy(spread, location=distr_spread$estimate["location"], scale=distr_spread$estimate["scale"])
  }
  if(best_distr == "normal")
  {
    uniform_spread = pnorm(spread, mean=distr_spread$estimate["mean"], sd=distr_spread$estimate["sd"])
  }
  
  law = list(type_law=best_distr, param1=distr_spread$estimate[1], param2=distr_spread$estimate[2], param3=distr_spread$estimate[3])
  
  return(list(uniform_spread = uniform_spread,law = law)) # we return uniform transform uniform spread from formation period and the law with parameter we gonna use in trading period
}

### Estimation Copula ###

match_copula_law = function(S1, S2)  # uniform spread from formation period list
{
  if(is.null(S1) | is.null(S2)) # in case of week without stationnary spreads
  {
    return(NULL)
  }
  
  uniform_law1 = transform_to_uniform(S1)
  uniform_law2 = transform_to_uniform(S2)
  
  U1 = uniform_law1$uniform_spread   # uniform transform of spread during formation period to estimate copula
  U2 = uniform_law2$uniform_spread
  
  law1 = uniform_law1$law   # law and parameter estimate in formation period to transform the spread in trading period
  law2 = uniform_law2$law
  
  # copula_family = c("gaussian", "t", "clayton", "gumbel", "frank", "joe", "BB1", "BB6", "BB7", "BB8", "tawn1", "tawn2") # copula type used for estimation
  #copula_id = c(1,2,3,4,5,6,7,8,9,10,104,204)  # without rotation
  copula_id = c(1,2,3,4,5,6,7,8,9,10,13,16,18,20,23,24,26,27,28,29,30,33,34,36,37,38,39,40,104,114,124,134,204,214,224,234)
  aic_min = Inf
  for (i in 1:length(copula_id)) 
  {
    copule_estime = BiCopEst(U1,U2, family = copula_id[i])
    if(copule_estime$AIC < aic_min)
    {
      aic_min = copule_estime$AIC
      best_id = copula_id[i]
    }
  }
  return(list(copule_id=best_id, law1=law1, law2=law2))  # we return the id of the family of copula that minimise AIC
}


#### Trading Period ####

### Mispricing Index ###

compute_finite_difference = function(U1, U2, copule_id, by, espilon) 
{
  copule = BiCopEst(U1,U2, family = copule_id)
  if(by=="U1")
  {
    C1 = BiCopCDF(pmax(U1-epsilon,0),U2, copule_id, par=copule$par, par2=copule$par2) # we replace u-epsilon by max(u-epsilon,0) to force input to be uniform([0,1])
    C2 = BiCopCDF(pmin(U1+epsilon,1),U2, copule_id, par=copule$par, par2=copule$par2) # we replace u+epsilon by max(u+epsilon,1)
  }
  if(by=="U2")
  {
    C1 = BiCopCDF(U1,pmax(U2-epsilon,0), copule_id, par=copule$par, par2=copule$par2)
    C2 = BiCopCDF(U1,pmin(U2+epsilon,1), copule_id, par=copule$par, par2=copule$par2)
  }
  h = pmin(pmax((C2-C1)/(2*epsilon),0),1)  # we check proba in [0,1]
  return(h)
}

transform_obs_uniform = function(S, law) # spread observation from trading period with its law 
{
  type_law = as.character(law$type_law)
  param1 = as.numeric(law$param1)
  param2 = as.numeric(law$param2)
  param3 = as.numeric(law$param3)
  
  if(type_law == "t")
  {
    U = pt((S-param1)/param2, df = param3) # here we center and reduct our spread because pt() function take only degree of freedom parameter and not mean and scale
  }
  if(type_law == "cauchy")
  {
    U = pcauchy(S, location=param1, scale=param2)
  }
  if(type_law == "normal")
  {
    U = pnorm(S, mean=param1, sd=param2)
  }
  return(U)
}

compute_h = function(S1, S2, copule_id, law1, law2, epsilon) # spread observation from trading period without transformation, the copule from formation period and law (NB:law = c("type_law",param1,param2)
{
  if(is.null(S1) | is.null(S2)) # in case of week without stationnary spreads
  {
    return(NULL)
  }
  
  U1 = transform_obs_uniform(S1, law1)
  U2 = transform_obs_uniform(S2, law2)
  
  h12 = compute_finite_difference(U1,U2,copule_id,"U2",epsilon)
  h21 = compute_finite_difference(U1,U2,copule_id,"U1",epsilon)
  
  cop_used = BiCopEst(U1,U2, family = copule_id)
  copule_type = cop_used$familyname
  
  return(list(h12=h12, h21=h21, copule_type=copule_type))
}


#### Main ####

spread_data = read.csv("C:/Users/matth/OneDrive/Documents/Master Dauphine/Dauphine/Cours/TOBAM/Project/spreads_selected_ADF.csv", header = TRUE, sep = ";")
spread_data_formation = spread_data[1:504,]  #we separate formation and trading period 
spread_data_trading = spread_data[505:672,]

### estimation of copula and law of marginal during formation period ###

result_formation = list()

for (n_week in 1:104)   # we go trough every week 
{
  spread_coin1 = spread_data_formation[[paste0("week_", n_week, "_coin_1")]]
  spread_coin2 = spread_data_formation[[paste0("week_", n_week, "_coin_2")]]
  
  nom_var = paste0("result_week_", n_week)
  result_formation[[nom_var]] = match_copula_law(spread_coin1, spread_coin2)  #estimation of law and copula of week n
}

### compute of partial derivative of copula during trading period (h12 and h21) ###

result_trading = list()
epsilon = 10^(-6)

for (n_week in 1:104)
{
  nom_var = paste0("result_week_", n_week)
  result_formation_week_n = result_formation[[nom_var]]
  
  spread_coin1 = spread_data_trading[[paste0("week_", n_week, "_coin_1")]]
  spread_coin2 = spread_data_trading[[paste0("week_", n_week, "_coin_2")]]
  copule_id = result_formation_week_n$copule_id
  law1 = result_formation_week_n$law1
  law2 = result_formation_week_n$law2
  
  result_trading[[nom_var]] = compute_h(spread_coin1, spread_coin2, copule_id, law1, law2, epsilon)
}


### make excel to export ###

result_h = list() 
type_cop = list()

for (n_week in 1:104) 
{
  nom_colonne_h12 = paste0("week_", n_week, "_h12")
  nom_colonne_h21 = paste0("week_", n_week, "_h21")
  result_h[[nom_colonne_h12]] = result_trading[[paste0("result_week_", n_week)]]$h12
  result_h[[nom_colonne_h21]] = result_trading[[paste0("result_week_", n_week)]]$h21
  
  nom_colonne_type = paste0("week_", n_week, "_type")
  type_cop[[nom_colonne_type]] = result_trading[[paste0("result_week_", n_week)]]$copule_type
}

occurence_copule = as.data.frame((table(sapply(type_cop, identity))/length(type_cop))*100)

df_h = data.frame(result_h) # dataframe with all h we compute during trading period 
df_type_copule = data.frame(type_cop) # dataframe with all copula estimate during formation period  

write.csv2(df_h, file = "C:/Users/matth/OneDrive/Documents/Master Dauphine/Dauphine/Cours/TOBAM/Project/trading_signal_ADF.csv")
write.csv2(df_type_copule, file = "C:/Users/matth/OneDrive/Documents/Master Dauphine/Dauphine/Cours/TOBAM/Project/type_copule_ADF.csv")
write.csv2(occurence_copule, file = "C:/Users/matth/OneDrive/Documents/Master Dauphine/Dauphine/Cours/TOBAM/Project/occurence_copule_ADF.csv")






