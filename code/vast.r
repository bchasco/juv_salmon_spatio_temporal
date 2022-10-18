library(VAST)
df <- read.csv("./data/cpue data.csv", header=TRUE)
df$c <- paste(df$species,df$age)

strata.limits <- data.frame(
  'STRATA' = c("OR/WA"),
  'north_border' = c(49.0),
  'south_border' = c(42.0)
)

# Make settings (turning off bias.correct to save time for example)
settings = make_settings( n_x = 100, 
                          Region = "california_current",
                          strata.limits = strata.limits,
                          purpose = "index", 
                          bias.correct = FALSE)

settings$FieldConfig['Beta',] <- "IID" #Intercepts
settings$FieldConfig['Omega',] <- 0 #Spatial
settings$FieldConfig['Epsilon',] <- 0 #Spatio-temporal
ObsModel = c(2,0) # Distribution for data, and link-function for linear predictors


# Run model
fit = fit_model( settings = settings, #List of all of the settings
                 Lat_i = df[,'lat'], 
                 Lon_i = df[,'long'], 
                 t_i = df[,'Year'], #Temporal effects
                 b_i = as_units(df[,'cpue'],'count/km2'), #density
                 c_i = as.numeric(as.factor(df[,'c']))-1, #Categrories
                 a_i = (df[,'a']*0.02), #Efforts with net width
                 getsd = TRUE, #Standard errors for derived variables
                 fine_scale = FALSE, 
                 Options = c('treat_nonencounter_as_zero' = TRUE)) #Some years have no salmon observations

# Plot correlations (showing Omega1 as example)
require(corrplot)
Cov_omega1 = fit$Report$L_omega1_cf %*% t(fit$Report$L_omega1_cf)
Cov_omega2 = fit$Report$L_omega2_cf %*% t(fit$Report$L_omega2_cf)
corrplot( cov2cor(Cov_omega1), method="pie", type="lower")
corrplot.mixed( cov2cor(Cov_omega1) )

# Plot results
results = plot( fit,
                plot_set = c(3,16,17),
                category_names = unique(df$c) )
