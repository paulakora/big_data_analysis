# Bootstrap ---------------------------------------------------------------

area <- main_data %>% select(S14A, S14B)
summary(area)

area$change <- area$S14B - area$S14A

area <- area[rowSums(is.na(area)) == 0, ] 

set.seed(1234)                                 
myBootstrap <- boot(area, foo, R = 1000, cor.type = "s") 

# Inspecting results ------------------------------------------------------

View(myBootstrap)

head(myBootstrap$t)

View(myBootstrap$t)

head(myBootstrap$t0)

print(myBootstrap)

# Plots -------------------------------------------------------------------

plot(myBootstrap, index=1)
boot.ci(myBootstrap, index=1)

plot(myBootstrap, index=2)
boot.ci(myBootstrap, index=2)

plot(myBootstrap, index=3)
boot.ci(myBootstrap, index=3)

# Results in data frame -----------------------------------------------------------------

ci <- boot.ci(myBootstrap, conf = 0.95, index=1) 

bootstrap <- data.frame(                                 
  'area change per 1 farm (ha)-' = ci[["bca"]][4],
  'area change per 1 farm (ha)' = mean(area$change),                          
  'area change per 1 farm (ha)+' = ci[["bca"]][5],
  'area change for total sample (ha)-' = ci[["bca"]][4],
  'area change for total sample (ha)' = sum(area$change),
  'area change for total sample (ha)+' = ci[["bca"]][5],
  'sample area' = sum(area$S14B), 
  'N' = nrow(area)
)
