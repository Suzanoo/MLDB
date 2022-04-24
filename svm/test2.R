Smarket %>% 
  select(matches("Lag1")) %>% 
  scale() %>% 
  as.factor(lev)


lapply(Smarket[,"Lag1"], as.factor)
s