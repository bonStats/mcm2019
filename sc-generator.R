library(dplyr)
library(ggplot2)
library(gganimate)

generate_sc <- function(i){
  
  rd <- runif(1)
  hwidth_x <- rd
  hwidth_y <- 1 - rd
  totalw <- rexp(1, rate = 2)
  
  constraint_label <- 
    paste(round(hwidth_x,2),
          "|x| +", round(hwidth_y,2),
          "|y|","<",round(totalw,2))
  
  x <- rnorm(n = 1, sd = 1)
  y <- rnorm(n = 1, sd = 1)
  
  sim_dat <- tibble(
    x = x,
    y = y,
    hwidth_x = hwidth_x,
    hwidth_y = hwidth_y,
    totalw = totalw,
    type = "1: Unconstrained",
    class = "point",
    iter = i,
    poly_group = NA,
    label = constraint_label
  )
 
  poly_dat <- tibble(
    x = c(0,totalw/hwidth_x,0,-totalw/hwidth_x),
    y = c(totalw/hwidth_y,0,-totalw/hwidth_y,0),
    hwidth_x = hwidth_x,
    hwidth_y = hwidth_y,
    totalw = totalw,
    type = "1: Unconstrained",
    class = "poly",
    poly_group = i,
    iter = i,
    label = constraint_label
  )
  
  if(hwidth_x * abs(x) + hwidth_y * abs(y) < totalw){
    sim_dat <- bind_rows(sim_dat, mutate(sim_dat, type = "2: Accepted points", iter = i))
    
    poly_dat <- bind_rows(poly_dat, mutate(poly_dat, type = "2: Accepted shape", iter = i))
  }

  
  return(
    list(
      sim_dat = sim_dat,
      poly_dat = poly_dat
    )
  )
}


N <- 10

sim_dat <- NULL
poly_dat <- NULL

for(i in 1:N){
  
  sc <- generate_sc(i)
  
  sim_dat <- bind_rows(sim_dat, sc$sim_dat)
  poly_dat <- bind_rows(poly_dat, sc$poly_dat)
  
  sim_dat <- bind_rows(sim_dat, filter(sim_dat, type == "2: Accepted points", iter < i) %>% mutate(iter = i))
  poly_dat <- bind_rows(poly_dat, filter(poly_dat, type == "2: Accepted shape", iter < i) %>% mutate(iter = i))
  cat(i,"\n")
}

dat <- bind_rows(sim_dat,poly_dat)


## 

pp <- dat %>%
ggplot() +
  geom_point(aes(x = x, y = y), data = function(x) filter(x, class == "point"), alpha = 0.3) +
  geom_polygon(aes(x = x, y = y, group = poly_group), data =  function(x) filter(x, class == "poly"), alpha = 0.3, fill = "blue", colour = NA) +
  theme_bw(base_size = 20) +
  xlab("x") + ylab("y") +
  facet_wrap(~type, drop = F) +
  transition_manual(frames = iter) + enter_fade() + exit_fade()

animate(pp, width = 1200, height = 330, fps = 5)

anim_save("imgs/sc-rejection-viz1.gif")

pp_pause <-  dat %>% filter(iter == max(iter), grepl("Accepted", type)) %>% ggplot() +
  geom_point(aes(x = x, y = y), data = function(x) filter(x, class == "point"), alpha = 0.3) +
  geom_polygon(aes(x = x, y = y, group = poly_group), data =  function(x) filter(x, class == "poly"), alpha = 0.3, fill = "blue", colour = NA) +
  theme_bw(base_size = 20) +
  xlab("x") + ylab("y") +
  facet_wrap(~type, drop = F)

pp_pause


####

N <- 500

poly_dat <- NULL

for(i in 1:N){
  
  sc <- generate_sc(i)
  
  #sim_dat <- bind_rows(sim_dat, sc$sim_dat)
  poly_dat <- bind_rows(poly_dat, sc$poly_dat)
  
}

pp_pause2 <-  poly_dat %>% filter(grepl("Accepted", type)) %>% ggplot() +
  geom_polygon(aes(x = x, y = y, group = poly_group), data =  function(x) filter(x, class == "poly"), alpha = 0.05, fill = "blue", colour = NA) +
  theme_bw(base_size = 20) +
  xlab("x") + ylab("y") + xlim(-20,20) + ylim(-20,20)

pp_pause2
