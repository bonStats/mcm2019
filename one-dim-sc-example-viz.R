
bsp <- ggplot(aes(x = x), data = data.frame(x = 0)) + theme_bw() + 
  xlab(expression(theta)) + ylab(expression("f ("~theta~")"))


flat <- function(x, constant){
  constant
}

bsp + stat_function(fun = flat, args = list(constant = 0.5)) +
  stat_function(fun=flat, geom = 'area', fill = 'red', alpha = 0.2,args = list(constant = 0.5))


mcp <- function(x, lambda, gamma){
  ifelse(abs(x) < lambda*gamma,
         lambda * abs(x) - (x^2)/(gamma*lambda),
         0.5 * gamma * (lambda^2)
  )
}

normal_mcp_exp_unnormalised <- function(x, mean, sd, lambda, gamma, rate){
  dnorm(x, mean = mean, sd = sd) * 
    exp(- rate * mcp(x, lambda, gamma))
}

normal_mcp_exp <- function(x, mean, sd, lambda, gamma, rate){
  
  normal_mcp_exp_unnormalised(x, mean, sd, lambda, gamma, rate) /
    
    integrate(normal_mcp_exp_unnormalised, lower = -20, upper = 20, 
              mean = mean, 
              sd = sd,
              lambda = lambda, 
              gamma = gamma, 
              rate = rate)$value
}

argsl <- list(
  mean = 0, 
  sd = 1, 
  lambda = 1, 
  gamma = 4, 
  rate = 1
)

bsp + stat_function(fun = dnorm, args = argsl[1:2]) +
  stat_function(fun=dnorm, geom = 'area', fill = 'red', alpha = 0.2, args = argsl[1:2])

bsp + stat_function(fun = normal_mcp_exp, args = argsl) +
  stat_function(fun=normal_mcp_exp, geom = 'area', fill = 'red', alpha = 0.2, args = argsl)

normal_l1_gamma <- function(x, sc){
  
  dnorm(x, mean = 0, sd = 1) * 
    pgamma(abs(x), shape = 0.5, rate = 1, lower.tail = F) / sc
  
}

normal_l1_exp <- function(x, sc){
  
  dnorm(x, mean = 0, sd = 1) * 
    pexp(abs(x), rate = 1, lower.tail = F) / sc
  
}

sc_normal_l1_gamma <-   integrate(normal_l1_gamma, lower = -20, upper = 20, 
                                  sc = 1)

sc_normal_l1_exp <-   integrate(normal_l1_exp, lower = -20, upper = 20, 
                                sc = 1)

plot_normal <- bsp + stat_function(fun = dnorm, n = 1000, args = list(mean = 0, sd = 1)) +
  stat_function(fun=dnorm, geom = 'area', fill = 'red', alpha = 0.2, n = 1000, args = list(mean = 0, sd = 1)) +
  xlim(-2,2) + ylim(0,1.5) + theme(text = element_text(size=20),plot.title = element_text(size=25, hjust = 0.0)) 
#ggtitle("Base: N(0,1)     Constraint: none     Penalty: none")

plot_normal_l1_exp <- bsp + stat_function(fun = normal_l1_exp, n = 1000, args = list(sc = sc_normal_l1_exp$value)) +
  stat_function(fun=normal_l1_exp, geom = 'area', fill = 'red', alpha = 0.2, n = 1000,  args = list(sc = sc_normal_l1_exp$value)) +
  xlim(-2,2) + ylim(0,1.5) + theme(text = element_text(size=20), plot.title = element_text(size=25, hjust = 0.0)) 
#ggtitle(expression("Base: N(0,1)     Constraint: Exp(1)     Penalty: |"~theta~"|"))


plot_normal_l1_gamma <- bsp + stat_function(fun = normal_l1_gamma, n = 1000, args = list(sc = sc_normal_l1_gamma$value)) +
  stat_function(fun=normal_l1_gamma, geom = 'area', fill = 'red', alpha = 0.2, n = 1000,  args = list(sc = sc_normal_l1_gamma$value)) +
  xlim(-2,2) + ylim(0,1.5)  + theme(text = element_text(size=20),plot.title = element_text(size=25, hjust = 0.0))
#ggtitle(expression("Base: N(0,1)     Constraint: Gamma(0.5,1)     Penalty: |"~theta~"|"))

