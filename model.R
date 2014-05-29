library(deSolve)
library(ggplot2)
library(reshape2)

model <- function(time, y, parms){
      with(as.list(c(y, parms)), {
              dS <- - (I*S)/(I+S+P) - gam*S
                  dI <- (I*S)/(I+S+P) - lambda*I
                      dP <- lambda*I + gam*S
                          return(list(c(dS, dI, dP)))
                            })
}

y <- c(S = 99, I = 1, P = 0)
params <- c(gam = 0.05, lambda = 0.1)
times <- 1:50 #seq(1, 50, 0.01)

solved_model <- ode(y, times, model, params)
solved_model <- data.frame(solved_model)
solved_model <- melt(solved_model, id.vars = 'time')

rieman_integral <- function(y_vals, step_size){
      return(sum(y_vals * step_size))
}

p <- ggplot(solved_model,
       aes(x = time, y = value, col = variable)) +
         geom_line()
         print(p)

         rieman_integral(solved_model$value[solved_model$variable == "I"], 1)
