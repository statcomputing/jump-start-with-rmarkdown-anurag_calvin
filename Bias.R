Bias <- function(simulation_no, t) {
    random_generated_values <- rnorm(simulation_no)
    random_values_lt_t <- random_generated_values[random_generated_values <= 
        t]
    number_satisfying <- length(random_values_lt_t)
    cdf_approx <- number_satisfying/simulation_no
    bias <- abs(cdf_approx - pnorm(t, 0, 1))
    return(bias)
}

data_gen <- function(t_values, simulation_values) {
    table_of_values <- data.frame()
    
    for (t in t_values) {
        for (simulation_no in simulation_values) {
            for (p in 1:100) {
                temp <- Bias(simulation_no, t)
                table_of_values <- rbind(table_of_values, c(t, simulation_no, 
                  p, temp))
            }
        }
    }
    colnames(table_of_values) <- c("t_value", "no_of_simulations", "replication_no", 
        "Bias")
    return(table_of_values)
}

proj_t_values <- c(0, 0.67, 0.84, 1.28, 1.65, 2.32, 2.58, 3.09, 3.72)
proj_simulation_value1 <- c(100)
proj_simulation_value2 <- c(1000)
proj_simulation_value3 <- c(10000)
# proj_simulation_values <- c(100,1000,10000)

table1 <- data_gen(proj_t_values, proj_simulation_value1)
boxplot(Bias ~ t_value, data = table1, ylab = "Bias", xlab = "t values", 
    main = "Bias Plot for n = 100")
table2 <- data_gen(proj_t_values, proj_simulation_value2)
boxplot(Bias ~ t_value, data = table2, ylab = "Bias", xlab = "t values", 
    main = "Bias Plot for n = 1000")
table3 <- data_gen(proj_t_values, proj_simulation_value3)
boxplot(Bias ~ t_value, data = table3, ylab = "Bias", xlab = "t values", 
    main = "Bias Plot for n = 10000")
