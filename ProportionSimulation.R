###(1) Simulate 20 values from a Bernoulli distribution where p = .50
set.seed(1)
data_1 <- rbinom(200000, 1, .5)
replication1 <- rep(c(1:10000), each = 20)
table(replication1)
sum(data_1 == 1)/length(data_1)

#create dataset
dataframe_1 <- data.frame(data_1, replication1)
write.csv(dataframe_1, file = "/Users/madelinecraft/Desktop/Sim_Data/size20", na = ".")

###(2) Simulate 25 values from a Bernoulli distribution where p = .40
set.seed(1)
data_2 <- rbinom(250000, 1, .4)
replication2 <- rep(c(1:10000), each = 25)
table(replication2)
sum(data_2 == 1)/length(data_2)

#create dataset
dataframe_2 <- data.frame(data_2, replication2)
write.csv(dataframe_2, file = "/Users/madelinecraft/Desktop/Sim_Data/size25", na = ".")

###(3) Simulate 34 values from a Bernoulli distribution where p = .3030
set.seed(1)
data_3 <- rbinom(340000, 1, .3030)
replication3 <- rep(c(1:10000), each = 34)
table(replication3)
sum(data_3 == 1)/length(data_3)

#create dataset
dataframe_3 <- data.frame(data_3, replication3)
write.csv(dataframe_3, file = "/Users/madelinecraft/Desktop/Sim_Data/size34", na = ".")

###(4) Simulate 50 values from a Bernoulli distribution where p = .2
set.seed(1)
data_4 <- rbinom(500000, 1, .20)
replication4 <- rep(c(1:10000), each = 50)
table(replication4)
sum(data_4 == 1)/length(data_4)

#create dataset
dataframe_4 <- data.frame(data_4, replication4)
write.csv(dataframe_4, file = "/Users/madelinecraft/Desktop/Sim_Data/size50", na = ".")

###(5) Simulate 100 values from a Bernoulli distribution where p = .1
set.seed(1)
data_5 <- rbinom(1000000, 1, .10)
replication5 <- rep(c(1:10000), each = 100)
table(replication5)
sum(data_5 == 1)/length(data_5)

#create dataset
dataframe_5 <- data.frame(data_5, replication5)
write.csv(dataframe_5, file = "/Users/madelinecraft/Desktop/Sim_Data/size100", na = ".")



###Unfinished simulation loop
si = 20, 25, 33, 50, 100
proba = .5, .4, .3030, .2, .1

draw_function <- function(si = 100, proba = .1)
{
sample_draw <- rbinom(n = si, 1, prob = proba)
prop_estimate <- sum(data_1 == 1)/length(sample_draw)
s_estimate <- sqrt(prop_estimate*(1 - prop_estimate))
confidence_interval <- list(c(prop_estimate - 1.96*s_estimate/sqrt(si), prop_estimate + 1.96*s_estimate/sqrt(si)))
list(mean_sample = mean_estimate, s_sample = s_estimate, c_sample = confidenc_interval)
}

draw_sample <- function(s = 1, r = 1, p = 0.5){
	for (i in 1:r) {
		inter <- list(mean_p = c(), mean_s = c(), mean_c = c())
		samp <- draw_function(si = s, proba = p)
		inter$mean_p = c(inter$mean_p, samp$mean_sample)
		inter$mean_s = c(inter$mean_s, samp$s_sample)
		inter$mean_c <- c(inter$mean_c, samp$c_sample)
			if(inter[[3]][[i]][1] > p & inter[[3]][[i]][2] < p)	{
				mean_c <- (mean_c + 1)/i
			}
			else {
				meanc <- (mean_c + 0)/i}
}

return(list(mean(inter$mean_p), mean(inter$mean_s), inter$mean_c))


