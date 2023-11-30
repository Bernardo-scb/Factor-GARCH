##############################################################
# Title: VaR High dimensional portfolio with Factor GARCH
#
# Author: Bernardo Scarpelli
#
# Last updated : 27/11/2023
#############################################################

# Packages
rm(list = ls())
library(tidyverse)
library(janitor)
library(rlang)
library(expm)
library(tsibble)
library(fBasics)
library(MTS)
library(rugarch)
library(rmgarch)
library(data.table)
library(tsibble)
library(lubridate)
library(furrr)


############################################################
# Function
############################################################

tv_matrix_c <- function(df, date, f,gmod = "sGARCH"){
  
  # Filtering the DF
  
  df %>% 
    filter(Date<date) %>% 
    dplyr::select(-Date) -> aux_1
  
  # Demeaning
  
  aux_1 %>% 
    mutate_all(~. - mean(.)) -> aux_1
  
  # Factors
  
  aux_1 %>% 
    cov() %>% 
    eigen() -> decomp
  
  V <- decomp$vectors[,1:f]
  
  D <- decomp$values[1:f] %>% 
    sqrt() %>% 
    diag() %>% 
    solve()
  
  Fact <- D%*%t(V)%*%t(aux_1) %>% 
    t()
  
  
  # Loading's
  
  Load <- V%*%solve(D)
  
  spec <- ugarchspec(variance.model = list(model = gmod, garchOrder = c(1,1)),
                     mean.model = list(armaOrder = c(0, 0)))
  
  
  (aux_1 - Fact%*%t(Load))^2 %>% 
    colMeans() %>% 
    diag() -> R
  
  # Estimating GARCH for each Factor
  
  
  Fact %>% 
    as.data.frame() %>% 
    as.list() %>%
    map(~ugarchfit(spec, data = .x,solver = "hybrid")) %>% 
    map(~sigma(ugarchforecast(.x,n.ahead = 1))) %>% 
    unlist() %>% 
    diag()-> sigma_pred
  
  sigma_pred <- sigma_pred^2
  
  
  
  
  # Estimating the Factor part of the Variance:
  
  
  
  
  
  Fac_vol <- Load%*%sigma_pred%*%t(Load)
  
  # Residual Var
  
  
  
  (Fac_vol + R ) %>% 
    return()
  
  
  
}


############################################################
# Loading Data 
############################################################

DD_df <- readRDS("C:/Users/berna/OneDrive/Área de Trabalho/AA EESP/Dissertação/R_codes/Proj_port_selec/Daily_data/output_data/DD_df.rds") %>% 
  as.data.frame()

# Selecting the prediction range

DD_df$Date[year(DD_df$Date)>2019] -> date_aux


############################################################
# Estimating Covariance Matrix
############################################################


# Estimating Each Covariance Matrix


plan(multisession, workers = 4)
date_aux %>% 
  future_map(~tv_matrix_c(DD_df,.x,10)) -> cov_list
plan(NULL)


# Saving Estimated Covariance matrices
cov_list %>% 
  saveRDS(file = file.path(getwd(), "output_data","cov_list.rds"))

############################################################
# Estimating VaR for a fictional portfolio
############################################################

set.seed(28112023)
### Creating a fictional Portfolio

w_list <- list()


for(i in c(1:nrow(DD_df))){
  
  (rep(1/272) + rnorm(272, sd = 0.015)) -> w
  
  (w/sum(w)) %>% 
    as.matrix()-> w_list[[i]]
  
}


w_list[[1]]

# Variance Of the fictional portfolio on the test sample

v_list <- list()

for(i in c(1:length(cov_list))){
  
  v_list[[i]] <- list(cov_list[[i]],w_list[[length(w_list) - length(cov_list) + i]])
}


v_list %>% 
  map(~t(.x[[2]])%*%.x[[1]]%*%.x[[2]]) %>% 
  unlist() -> var_vec

plot(var_vec, type = "l")



# Actual Return of the portfolio on the test sample

# matrix to calculate returns

DD_df %>% 
  filter(Date>=date_aux[[1]]) %>% 
  select(-Date) %>% 
  as.matrix() -> aux_ret_mat

w_list %>% 
  tail(length(date_aux)) %>% 
  purrr::reduce(cbind) -> w_mat

aux_ret_mat%*%w_mat %>% 
  diag() -> ret


# Data Frame to Plot Returns
df_plot <- data.frame(ret = ret, Date = date_aux )


ggplot(data = df_plot, aes(x = Date)) +
  theme_bw(base_size = 10) +
  theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
  xlab("Date") + ylab("Return") +
  geom_line(
    aes(y = ret, color = "OBS"),
    size = 1
  ) +
  scale_colour_manual(values = c(
    "black"
  )) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  ) +
  labs(title = "Return")



# Estimating the mean, using the training sample

DD_df %>% 
  filter(Date<date_aux[[1]]) %>% 
  select(-Date) %>% 
  as.matrix() -> m_mat


w_list %>% 
  head((nrow(DD_df) - length(date_aux))) %>% 
  purrr::reduce(cbind) -> w_mat_2
  

m_mat%*%w_mat_2 %>% 
  diag() %>% 
  mean() -> m_port


# Creating data Frame with the return and the Var result

data.frame(ret = ret,
           lu = m_port + 1.96*sqrt(var_vec),
           li = (m_port - 1.96*sqrt(var_vec)),
           Date = date_aux) -> var_df


# Ploting the Result
gg <- ggplot(data = var_df, aes(x = Date)) +
  theme_bw(base_size = 10) +
  theme(plot.margin = unit(c(5, 7, 2, 2), "mm")) +
  xlab("Date") + ylab("Excess Return") +
  geom_line(
    aes(y = ret, color = "OBS"),
    size = 1
  ) +
  geom_line(
    aes(y = lu, color = "IC"),
    size = 1,
    linetype = "dashed"
  ) +
  geom_line(
    aes(y = li, color = "IC"),
    size = 1,
    linetype = "dashed"
  ) +
  
  scale_colour_manual(values = c(
     "red", "black"
  )) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  ) +
  labs(title = "VaR 95")
gg














































