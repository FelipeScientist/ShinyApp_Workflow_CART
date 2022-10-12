# Pacotes necessarios ----
#install.packages('bs4Dash')
#install.packages('DT')
#install.packages('shinyWidgets')
#install.packages("heatmaply")
#install.packages("ggcorrplot")
#install.packages('ggridges')
#install.packages('ggExtra')
install.packages("rlang")
#install.packages("ggplot2", dependencies = TRUE, repos = "http://cran.us.r-project.org")
remove.packages('rlang')
update.packages('rlang')
library(rlang)
library(heatmaply)
library(ggcorrplot)
library(tidyverse)
library(shiny)
library(DT)
library(shinyWidgets)
library(bs4Dash)
library(rpart.plot)
library(rpart)
library(mlbench)
library(ggplot2)
library(stringr)
library(MLmetrics)
library(magrittr)
library(GGally)
library(plotly)
library(RColorBrewer)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)
library(ggridges)
library(ggExtra)
library(shinyjs)
library(r2d3)
library(shinythemes)


# Base de exemplo ----
library(mlbench)
data('BreastCancer')
base <- BreastCancer 
base <- base[complete.cases(base),] 
base <- base %>% na.omit()
levels(base$Class)[match("malignant",
                         levels(base$Class))] <- 1
levels(base$Class)[match("benign",
                         levels(base$Class))] <- 0

base$Class
base <- subset(base, 
               select = -c(1))
splits <- sample(
  c(rep(0, 0.8*nrow(base)), 
    rep(1, 0.2*nrow(base)), 1))

base$split <- splits

data <- data.frame( var1 = 1:100 + rnorm(100,sd=20), v2 = 1:100 + rnorm(100,sd=27), v3 = rep(1, 100) + rnorm(100, sd = 1)) 
data$v4 = data$var1 ** 2 
data$v5 = -(data$var1 ** 2) 


# Funcoes app ----

algor_graph_corr <- function(base) {
  temp <- base %>% 
    select_if(!(base %>% 
                  sapply(is.factor)))
  if(dim(temp)[2] == 1) {
    return('NOK')
  } else {
    # Return ggplot corr ----
    return(ggcorr(base ,
                  method = c("everything", "pearson"),
                  geom =  "blank",
                  label = TRUE,
                  hjust = 0.5) +
             geom_point(size = 15,
                        aes(color = coefficient > 0,
                            alpha = abs(coefficient) > 0.5)) +
             scale_alpha_manual(values = c("TRUE" = 0.25,
                                           "FALSE" = 0)) +
             guides(color = FALSE,
                    alpha = FALSE))
    # Return plotly corr ----
    # corr <- round(cor(temp), 1)
    # corr_plot <- heatmaply_cor(
    #   corr,
    #   node_type = "scatter",
    #   point_size_mat = corr, 
    #   colors = colorRampPalette(brewer.pal(3, "RdBu"))(256),
    #   xlab = "Features", 
    #   ylab = "Features",
    #   k_col = 2, 
    #   k_row = 2,
    #   label_names = c("x", "y", "Correlation")
    # )
    # return(corr_plot)
  }
} # DONE 
algor_graph_corr(data)

algor_graph_density <- function(base, var_x, var_fator) {
  # Pelo sobreposto ----
  # ggplot(data=base, aes(x= base[[var_x]], 
  #                       group = base[[var_fator]], 
  #                       fill = base[[var_fator]])) +
  #   geom_density(adjust=1.5, alpha=.4) +
  #   theme_ipsum_es() + 
  #   xlab(var_x) + 
  #   scale_fill_discrete(name = var_fator)
  # 
  # Pelas ondas ----
  
  ggplot(base, 
         aes(x = base[[var_x]], 
             y = base[[var_fator]], 
             fill = stat(x))) +
    xlab(var_x) + 
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
    scale_fill_viridis_c(name = var_x, option = "C")
  
} # DONE
algor_graph_density(base %>%
                      mutate_all(as.numeric) %>%
                      mutate(Class = as.factor(Class)), 'Cl.thickness', 'Class')

algor_graph_marg_dist <- function(base, var_x, var_y, var_sep, tipo = "boxplot") {
  step_one <- ggplot(base, aes(x = base[[var_x]], 
                               y = base[[var_y]], 
                               color = base[[var_sep]] %>% as.factor())) +
    geom_point() +
    xlab(var_x) + 
    ylab(var_y)  + labs(color=var_sep) 
    theme(legend.position="none")
  step_two <- ggMarginal(step_one, 
                         type= tipo, 
                         data = base,
                         groupColour = TRUE, groupFill = TRUE)
  step_two
} # DONE
algor_graph_marg_dist(base %>%
                        mutate(x = rexp(dim(base)[1]),
                               y = rnorm(dim(base)[1], 10, 3)) , 'x', 'y', 'Class', tipo = "boxplot")

algor_graph_badrate <- function(base, bad_rate_var, var) {
  
  #if(tipo == 'agrupar') {
    temp <- base %>% 
      mutate(x = ntile(.data[[var]], 10)) %>% 
      group_by(x) 
    temp[[bad_rate_var]] <- temp[[bad_rate_var]]  %>% as.numeric() - 1
    temp <- temp %>% 
      summarise(n = n(),
                f = sum(.data[[bad_rate_var]]),
                bd = f/n)
    
    ggplot(temp,
           aes(x = x, y = bd)) +
      geom_segment(aes(x = x,
                       xend = x,
                       y = 0,
                       yend = bd)) +
      geom_point(size = 4, 
                 color = 'red', 
                 fill = alpha("orange", 0.3), 
                 alpha=0.7, 
                 shape=21, 
                 stroke=2) +
      theme_light() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank()
      ) +
      xlab(var) +
      ylab('Bad Rate')
   #} else {
  #   ggplot(base,
  #          aes(x = .data[[var]], y = .data[[bad_rate_var]])) +
  #     geom_segment(aes(x = .data[[var]],
  #                      xend = .data[[var]],
  #                      y = 0,
  #                      yend = .data[[bad_rate_var]])) +
  #     geom_point(size = 4, 
  #                color = 'red', 
  #                fill = alpha("orange", 0.3), 
  #                alpha=0.7, 
  #                shape=21, 
  #                stroke=2) +
  #     theme_light() +
  #     theme(
  #       panel.grid.major.x = element_blank(),
  #       panel.border = element_blank(),
  #       axis.ticks.x = element_blank()
  #     ) +
  #     xlab(var) +
  #     ylab('Bad Rate')
  # }
  # 
  # 
  # 
} # DONE
algor_graph_badrate(base %>%
                        mutate_all(as.numeric), 'Class', 'Mitoses')

algor_laco_arvore <- function(base, str_formula, lacos){
  aux2 <- data.frame()
  k <- 1
  for (i in lacos){
    set.seed(123)
    arvore <- rpart(data = base %>% 
                      filter(splits == 0), 
                    formula = as.formula(paste0(str_formula)),
                    control = rpart.control(minbucket = i, 
                                            cp = 0, 
                                            maxsurrogate = 0, 
                                            xval = 5))
    
    best_cp <- arvore$cptable[which.min(arvore$cptable[, 'xerror']), 
                              c('CP','xerror')][1]
    arvore_podada <- prune(arvore, cp = best_cp)
    
    aux <- left_join(base %>% 
                       filter(split == 0) %>% 
                       mutate(score_tree = round(predict(arvore_podada,.),6)[,2]) %>% 
                       group_by(score_tree) %>% 
                       summarise(n = n(),
                                 f = as.numeric(as.character(Class)) %>% sum(),
                                 hr = f/n) %>% 
                       arrange(desc(score_tree)),
                     
                     base %>% 
                       filter(split == 1) %>% 
                       mutate(score_tree = round(predict(arvore_podada,.),6)[,2]) %>% 
                       group_by(score_tree) %>% 
                       summarise(n = n(),
                                 f = as.numeric(as.character(Class)) %>% sum(),
                                 hr = f/n), by = 'score_tree') %>% 
      mutate(rz_hr = hr.y/hr.x) %>% 
      mutate(hrcum_x = cumsum(f.x)/cumsum(n.x),
             hrcum_y = cumsum(f.y)/cumsum(n.y),
             total_vol = cumsum(n.x) + cumsum(n.y),
             total_hr = (cumsum(f.x) + cumsum(f.y))/(cumsum(n.x) + cumsum(n.y)),
             min_bucket = i,
             cp = best_cp,
             comb = k,
             folhas = row_number()) %>% 
      filter(total_vol < 500)
    aux2 <- rbind(aux, aux2)
    k <- k + 1
  }
  aux4 <- aux2 %>%
    mutate(rz_hrcum = hrcum_y/hrcum_x) %>%  arrange(comb, desc(folhas))
  aux4 <- aux4[!duplicated(aux4$comb),]
  return(aux4)
}
algor_final_arvore <- function(tabela, combinacao, base, str_formula){
  set.seed(123)
  arvore_final <- rpart(data = base %>%
                          filter(splits == 0),
                        formula = as.formula(paste0(str_formula)),
                        control = rpart.control(minbucket = tabela[tabela['comb'] == combinacao,]$min_bucket,
                                                cp = tabela[tabela['comb'] == combinacao,]$cp,
                                                maxsurrogate = 0,
                                                xval = 5))
  regras <- rpart.rules(arvore_final)
  retorno <- list('arvore' = arvore_final,'regras' = regras)
  return(retorno)
}

algor_pad <- function(coluna) {
  return((coluna - mean(coluna))/sd(coluna))
}
algor_norm <- function(coluna) {
  return((coluna - min(coluna))/(max(coluna) - min(coluna)))
}