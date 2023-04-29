#######################################################################
##### Lapse risk modelling in insurance: a Bayesian mixture approach
##### Authors: Viviana G R Lobo, Thais C O Fonseca and Mariana B Alves
##### Annals of Actuarial Science
#######################################################################

source("up(telco).R")

lapse1=df %>%
  group_by(lapse=Churn) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
lapse1

### histogram 
ggplot(data =df , aes(x = tenure), col=" black") +
  xlab("survival time (in months)") + ylab("frequency")+
  theme_bw() +
  theme(axis.title.x = element_text(color = "black",size=22),
        axis.title.y = element_text(color = "black",size=22), 
        axis.text=element_text(color = "black",size=22), 
        axis.title = element_text(color = "black",size=22)) +
  # facet_wrap() + 
  geom_histogram(bins=15, color='white',fill= "grey50")

### survival analysis
formula_km <- Surv(tenure, status) ~ 1
formula <- Surv(tenure, status) ~ NULL
km <- survfit2(formula_km,df)

# Manipulating the KM to format that matches parsnip predictions.
surv_km <- tidy_survfit(km, type = "surv") %>%
  dplyr::select(.eval_time = time, .pred_survival = estimate) %>%
  tidyr::nest(.pred = c(.eval_time, .pred_survival))


km_fit <- surv_km %>%
  tidyr::unnest(cols = .pred) %>%
  filter(.eval_time < 75)

haz.km = km_fit %>%
  mutate(.haz=tx.emp(.eval_time, .pred_survival)) 

ggplot(data= km_fit, aes(x= .eval_time, y = .pred_survival)) + 
  geom_step(color = "grey50", linewidth=1.5) +
  scale_y_continuous(limits = c(0.5,1),breaks = seq(0, 1, 0.2), labels = point) + 
  scale_x_continuous(limits=c(0,75), breaks = seq(0,60,by=20), labels = grad) +
  theme_bw() +
  theme(plot.title = element_text(lineheight = 2),
        axis.title.x = element_text(color = "black", size = 22),
        axis.title.y = element_text(color = "black", size = 22),
        axis.text = element_text(color="black",size=22)) +
  labs(x =  "survival time (in months)", y = "survival probabilities")


ggplot(data = haz.km, aes(x=.eval_time, y=.haz)) +
  xlab("survival time (in months)") + ylab("hazard curve")+
  theme_bw(base_size = 25) +
  scale_y_continuous(limits = c(0,0.06),breaks = seq(0,0.06, 0.02), labels = point) + 
  scale_x_continuous(limits=c(0,75), breaks = seq(0,75,by=20), labels = grad) +
  geom_line(linewidth=1.5, color="grey50") 



##### usual parametric survival models


exponential_survival <- survival_reg(dist = "exponential") %>%
  set_engine("survival") ### pacote utilizado para modelagem

weibull_survival <- survival_reg(dist="weibull") %>%
  set_engine("survival")

ln_survival <- survival_reg(dist = "lognormal") %>%
  set_engine("survival")


### Bayesian mixture survival model
M=30000
lag=15
ln_mixture1 <- survival_ln_mixture(formula,
                                   data=df, intercept = TRUE, iter=M, warmup=M/2, thin= lag, chains=1,
                                   numero_componentes=2)
ln_mixture2 <- survival_ln_mixture(formula,
                                   data=df, intercept = TRUE, iter=M, warmup=M/2, thin= lag, chains=1,
                                   numero_componentes=3)
ln_mixture3 <- survival_ln_mixture(formula,
                                   data=df, intercept = TRUE, iter=M, warmup=M/2, thin= lag, chains=1,
                                   numero_componentes=4)
ln_mixture4 <- survival_ln_mixture(formula,
                                   data=df, intercept = TRUE, iter=M, warmup=M/2, thin= lag, chains=1,
                                   numero_componentes=5)
ln_mixture5 <- survival_ln_mixture(formula,
                                   data=df, intercept = TRUE, iter=M, warmup=M/2, thin= lag, chains=1,
                                   numero_componentes=6)


specs <- list(Exponential=exponential_survival, Weibull = weibull_survival,LN=ln_survival) #, BMLN= ln_mixture)

models <- map(specs, ~ fit(.x, formula, df, intercept = TRUE))
pred_sob <- map(models, ~ predict(.x, data.frame(val = NA), type = "survival", eval_time = seq(75)))
# Add bayesian fit do models and predictions
models$BMLN2 <- ln_mixture1
models$BMLN3 <- ln_mixture2
models$BMLN4 <- ln_mixture3
models$BMLN5 <- ln_mixture4
models$BMLN6<- ln_mixture5

### 95% credible interval
predIC2=predict(ln_mixture1, data.frame(val = NA), interval='credible', type='survival',eval_time=seq(75))
predIC2 %>% unnest(cols = .pred)
predIC3=predict(ln_mixture2, data.frame(val = NA), interval='credible', type='survival',eval_time=seq(75))
predIC3 %>% unnest(cols = .pred)
predIC4=predict(ln_mixture3, data.frame(val = NA), interval='credible', type='survival',eval_time=seq(75))
predIC4 %>% unnest(cols = .pred)
predIC5=predict(ln_mixture4, data.frame(val = NA), interval='credible', type='survival',eval_time=seq(75))
predIC5 %>% unnest(cols = .pred)
predIC6=predict(ln_mixture5, data.frame(val = NA), interval='credible', type='survival',eval_time=seq(75))
predIC6 %>% unnest(cols = .pred)

all_preds <- bind_rows(pred_sob, .id = "modelo") %>%
  group_by(modelo) %>%
  ungroup() %>%
  tidyr::unnest(cols = .pred) %>%
  mutate(modelo = factor(modelo, levels = c("Exponential", "Weibull", "LN", "BMLN2",
                                            "BMLN3","BMLN4","BMLN5","BMLN6")))


ggplot(aes(x = .eval_time, y = .pred_survival), data = all_preds) +
  geom_step(data = km_fit, linewidth=1) +
  xlab("survival time (in months)") + ylab("survival probabilities")+
  theme_bw(base_size = 25) +
  scale_y_continuous(limits = c(0.6,1),breaks = seq(0.6, 1, 0.2), labels = point) + 
  scale_x_continuous(limits=c(0,75), breaks = seq(0,75,by=20), labels = grad) +
  geom_line(linewidth=1.5, color="grey50") +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper), alpha = 0.4, colour = NA, fill="grey70") +
  facet_wrap(~modelo, ncol=4, nrow=2) 


haz.fit= all_preds %>%
  group_by(modelo) %>%
  mutate(.haz=tx.emp(.eval_time, .pred_survival))

ggplot(aes(x = .eval_time, y = .haz), data = haz.fit) +
  geom_line(data = haz.km, aes(x=.eval_time, y=.haz), linewidth=1) +
  xlab("survival time (in months)") + ylab("hazard curve")+
  theme_bw(base_size = 25) +
  scale_y_continuous(limits = c(0,0.06),breaks = seq(0,0.06, 0.02), labels = point) + 
  scale_x_continuous(limits=c(0,75), breaks = seq(0,75,by=20), labels = grad) +
  geom_line(linewidth=1.5, color="grey50") +
  facet_wrap(~modelo, ncol=4) 

pred_sob$BMLN2 <- predIC2
pred_sob$BMLN3 <- predIC3
pred_sob$BMLN4 <- predIC4
pred_sob$BMLN5 <- predIC5
pred_sob$BMLN6 <- predIC6

all_preds <- bind_rows(pred_sob, .id = "modelo") %>%
  group_by(modelo) %>%
  ungroup() %>%
  tidyr::unnest(cols = .pred) %>%
  mutate(modelo = factor(modelo, levels = c("Exponential", "Weibull", "LN", "BMLN2",
                                            "BMLN3","BMLN4","BMLN5","BMLN6")))
