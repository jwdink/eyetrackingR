library(eyetrackingR)
library(lme4)
library(dplyr)
library(ggplot2)
set.seed(5)
df <- simulate_eyetrackingr_data(num_participants = 60, 
                                 num_items_per_condition = 10,
                                 trial_length = 5000, 
                                 pref = .75,
                                 pref_window = c(2500, 5000)
)
df_time <- make_time_sequence_data(data = df, time_bin_size = 100, aois = "AOI1", predictor_columns = "Condition", 
                                   summarize_by = "Participant")
fit <- lmer(Prop ~ (ot1+ot2)*Condition + (ot1+ot2|Participant), data = df_time) # try fitting more/less complex models
plot(df_time, predictor_column = "Condition", model = fit)                      # only ot1 is signifigicant, but we know
drop1(fit, test="Chisq")                                                        # 'true' curve more like ot1+ot2+ot3

# Generate CIs, Extract Divergences
new_dat <- distinct(df_time[,c('Time','ot1','ot2','ot3','Condition')])
mm <- model.matrix(~(ot1+ot2)*Condition, data= new_dat)
pred_fun <- function(.) mm%*%fixef(.)
bb <- bootMer(fit, FUN= pred_fun, nsim=50, .progress='txt') 
colnames(bb$t) <- new_dat$Condition
new_dat$Sig <- apply(X = bb$t[,colnames(bb$t)=="High"] - bb$t[,colnames(bb$t)=="Low"], 
                    MARGIN = 2, FUN = function(x) (mean(x>0) < .05) | (mean(x<0) < .05) )
df_graph <- left_join(df_time, new_dat[,c('Time','Condition', 'Sig')], by = c('Time', 'Condition'))

# Plot:
plot(df_graph, predictor_column = "Condition", model = fit) + 
  coord_cartesian(ylim = c(.4, .7)) +
  geom_ribbon(aes(ymin = ifelse(Sig, 0, .5), ymax = ifelse(Sig, 1, .5)), color=NA, alpha = .25)
# does pretty well with ot2 (divergence around the right place
# but ot2 wasn't significant, so should it be used?
# but if we use just ot1 divergence is way too early.



