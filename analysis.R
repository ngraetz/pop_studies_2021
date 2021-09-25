## Load libraries
library(data.table)
library(ggplot2)
library(sf)
library(MASS)
library(pscl)
library(grid)
library(gridExtra)
library(interplot)
library(rsq)
element_textbox <- function(...) {
  el <- element_text(...)
  class(el) <- c("element_textbox", class(el))
  el
}
element_grob.element_textbox <- function(element, ...) {
  text_grob <- NextMethod()
  rect_grob <- element_grob(calc_element("strip.background", theme_bw()))
  
  ggplot2:::absoluteGrob(
    grid::gList(
      element_grob(calc_element("strip.background", theme_bw())),
      text_grob
    ),
    height = grid::grobHeight(text_grob), 
    width = grid::unit(1, "npc")
  )
}

## Set working directory
repo <- 'C:/Users/ncgra/Dropbox/Penn/repos/pop_studies_2021'

###########################################################################################
###########################################################################################

## Load input data
grav_data <- fread(paste0(repo, '/input_data.csv'))
grav_data[, migrants := da_pb_closed]
grav_data[, region_f := factor(region_f, levels=c('North Africa and Middle East','Sub-Saharan Africa','Asia','Latin America and Caribbean'))]

## Model settings (select IV, interaction IV, etc.)
iv <- 'l.r.15.24'
int_iv <- 'gdp_orig'
#int_iv <- 'lni_orig'
#int_iv <- 'unemployment'
int_iv_name <- 'Log GDP per capita (standard deviations)'
#int_iv_name <- 'Unemployment rate (ages 15-24)'
intervention_variable <- 'l.r.15.24'
#intervention_variable <- 'gdp_orig'
#intervention_variable <- 'mort_nature'
#intervention_variable <- 'gdp_gap'
intervention_value <- 0
ldi_inc <- 0.33
keep_regions <- paste0('ALLINT_',int_iv,'_1990_2015_RegionFE_DestCOVS')
use_quants <- FALSE
use_zero <- FALSE
country_dest_fe <- FALSE
country_dest_covs <- TRUE
plot_map <- FALSE 
make_plots <- FALSE

message(keep_regions)
file_tag <- keep_regions
if(grepl('ALL',keep_regions)) keep_regions <- unique(grav_data[, region_f])

## Fit gravity model
gravity_model <- glm(as.formula(paste0('migrants ~ ',ifelse(grepl('ALLINT',file_tag), paste0(iv,'*',int_iv), paste0(iv,'+',int_iv)),' + epr_15_24 + mort_nature + mort_war + polity2 + urbanicity + epr_15_24_dest + mort_nature_dest + mort_war_dest + polity2_dest + urbanicity_dest + gdp_gap + log_distance + log_pop_dest + log_pop_orig + country_orig + gbd_super_region_dest')), 
                     family = quasipoisson(link='log'), 
                     data = grav_data)
coefs <- data.table(var=names(gravity_model$coefficients),
                    coef=exp(coef(gravity_model)),
                    p=summary(gravity_model)$coefficients[,4])

## Zero-inflated option
# gravity_model <- zeroinfl(as.formula(paste0('migrants ~ ',ifelse(grepl('ALLINT',file_tag), paste0(iv,'*',int_iv), paste0(iv,'+',int_iv)),' + epr_15_24 + mort_nature + mort_war + polity2 + urbanicity + epr_15_24_dest + mort_nature_dest + mort_war_dest + polity2_dest + urbanicity_dest + gdp_gap + log_distance + log_pop_dest + log_pop_orig + country_orig + gbd_super_region_dest')),
#                               data = grav_data, dist='poisson')
# coefs <- data.table(var=names(gravity_model$coefficients$count),
#                     coef=exp(summary(gravity_model)$coefficients$count[,1]),
#                     p=summary(gravity_model)$coefficients$count[,4])

## Extract coefficients (TABLE 1)
coefs <- coefs[!grepl('country_orig|country_dest|gbd_super_region_dest', var)]
coefs[p<0.05, pval := '*']
coefs[p<0.01, pval := paste0(pval,'*')]
coefs[p<0.001, pval := paste0(pval,'*')]
coefs[, var := factor(var, levels=c(iv,int_iv,paste0(iv,':',int_iv),'gdp_gap','epr_15_24','polity2','urbanicity','mort_war','mort_nature',paste0(c('epr_15_24','polity2','urbanicity','mort_war','mort_nature'),'_dest'),'log_pop_orig','log_pop_dest','log_distance'))]
coefs[, p := NULL]
coefs[is.na(pval), pval := '']
coefs[, coef := paste0(round(coef,2),pval)]
coefs[, pval := NULL]
coefs <- coefs[order(var)]
print(coefs)
message(round(rsq(gravity_model),2))
message(round(rsq(gravity_model,adj=T),2))
message(dim(grav_data)[1])

###########################################################################################
###########################################################################################

## Make interaction figure
inter_gg <- interplot(gravity_model, var1=iv, var2=int_iv)
results <- inter_gg$data$coef1
inter_data <- as.data.table(inter_gg$data)
for(i in 1:dim(inter_data)[1]) {
  draws <- exp(rnorm(10000, mean=inter_data[i,coef1],sd=(inter_data[i,ub]-inter_data[i,coef1])/1.96))
  inter_data[i, pred_coef := mean(draws)]
  inter_data[i, pred_coef_lower := quantile(draws,probs=0.025)]
  inter_data[i, pred_coef_upper := quantile(draws,probs=0.975)]
}
results <- copy(grav_data)
results[, pred_coef := exp(get(int_iv) * gravity_model$coefficients[paste0(iv,':',int_iv)] + gravity_model$coefficients[iv])]
inter_data[, (int_iv) := fake]
gg_int <- ggplot() +
  geom_jitter(data=unique(results[, c(int_iv,'pred_coef','region_f'), with=F]),
              aes(x=get(int_iv),
                  y=pred_coef,
                  fill=region_f),
              shape=21,
              alpha=0.8,
              size=10, width=0.01, height=0.03) +
  geom_line(data=inter_data,
            aes(x=get(int_iv),
                y=pred_coef),
            color='black',
            size=1) +
  geom_smooth(data=inter_data,
              aes(x=get(int_iv),
                  y=pred_coef_upper),
              linetype='dashed',
              color='black',
              size=1,se=F) +
  geom_smooth(data=inter_data,
              aes(x=get(int_iv),
                  y=pred_coef_lower),
              linetype='dashed',
              color='black',
              size=1,se=F) +
  geom_hline(yintercept = 1) + 
  geom_vline(xintercept = 0) + 
  scale_color_manual(name='', values=c('#253494','#2ca25f','#bdbdbd','#de2d26','#ff7f00','#ffff33'),guide=F) +
  scale_fill_manual(name='Region', values=c('#253494','#2ca25f','#ff7f00','#de2d26','#ff7f00','#ffff33')) +
  guides(fill = guide_legend(override.aes = list(size = 15))) + 
  scale_x_continuous(breaks=c(-4,-2,0,2),labels=c(paste0('-4\n($', 99, ')'),
                                                  paste0('-2\n($', 663, ')'),
                                                  paste0('0\n($', 4860, ')'),
                                                  paste0('2\n($', 44696, ')'))) + 
  labs(y='Multiplicative effect of unit increase in growth rate on out-migration',
       x='Standard deviations of log(GDP/pc) (2018 USD)',
       title='Interactive effect of growth rate and GDP/pc on total out-migrants') + 
  theme_bw() + 
  theme(axis.title.y = element_text(size = 16, margin = margin(r=10)),
        axis.title.x = element_text(size = 16, margin = margin(t=10)),
        axis.text = element_text(size = 16))
print(gg_int +
        theme(
          plot.title = element_textbox(
            hjust = 0.5, margin = margin(t = 5, b = 5)
          ),
          legend.position=c(0.83,0.8),
          legend.background = element_rect(fill='white',color='black'),
          axis.title.y = element_text(size = 16, margin = margin(r=10)),
          axis.title.x = element_text(size = 16, margin = margin(t=10)),
          axis.text = element_text(size = 16),
          plot.margin = margin(0, 2, 0, 0, "cm")
        ))

###########################################################################################
###########################################################################################

## Make total migrant predictions
results <- copy(grav_data[country_orig!='Trinidad and Tobago' & !is.na(log_distance),])
if(use_quants) {
  for(q in quants) {
    results[ldi_quantile==as.character(q),
            pred_coef := ifelse(q!=0,
                                gravity_model$coefficients[paste0(iv,':',int_iv,q)] + gravity_model$coefficients[iv],
                                gravity_model$coefficients[iv])]
  }
  results[, pred_coef := exp(pred_coef)]
}
if(!use_quants) results[, pred_coef := exp(get(int_iv) * gravity_model$coefficients[paste0(iv,':',int_iv)] + gravity_model$coefficients[iv])]
if(use_zero) results[, pred_coef := exp(get(paste0(int_iv)) * grav_zero$coefficients$count[paste0(iv,':',int_iv)] + grav_zero$coefficients$count[iv])]
set.seed(1990)
total_draws <- 1000
create_preds <- function(intervention, d, intervention_value) {
  coef_data <- copy(d)
  if(!is.null(intervention)) {
    coef_data[, (intervention) := intervention_value]
    if(intervention=='gdp_orig') {
      coef_data[, gdp1990 := ifelse(year==1990, gdp_orig, 0)]
      coef_data[, (intervention) := max(gdp1990), by=country_orig]
    }
  }
  if(country_dest_fe) for(c in unique(coef_data[, country_dest])) coef_data[, (paste0('country_dest',c)) := ifelse(country_dest==c,1,0)]
  for(c in unique(coef_data[, country_orig])) coef_data[, (paste0('country_orig',c)) := ifelse(country_orig==c,1,0)]
  for(c in unique(coef_data[, gbd_super_region_dest])) coef_data[, (paste0('gbd_super_region_dest',c)) := ifelse(gbd_super_region_dest==c,1,0)]
  if(use_quants) {
    for(q in unique(coef_data[, ldi_quantile])) {
      coef_data[, (paste0('ldi_quantile',q)) := ifelse(ldi_quantile==q,1,0)]
      coef_data[, (paste0('l.r.15.24:ldi_quantile',q)) := get(paste0('ldi_quantile',q)) * l.r.15.24]
    }
  }
  coef_data[, (paste0('l.r.15.24:',int_iv)) := l.r.15.24 * get(int_iv)]
  coef_data[, ('(Intercept)') := 1]
  betas <- MASS::mvrnorm(total_draws, mu = coef(gravity_model), Sigma = vcov(gravity_model))
  new_d <- coef_data[, colnames(betas), with=F]
  setcolorder(new_d, colnames(betas))
  ## 1000 predictions
  preds <- betas %*% t(as.matrix(new_d))
  preds <- as.data.table(t(preds))
  cols <- paste0('draw',1:total_draws)
  setnames(preds, cols)
  preds[, (cols) := lapply(.SD,exp), .SDcols=cols]
  return(preds)
}
cols <- paste0('draw',1:total_draws)
preds_growth <- create_preds(intervention=NULL, d=results, intervention_value=intervention_value)
preds_no_growth <- create_preds(intervention=intervention_variable, d=results, intervention_value=intervention_value)
pred_diffs <- preds_growth - preds_no_growth

results <- cbind(results, preds_growth)
setnames(results, cols, paste0('growth_',cols))
results <- cbind(results, preds_no_growth)
setnames(results, cols, paste0('nogrowth_',cols))
results <- cbind(results, pred_diffs)

preds_growth[, pred_mean := apply(.SD,1,median), .SDcols=cols]
preds_growth[, pred_lower := apply(.SD,1,quantile,probs=0.025), .SDcols=cols]
preds_growth[, pred_upper := apply(.SD,1,quantile,probs=0.975), .SDcols=cols]

preds_no_growth[, pred_no_growth_mean := apply(.SD,1,median), .SDcols=cols]
preds_no_growth[, pred_no_growth_lower := apply(.SD,1,quantile,probs=0.025), .SDcols=cols]
preds_no_growth[, pred_no_growth_upper := apply(.SD,1,quantile,probs=0.975), .SDcols=cols]

results <- cbind(results, preds_growth[, c('pred_mean'), with=F])
results <- cbind(results, preds_no_growth[, c('pred_no_growth_mean'), with=F])

## Get means with predict to compare
results[, pred := exp(predict(gravity_model, results))]
results[, (intervention_variable) := 0]
results[, pred_no_growth := exp(predict(gravity_model, results))]

## Country aggregates
results[, diff := pred - pred_no_growth]
results[diff>0, direction := 'Positive contributions']
results[diff<=0, direction := 'Negative contributions']
get_aggregate_table <- function(levels) {
  ## Total observed migrants
  migrant_totals <- results[country_orig!='Equatorial Guinea', list(migrants=sum(migrants)), by=c(levels)]
  ## Expected percent change in net migrants given no growth
  pred_totals <- results[country_orig!='Equatorial Guinea', lapply(.SD,sum), .SDcols=paste0('growth_',cols), by=c(levels)]
  pred_no_growth_totals <- results[country_orig!='Equatorial Guinea', lapply(.SD,sum), .SDcols=paste0('nogrowth_',cols), by=c(levels)]
  perc_cols <- paste0('perc_draw',1:total_draws)
  these_levels <- pred_totals[, levels, with=F]
  all <- as.matrix(pred_totals[, paste0('growth_',cols), with=F]) / as.matrix(pred_no_growth_totals[, paste0('nogrowth_',cols), with=F])
  all <- as.data.table(all)
  setnames(all, perc_cols)
  all <- cbind(these_levels, all)
  all[, perc_mean := apply(.SD,1,median), .SDcols=perc_cols]
  all[, perc_lower := apply(.SD,1,quantile,probs=0.025), .SDcols=perc_cols]
  all[, perc_upper := apply(.SD,1,quantile,probs=0.975), .SDcols=perc_cols]
  all <- all[, c(levels,'perc_mean','perc_lower','perc_upper'), with=F]
  ## Total expected net migrants given no growth
  pred_no_growth_totals <- results[country_orig!='Equatorial Guinea', lapply(.SD,sum), .SDcols=cols, by=c(levels)]
  pred_no_growth_totals[, diff_mean := apply(.SD,1,median), .SDcols=cols]
  pred_no_growth_totals[, diff_lower := apply(.SD,1,quantile,probs=0.025), .SDcols=cols]
  pred_no_growth_totals[, diff_upper := apply(.SD,1,quantile,probs=0.975), .SDcols=cols]
  pred_no_growth_totals <- pred_no_growth_totals[, c(levels,'diff_mean','diff_lower','diff_upper'), with=F]
  ## Total positive contribution
  positive_cont <- results[diff>0 & country_orig!='Equatorial Guinea',]
  positive_cont[, positive_mean := apply(.SD,1,median), .SDcols=cols]
  positive_cont[, positive_lower := apply(.SD,1,quantile,probs=0.025), .SDcols=cols]
  positive_cont[, positive_upper := apply(.SD,1,quantile,probs=0.975), .SDcols=cols]
  positive_cont <- positive_cont[, lapply(.SD,sum), .SDcols=c('positive_mean','positive_lower','positive_upper'), by=c(levels)]
  positive_cont <- positive_cont[, c(levels,'positive_mean','positive_lower','positive_upper'), with=F]
  ## Total negative contribution
  negative_cont <- results[diff<=0 & country_orig!='Equatorial Guinea',]
  negative_cont[, negative_mean := apply(.SD,1,median), .SDcols=cols]
  negative_cont[, negative_lower := apply(.SD,1,quantile,probs=0.025), .SDcols=cols]
  negative_cont[, negative_upper := apply(.SD,1,quantile,probs=0.975), .SDcols=cols]
  negative_cont <- negative_cont[, lapply(.SD,sum), .SDcols=c('negative_mean','negative_lower','negative_upper'), by=c(levels)]
  negative_cont <- negative_cont[, c(levels,'negative_mean','negative_lower','negative_upper'), with=F]
  ## Combine
  conts <- merge(positive_cont, negative_cont, all.x=T, all.y=T, by=levels)
  conts[is.na(positive_mean), positive_mean := 0]
  conts[is.na(negative_mean), negative_mean := 0]
  totals <- Reduce(merge, list(migrant_totals, pred_no_growth_totals, conts, all))
  return(totals)
}
get_aggregate_table <- function(levels) {
  totals <- results[country_orig!='Equatorial Guinea', lapply(.SD,sum), .SDcols='diff', by=c(levels,'direction')]
  totals <- dcast(totals, as.formula(paste0(paste(levels,collapse='+'),'~direction')), value.var='diff')
  setnames(totals, c('Negative contributions','Positive contributions'), c('negative_mean','positive_mean'))
  perc <- results[country_orig!='Equatorial Guinea', lapply(.SD,sum), .SDcols=c('pred','pred_no_growth'), by=c(levels)]
  perc[, perc_mean := pred / pred_no_growth]
  migrant_totals <- results[country_orig!='Equatorial Guinea', list(migrants=sum(migrants)), by=c(levels)]
  totals <- merge(totals, perc[,c(levels,'perc_mean'), with=F])
  totals <- merge(totals, migrant_totals)
  return(totals)
}
country_aggs <- get_aggregate_table(c('country_orig','region_f'))
country_aggs <- country_aggs[country_orig!='Equatorial Guinea',]
options(scipen=999)
country_aggs <- country_aggs[order(-positive_mean)]
country_aggs[, country_orig := factor(country_orig, levels=rev(country_orig))]

country_all <- melt(country_aggs, id.vars = c('region_f','country_orig'), measure.vars = c('perc_mean','positive_mean'))
country_all <- country_all[order(variable,-value)]
country_all[, country_orig_perc := factor(country_orig, levels=rev(country_all[variable=='perc_mean', country_orig]))]
country_all[, country_orig_abs := factor(country_orig, levels=rev(country_all[variable=='positive_mean', country_orig]))]
country_all[, rank := 1:.N, by='variable']
country_map <- copy(country_all[variable=='positive_mean',])
country_map[is.na(value), value := 0]

if(make_plots) {
  country_all[, clean_region := region_f]
  country_all[clean_region == 'North Africa and Middle East', clean_region := 'NAME']
  country_all[clean_region == 'Latin America and Caribbean', clean_region := 'Latin America']
  country_all[clean_region == 'Sub-Saharan Africa', clean_region := 'Sub-Saharan\nAfrica']
  country_all[, clean_region := factor(clean_region, levels=c('Sub-Saharan\nAfrica','Asia','NAME'))]
  gg1 <- ggplot() + 
    geom_bar(data=country_all[variable=='perc_mean' & rank<=11,],
             aes(x=country_orig_perc,
                 y=value*100-100,
                 fill=clean_region),
             color='black',
             stat='identity',
             alpha=0.8) +
    theme_bw() + 
    labs(y='Percent increase in total out-migrants',x='',title='Percent increase in net out-migrants') + 
    scale_fill_manual(name='Region', values=c('Sub-Saharan\nAfrica'='#4daf4a',
                                              'Asia'='#e41a1c',
                                              'Latin America'='#35B779FF',
                                              'NAME'='#FDE725FF')) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.y = element_text(size = 15, margin = margin(r=10)),
          axis.title.x = element_text(size = 15, margin = margin(t=10)),
          axis.text = element_text(size = 12),
          legend.key.size = unit(2,'line'),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 12),
          plot.title = element_textbox(
            hjust = 0.5, margin = margin(t = 5, b = 5)
          )
    ) 
  gg2 <- ggplot() + 
    geom_bar(data=country_all[variable=='positive_mean' & rank<=11,],
             aes(x=country_orig_abs,
                 y=value/1000,
                 fill=clean_region),
             color='black',
             stat='identity') +
    theme_bw() + 
    labs(y='Increase in total out-migrants (thousands)',x='',title='Absolute increase in net out-migrants') + 
    scale_fill_manual(name='Region', values=c('Sub-Saharan\nAfrica'='#4daf4a',
                                              'Asia'='#e41a1c',
                                              'NAME'='#FDE725FF')) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.y = element_text(size = 15, margin = margin(r=10)),
          axis.title.x = element_text(size = 15, margin = margin(t=10)),
          axis.text = element_text(size = 12),
          legend.key.size = unit(2,'line'),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 12),
          plot.title = element_textbox(
            hjust = 0.5, margin = margin(t = 5, b = 5)
          )) 
  grid.arrange(grobs=list(gg1,gg2),nrow=1)

  ## Map of absolute additional migrants
  if(plot_map) {
    map <- readRDS(paste0(repo,'/simple_map.RDS'))
    map_theme <- theme_minimal() +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            legend.text = element_text(size=16),
            legend.justification=c(0,0),
            legend.position=c(0.85,0.08),
            panel.grid.major = element_line(color=NA),
            panel.grid.minor = element_line(color=NA),
            legend.key.width=unit(1,"line"),
            legend.key.height=unit(2,"line"),
            plot.title = element_textbox(
              hjust = 0.5, margin = margin(t = 5, b = 5)
            ))
    map_names <- data.table(map=unique(map$ADM0_NAME))
    country_map[!(country_orig_abs %in% map$ADM0_NAME), unique(country_orig_abs)]
    country_map[, ADM0_NAME := country_orig_abs]
    country_map_abs <- country_map[variable=='positive_mean',]
    country_map_abs[value>500000, value := 500000]
    country_map_abs[, value := value / 1000]
    map_merge <- st_as_sf(map)
    map_merge <- merge(map_merge, country_map_abs, by='ADM0_NAME', all.x=T)
    map.gg <- ggplot(data=map_merge) + 
      geom_sf(aes(fill=value),
              color='black',
              lwd=0.1) +
      scale_fill_viridis_c(name='Out-migrants', na.value = 'grey') + 
      labs(title='Absolute increase in net out-migrants') + 
      theme_minimal() + 
      map_theme
    vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
    gLegend<-function(a.plot){
      if ("ggplot" %in% class(a.plot)) {
        tmp <- ggplot_gtable(ggplot_build(a.plot))
      } else if ("grob" %in% class(a.plot)) {
        tmp <- .gplot
      }
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
      legend <- tmp$grobs[[leg]]
      return(legend)
    }
    p.legend1 <- gLegend(gg2)
    p.legend1$vp <- viewport(layout.pos.row = 8:14, layout.pos.col = 11:12)
    p.legend2 <- gLegend(map.gg)
    p.legend2$vp <- viewport(layout.pos.row = 1:7, layout.pos.col = 11:12)
    vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(14, 12)))
    print(map.gg + theme(legend.position="none"), vp = vplayout(1:7, 1:10))
    print(gg1 + theme(legend.position="none"), vp = vplayout(8:14, 1:5))
    print(gg2 + theme(legend.position="none"), vp = vplayout(8:14, 6:10))
    grid.draw(p.legend1)
    grid.draw(p.legend2)
  }
  
}

###########################################################################################
###########################################################################################

## Region aggregates (TABLE 2)
wb_aggs <- get_aggregate_table('region_name')
setnames(wb_aggs, 'region_name', 'level')
region_aggs <- get_aggregate_table('gbd_region')
region_aggs[order(gbd_region)]
setnames(region_aggs, 'gbd_region', 'level')
results[, global := 'global']
global_aggs <- get_aggregate_table('global')
setnames(global_aggs, 'global', 'level')
all_aggs <- rbindlist(list(global_aggs,wb_aggs,region_aggs),fill=T)
all_aggs

###########################################################################################
###########################################################################################

