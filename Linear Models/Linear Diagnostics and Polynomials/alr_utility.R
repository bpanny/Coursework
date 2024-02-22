##
## Plot covratio, leverage, Cook's distance vs index or yhat
##
## alr_plot_covratio (fit, x, label, type="both")
## alr_plot_leverage (fit, x, label, type="both)
## alr_plot_cookd (fit, x, label, type="both")
## alr_plot_dffit(fit, x, label, type="both")
##     fit: linear fit from lm()
##     x: is optional, x="yhat" uses yhat as axis, other use index
##     label: is optional, uses to print the labels of outliers.
##               default is using index
##     type: "point"  point only
##           "line"   lines only
##           "both"   plot both point and lines

##  alr_plot_residuals(fit, x, label, cutoff=NULL)
##     cutoff: to draw cutoff line. The default is using t-distribution
##

##  alr_plot_QQ(fit)

alr_plot_residual_QQ <- function(fit, dist="norm") {
  residual <- rstudent(fit)
  p <- tibble(residual = rstudent(fit)) %>%
       ggplot(aes(sample=residual))

  if(dist=="t") {
    res_df <- fit$df.residual-1
    p <- p +
      stat_qq(distribution=qt, dparams = list(df=res_df),
              color="blue") +
      stat_qq_line(distribution=qt, dparams = list(df=res_df),
                   color="red") +
      ggtitle("Deleted Studentized Residual T Q-Q Plot")
  } else {
     p <- p +
       stat_qq(color="blue") +
       stat_qq_line(color="red") +
       ggtitle("Deleted Studentized Residual Normal Q-Q Plot")
  }

  p +
    labs(x="Theoritical quantile", y="Emperical quantile") +
    theme_bw()

}

alr_plot_residual_dist <- function(fit, ref_dist = "normal") {
  if(ref_dist == "t") {
    ref_density <- function(q) dt(q, fit$df.residual-1)
    title <- 'Probability Density of Residuals with t reference'
  } else {
    ref_density <- dnorm
    title <- 'Probability Density of Residuals with normal reference'
  }

  tibble(residuals = rstudent(fit)) %>%
    ggplot(aes(x=residuals)) +
    geom_histogram(aes(y = after_stat(density)), fill = 'steelblue',
                 color = 'black') +
    geom_density(color = 'red') +
    stat_function(fun = ref_density, color = "green") +
    ggtitle(title) +
    labs(x='Deleted Studentized Residual') +
    theme_bw()
}

alr_plot_residual <- function(fit,x="index", label=NULL, cutoff=NULL, bubble=FALSE) {
    n=NROW(fit$fitted.values)

    if(is.null(cutoff)) {

    cutoff = qt(0.025/n, df=fit$df.residual, lower.tail=F)
  }

  if(is.null(label)) label=1:n
  dat <- as_tibble(fit$model) %>%
    mutate(index=1:n(),
           label=label,
           yhat=predict(fit),
           resid = rstudent(fit))

  if(bubble==TRUE)
    dat <- dat %>% mutate(`Cook's D` = cooks.distance(fit))

  p <- dat %>% ggplot(aes(y=resid))

  if(x=="yhat") {
    figTitle <- "Deleted Studentized Residual vs. yhat"
    p <- p + aes(x=yhat)
  } else {
   figTitle <- "Deleted Studentized Residual Plot"

   p <- p + aes(x=index)
  }

  if(bubble==TRUE) {
    p <- p + aes(size=`Cook's D`)

    repel_data <- dat%>%filter(abs(resid) > cutoff |
                                 `Cook's D` > alr_cutoff_cookd(fit))
  } else {
    repel_data <- dat%>%filter(abs(resid) > cutoff)
  }
  p + geom_point(color="blue") +
    annotate("text", x= Inf, y=Inf,
               label=str_c("Threshold: ", round(cutoff,2)),
               hjust=1.5,vjust=1.5, color="black") +
    ggrepel::geom_text_repel(
      data=repel_data,
      aes(label = label), size = 3, color="red") +
    geom_hline(yintercept=c(-cutoff, cutoff), color="red") +
    ylab("Deleted Studentized Residual") +
    ggtitle(figTitle) +
    theme_bw()
}

alr_boxplot_leverage <- function(fit, type="boxplot") {
  inffit <- tibble(Leverage = hatvalues(fit))

  p <- inffit %>% ggplot(aes(y=Leverage))

  if(type=="violin") {
    p <- p + geom_violin(aes(x=1)) +
             geom_jitter(aes(x=1), color="blue", width=0.05, height=0)
  } else {
    p <- p + geom_boxplot()
  }

  p + theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
}

alr_boxplot_cookd <- function(fit, type="boxplot") {
  inffit <- tibble(`Cook's D` = cooks.distance(fit))

  p <- inffit %>% ggplot(aes(y=`Cook's D`))

  if(type=="violin") {
    p <- p + geom_violin(aes(x=1)) +
      geom_jitter(aes(x=1), color="blue", width=0.05, height=0)
  } else {
    p <- p + geom_boxplot()
  }

  p + theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
}

alr_boxplot_dffit <- function(fit, type="boxplot") {
  inffit <- tibble(DFFIT = dffits(fit))

  p <- inffit %>% ggplot(aes(y=DFFIT))

  if(type=="violin") {
    p <- p + geom_violin(aes(x=1)) +
      geom_jitter(aes(x=1), color="blue", width=0.05, height=0)
  } else {
    p <- p + geom_boxplot()
  }

  p + theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
}

alr_boxplot_covratio <- function(fit, type="boxplot") {
  inffit <- tibble(COVRATIO = covratio(fit))

  p <- inffit %>% ggplot(aes(y=COVRATIO))

  if(type=="violin") {
    p <- p + geom_violin(aes(x=1)) +
      geom_jitter(aes(x=1), color="blue", width=0.05, height=0)
  } else {
    p <- p + geom_boxplot()
  }

  p + theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
}

alr_boxplot_dfbetas <- function(fit, type="boxplot") {
  inffit <- as_tibble(dfbetas(fit)) %>%
    pivot_longer(cols=everything(),
                 names_to="variable",
                 values_to="DFBETAS")

  p <- inffit %>% ggplot(aes(x=variable, y=DFBETAS))

  if(type=="violin") {
    p <- p + geom_violin() +
      geom_jitter(width=0.05, color="blue", height=0)
  } else {
    p <- p + geom_boxplot()
  }

  p + theme_bw() +
    theme(axis.title.x = element_blank())
}

alr_plot_leverage<- function(fit, x="index", label=NULL, type="both") {
  n=NROW(fit$fitted.values)
  cutoff = alr_cutoff_leverage(fit)
  independentV = names(fit$model)[1]

  if(is.null(label)) label=1:n
  inffit <- tibble(Leverage = hatvalues(fit),
                   index=1:n,
                   yhat=predict(fit),
                   label=label)

  if(x=="yhat") {
    p <- inffit %>% ggplot(aes(y=Leverage, x=yhat))
    if(type != "point")
      p <- p + geom_segment(aes(xend=yhat, yend=0),
                   color="blue", alpha=0.5)
  } else {
    p <- inffit %>% ggplot(aes(y=Leverage, x=index))
      if(type != "point")
        p <- p + geom_segment(aes(xend=index, yend=0),
                   color="blue", alpha=0.5)
  }

  if(type!="line")
    p <- p + geom_point(shape=21, color="blue")

  p + annotate("text", x= Inf, y=Inf,
             label=str_c("Threshold: ", round(cutoff,3)),
             hjust=1.5,vjust=1.5, color="black") +
    ggrepel::geom_text_repel(
      data=inffit%>%filter(Leverage > cutoff),
      aes(label = label), size = 3, color="red") +
    geom_hline(yintercept=c(0, cutoff), color="red") +
    ggtitle(str_c("Influence Diagnosis for ", independentV)) +
    theme_bw()
}

alr_plot_covratio<- function(fit, x="index", label=NULL, type="both") {
  n=NROW(fit$fitted.values)
  cutoff = alr_cutoff_covratio(fit)
  independentV = names(fit$model)[1]

  if(is.null(label)) label=1:n

  inffit <- tibble(COVRATIO = covratio(fit),
                   index=1:n,
                   yhat=predict(fit),
                   label=label)

  if(x=="yhat") {
    p <- inffit %>% ggplot(aes(y=COVRATIO, x=yhat))
      if(type != "point")
        p <- p + geom_segment(aes(xend=yhat, yend=1),
                   color="blue", alpha=0.5)
  } else {
    p <- inffit %>% ggplot(aes(y=COVRATIO, x=index))
      if(type != "point")
        p <- p + geom_segment(aes(xend=index, yend=1),
                   color="blue", alpha=0.5)
  }

  if(type!="line")
    p <- p + geom_point(shape=21, color="blue")

  p + annotate("text", x= Inf, y=Inf,
             label=str_c("Threshold: ", round(cutoff,3)),
             hjust=1.5,vjust=1.5, color="black") +
    ggrepel::geom_text_repel(
      data=inffit%>%filter(abs(COVRATIO-1) > cutoff),
      aes(label = label), size = 3, color="red") +
    geom_hline(yintercept=c(1-cutoff, 1+cutoff), color="red") +
    ggtitle(str_c("Influence Diagnosis for ", independentV)) +
    theme_bw()
}

alr_plot_cookd<- function(fit, x="index", label=NULL, type="both") {
  n=NROW(fit$fitted.values)
  cutoff = alr_cutoff_cookd(fit)
  independentV = names(fit$model)[1]

  if(is.null(label)) label=1:n

  inffit <- tibble(cookd = cooks.distance(fit),
                   index=1:n,
                   yhat=predict(fit),
                   label=label)

  if(x=="yhat") {
    p <- inffit %>% ggplot(aes(y=cookd, x=yhat))
    if(type != "point")
      p <- p + geom_segment(aes(xend=yhat, yend=0),
                            color="blue", alpha=0.5)
  } else {
    p <- inffit %>% ggplot(aes(y=cookd, x=index))
      if(type != "point")
        p <- p + geom_segment(aes(xend=index, yend=0),
                              color="blue", alpha=0.5)
  }

  if(type!="line")
    p <- p + geom_point(shape=21, color="blue")

  p + annotate("text", x= Inf, y=Inf,
               label=str_c("Threshold: ", round(cutoff,3)),
               hjust=1.5,vjust=1.5, color="black") +
    ggrepel::geom_text_repel(
      data=inffit%>%filter(cookd > cutoff),
      aes(label = label), size = 3, color="red") +
    geom_hline(yintercept=c(0, cutoff), color="red") +
    ylab("Cook's Distance") +
    ggtitle(str_c("Influence Diagnosis for ", independentV)) +
    theme_bw()
}

alr_plot_dffit<- function(fit, x="index", label=NULL, type="both") {
  n=NROW(fit$fitted.values)
  cutoff = alr_cutoff_dffit(fit)
  independentV = names(fit$model)[1]

  if(is.null(label)) label=1:n
  inffit <- tibble(DFFIT = dffits(fit),
                   index=1:n,
                   yhat=predict(fit),
                   label=label)

  if(x=="yhat") {
    p <- inffit %>% ggplot(aes(y=DFFIT, x=yhat))
    if(type != "point")
      p <- p + geom_segment(aes(xend=yhat, yend=0),
                            color="blue", alpha=0.5)
  } else {
    p <- inffit %>% ggplot(aes(y=DFFIT, x=index))
    if(type != "point")
      p <- p + geom_segment(aes(xend=index, yend=0),
                            color="blue", alpha=0.5)
  }

  if(type!="line")
    p <- p + geom_point(shape=21, color="blue")

  p + annotate("text", x= Inf, y=Inf,
               label=str_c("Threshold: ", round(cutoff,3)),
               hjust=1.5,vjust=1.5, color="black") +
    ggrepel::geom_text_repel(
      data=inffit%>%filter(abs(DFFIT) > cutoff),
      aes(label = label), size = 3, color="red") +
    geom_hline(yintercept=c(-cutoff, cutoff), color="red") +
    ggtitle(str_c("Influence Diagnosis for ", independentV)) +
    theme_bw()
}

alr_plot_dfbetas<- function(fit, x="index", label=NULL, type="both") {
  n=NROW(fit$fitted.values)
  cutoff = alr_cutoff_dfbetas(fit)
  independentV = names(fit$model)[1]

  if(is.null(label)) label=1:n
  inffit <- as_tibble(dfbetas(fit))

  inffit_long <- inffit %>% mutate(index=1:n(), label=label)

  inffit_long <-  inffit_long %>% pivot_longer(cols=1:(NCOL(inffit_long)-2), names_to = "variable")

  if(x=="yhat") {
    p <- inffit_long %>% ggplot(aes(y=value, x=yhat))
    if(type != "point")
      p <- p + geom_segment(aes(xend=yhat, yend=0),
                            color="blue", alpha=0.5)
  } else {
    p <- inffit_long %>% ggplot(aes(y=value, x=index))
    if(type != "point")
      p <- p + geom_segment(aes(xend=index, yend=0),
                            color="blue", alpha=0.5)
  }

  if(type!="line")
    p <- p + geom_point(shape=21, color="blue")

  p + annotate("text", x= Inf, y=Inf,
               label=str_c("Threshold: ", round(cutoff,3)),
               hjust=1.5,vjust=1.5, color="black") +
    ggrepel::geom_text_repel(
      data=inffit_long%>%filter(abs(value) > cutoff),
      aes(label = label), size = 3, color="red") +
    geom_hline(yintercept=c(-cutoff, cutoff), color="red") +
    ylab("DFBETAS") +
    facet_wrap(~str_c("Influence Diagnosis for ", variable))+
    theme_bw()
}

alr_outlier_response <- function(fit, cutoff=NULL) {
  if(is.null(cutoff)) {
    n=NROW(fit$fitted.values)
    cutoff = qt(0.025/n, df=fit$df.residual, lower.tail=F)
  }

  which(abs(rstudent(fit)) > cutoff)
}

alr_high_leverage <- function(fit, cutoff=NULL) {
  if(is.null(cutoff)) {
    cutoff = alr_cutoff_leverage(fit)
  }

  which(hatvalues(fit) > cutoff)
}

alr_influential_cookd <- function(fit, cutoff=NULL) {
  if(is.null(cutoff)) {
    cutoff = alr_cutoff_cookd(fit)
  }

  which(cooks.distance(fit) > cutoff)
}

alr_influential_dffit <- function(fit, cutoff=NULL) {
  if(is.null(cutoff)) {
    cutoff = alr_cutoff_dffit(fit)
  }

  which(abs(dffits(fit)) > cutoff)
}

alr_influential_covratio <- function(fit, cutoff=NULL) {
  if(is.null(cutoff)) {
    cutoff = alr_cutoff_covratio(fit)
  }

  which(abs(covratio(fit)-1) > cutoff)
}

alr_influential_dfbetas <- function(fit, cutoff=NULL, union=TRUE) {
  if(is.null(cutoff)) {
    cutoff = alr_cutoff_dfbetas(fit)
  }

  if(union==TRUE) {
    which(apply(abs(dfbetas(fit)) > cutoff,1,any))
  } else {
    abs(dfbetas(fit)) > cutoff
  }
}

alr_cutoff_cookd <- function(fit) {
  alr_cutoff(fit, "cookd")
}

alr_cutoff_covratio <- function(fit) {
  alr_cutoff(fit, "covratio")
}

alr_cutoff_dffit <- function(fit) {
  alr_cutoff(fit, "dffit")
}

alr_cutoff_leverage <- function(fit) {
  alr_cutoff(fit, "leverage")
}

alr_cutoff_dfbetas <- function(fit) {
  alr_cutoff(fit, "dfbetas")
}

alr_cutoff <- function(fit, what) {
  n=NROW(fit$fitted.values)
  p=NROW(fit$coefficients) ##p = k+1

  cutoffs <- c("dfbetas"  = 2/sqrt(n),
               "leverage" = 2*p/n,
               "dffit" = 2*sqrt(p/n),
               "covratio" = 3*p/n,
               "cookd" = 4/n)

  cutoffs[what]
}
