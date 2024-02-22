##
## Plot covratio, leverage, Cook's distance vs index or yhat
##
## alr_plot_covratio (fit, x, label, type="both")
## alr_ (fit, x, label, type="both)
## alr_plot_cookd (fit, x, label, type="both")
## alr_plot_dffit(fit, x, label, type="both")
##     fit: linear fit from lm()
##     x: is optional, x="yhat" uses yhat as axis, other use index
##     label: is optional, uses to print the labels of outliers.
##               default is using index
##     type: "point"  point only
##           "line"   lines only
##           "both"   plot both point and lines

##  alr_plot_residual(fit, x, label, cutoff=NULL)
##     cutoff: to draw cutoff line. The default is using t-distribution
##

##  alr_plot_QQ(fit)

require(tidyverse)
require(splines2)
require(mfp)

alr_plot_residual_QQ <- function(fit, dist="norm") {
  if("glm" %in% class(fit)) {
    residual <- stats::rstandard(fit)
    x_label <- "Standardized Deviance Residual"
  } else {
    residual <- stats::rstudent(fit)
    x_label <- "Deleted Studentized Residual"
  }

  p <- tibble::tibble(residual = residual) %>%
       ggplot2::ggplot(aes(sample=residual))

  if(dist=="t") {
    res_df <- fit$df.residual-1
    p <- p +
      ggplot2::stat_qq(distribution=qt, dparams = list(df=res_df),
              color="blue") +
      ggplot2::stat_qq_line(distribution=qt, dparams = list(df=res_df),
                   color="red") +
      ggplot2::ggtitle(stringr::str_c(x_label, " T Q-Q Plot"))
  } else {
     p <- p +
       ggplot2::stat_qq(color="blue") +
       ggplot2::stat_qq_line(color="red") +
       ggplot2::ggtitle(stringr::str_c(x_label, " Normal Q-Q Plot"))
  }

  p +
    ggplot2::labs(x="Theoritical quantile", y="Emperical quantile")
}

alr_plot_residual_dist <- function(fit, ref_dist = "normal") {
  if(ref_dist == "t") {
    ref_density <- function(q) dt(q, fit$df.residual-1)
    title <- 'Probability Density of Residuals with t reference'
  } else {
    ref_density <- dnorm
    title <- 'Probability Density of Residuals with normal reference'
  }

  if("glm" %in% class(fit)) {
    residual <- stats::rstandard(fit)
    x_label <- "Standardized Deviance Residual"
  } else {
    residual <- stats::rstudent(fit)
    x_label <- "Deleted Studentized Residual"
  }

  tibble::tibble(residuals = residual) %>%
    ggplot2::ggplot(aes(x=residuals)) +
    ggplot2::geom_histogram(aes(y = after_stat(density)), fill = 'steelblue',
                 color = 'black') +
    ggplot2::geom_density(color = 'red') +
    ggplot2::stat_function(fun = ref_density, color = "green") +
    ggplot2::ggtitle(title) +
    ggplot2::labs(x=x_label)
}

alr_plot_residual <- function(fit, x="index", label=NULL, cutoff=NULL, bubble=FALSE) {
  n=NROW(fit$fitted.values)

  x = stringr::str_to_upper(x,locale = "en")
  if(x=="YHAT") x="PREDICTED"

  if(is.null(cutoff)) {

    if("glm" %in% class(fit)) {
      cutoff = qnorm(0.025/n, lower.tail=F)
    } else {
      cutoff = qt(0.025/n, df=fit$df.residual, lower.tail=F)
    }
  }

  if(is.null(label)) label=1:n

  dat <- tibble::tibble(
           index=1:n,
           label=label,
           yhat=predict(fit),
           resid = stats::rstudent(fit))

  x_label <- NULL
  y_label = "Deleted Studentized Residual"

  if("glm" %in% class(fit)) {
    dat <- dat %>% mutate(resid = stats::rstandard(fit))
    y_label = "Standardized Deviance Residual"

    if(x=="PREDICTED") {
       dat <- dat %>% mutate(`logit(yhat)` = yhat,
                            yhat = predict(fit, type = 'response'))
       x_label = "Predicted Response"
    } else if(x=="LINK") {
      dat <- dat %>% mutate(`logit(yhat)` = yhat,
                            yhat = predict(fit))
       x_label = "logit(predicted response)"
     }
  }

  if(bubble==TRUE)
    dat <- dat %>% dplyr::mutate(`Cook's D` = cooks.distance(fit))

  p <- dat %>% ggplot2::ggplot(aes(y=resid))

  if(x=="PREDICTED") {
    figTitle <- stringr::str_c(y_label, " vs. predicted response")
    p <- p + aes(x=yhat)
    if(is.null(x_label)) x_label <- "Predicted Response"
  } else if("glm" %in% class(fit) && x=="LINK") {
    figTitle <- stringr::str_c(y_label, " vs. predicted response")
    p <- p + aes(x=yhat)
    if(is.null(x_label)) x_label <- "logit(predicted response)"
  } else {
    figTitle <- stringr::str_c(y_label, " plot")
    p <- p + aes(x=index)
    if(is.null(x_label)) x_label <- "index"
  }

  if(bubble==TRUE) {
    p <- p + ggplot2::aes(size=`Cook's D`)

    repel_data <- dat%>%dplyr::filter(abs(resid) > cutoff |
                                 `Cook's D` > alr_cutoff_cookd(fit))
  } else {
    repel_data <- dat%>%dplyr::filter(abs(resid) > cutoff)
  }

  if(x == "index")  p <- p + ggplot2::geom_smooth(se=F, color="red")

  p + ggplot2::geom_point(color="blue") +
    ggplot2::annotate("text", x= Inf, y=Inf,
                      label=stringr::str_c("Threshold: ", round(cutoff,2)),
                      hjust=1.5,vjust=1.5, color="black") +
    ggrepel::geom_text_repel(data=repel_data,
                             aes(label = label), size = 3, color="red") +
    ggplot2::geom_hline(yintercept=c(-cutoff, cutoff), color="red") +
    ggplot2::labs(y=y_label,
                  x=x_label) +
    ggplot2::ggtitle(figTitle)
}

alr_boxplot_leverage <- function(fit, type="boxplot") {
  inffit <- tibble::tibble(Leverage = hatvalues(fit))

  p <- inffit %>% ggplot2::ggplot(aes(y=Leverage))

  if(type=="violin") {
    p <- p +
      ggplot2::geom_violin(aes(x=1)) +
      ggplot2::geom_jitter(aes(x=1), color="blue",
                           width=0.05, height=0)
  } else {
    p <- p +  ggplot2::geom_boxplot()
  }

  p + ggplot2::theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
}

alr_boxplot_cookd <- function(fit, type="boxplot") {
  inffit <- tibble::tibble(`Cook's D` = cooks.distance(fit))

  p <- inffit %>% ggplot2::ggplot(aes(y=`Cook's D`))

  if(type=="violin") {
    p <- p +
      ggplot2::geom_violin(aes(x=1)) +
      ggplot2::geom_jitter(aes(x=1), color="blue",
                           width=0.05, height=0)
  } else {
    p <- p +  ggplot2::geom_boxplot()
  }

  p + ggplot2::theme(axis.text.x = element_blank(),
                     axis.ticks.x = element_blank())
}

alr_boxplot_dffit <- function(fit, type="boxplot") {
  inffit <- tibble::tibble(DFFIT = stats::dffits(fit))

  p <- inffit %>%
    ggplot2::ggplot(aes(y=DFFIT))

  if(type=="violin") {
    p <- p +
      ggplot2::geom_violin(aes(x=1)) +
      ggplot2::geom_jitter(aes(x=1), color="blue",
                           width=0.05, height=0)
  } else {
    p <- p +  ggplot2::geom_boxplot()
  }

  p + ggplot2::theme(axis.text.x = element_blank(),
                     axis.ticks.x = element_blank())
}

alr_boxplot_covratio <- function(fit, type="boxplot") {
  inffit <- tibble::tibble(COVRATIO = stats::covratio(fit))

  p <- inffit %>%
    ggplot2::ggplot(aes(y=COVRATIO))

  if(type=="violin") {
    p <- p +
      ggplot2::geom_violin(aes(x=1)) +
      ggplot2::geom_jitter(aes(x=1), color="blue",
                           width=0.05, height=0)
  } else {
    p <- p +  ggplot2::geom_boxplot()
  }

  p + ggplot2::theme(axis.text.x = element_blank(),
                     axis.ticks.x = element_blank())
}

alr_boxplot_dfbetas <- function(fit, type="boxplot") {
  inffit <- tibble::as_tibble(dfbetas(fit)) %>%
    tidyr::pivot_longer(cols=everything(),
                        names_to="variable",
                        values_to="DFBETAS")

  p <- inffit %>%
    ggplot2::ggplot(aes(x=variable, y=DFBETAS))

  if(type=="violin") {
    p <- p +
      ggplot2::geom_violin() +
      ggplot2::geom_jitter(width=0.05, color="blue", height=0)
  } else {
    p <- p +
      ggplot2::geom_boxplot()
  }

  p + ggplot2::theme(axis.title.x = element_blank())
}

alr_plot_leverage<- function(fit, x="index", label=NULL, type="both") {

  x = stringr::str_to_upper(x,locale = "en")
  if(x=="YHAT") x="PREDICTED"
  type = stringr::str_to_upper(type,locale = "en")

  n=NROW(fit$fitted.values)
  cutoff = alr_cutoff_leverage(fit)

  independentV <- alr_internal_get_independent_variable_name(fit)

  if(is.null(label)) label=1:n
  inffit <- tibble::tibble(Leverage = stats::hatvalues(fit),
                           x=1:n, label=label)
  x_label <- "index"
  if(x=="PREDICTED") {
    x_label <- "Predicted Response"
    if("glm" %in% class(fit)) {
      inffit <- inffit %>% mutate(x = predict(fit, type="response"))
    } else {
      inffit <- inffit %>% mutate(x = predict(fit))
    }
  } else if("glm" %in% class(fit) && x=="LINK") {
    inffit <- inffit %>% mutate(x = predict(fit))
    x_label <- stringr::str_c(fit$family$link, "(predicted response)")
  }

  p <- inffit %>%
      ggplot2::ggplot(aes(y=Leverage, x=x))

  if(type != "POINT")
      p <- p +
        ggplot2::geom_segment(aes(xend=x, yend=0),
                              color="blue", alpha=0.5)

  if(type!="LINE")
    p <- p + ggplot2::geom_point(shape=21, color="blue")

  p + ggplot2::annotate("text", x= Inf, y=Inf,
                        label=stringr::str_c("Threshold: ", round(cutoff,3)),
                        hjust=1.5,vjust=1.5, color="black") +
    ggplot2::xlab(x_label) +
    ggrepel::geom_text_repel(data=inffit%>%dplyr::filter(Leverage > cutoff),
                             aes(label = label), size = 3,
                             color="red") +
    ggplot2::geom_hline(yintercept=c(0, cutoff), color="red") +
    ggplot2::ggtitle(stringr::str_c("Influence Diagnosis for ", independentV))
}

alr_plot_covratio<- function(fit, x="index", label=NULL, type="both") {
  n=NROW(fit$fitted.values)
  cutoff = alr_cutoff_covratio(fit)

  independentV <- alr_internal_get_independent_variable_name(fit)

  if(is.null(label)) label=1:n

  inffit <- tibble::tibble(COVRATIO = stats::covratio(fit),
                   index=1:n,
                   yhat=predict(fit),
                   label=label)

  if("glm" %in% class(fit))
    inffit <- inffit %>% mutate(`logit(yhat)` = yhat,
                          yhat = predict(fit, type = 'response'))


  if(x=="yhat") {
    p <- inffit %>% ggplot2::ggplot(aes(y=COVRATIO, x=yhat))
      if(type != "point")
        p <- p + ggplot2::geom_segment(aes(xend=yhat, yend=1),
                                       color="blue", alpha=0.5)
  } else {
    p <- inffit %>% ggplot2::ggplot(aes(y=COVRATIO, x=index))
      if(type != "point")
        p <- p + ggplot2::geom_segment(aes(xend=index, yend=1),
                                       color="blue", alpha=0.5)
  }

  if(type!="line")
    p <- p + ggplot2::geom_point(shape=21, color="blue")

  p + ggplot2::annotate("text", x= Inf, y=Inf,
                        label=stringr::str_c("Threshold: ", round(cutoff,3)),
                        hjust=1.5,vjust=1.5, color="black") +
    ggrepel::geom_text_repel(
      data=inffit%>%dplyr::filter(abs(COVRATIO-1) > cutoff),
      aes(label = label), size = 3, color="red") +
    ggplot2::geom_hline(yintercept=c(1-cutoff, 1+cutoff), color="red") +
    ggplot2::ggtitle(stringr::str_c("Influence Diagnosis for ", independentV))
}

alr_plot_cookd<- function(fit, x="index", label=NULL, type="both") {
  x = stringr::str_to_upper(x,locale = "en")
  if(x=="YHAT") x="PREDICTED"
  type = stringr::str_to_upper(type,locale = "en")

    n=NROW(fit$fitted.values)
  cutoff = alr_cutoff_cookd(fit)

  independentV <- alr_internal_get_independent_variable_name(fit)

  if(is.null(label)) label=1:n

  inffit <- tibble::tibble(cookd = stats::cooks.distance(fit),
                           x=1:n, label=label)
  x_label <- "index"
  if(x=="PREDICTED") {
    x_label <- "Predicted Response"
    if("glm" %in% class(fit)) {
      inffit <- inffit %>% mutate(x = predict(fit, type="response"))
    } else {
      inffit <- inffit %>% mutate(x = predict(fit))
    }
  } else if("glm" %in% class(fit) && x=="LINK") {
    inffit <- inffit %>% mutate(x = predict(fit))
    x_label <- stringr::str_c(fit$family$link, "(predicted response)")
  }

  p <- inffit %>% ggplot2::ggplot(aes(y=cookd, x=x))
  if(type != "POINT")
    p <- p + ggplot2::geom_segment(aes(xend=x, yend=0),
                            color="blue", alpha=0.5)

  if(type!="LINE")
    p <- p + ggplot2::geom_point(shape=21, color="blue")

  p + ggplot2::annotate("text", x= Inf, y=Inf,
               label=stringr::str_c("Threshold: ", round(cutoff,3)),
               hjust=1.5,vjust=1.5, color="black") +
    ggplot2::xlab(x_label) +
    ggrepel::geom_text_repel(
      data=inffit%>%dplyr::filter(cookd > cutoff),
      aes(label = label), size = 3, color="red") +
    ggplot2::geom_hline(yintercept=c(0, cutoff), color="red") +
    ggplot2::ylab("Cook's Distance") +
    ggplot2::ggtitle(stringr::str_c("Influence Diagnosis for ", independentV))
}

alr_plot_dffit<- function(fit, x="index", label=NULL, type="both") {
  n=NROW(fit$fitted.values)
  cutoff = alr_cutoff_dffit(fit)

  independentV <- alr_internal_get_independent_variable_name(fit)

  if(is.null(label)) label=1:n
  inffit <- tibble::tibble(DFFIT = dffits(fit),
                   index=1:n,
                   yhat=predict(fit),
                   label=label)

  if("glm" %in% class(fit))
    inffit <- inffit %>% mutate(`logit(yhat)` = yhat,
                          yhat = predict(fit, type = 'response'))

  if(x=="yhat") {
    p <- inffit %>% ggplot2::ggplot(aes(y=DFFIT, x=yhat))
    if(type != "point")
      p <- p + ggplot2::geom_segment(aes(xend=yhat, yend=0),
                            color="blue", alpha=0.5)
  } else {
    p <- inffit %>% ggplot2::ggplot(aes(y=DFFIT, x=index))
    if(type != "point")
      p <- p + ggplot2::geom_segment(aes(xend=index, yend=0),
                            color="blue", alpha=0.5)
  }

  if(type!="line")
    p <- p + ggplot2::geom_point(shape=21, color="blue")

  p + ggplot2::annotate("text", x= Inf, y=Inf,
               label=stringr::str_c("Threshold: ", round(cutoff,3)),
               hjust=1.5,vjust=1.5, color="black") +
    ggrepel::geom_text_repel(
      data=inffit%>%dplyr::filter(abs(DFFIT) > cutoff),
      aes(label = label), size = 3, color="red") +
    ggplot2::geom_hline(yintercept=c(-cutoff, cutoff), color="red") +
    ggplot2::ggtitle(stringr::str_c("Influence Diagnosis for ", independentV))
}

alr_plot_dfbetas<- function(fit, x="index", label=NULL, type="both") {
  n=NROW(fit$fitted.values)
  cutoff = alr_cutoff_dfbetas(fit)

  independentV <- alr_internal_get_independent_variable_name(fit)

  if(is.null(label)) label=1:n
  inffit <- tibble::as_tibble(dfbetas(fit))

  inffit_long <- inffit %>% dplyr::mutate(index=1:n(),
                                          label=label,
                                          yhat=predict(fit),
                                          label=label)

  if("glm" %in% class(fit))
    inffit_long <- inffit_long %>% mutate(`logit(yhat)` = yhat,
                          yhat = predict(fit, type = 'response'))


  inffit_long <-  inffit_long %>% tidyr::pivot_longer(cols=1:(NCOL(inffit_long)-2), names_to = "variable")

  if(x=="yhat") {
    p <- inffit_long %>% ggplot2::ggplot(aes(y=value, x=yhat))
    if(type != "point")
      p <- p + ggplot2::geom_segment(aes(xend=yhat, yend=0),
                            color="blue", alpha=0.5)
  } else {
    p <- inffit_long %>% ggplot2::ggplot(aes(y=value, x=index))
    if(type != "point")
      p <- p + ggplot2::geom_segment(aes(xend=index, yend=0),
                            color="blue", alpha=0.5)
  }

  if(type!="line")
    p <- p + ggplot2::geom_point(shape=21, color="blue")

  p + ggplot2::annotate("text", x= Inf, y=Inf,
                        label=stringr::str_c("Threshold: ", round(cutoff,3)),
                        hjust=1.5,vjust=1.5, color="black") +
    ggrepel::geom_text_repel(
      data=inffit_long%>%dplyr::filter(abs(value) > cutoff),
      aes(label = label), size = 3, color="red") +
    ggplot2::geom_hline(yintercept=c(-cutoff, cutoff), color="red") +
    ggplot2::ylab("DFBETAS") +
    ggplot2::facet_wrap(~stringr::str_c("Influence Diagnosis for ", variable))
}

alr_outlier_response <- function(fit, cutoff=NULL) {
  if(is.null(cutoff)) {
    n=NROW(fit$fitted.values)
    cutoff = qt(0.025/n, df=fit$df.residual, lower.tail=F)
  }

  which(abs(stats::rstudent(fit)) > cutoff)
}

alr_high_leverage <- function(fit, cutoff=NULL,
                              show_leverage=FALSE,
                              label=NULL, label_name="label") {
  if(is.null(cutoff)) {
    cutoff = alr_cutoff_leverage(fit)
  }

  if(show_leverage==FALSE)
    return(which(stats::hatvalues(fit) > cutoff))
  else {
    ret <- tibble::tibble(index=1, leverage = stats::hatvalues(fit)) %>%
      dplyr::mutate(index=1:n())

    if(!is.null(label)){
      ret <- ret %>% dplyr::mutate(index = label) %>%
        rename_with(function(x) gsub("index", label_name, x))
    }

    return(ret  %>% dplyr::filter(leverage > cutoff))
  }
}

alr_influential_cookd <- function(fit, cutoff=NULL,
                                  show_cookd=FALSE,
                                  label=NULL, label_name="label") {
  if(is.null(cutoff)) {
    cutoff = alr_cutoff_cookd(fit)
  }

  which(stats::cooks.distance(fit) > cutoff)

  if(show_cookd==FALSE)
    return(which(stats::cooks.distance(fit) > cutoff))
  else {
    ret <- tibble::tibble(index=1, cookd = stats::cooks.distance(fit)) %>%
      dplyr::mutate(index=1:n())

    if(!is.null(label)){
      ret <- ret %>% dplyr::mutate(index = label) %>%
        rename_with(function(x) gsub("index", label_name, x))
    }

    return(ret  %>% dplyr::filter(cookd > cutoff))
  }
}

alr_influential_dffit <- function(fit, cutoff=NULL) {
  if(is.null(cutoff)) {
    cutoff = alr_cutoff_dffit(fit)
  }

  which(abs(stats::dffits(fit)) > cutoff)
}

alr_influential_covratio <- function(fit, cutoff=NULL) {
  if(is.null(cutoff)) {
    cutoff = alr_cutoff_covratio(fit)
  }

  which(abs(stats::covratio(fit)-1) > cutoff)
}

alr_influential_dfbetas <- function(fit, cutoff=NULL, union=TRUE) {
  if(is.null(cutoff)) {
    cutoff = alr_cutoff_dfbetas(fit)
  }

  if(union==TRUE) {
    which(apply(abs(stats::dfbetas(fit)) > cutoff,1,any))
  } else {
    abs(stats::dfbetas(fit)) > cutoff
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


### CCPR ####
### Component and component plus residual plot
###

alr_plot_ccpr <- function(fit, component=TRUE,
                          smoother = NULL, spline_df = 3,
                          variable=NULL) {

  if("mfp"%in%class(fit)) {
    dat <- tibble::as_tibble(fit$x) %>%
      dplyr::select(-"Intercept") %>%
      dplyr::select(where(is.numeric)) %>%
      dplyr::mutate(e=resid(fit))
  } else {
     dat <- tibble::as_tibble(fit$model) %>%
       dplyr::select(-1) %>%
       dplyr::select(where(is.numeric)) %>%
       dplyr::mutate(e = resid(fit))
  }

  k=NCOL(dat)-1

  if(is.null(variable)) {
    crange = 1:k
  } else {
    crange = which(names(dat)[1:k]  %in% variable)
  }

  dat <- tidyr::pivot_longer(dat, cols=all_of(crange),
                             names_to = "variable") %>%
    dplyr::mutate(component = value*fit$coefficients[variable],
                  `component+residual` = component + e)

  if(is.null(smoother)) smoother="lowess"

  p <- dat %>% ggplot2::ggplot(aes(y=`component+residual`,
                                   x=value, group=variable)) +
    ggplot2::geom_point()

  if(component==TRUE) {
    p <- p + ggplot2::geom_line(aes(y=component, color="fitted"))
  }

  if(any(c("lowess", "Lowess") %in% smoother))
    p <- p+ ggplot2::geom_smooth(se=F, aes(color="lowess"))

  if("mSpline" %in% smoother || "mspline" %in% smoother)
    p <- p + ggplot2::geom_smooth(se=F, aes(color="mSpline"),
                         method="lm", formula = y ~ splines2::mSpline(x, df=spline_df))

  if("bSpline" %in% smoother || "bspline" %in% smoother)
    p <- p + ggplot2::geom_smooth(se=F, aes(color="bSpline"),
                         method="lm", formula = y ~ splines2::bSpline(x, df=spline_df))
   if("fp" %in% smoother) {
     fp_dat <- dat %>%
       dplyr::group_by(variable) %>%
       dplyr::summarise(fp = predict(mfp::mfp(`component+residual` ~ fp(value, df = 4))),
                 value=value)
     p <- p + ggplot2::geom_line(data=fp_dat, aes(y=fp, x=value, color="Fractional Polynomial"))
   }
  p + scale_color_discrete("") +
      ggplot2::facet_wrap(~variable, scales="free")
}

alr_print_modelSelection <- function(fit) {
  fit <- fit %>% dplyr::select(n, predictors, rsquare, adjr, cp, aic, sbc)

  best_predictors <-
    fit %>% dplyr::arrange(n,aic) %>%
    dplyr::group_by(n)%>%dplyr::summarise(predictors=predictors[1])

  nSample <- round(exp((fit$sbc[1]-fit$aic[1])/(fit$n[1]+2)+2))

  best_single <-
    fit%>%dplyr::filter(predictors %in% best_predictors$predictors)  %>%
    dplyr::mutate(models=factor(n,labels=gsub(" ","+",predictors)),
                  R2 = rsquare, adj_R2= adjr,
                  `Mallow's Cp`=cp, AIC=aic,
                  AICc = aic + 2*(n+2)*(n+3)/(nSample-n+1),
                  BIC = sbc)
  best_single%>%
    dplyr::select(n, models, R2, adj_R2, `Mallow's Cp`, AIC, AICc, BIC)%>%
    dplyr::rename(`# Pred`=n)
}

alr_plot_modelSelection <- function(fit) {
  fit <- fit %>% dplyr::select(n, predictors, rsquare, adjr, cp, aic, sbc)

  best_predictors <- fit %>%
    dplyr::arrange(n,aic) %>%
    dplyr::group_by(n) %>%
    dplyr::summarise(predictors=predictors[1])

  nSample <- round(exp((fit$sbc[1]-fit$aic[1])/(fit$n[1]+2)+2))

  best_single <-
    fit%>%dplyr::filter(predictors %in% best_predictors$predictors)  %>%
    dplyr::mutate(aicc = aic + 2*(n+2)*(n+3)/(nSample-n+1)) %>%
    dplyr::mutate(models=factor(n,labels=gsub(" ","+",predictors)),
                  R2 = (rsquare-min(rsquare))/(max(rsquare)-min(rsquare)),
                  adj_R2= (adjr-min(adjr))/(max(adjr)-min(adjr)),
                  `Mallow's Cp`=(cp-min(cp))/(max(cp)-min(cp)),
                  AIC=(aic-min(aic))/(max(aic)-min(aic)),
                  AICc = (aicc - min(aicc))/(max(aicc)-min(aicc)),
                  BIC=(sbc-min(sbc))/(max(sbc-min(sbc))))

  best_single%>%
    tidyr::pivot_longer(cols=c("R2","adj_R2","Mallow's Cp","AIC","AICc", "BIC"), names_to="Criteria") %>%
    ggplot2::ggplot(aes(x=models,y=value, group=Criteria, color=Criteria, shape=Criteria)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::ggtitle("All Possible Model Selection") +
    ggplot2::scale_x_discrete(labels=c("1"="R"))+
    ggplot2::theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x=element_text(angle=45,vjust=1,hjust=1))
}

alr_show_bestModel <- function(fit) {

  nSample <- base::round(exp((fit$sbc[1]-fit$aic[1])/(fit$n[1]+2)+2))
  fit <- fit %>% dplyr::mutate(aicc = aic + 2*(n+2)*(n+3)/(nSample-n+1))

  fit[which.max(fit$adjr),] %>% dplyr::select(n, predictors) %>%
    dplyr::mutate(variable="adj_R2") %>%
    dplyr::bind_rows(fit[which.min(fit$cp),] %>%
                dplyr::select(n, predictors) %>%
                dplyr::mutate(variable="Mallow's Cp")) %>%
    dplyr::bind_rows(fit[which.min(fit$aic),] %>%
                dplyr::select(n, predictors) %>%
                dplyr::mutate(variable="AIC")) %>%
    dplyr::bind_rows(fit[which.min(fit$aicc),] %>%
                dplyr::select(n, predictors) %>%
                dplyr::mutate(variable="AICc")) %>%
    dplyr::bind_rows(fit[which.min(fit$sbc),] %>%
                dplyr::select(n, predictors) %>%
                dplyr::mutate(variable="BIC")) %>%
    dplyr::group_by(n, predictors) %>%
    dplyr::summarise(Criteria=stringr::str_c(variable, collapse=", " )) %>%
    dplyr::mutate(models=gsub(" ", "+", predictors)) %>%
    dplyr::select(-predictors) %>% dplyr::rename(`# Pred`=n)
}

alr_internal_get_independent_variable_name <- function(fit) {
  if("mfp" %in% class(fit)) {
    return(as.character(fit$formula)[2])
  } else if(sum(c("glm", "lm") %in% class(fit)>0)) {
    return(names(fit$model)[1])
  } else {
    stop("Only lm or glm types are supported.")
  }
}
