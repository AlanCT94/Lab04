plot.linreg <- function(obj) {
  
  #----------------------------------------------------------------------------#
  #- Set data for plotting
  #----------------------------------------------------------------------------#
  df <- data.frame(cbind(pred(obj), resid(obj)))
  
  #----------------------------------------------------------------------------#
  #- Data management for plotting
  #----------------------------------------------------------------------------#
  #- Standardized residuals
  resid_std <- resid(obj)/sqrt(obj$residual_variance)
  resid_std_abs_sqrt <- sqrt(abs(resid_std))
  #- Square root of the absolute value of standardized residuals
  df$resid_std_abs_sqrt <- resid_std_abs_sqrt
  df$id <- row.names(df)

  #- Find top third value
  third_topval <- resid_std_abs_sqrt[order(resid_std_abs_sqrt, decreasing = TRUE)][3]
  names(df) <- c("pred","resid","resid_std_abs_sqrt","id")

  #- Derive caption
  frm <- obj$formula
  cpt <- paste0("linreg(",format(frm),")")

  #- Calculate summary statistics
  resid_means <- aggregate(df$resid, list(df$pred), FUN=mean)
  names(resid_means) <- c("pred","resid")
  resid_medians <- aggregate(df$resid, list(df$pred), FUN=median)
  names(resid_medians) <- c("pred","resid")

  resid_std_abs_sqrt_means <- aggregate(df$resid_std_abs_sqrt, list(df$pred), FUN=mean)
  names(resid_std_abs_sqrt_means) <- c("pred","resid_std_abs_sqrt")
  resid_std_abs_sqrt_medians <- aggregate(df$resid_std_abs_sqrt, list(df$pred), FUN=median)
  names(resid_std_abs_sqrt_medians) <- c("pred","resid_std_abs_sqrt")

  #----------------------------------------------------------------------------#
  #- Make plots
  #----------------------------------------------------------------------------#

  theme_set(theme_bw())
  #- Plot 1

  pl1 <-
    ggplot2::ggplot(data = df) +
    ggplot2::aes(x=pred, y=resid) +
    ggplot2::geom_point(shape = 1) +

    ggplot2::ylim(round(min(df$resid),2), round(max(df$resid),2)) +

    ggplot2::geom_text(aes(label=ifelse(resid_std_abs_sqrt>=third_topval,
              as.character(id),'')), hjust=-0.2, vjust=-0.2, check_overlap = TRUE,
              size=3) +

    ggplot2::geom_line(data=resid_means, aes(color="mean")) +
    ggplot2::geom_line(data=resid_medians, aes(color="median")) +
    ggplot2::scale_color_manual(name = "", values = c("mean" = "red",
             "median" = "darkblue")) +

    ggplot2::labs(title="Residuals Vs Fitted",
               y="Residuals", x="Fitted values", caption=paste0(cpt)) +
    ggplot2::theme(plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5))

  plot(pl1)

  #- Plot 2
  pl2 <- ggplot2::ggplot(data = df) + ggplot2::aes(x=pred, y=resid_std_abs_sqrt) +
    ggplot2::geom_point(shape = 1) +

    ggplot2::ylim(0, round(max(resid_std_abs_sqrt),1)) +

    ggplot2::geom_text(aes(label=ifelse((resid_std_abs_sqrt>=third_topval) ,as.character(id),'')),
              hjust=-0.2, vjust=-0.2, check_overlap = TRUE, size=3) +

    ggplot2::geom_line(data=resid_std_abs_sqrt_means, aes(color="mean")) +
    ggplot2::geom_line(data=resid_std_abs_sqrt_medians, aes(color="median")) +
    ggplot2::scale_color_manual(name = "", values = c("mean" = "red",
            "median" = "darkblue")) +

    ggplot2::labs(title="Scale-Location",
        y=expression(sqrt("|standardised residuals|")), x="Fitted values",
        caption=paste0(cpt),col="T") +
    ggplot2::theme(plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5))
  plot(pl2)

}