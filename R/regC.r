#' Linear regression analysis (OLS regression), with options for weighted observations, diagnostic tests, and plots of residuals
#'
#' @description Linear regression analysis function with many useful features. Standard output of results includes table of coefficients, table of residuals, and additional model information. Options for weighting observations, analysis of variance (ANOVA), performing post-estimation diagnostic tests, including testing normality of residuals and constant variance, and generating diagnostic plots of residuals.
#' @param formula should be in dataset$dv ~ datatset$iv1 + dataset$iv2 unless dataset specified in optional data argument. If weights are specified using w argument, the formula cannot contain functions or logical statements (all variables in the function must be named in the dataset).
#' @param w (Optional) Sampling weights of variable, must be numeric; should be in dataset$weightvar form unless dataset specified in optional data argument.
#' @param data (Optional) Name of dataset that contains dv, iv (and w) variables.
#' @param digits (Optional) Number of decimal places reported in result (defaults to 3).
#' @param anova (Optional) Do you want ANOVA table reporting F-test for all predictors? (default: FALSE)
#' @param norm.test (Optional) Test assumption that regression residuals follow normal distribution (default: FALSE)
#' @param ncv.test (Optional) Test assumption that regression residuals have constant variance (default: FALSE)
#' @param linear.test (Optional) Report results of linearity test? (default: FALSE)
#' @param reset.test (Optional) Report results of model specification test? (default: FALSE)
#' @param outlier.test (Optional) Test whether outlier observations have outsized leverage on results (default: FALSE)
#' @param vif (Optional) Report variance inflation factors to assess multicollinearity? (default: FALSE) 
#' @param table (Optional) Do you want regression result tables reported? (default: TRUE)
#' @param printC (Optional) Do you want to print tables of results (and residuals plots if plot.res=TRUE) to .html file in working directory? (default: FALSE)
#' @param plot.res (Optional)
#' @param simple.factors (Optional) Treat ordered factors like regular factors in regression models? Default is TRUE. Set to FALSE to use R's current contrast setting for ordered factors.
#' @param ... (Optional) Additional arguments passed to \code{\link[stats]{lm}} function (unweighted models) or \code{\link[survey]{svyglm}} function (weighted models).
#' @return Returns an augmented lm or svyglm object. The primary model object is
#' unchanged for standard methods (for example `residuals`, `coef`, `predict`),
#' and auxiliary result tables are available via `[[` (for example
#' `fit[["model_fit_table"]]`).
#' @examples 
#'   library(RCPA4)
#'   
#'   \donttest{ 
#'   # basic usage
#'   regC(states$vep20_turnout ~ states$hs_or_more)
#'    
#'   # with w and data arguments
#'   regC(anes$ft_labor_unions ~ anes$ft_dem_party, w=anes$wt)
#'   
#'   # multiple IV with some post-estimation tests
#'   regC(peace_index ~ vdem_edi_score + hdi, data=world, norm.test=TRUE, ncv.test=TRUE)
#'   }
#' @export
#' @template console-output
#' @template result-tables
#' @section RCPA4 Package Tutorial Videos:
#' * [Bivariate Regression Analysis with RCPA4 Package's regC Function](https://www.youtube.com/watch?v=53NPD-BGvwU) 10:31   
#' * [Multiple Regression Analysis with RCPA4 Package's regC Function](https://youtu.be/17Dxg5D3qtQ) 14:55   
#' * [Analyzing Regression Residuals with RCPA4 Package's regC Function](https://youtu.be/97hoiYuQnl4) 12:41   
#' * [Complete Playlist of RCPA4 Package Tutorial Videos](https://www.youtube.com/playlist?list=PL3jY4WDTUxoNqrxSSQH4q7XPLPYipeNCu), includes video for this function and many more. 
#' @section Textbook References:
#' * Barry C. Edwards and Philip H. Pollock, _An R Companion to Political Analysis, 4th Edition_ (Political Science Data, Forthcoming 2026), Chapters 11, 12, 13. 
#' * Philip H. Pollock and Barry C. Edwards, _The Essentials of Political Analysis, 6th Edition_ (Sage Publications, 2020), pp. 244-271. ISBN-13: 978-1506379616; ISBN-10: 150637961.
#' @section Online Resources:
#' * [R Tutorials & Resources for Correlation and Bivariate Regression](https://www.poliscidata.com/pages/rDemosResources.php?chapter=11), [Multiple Regression](https://www.poliscidata.com/pages/rDemosResources.php?chapter=12), and [Analyzing Regression Residuals](https://www.poliscidata.com/pages/rDemosResources.php?chapter=13), compiled by Barry C. Edwards.
#' * [Sage Edge Resources for Political Analysis Series](https://edge.sagepub.com/pollock), for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications.  
#' * [Political Science Data Web Site](https://www.poliscidata.com): Find datasets for your own research and resources to help with the analysis. 
#' @importFrom methods is
#' @importFrom survey svydesign svyglm
#' @importFrom car Anova outlierTest vif
#' @importFrom stats lm as.formula pchisq shapiro.test
#' @importFrom lmtest harvtest resettest bptest hmctest
#' @importFrom graphics par
#' @importFrom grDevices png dev.off
#' @md

regC <- function(formula, w=NULL, data=NULL, digits=3, anova=FALSE, 
                 norm.test=FALSE, ncv.test=FALSE, linear.test=FALSE, reset.test=FALSE, 
                 outlier.test=FALSE, vif=FALSE, printC=FALSE, plot.res=FALSE, table=TRUE, 
                 simple.factors=TRUE, ...) 
{
  assert_required(formula)
  if(length(formula) != 3) stop(wrap_console_text("There's a problem: Your formula is missing or specified incorrectly. Try example(regC) or help(regC) for assistance with this function."))
  if(!methods::is(formula, class2="formula")) stop(wrap_console_text("There's a problem: The regC function works with a formula in the form dv ~ iv."))
  assert_nonnegative_integer(digits)
  if(!is.logical(simple.factors) || length(simple.factors) != 1L || is.na(simple.factors)) stop("simple.factors must be TRUE or FALSE.")
  if(simple.factors) {
    old.contrasts <- options("contrasts")
    current.contrasts <- getOption("contrasts")
    if(length(current.contrasts) < 2L) current.contrasts <- c("contr.treatment", "contr.poly")
    options(contrasts = c(current.contrasts[1], "contr.treatment"))
    on.exit(options(old.contrasts), add=TRUE)
  }
  if((table == FALSE) && (plot.res == FALSE)) stop("At least one of table or plot.res must be TRUE.")
  
  if(plot.res==TRUE)
  {
    old.par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old.par), add=TRUE)
  }
  
  env <- parent.frame()
  formula_expr <- substitute(formula)
  w_expr <- if (missing(w)) NULL else substitute(w)
  data_expr <- if (missing(data)) quote(NULL) else substitute(data)
  
  data_ctx <- setup_data_context(data_expr = data_expr, env = env, data_missing = missing(data))
  data_val <- data_ctx$value
  
  formula_val <- eval(formula_expr, env, env)
  if(!methods::is(formula_val, class2="formula")) stop(wrap_console_text("There's a problem: The regC function works with a formula in the form dv ~ iv."))
  
  mf <- stats::model.frame(formula_val, data = data_val, na.action = stats::na.pass)
  n_total_cases <- nrow(mf)
  n_used_cases <- NA_integer_
  n_missing_cases <- NA_integer_
  
  if(!is.null(w_expr)) {
    w_raw <- eval_in_data_expr(w_expr, data = data_val, env = env)
    if(!is.numeric(w_raw)) stop("Weights must be numeric.")
    if(length(w_raw) != nrow(mf)) stop("Weights must have same length as variables.")
    keep <- stats::complete.cases(cbind(mf, w_raw))
    n_used_cases <- sum(keep)
    n_missing_cases <- n_total_cases - n_used_cases
    mf <- mf[keep, , drop = FALSE]
    w <- as.numeric(w_raw[keep])
    w.name <- friendly_expr_label(w_expr)
    weighted <- TRUE

  } else {
    keep <- stats::complete.cases(mf)
    n_used_cases <- as.integer(sum(keep))
    n_missing_cases <- as.integer(n_total_cases - n_used_cases)
    weighted <- FALSE
  }
  
  if(weighted==FALSE) reg.model <- stats::lm(formula_val, data=data_val, na.action="na.exclude", ...)
  
  if(weighted==TRUE)
  {
    mf2 <- mf
    mf2$w <- as.numeric(w)
    names(mf2)[1] <- ".dv"
    pred_old <- setdiff(names(mf2), c(".dv", "w"))
    pred_new <- make.names(pred_old, unique = TRUE)
    names(mf2)[match(pred_old, names(mf2))] <- pred_new
    
    svy.design <- survey::svydesign(ids=~0, weights=~w, data=mf2)
    svy.formula <- stats::reformulate(pred_new, response = ".dv")
    reg.model <- survey::svyglm(formula=svy.formula, design=svy.design, na.action="na.exclude", family="gaussian", ...)
    svy.design <- NULL
    mf2 <- NULL
  }
  
  # ---- NEW: collect printC blocks ----------------------------------------
  blocks <- list()
  # ------------------------------------------------------------------------
  
  # Heading (ALWAYS printed to console)
  blocks <- render_heading(blocks, "Linear Regression Analysis", marker="=", printC=printC)
  
  fmt_num <- function(x, digits = 3, nsmall = digits) {
    out <- rep("", length(x))
    keep <- !is.na(x) & is.finite(x)
    if(any(keep)) {
      out[keep] <- format(
        round(x[keep], digits),
        scientific = FALSE,
        trim = TRUE,
        nsmall = nsmall
      )
    }
    out
  }
  
  fmt_p <- function(x, digits = 3) {
    format_p_value(x, digits = digits)
  }

  fmt_coef_p <- function(x, digits = 3) {
    sub("^<\\s+", "<", format.pval(x, digits = digits, eps = 10^-digits, na.form = ""))
  }
  
  signif_symbol <- function(p) {
    out <- rep("", length(p))
    keep <- !is.na(p) & is.finite(p)
    if(any(keep)) {
      out[keep & p <= 0.001] <- "***"
      out[keep & p > 0.001 & p <= 0.01] <- "**"
      out[keep & p > 0.01 & p <= 0.05] <- "*"
    }
    out
  }
  
  test_table_from_htest <- function(test_obj) {
    stats <- character(0)
    vals <- character(0)
    
    if(!is.null(test_obj$statistic)) {
      s.name <- names(test_obj$statistic)
      if(length(s.name) == 0 || !nzchar(s.name[1])) s.name <- "Test statistic"
      stats <- c(stats, s.name[1])
      vals <- c(vals, fmt_num(as.numeric(test_obj$statistic)[1], digits = digits, nsmall = digits))
    }
    
    if(!is.null(test_obj$parameter)) {
      p.names <- names(test_obj$parameter)
      if(is.null(p.names)) p.names <- rep("", length(test_obj$parameter))
      for(i in seq_along(test_obj$parameter)) {
        this.name <- p.names[i]
        if(!nzchar(this.name)) this.name <- if(length(test_obj$parameter) == 1) "df" else paste("Parameter", i)
        this.val <- as.numeric(test_obj$parameter[i])
        if(grepl("df", this.name, ignore.case = TRUE)) {
          this.val.fmt <- fmt_num(this.val, digits = 0, nsmall = 0)
        } else {
          this.val.fmt <- fmt_num(this.val, digits = digits, nsmall = digits)
        }
        stats <- c(stats, this.name)
        vals <- c(vals, this.val.fmt)
      }
    }
    
    if(!is.null(test_obj$p.value)) {
      stats <- c(stats, "P-value")
      vals <- c(vals, fmt_p(as.numeric(test_obj$p.value)[1], digits = digits))
    }
    
    data.frame(
      Statistic = stats,
      Value = vals,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
  
  dv.name <- friendly_expr_label(formula_val[[2]])
  
  coef.mat <- summary(reg.model)$coefficients
  coef.table <- data.frame(
    Estimate = fmt_num(coef.mat[, 1], digits = digits, nsmall = digits),
    `Std. Error` = fmt_num(coef.mat[, 2], digits = digits, nsmall = digits),
    `t value` = fmt_num(coef.mat[, 3], digits = digits, nsmall = digits),
    `Pr(>|t|)` = fmt_coef_p(coef.mat[, 4], digits = digits),
    Signif = signif_symbol(coef.mat[, 4]),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  names(coef.table)[ncol(coef.table)] <- ""
  rownames(coef.table) <- rownames(coef.mat)
  coef.heading <- "Linear Regression Coefficients"
  residual.se <- if(weighted==FALSE) summary(reg.model)$sigma else sqrt(summary(reg.model)$dispersion[1])
  coef.footer.lines <- c("---")
  if (any(nzchar(coef.table[[ncol(coef.table)]]))) {
    coef.footer.lines <- c(coef.footer.lines, "Signif. codes:   *** 0.001   ** 0.01   * 0.05")
  }
  dep_weight_line <- if(weighted==TRUE) {
    paste0("Dependent variable: ", dv.name, ". Weight variable: ", w.name)
  } else {
    paste0("Dependent variable: ", dv.name)
  }
  coef.footer.lines <- c(
    coef.footer.lines,
    paste0(
      "N: ",
      fmt_num(n_used_cases, digits = 0, nsmall = 0),
      " (",
      fmt_num(n_missing_cases, digits = 0, nsmall = 0),
      " observations dropped due to missingness)"
    ),
    paste0(
      "Residual standard error: ",
      fmt_num(residual.se, digits = digits, nsmall = digits),
      " on ",
      fmt_num(reg.model$df.residual, digits = 0, nsmall = 0),
      " degrees of freedom"
    ),
    dep_weight_line
  )
  coef.footer <- paste(coef.footer.lines, collapse = "\n")
  coef.align <- c(rep("r", ncol(coef.table) - 1L), "l")
  coef.html.colnames <- names(coef.table)
  coef.html.colnames[ncol(coef.html.colnames)] <- " "

  # Console table output, HTML table output (for printC)
  if(table!=FALSE) blocks <- render_table(blocks, x=coef.table, caption=coef.heading, digits=digits, printC=printC, footer=coef.footer, align=coef.align, html_colnames=coef.html.colnames, html_table_class="rcpa4-signif-table")
  
  slightpause()  
  residuals.table <- round(t(data.matrix(summary(reg.model$residuals)[-4])), digits)
  residuals.heading <- "Regression Residuals"

  # Console table output, HTML table output (for printC)
  if(table!=FALSE) blocks <- render_table(blocks, x=residuals.table, caption=residuals.heading, digits=digits, printC=printC)
  
  slightpause()
  
  # Model Fit Statistics
  r2 <- NA_real_
  adj.r2 <- NA_real_
  f.stat <- NA_real_
  f.df.num <- NA_real_
  f.df.denom <- NA_real_
  f.stat.p.value <- NA_real_
  
  if(weighted==FALSE) {
    reg.summary <- summary(reg.model)
    r2 <- reg.summary$r.squared
    adj.r2 <- reg.summary$adj.r.squared
    if(!is.null(reg.summary$fstatistic) && length(reg.summary$fstatistic) >= 3) {
      f.stat <- as.numeric(reg.summary$fstatistic[1])
      f.df.num <- as.numeric(reg.summary$fstatistic[2])
      f.df.denom <- as.numeric(reg.summary$fstatistic[3])
      f.stat.p.value <- stats::pf(f.stat, df1=f.df.num, df2=f.df.denom, lower.tail=FALSE)
    }
  } else {
    if(is.finite(reg.model$null.deviance) && reg.model$null.deviance != 0) {
      r2 <- (reg.model$null.deviance - reg.model$deviance) / reg.model$null.deviance
    }
    if(is.finite(r2) && is.finite(reg.model$df.null) && is.finite(reg.model$df.residual) && reg.model$df.residual != 0) {
      adjust <- reg.model$df.null / reg.model$df.residual
      adj.r2 <- 1 - ((1-r2) * adjust)
    }
    f.df.num <- reg.model$df.null - reg.model$df.residual
    f.df.denom <- reg.model$df.residual
    if(is.finite(f.df.num) && is.finite(f.df.denom) && f.df.num > 0 && f.df.denom > 0) {
      f.stat.num <- (reg.model$null.deviance - reg.model$deviance) / f.df.num
      f.stat.denom <- reg.model$deviance / f.df.denom
      if(is.finite(f.stat.num) && is.finite(f.stat.denom) && f.stat.denom != 0) {
        f.stat <- f.stat.num / f.stat.denom
        f.stat.p.value <- stats::pf(f.stat, df1=f.df.num, df2=f.df.denom, lower.tail=FALSE)
      }
    }
  }
  
  fit.stats.table <- data.frame(
    Statistic = c("R-squared", "Adjusted R-squared", "F statistic", "F degrees of freedom", "Model P-value"),
    Value = c(
      fmt_num(r2, digits = digits, nsmall = digits),
      fmt_num(adj.r2, digits = digits, nsmall = digits),
      fmt_num(f.stat, digits = digits, nsmall = digits),
      if(is.finite(f.df.num) && is.finite(f.df.denom)) {
        paste0("(", fmt_num(f.df.num, digits = 0, nsmall = 0), ", ", fmt_num(f.df.denom, digits = 0, nsmall = 0), ")")
      } else {
        ""
      },
      fmt_p(f.stat.p.value, digits = digits)
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  model.fit.table <- fit.stats.table
  anova.table <- NULL
  norm.test.table <- NULL
  outlier.table <- NULL
  ncv.table <- NULL
  vif.table <- NULL
  reset.table <- NULL
  linear.table <- NULL
  
  if(table!=FALSE) {
    blocks <- render_table(
      blocks,
      x = model.fit.table,
      caption = "Model Fit Statistics",
      digits = digits,
      printC = printC
    )
  }
  slightpause()
  
  if(anova==TRUE) 
  {
    anova.heading <- "ANOVA Table"
    # blocks <- render_heading(blocks, anova.heading, marker="-", printC=FALSE)
    anova.obj <- car::Anova(reg.model)
    
    anova.table <- tryCatch(
      as.data.frame(anova.obj, check.names = FALSE, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
    
    if(table!=FALSE) {
      if(!is.null(anova.table)) {
        blocks <- render_table(blocks, x=anova.table, caption=anova.heading, digits=digits, printC=printC)
      } else {
        print(anova.obj)
        if(isTRUE(printC)) blocks <- add_block(blocks, utils::capture.output(print(anova.obj)))
      }
    }
    slightpause()
  }
  
  if(norm.test==TRUE)
  {
    heading.norm.test <- "Test of the Normality of Residuals"
    # blocks <- render_heading(blocks, heading.norm.test, marker="-", printC=FALSE)
    null.hypo.norm.test <- "Null Hypothesis: Residuals follow normal distribution."
    residuals <- reg.model$residuals
    res.skewness <- wtd_skewness(residuals, digits=NULL)
    res.kurtosis <- wtd_kurtosis(residuals, digits=NULL) - 3
    jb.stat <- length(residuals) * ((res.skewness^2)/6 + (res.kurtosis^2)/24)
    p.value <- stats::pchisq(jb.stat, df=2, lower.tail=FALSE)
    
    norm.test.table <- data.frame(
      Statistic = c("Jarque-Bera", "df", "P-value"),
      Value = c(
        fmt_num(jb.stat, digits = digits, nsmall = digits),
        fmt_num(2, digits = 0, nsmall = 0),
        fmt_p(p.value, digits = digits)
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    if(table!=FALSE) {
      blocks <- render_table(
        blocks,
        x = norm.test.table,
        caption = heading.norm.test,
        digits = digits,
        printC = printC,
        footer = null.hypo.norm.test
      )
    }
    slightpause()
  }
  
  if(outlier.test==TRUE) 
  {
    outlier.test.heading <- "Bonferroni Outlier Test"
    # blocks <- render_heading(blocks, outlier.test.heading, marker="-", printC=FALSE)
    outlier.test.null.hypo <- "Null Hypothesis: Largest regression residual is not an outlier."
    outlier.res <- car::outlierTest(reg.model)
    
    if(is.null(outlier.res)) {
      outlier.table <- data.frame(
        Statistic = "Result",
        Value = "No Bonferroni outlier detected.",
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    } else {
      outlier.df <- as.data.frame(outlier.res, stringsAsFactors = FALSE, check.names = FALSE)
      outlier.stats <- c("Observation(s)")
      outlier.vals <- c(paste(rownames(outlier.df), collapse = ", "))
      for(col in colnames(outlier.df)) {
        col.values <- outlier.df[[col]]
        if(is.numeric(col.values)) {
          if(grepl("p", col, ignore.case = TRUE)) {
            col.fmt <- fmt_p(as.numeric(col.values), digits = digits)
          } else {
            col.fmt <- fmt_num(as.numeric(col.values), digits = digits, nsmall = digits)
          }
          this.val <- paste(col.fmt, collapse = ", ")
        } else {
          this.val <- paste(as.character(col.values), collapse = ", ")
        }
        outlier.stats <- c(outlier.stats, col)
        outlier.vals <- c(outlier.vals, this.val)
      }
      outlier.table <- data.frame(
        Statistic = outlier.stats,
        Value = outlier.vals,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }
    
    if(table!=FALSE) {
      blocks <- render_table(
        blocks,
        x = outlier.table,
        caption = outlier.test.heading,
        digits = digits,
        printC = printC,
        footer = outlier.test.null.hypo
      )
    }
    slightpause()
  }
  
  if(ncv.test==TRUE)
  {
    ncv.test.heading <- "Non-Constant Error Variance Test"
    # blocks <- render_heading(blocks, ncv.test.heading, marker="-", printC=FALSE)
    ncv.test.null.hypo <- "Null hypothesis: Residuals have constant variance."
    ncv.res <- lmtest::bptest(reg.model)
    ncv.table <- test_table_from_htest(ncv.res)
    if("Statistic" %in% names(ncv.table)) {
      ncv.table$Statistic <- sub("^BP$", "Breusch-Pagan", as.character(ncv.table$Statistic))
    }
    if(table!=FALSE) {
      blocks <- render_table(
        blocks,
        x = ncv.table,
        caption = ncv.test.heading,
        digits = digits,
        printC = printC,
        footer = ncv.test.null.hypo
      )
    }
    slightpause()
  }
  
  if(vif==TRUE) 
  {
    n.iv <- length(reg.model$coefficients) - 1
    vif.heading <- "Variance Inflation Factors"
    # blocks <- render_heading(blocks, vif.heading, marker="-", printC=FALSE)
    if(n.iv > 1) 
    {
      vif.raw <- car::vif(reg.model)
      if(is.matrix(vif.raw) || is.data.frame(vif.raw))
      {
        if("GVIF^(1/(2*Df))" %in% colnames(vif.raw)) {
          vif.values <- as.numeric(vif.raw[, "GVIF^(1/(2*Df))"])
        } else if("GVIF" %in% colnames(vif.raw)) {
          vif.values <- as.numeric(vif.raw[, "GVIF"])
        } else {
          vif.values <- as.numeric(vif.raw[, 1])
        }
        var.names <- rownames(vif.raw)
      } else {
        vif.values <- as.numeric(vif.raw)
        var.names <- names(vif.raw)
      }
      if(is.null(var.names)) var.names <- rep("", length(vif.values))
      vif.table <- data.frame(
        Variable = as.character(var.names),
        VIF = vif.values,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      # Console table output, HTML table output (for printC)
      if(table!=FALSE) blocks <- render_table(blocks, x=vif.table, caption=vif.heading, digits=digits, printC=printC)
      
    } else {
      message("Sorry, can only report VIF for regression models with two or more independent variables.")
      if(isTRUE(printC)) {
        msg <- "Sorry, can only report VIF for regression models with two or more independent variables."
        class(msg) <- "statement"
        blocks <- add_block(blocks, msg)
      }
    }
    if(table!=FALSE) cat("\n")
    slightpause()
  }
  
  if(reset.test==TRUE) 
  {
    reset.test.heading <- "Model Specification Test"
    # blocks <- render_heading(blocks, reset.test.heading, marker="-", printC=FALSE)
    reset.test.null.hypo <- "Null Hypothesis: Regression model is correctly specified."
    reset.res <- lmtest::resettest(reg.model)
    reset.table <- test_table_from_htest(reset.res)
    if(table!=FALSE) {
      blocks <- render_table(
        blocks,
        x = reset.table,
        caption = reset.test.heading,
        digits = digits,
        printC = printC,
        footer = reset.test.null.hypo
      )
    }
    slightpause()
  }
  
  if(linear.test==TRUE) 
  {
    linear.test.heading <- "Test of Linearity"
    # blocks <- render_heading(blocks, linear.test.heading, marker="-", printC=FALSE)
    linear.test.null.hypo <- "Null Hypothesis: The relationship in regression model is linear."
    linear.res <- lmtest::harvtest(reg.model)
    linear.table <- test_table_from_htest(linear.res)
    if(table!=FALSE) {
      blocks <- render_table(
        blocks,
        x = linear.table,
        caption = linear.test.heading,
        digits = digits,
        printC = printC,
        footer = linear.test.null.hypo
      )
    }
    slightpause()
  }
  
  if(plot.res==TRUE)
  {
    par.mfrow.restore <- graphics::par("mfrow")
    par.mar.restore <- graphics::par("mar")
    
    draw_res_plots <- function() {
      graphics::par(mfrow=c(2,2), mar=c(4.5,4.5,2,1.5))
      plot(reg.model)
      graphics::par(mfrow=par.mfrow.restore, mar=par.mar.restore)
    }
    
    # show to user
    draw_res_plots()
    
    # export via printC
    if(isTRUE(printC)) {
      blocks <- add_block(
        blocks,
        plot_block(draw = draw_res_plots,
                   filename_base = "regC.plot",
                   width = 6,
                   height = 6)
      )
    }
  }
  
  if(isTRUE(printC)) {
    blocks <- add_block(blocks, match.call(expand.dots = FALSE))
    print_blocksC(blocks)
  }
  
  return.tables <- list(
    coef_table = coef.table,
    residuals_table = residuals.table,
    model_fit_table = model.fit.table,
    anova_table = anova.table,
    norm_test_table = norm.test.table,
    outlier_test_table = outlier.table,
    ncv_test_table = ncv.table,
    vif_table = vif.table,
    reset_test_table = reset.table,
    linear_test_table = linear.table
  )
  
  out <- reg.model
  out <- rcpa4_attach_tables(
    out = out,
    tables = return.tables,
    fn_name = "regC",
    call_obj = match.call(expand.dots = FALSE)
  )
  class(out) <- unique(c("regC_result", class(out)))
  
  invisible(out)
}
  

