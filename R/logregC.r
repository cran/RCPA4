#' Logistic regression analysis with options for weighted observations, odds ratio reports, model fit statistics, and plots of residuals 
#'
#' @description Logistic regression analysis function with many useful features. Its standard output includes a table of coefficients, table of deviance residuals, and table of estimation information. Options include weighting observations, additional reports on odds ratios, ANOVA, classification table, multiple measures of model fit (including Lambda PRE), and diagnostic plots of residuals.
#' @param formula should be in dataset$dv ~ datatset$iv1 + dataset$iv2 unless dataset specified in optional data argument.
#' @param w (Optional) Sampling weights of variable, must be numeric; should be in dataset$weightvar form unless dataset specified in optional data argument.
#' @param data (Optional) Name of dataset that contains dv, iv (and w) variables.
#' @param digits (Optional) Number of decimal places reported in result (defaults to 3).
#' @param orci (Optional) Do you want table reporting odds ratios for coefficients with confidence intervals? (default: FALSE)
#' @param fit.stats (Optional) Do you want a table of assorted model fit statistics? (default: FALSE)
#' @param anova (Optional) Do you want ANOVA table reported? (default: FALSE)
#' @param classify (Optional) Do you want a classification table (observed vs predicted outcomes)? (default: FALSE)
#' @param table (Optional) Do you want logistic regression result tables reported? (default: TRUE)
#' @param printC (Optional) Do you want results printed to .html file in your working directory? Default is FALSE. Set to TRUE to print results.
#' @param plot.res (Optional) Do you want a set of diagnostic plots of model residuals? (default: FALSE)
#' @param simple.factors (Optional) Treat ordered factors like regular factors in regression models? Default is TRUE. Set to FALSE to use R's current contrast setting for ordered factors.
#' @param ... (Optional) Additional arguments passed to \code{\link[stats]{glm}} function (unweighted models) or \code{\link[survey]{svyglm}} function (weighted models).
#' @return Returns an augmented glm (unweighted models) or svyglm (weighted models)
#' object. The primary model object is unchanged for standard methods (for example
#' `predict`, `coef`, `summary`), and auxiliary result tables are available via
#' `[[` (for example `fit[["model_fit_table"]]`).
#' @examples 
#'   library(RCPA4)
#'   
#'   \donttest{
#'   # basic usage with variable vectors
#'   logregC(states$battleground2020 ~ states$vep20_turnout)
#'   
#'   # with post-estimation analysis
#'   logregC(states$battleground2020 ~ states$vep20_turnout, orci=TRUE, fit.stats=TRUE, 
#'           anova=TRUE, classify=TRUE, plot.res=TRUE)
#'           
#'   logregC(anes$has_children=="Yes" ~ anes$gender_s_sex + anes$r_age_election_day, 
#'            w=anes$wt)
#'   }
#' @export
#' @template console-output
#' @template result-tables
#' @section RCPA4 Package Tutorial Videos:
#' * [Logistic Regression Analysis with RCPA4 Package's logregC Function](https://youtu.be/tbFzTNtU8aw) 13:36   
#' * [Complete Playlist of RCPA4 Package Tutorial Videos](https://www.youtube.com/playlist?list=PL3jY4WDTUxoNqrxSSQH4q7XPLPYipeNCu), includes video for this function and many more. 
#' @section Textbook References:
#' * Barry C. Edwards and Philip H. Pollock, _An R Companion to Political Analysis, 4th Edition_ (Political Science Data, Forthcoming 2026), Chapter 14. 
#' * Philip H. Pollock and Barry C. Edwards, _The Essentials of Political Analysis, 6th Edition_ (Sage Publications, 2020), Chapter 9. ISBN-13: 978-1506379616; ISBN-10: 150637961.
#' @section Online Resources:
#' * [R Tutorials & Resources for Logistic Regression](https://www.poliscidata.com/pages/rDemosResources.php?chapter=14), compiled by Barry C. Edwards
#' * [Sage Edge Resources for Political Analysis Series](https://edge.sagepub.com/pollock), for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications. 
#' * [Political Science Data Web Site](https://www.poliscidata.com): Find datasets for your own research and resources to help with the analysis. 
#' @importFrom methods is
#' @importFrom survey svydesign svyglm
#' @importFrom grDevices png dev.off
#' @importFrom car Anova
#' @importFrom graphics par
#' @importFrom stats as.formula pchisq glm
#' @md

logregC <- function(formula, w=NULL, data=NULL, digits=3, orci=FALSE, fit.stats=FALSE, 
                    anova=FALSE, classify=FALSE, printC=FALSE, plot.res=FALSE, table=TRUE, 
                    simple.factors=TRUE, ...) 
{
  assert_required(formula)
  if (length(formula) != 3) stop(wrap_console_text("There's a problem: Your formula is missing or incorrect. To see how to use this function, try example(logregC) or help(logregC)."))
  if(!methods::is(formula, class2="formula")) stop(wrap_console_text("There's a problem: The logregC function works with a formula in the form dv ~ iv."))
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
  
  # ---- INTERNAL DATA MANAGEMENT (standard block; formula + optional weights) ----
  env <- parent.frame()
  
  formula_expr <- substitute(formula)
  w_expr       <- if (missing(w)) NULL else substitute(w)
  data_expr    <- if (missing(data)) quote(NULL) else substitute(data)
  
  data_ctx <- setup_data_context(data_expr = data_expr, env = env, data_missing = missing(data))
  data_val <- data_ctx$value
  
  # ensure formula is evaluated as a formula object
  formula_val <- eval(formula_expr, env, env)
  if (!methods::is(formula_val, "formula")) stop(wrap_console_text("There's a problem: The logregC function works with a formula in the form dv ~ iv."))
  
  # build model.frame data for the variables used in the formula
  mf <- stats::model.frame(formula_val, data = data_val, na.action = stats::na.pass)
  n_total_cases <- nrow(mf)
  n_used_cases <- NA_integer_
  n_missing_cases <- NA_integer_
  
  # resolve weights aligned with formula variables
  if (!is.null(w_expr)) {
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
    w <- rep(1, nrow(mf))
    w.name <- NULL
    weighted <- FALSE
  }

  
  # ----------------------------------------------------------------------------- 
  
  
  if(weighted==FALSE)  logit.model <- stats::glm(formula_val, data=data_val, family="binomial", na.action="na.exclude", ...)
  if (weighted == TRUE) {
    
    mf2 <- mf
    mf2[["w"]] <- w
    
    # Rename DV column to .dv
    names(mf2)[1] <- ".dv"
    
    # ---- KEY FIX: make predictor column names syntactic and remove `$` etc ----
    keep <- names(mf2)
    pred_old <- setdiff(keep, c(".dv", "w"))
    
    pred_new <- make.names(pred_old, unique = TRUE)
    
    # rename predictors in mf2
    names(mf2)[match(pred_old, names(mf2))] <- pred_new
    
    # Build design from mf2 (now all columns are “normal” names)
    svy.design <- survey::svydesign(ids = ~0, weights = ~w, data = mf2)
    
    # Build formula that references the normalized model-frame columns.
    svy.formula <- stats::reformulate(pred_new, response = ".dv")
    
    logit.model <- survey::svyglm(
      formula   = svy.formula,
      design    = svy.design,
      na.action = "na.exclude",
      family    = "quasibinomial",
      ...
    )
    
    svy.design <- NULL
    mf2 <- NULL
  }
  
  # Preserve user-facing formula/data references in returned call for
  # downstream tools that evaluate model$call (e.g., effects::allEffects).
  if(inherits(logit.model, "glm")) {
    model_call <- as.list(logit.model$call)
    model_call$formula <- formula_expr
    if(!missing(data)) model_call$data <- data_expr
    else model_call$data <- NULL
    logit.model$call <- as.call(model_call)
  }
  
  # ---- NEW: collect printC blocks ----------------------------------------
  blocks <- list()
  # ------------------------------------------------------------------------
  
  # Heading (ALWAYS printed to console)
  blocks <- render_heading(blocks, "Logistic Regression Analysis", marker="=", printC=printC)
  
  dv.name <- friendly_expr_label(formula_val[[2]])
  
  fmt_num <- function(x, digits = 3, nsmall = digits) {
    out <- rep("", length(x))
    keep_num <- !is.na(x) & is.finite(x)
    if(any(keep_num)) {
      out[keep_num] <- format(
        round(x[keep_num], digits),
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
  
  coef.table.caption <- "Logistic Regression Coefficients"
  coef.mat <- summary(logit.model)$coefficients
  coef.table <- data.frame(
    Estimate = fmt_num(coef.mat[, 1], digits = digits, nsmall = digits),
    `Std. Error` = fmt_num(coef.mat[, 2], digits = digits, nsmall = digits),
    `z stat` = fmt_num(coef.mat[, 3], digits = digits, nsmall = digits),
    `Pr(>|z|)` = fmt_coef_p(coef.mat[, 4], digits = digits),
    Signif = signif_symbol(coef.mat[, 4]),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  names(coef.table)[ncol(coef.table)] <- ""
  rownames(coef.table) <- rownames(coef.mat)
  coef.footer.lines <- c("---")
  if (any(nzchar(coef.table[[ncol(coef.table)]]))) {
    coef.footer.lines <- c(coef.footer.lines, "Signif. codes:   *** 0.001   ** 0.01   * 0.05")
  }
  dep_weight_line <- if(weighted==TRUE) {
    paste0("Dependent variable: ", dv.name, ". Weight variable: ", w.name)
  } else {
    paste0("Dependent variable: ", dv.name)
  }
  null_df <- if(!is.null(logit.model$df.null) && is.finite(logit.model$df.null[1])) {
    fmt_num(logit.model$df.null[1], digits = 0, nsmall = 0)
  } else {
    "NA"
  }
  aic_value <- if(!is.null(logit.model$aic) && is.finite(logit.model$aic[1])) {
    fmt_num(logit.model$aic[1], digits = digits, nsmall = digits)
  } else {
    NA_character_
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
      "Null deviance: ",
      fmt_num(logit.model$null.deviance, digits = digits, nsmall = digits),
      " on ",
      null_df,
      " degrees of freedom"
    ),
    paste0(
      "Residual deviance: ",
      fmt_num(logit.model$deviance, digits = digits, nsmall = digits),
      " on ",
      fmt_num(logit.model$df.residual, digits = 0, nsmall = 0),
      " degrees of freedom"
    ),
    dep_weight_line
  )
  if(!is.na(aic_value) && nzchar(aic_value)) {
    coef.footer.lines <- c(coef.footer.lines, paste0("AIC: ", aic_value))
  }
  coef.footer <- paste(coef.footer.lines, collapse = "\n")
  coef.align <- c(rep("r", ncol(coef.table) - 1L), "l")
  coef.html.colnames <- names(coef.table)
  coef.html.colnames[ncol(coef.html.colnames)] <- " "
  # Console table output, HTML table output (for printC)
  if(table!=FALSE) blocks <- render_table(blocks, x=coef.table, caption=coef.table.caption, digits=digits, printC=printC, footer=coef.footer, align=coef.align, html_colnames=coef.html.colnames, html_table_class="rcpa4-signif-table")
  
  slightpause()  
  
  res.table.caption <- "Deviance Residuals"
  dev.res <- summary(logit.model)$deviance.resid
  dev.res.table <- t(data.matrix((summary(dev.res[!is.na(dev.res)]))[-4]))
  # Console table output, HTML table output (for printC)
  if(table!=FALSE) blocks <- render_table(blocks, x=dev.res.table, caption=res.table.caption, digits=digits, printC=printC)
  
  slightpause()
  
  # Estimation Information (key items are currently reported in coefficient footer)
  estimation.info.table <- NULL
  if((table!=FALSE) && !isTRUE(fit.stats) && !is.null(estimation.info.table) && nrow(estimation.info.table) > 0L) {
    blocks <- render_table(blocks, x=estimation.info.table, caption="Estimation Information", digits=digits, printC=printC)
    slightpause()
  }
  
  # Classification table and Lambda (PRE), used by classify and/or fit.stats
  classification.tab <- NULL
  model.fit.table <- NULL
  orci.table <- NULL
  anova.table <- NULL
  lambda.pre <- NA_real_
  if(isTRUE(classify) || isTRUE(fit.stats))
  {
    predicted.outcome <- as.numeric(logit.model$fitted.values >= .5)
    observed.outcome <- logit.model$y
    if(weighted==TRUE && !is.null(logit.model$prior.weights)) class.w <- logit.model$prior.weights
    else class.w <- rep(1, length(predicted.outcome))
    classtab <- weighted_crosstab(
      dep = observed.outcome,
      indep = predicted.outcome,
      w = class.w,
      dep_levels = sort(unique(observed.outcome)),
      indep_levels = sort(unique(predicted.outcome)),
      dnn = c("Observed Outcomes", "Predicted Outcomes")
    )
    classification.tab <- as.matrix(classtab$tab)
    # Label rows/columns directly so caption can stay concise.
    if(identical(dim(classification.tab), c(2L, 2L))) {
      rownames(classification.tab) <- c("Observed=0", "Observed=1")
      colnames(classification.tab) <- c("Predicted=0", "Predicted=1")
    } else {
      rownames(classification.tab) <- paste0("Observed=", rownames(classification.tab), ")")
      colnames(classification.tab) <- paste0("Predicted=", colnames(classification.tab), ")")
    }
    if(!isTRUE(weighted)) {
      classification.tab <- round(classification.tab)
      storage.mode(classification.tab) <- "integer"
    }
    lambda.pre <- suppressWarnings(as.numeric(lambda(classtab$tab, digits=NULL))[1])
    if(!is.finite(lambda.pre)) lambda.pre <- NA_real_
  }

  
  if(fit.stats==TRUE) 
  {
    fit.stats.heading <- "Model Fit Statistics"
    # blocks <- render_heading(blocks, fit.stats.heading, marker="-", printC=FALSE)
    fitstats <- logregR2(logit.model, digits=digits)
    fitstats <- data.frame(
      Statistic = as.character(fitstats$Statistic),
      Value = as.character(fitstats$Value),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    fitstats <- rbind(
      fitstats,
      data.frame(
        Statistic = "Lambda (PRE)",
        Value = if(is.finite(lambda.pre)) fmt_num(lambda.pre, digits = digits, nsmall = digits) else "NA",
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    )
    model.fit.table <- fitstats
    # Console table output, HTML table output (for printC)
    if(table!=FALSE) blocks <- render_table(blocks, x=model.fit.table, caption=fit.stats.heading, digits=digits, printC=printC)
    
    slightpause()
  }
  
  if(orci==TRUE) 
  {
    orci.heading <- "Odds Ratios for Logistic Regression Coefficients"
    # blocks <- render_heading(blocks, orci.heading, marker="-", printC=FALSE)
    oddsratios <- orci(logit.model, digits)
    orci.table <- oddsratios
    # Console table output, HTML table output (for printC)
    if(table!=FALSE) blocks <- render_table(blocks, x=oddsratios, caption=orci.heading, digits=digits, printC=printC)
    slightpause()
  }
  
  if(anova==TRUE) 
  {
    anova.heading <- "ANOVA Table"
    # blocks <- render_heading(blocks, anova.heading, marker="-", printC=FALSE)
    anova.obj <- car::Anova(logit.model)
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
  
  
  if((table!=FALSE) && isTRUE(classify) && !is.null(classification.tab))
  {
    classify.heading <- "Classification Table"
    # Keep this as the final table in the output sequence.
    blocks <- render_table(blocks, x=classification.tab, caption=classify.heading, digits=digits, printC=printC)
    slightpause()
  }
  
  
  if(plot.res==TRUE)
  {
    par.mfrow.restore <- graphics::par("mfrow")
    par.mar.restore <- graphics::par("mar")
    
    draw_res_plots <- function() {
      graphics::par(mfrow=c(2,2), mar=c(4.5,4.5,2,1.5))
      plot(logit.model)
      graphics::par(mfrow=par.mfrow.restore, mar=par.mar.restore)
    }
    
    # show to user
    draw_res_plots()
    
    # export via printC
    if(isTRUE(printC)) {
      blocks <- add_block(
        blocks,
        plot_block(draw = draw_res_plots,
                   filename_base = "logregC.plot",
                   width = 6,
                   height = 4.5)
      )
    }
  }
  
  if(isTRUE(printC)) {
    blocks <- add_block(blocks, match.call(expand.dots = FALSE))
    print_blocksC(blocks)
  }
  
  return.tables <- list(
    coef_table = coef.table,
    deviance_residuals_table = dev.res.table,
    estimation_info_table = estimation.info.table,
    model_fit_table = model.fit.table,
    orci_table = orci.table,
    anova_table = anova.table,
    classification_table = classification.tab
  )
  
  out <- logit.model
  out <- rcpa4_attach_tables(
    out = out,
    tables = return.tables,
    fn_name = "logregC",
    call_obj = match.call(expand.dots = FALSE)
  )
  class(out) <- unique(c("logregC_result", class(out)))
  
  invisible(out)
}
  
  

