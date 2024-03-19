#### alternate version for cv.enm

function (d, envs, enm, partitions, tune.tbl.i, doClamp, other.settings, 
                        partition.settings, user.val.grps, occs.testing.z, user.eval, 
                        algorithm, quiet) 
{
  tune.validate_mod <- function (enm, occs.train.z, occs.val.z, bg.train.z, bg.val.z, 
                                 mod.k, nk, tune.tbl.i, other.settings, partitions, user.eval, 
                                 quiet, k) 
  {
    occs.train.pred <- enm@predict(mod.k, occs.train.z, other.settings)
    occs.val.pred <- enm@predict(mod.k, occs.val.z, other.settings)
    bg.train.pred <- enm@predict(mod.k, bg.train.z, other.settings)
    if (nrow(bg.val.z) > 0) {
      bg.val.pred <- enm@predict(mod.k, bg.val.z, other.settings)
    }
    else {
      bg.val.pred <- NULL
    }
    if (other.settings$validation.bg == "full") {
      e.train <- dismo::evaluate(occs.train.pred, c(bg.train.pred, 
                                                    bg.val.pred))
      auc.train <- e.train@auc
      e.val <- dismo::evaluate(occs.val.pred, c(bg.train.pred, 
                                                bg.val.pred))
      auc.val <- e.val@auc
      auc.diff <- auc.train - auc.val
      if (other.settings$ecospat.use == TRUE) {
        if (partitions != "jackknife") {
          cbi.val <- ecospat::ecospat.boyce(c(bg.train.pred, 
                                              bg.val.pred, occs.val.pred), occs.val.pred, 
                                            PEplot = FALSE)$cor
        }
        else {
          cbi.val <- NA
        }
      }
      else {
        cbi.val <- NA
      }
    }
    else if (other.settings$validation.bg == "partition") {
      e.train <- dismo::evaluate(occs.train.pred, bg.train.pred)
      auc.train <- e.train@auc
      e.val <- dismo::evaluate(occs.val.pred, bg.val.pred)
      auc.val <- e.val@auc
      auc.diff <- auc.train - auc.val
      if (other.settings$ecospat.use == TRUE) {
        if (partitions != "jackknife") {
          cbi.val <- ecospat::ecospat.boyce(c(bg.val.pred, 
                                              occs.val.pred), occs.val.pred, PEplot = FALSE)$cor
        }
        else {
          cbi.val <- NA
        }
      }
      else {
        cbi.val <- NA
      }
    }
    if (other.settings$abs.auc.diff == TRUE) 
      auc.diff <- abs(auc.diff)
    min.train.thr <- min(occs.train.pred)
    or.mtp <- mean(occs.val.pred < min.train.thr)
    pct10.train.thr <- ENMeval:::calc.10p.trainThresh(occs.train.pred)
    or.10p <- mean(occs.val.pred < pct10.train.thr)
    if (is.function(user.eval)) {
      vars <- list(enm, occs.train.z, occs.val.z, bg.train.z, 
                   bg.val.z, mod.k, nk, other.settings, partitions, 
                   occs.train.pred, occs.val.pred, bg.train.pred, bg.val.pred, k)
      names(vars) <- c("enm", "occs.train.z", "occs.val.z", 
                       "bg.train.z", "bg.val.z", "mod.k", "nk", "other.settings", 
                       "partitions", "occs.train.pred", "occs.val.pred", 
                       "bg.train.pred", "bg.val.pred", 'k_num')
      user.eval.out <- user.eval(vars)
    }
    else {
      user.eval.out <- NULL
    }
    out.df <- data.frame(auc.val = auc.val, auc.diff = auc.diff, 
                         cbi.val = cbi.val, or.mtp = or.mtp, or.10p = or.10p)
    if (!is.null(user.eval.out)) 
      out.df <- cbind(out.df, user.eval.out)
    return(out.df)
  }
  
  envs.names <- names(d[, 3:(ncol(d) - 2)])
  occs.xy <- d %>% dplyr::filter(pb == 1) %>% dplyr::select(1:2)
  occs.z <- d %>% dplyr::filter(pb == 1) %>% dplyr::select(dplyr::all_of(envs.names))
  bg.xy <- d %>% dplyr::filter(pb == 0) %>% dplyr::select(1:2)
  bg.z <- d %>% dplyr::filter(pb == 0) %>% dplyr::select(dplyr::all_of(envs.names))
  nk <- length(unique(d[d$pb == 1, "grp"]))
  mod.full.args <- enm@args(occs.z, bg.z, tune.tbl.i, other.settings)
  mod.full <- do.call(enm@fun, mod.full.args)
  if (is.null(mod.full)) 
    stop("Training model is NULL. Consider changing the tuning parameters or inputting more background points.")
  if (!is.null(envs)) {
    pred.envs <- envs
  }
  else {
    pred.envs <- d %>% dplyr::select(dplyr::all_of(envs.names))
  }
  if (algorithm == "bioclim") 
    other.settings$tails <- tune.tbl.i$tails
  mod.full.pred <- enm@predict(mod.full, pred.envs, other.settings)
  train <- tune.train(enm, occs.z, bg.z, mod.full, envs, tune.tbl.i, 
                      other.settings, partitions, quiet)
  tune.args.col <- paste(names(tune.tbl.i), tune.tbl.i, collapse = "_", 
                         sep = ".")
  train.stats.df <- data.frame(tune.args = tune.args.col, 
                               stringsAsFactors = FALSE) %>% cbind(train)
  if (partitions == "none") {
    cv.res <- list(mod.full = mod.full, mod.full.pred = mod.full.pred, 
                   train.stats = train.stats.df, cv.stats = NULL)
    return(cv.res)
  }
  if (partitions == "testing") {
    bg.val.z <- data.frame()
    occs.testing.zEnvs <- occs.testing.z %>% dplyr::select(dplyr::all_of(envs.names))
    if (doClamp == TRUE) {
      occs.testing.zEnvs <- clamp.vars(orig.vals = occs.testing.zEnvs, 
                                       ref.vals = rbind(occs.z, bg.z), left = other.settings$clamp.directions$left, 
                                       right = other.settings$clamp.directions$right, 
                                       categoricals = other.settings$categoricals)
    }
    validate <- tune.validate_mod(enm, occs.z, occs.testing.zEnvs, 
                                  bg.z, bg.val.z, mod.full, 0, tune.tbl.i, other.settings, 
                                  partitions, user.eval, quiet)
    test.stats.df <- data.frame(tune.args = tune.args.col, 
                                fold = 0, stringsAsFactors = FALSE) %>% cbind(validate)
    cv.res <- list(mod.full = mod.full, mod.full.pred = mod.full.pred, 
                   train.stats = train.stats.df, cv.stats = test.stats.df)
    return(cv.res)
  }
  cv.stats <- list()
  for (k in 1:nk) {
    occs.train.z <- d %>% dplyr::filter(pb == 1, grp != 
                                          k) %>% dplyr::select(dplyr::all_of(envs.names))
    bg.train.z <- d %>% dplyr::filter(pb == 0, grp != k) %>% 
      dplyr::select(dplyr::all_of(envs.names))
    if (is.null(user.val.grps)) {
      occs.val.z <- d %>% dplyr::filter(pb == 1, grp == 
                                          k) %>% dplyr::select(dplyr::all_of(envs.names))
      bg.val.z <- d %>% dplyr::filter(pb == 0, grp == 
                                        k) %>% dplyr::select(dplyr::all_of(envs.names))
    }
    else {
      occs.val.z <- user.val.grps %>% dplyr::filter(grp == 
                                                      k) %>% dplyr::select(dplyr::all_of(envs.names))
      bg.val.z <- d %>% dplyr::filter(pb == 0, grp == 
                                        k) %>% dplyr::select(envs.names)
    }
    if (doClamp == TRUE) {
      val.z <- clamp.vars(orig.vals = rbind(occs.val.z, 
                                            bg.val.z), ref.vals = rbind(occs.train.z, bg.train.z), 
                          left = other.settings$clamp.directions$left, 
                          right = other.settings$clamp.directions$right, 
                          categoricals = other.settings$categoricals)
      occs.val.z <- val.z[1:nrow(occs.val.z), ]
      if (nrow(bg.val.z) > 0) 
        bg.val.z <- val.z[(nrow(occs.val.z) + 1):nrow(bg.val.z), 
        ]
    }
    mod.k.args <- enm@args(occs.train.z, bg.train.z, tune.tbl.i, 
                           other.settings)
    mod.k <- tryCatch({
      do.call(enm@fun, mod.k.args)
    }, error = function(cond) {
      if (quiet != TRUE) 
        message(paste0("\n", cond, "\n"))
      return(NULL)
    })
    if (is.null(mod.k)) {
      if (quiet != TRUE) 
        message(paste0("\nThe model for settings ", 
                       paste(names(tune.tbl.i), tune.tbl.i, collapse = ", "), 
                       " for partition ", k, " failed (resulted in NULL). Consider changing partitions. Cross validation averages will ignore this model."))
      next
    }
    validate <- tune.validate_mod(enm, occs.train.z, occs.val.z, 
                                  bg.train.z, bg.val.z, mod.k, nk, tune.tbl.i, other.settings, 
                                  partitions, user.eval, quiet, k)
    cv.stats[[k]] <- data.frame(tune.args = tune.args.col, 
                                fold = k, stringsAsFactors = FALSE) %>% cbind(validate)
  }
  cv.stats.df <- dplyr::bind_rows(cv.stats)
  cv.res <- list(mod.full = mod.full, mod.full.pred = mod.full.pred, 
                 train.stats = train.stats.df, cv.stats = cv.stats.df)
  return(cv.res)
}
