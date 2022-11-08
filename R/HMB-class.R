#' Class HMB
#'
#' Class \code{HMB} is the base class for the HMB-package
#'
#' @name HMB-class
#' @rdname HMB-class
#' @exportClass HMB
#' @seealso \code{\link{hmb}}, \code{\link{ghmb}}, \code{\link{hmb_nonlin}}, \code{\link{ghmb_nonlin}}
setClass(
  'HMB',
  slots = c(
    method = 'character',
    n = 'list',
    data = 'list',
    modelArgs = 'list',
    
    Alpha = 'matrix',
    Beta = 'matrix',
    Gamma = 'matrix',
    AlphaCov = 'matrix',
    BetaCov = 'matrix',
    mu = 'numeric',
    muVar = 'numeric',
    predict = 'vector',
    resids = 'list'
  )
)

#### Set validitiy ####
setValidity(
  'HMB',
  function(object) {
    errors = character()
    method = toupper(object@method)
    
    # Check if method is correct
    if (!(method %in% c(
      'HMB',
      'GHMB'
    ))) {
      msg = "Method is not of correct type."
      errors = c(errors, msg)
    }
    
    if (length(errors) == 0) {
      return(TRUE)
    } else {
      return(errors)
    }
  }
)


#' @export
#' @docType methods
#' @rdname getSpec-methods
setGeneric(
  name = "getSpec",
  def = function(obj) standardGeneric("getSpec")
)

#' Get model specifications of HMB-class object
#'
#' @rdname getSpec-methods
#' @aliases getSpec,HMB-method
#' @param obj Object of class HMB
#' @return A list containing the estimated parameters, together with model arguments
#' @export
#' @examples
#' pop_U  = sample(nrow(HMB_data), 20000)
#' pop_Sa = sample(pop_U, 5000)
#' pop_S  = sample(pop_U, 300)
#'
#' y_S    = HMB_data[pop_S, "GSV"]
#' X_S    = HMB_data[pop_S, c("hMAX", "h80", "CRR", "pVeg")]
#' X_Sa   = HMB_data[pop_Sa, c("hMAX", "h80", "CRR", "pVeg")]
#' Z_Sa   = HMB_data[pop_Sa, c("B20", "B30", "B50")]
#' Z_U    = HMB_data[pop_U, c("B20", "B30", "B50")]
#'
#' hmb_model = hmb(y_S, X_S, X_Sa, Z_Sa, Z_U)
#' getSpec(hmb_model)
setMethod(
  "getSpec",
  "HMB",
  definition = function(obj) {
    validObject(obj)
    
    retlist = list(
      Alpha = obj@Alpha,
      AlphaCov = obj@AlphaCov,
      Beta = obj@Beta,
      BetaCov = obj@BetaCov
    )
    
    if (length(obj@modelArgs) > 0) {
      retlist$modelArgs = obj@modelArgs
    }
    
    if (length(obj@resids) > 0) {
      retlist$resids = obj@resids
    }
    
    if (obj@method %in% c('TSMB', 'GTSMB')) {
      retlist$Gamma = obj@Gamma
    }
    
    return(retlist)
  }
)


#' Display HMB model outputs
#'
#' @docType methods
#' @rdname show-methods
#' @aliases show,HMB,HMB-method
#' @param object Object of class HMB
#' @export
#' @examples
#' pop_U  = sample(nrow(HMB_data), 20000)
#' pop_Sa = sample(pop_U, 5000)
#' pop_S  = sample(pop_U, 300)
#'
#' y_S    = HMB_data[pop_S, "GSV"]
#' X_S    = HMB_data[pop_S, c("hMAX", "h80", "CRR", "pVeg")]
#' X_Sa   = HMB_data[pop_Sa, c("hMAX", "h80", "CRR", "pVeg")]
#' Z_Sa   = HMB_data[pop_Sa, c("B20", "B30", "B50")]
#' Z_U    = HMB_data[pop_U, c("B20", "B30", "B50")]
#'
#' hmb_model = hmb(y_S, X_S, X_Sa, Z_Sa, Z_U)
#' show(hmb_model)
setMethod(
  "show",
  "HMB",
  definition = function(object) {
    cat('Estimated population mean:', object@mu, '\n')
    cat('Estimated variance:', object@muVar, '\n')
  }
)


setGeneric(
  name = "summary",
  def = function(object, ...) standardGeneric("summary")
)

#'Summary of HMB model
#'
#' @rdname summary-methods
#' @export
#' @param object Object of class HMB
#' @return Summary of HMB model.
#' @examples
#' pop_U  = sample(nrow(HMB_data), 20000)
#' pop_Sa = sample(pop_U, 5000)
#' pop_S  = sample(pop_U, 300)
#'
#' y_S    = HMB_data[pop_S, "GSV"]
#' X_S    = HMB_data[pop_S, c("hMAX", "h80", "CRR", "pVeg")]
#' X_Sa   = HMB_data[pop_Sa, c("hMAX", "h80", "CRR", "pVeg")]
#' Z_Sa   = HMB_data[pop_Sa, c("B20", "B30", "B50")]
#' Z_U    = HMB_data[pop_U, c("B20", "B30", "B50")]
#'
#' S_Sa_map = matrix(pop_S, nrow = nrow(X_S), ncol = nrow(X_Sa))
#' S_Sa_map = t(apply(S_Sa_map, 1, function(x) {
#'   return(x == pop_Sa)
#' })) * 1
#'
#' hmb_model = hmb(y_S, X_S, X_Sa, Z_Sa, Z_U)
#' summary(hmb_model)
setMethod(
  "summary",
  "HMB",
  definition = function(object) {
    validObject(object)
    
    res = new('SummaryHMB')
    res@method = object@method
    
    res@samples = matrix(
      c(object@n$S, object@n$Sa, object@n$U),
      1L, 3L, dimnames = list('')
    )
    colnames(res@samples) = c('S', 'Sa', 'U')
    
    res@estimation = matrix(
      c(object@mu, object@muVar, object@mu + qnorm(c(.025, .975), 0, sqrt(object@muVar))),
      1L, 4L, dimnames = list('')
    )
    colnames(res@estimation) = c('Mean', 'Variance', 'Lower 95 % conf', 'Upper 95 % conf')
    
    betastd = sqrt(diag(object@BetaCov))
    res@betacoef = cbind(
      object@Beta, betastd, object@Beta / betastd,
      2 * pnorm(abs(object@Beta), 0, betastd, lower.tail = FALSE)
    )
    
    dimnames(res@betacoef) = list(
      c('(Intercept)', colnames(object@data$X_S)[-1]),
      c('Estimate', 'Std. error', 'Z value', 'Pr(>|Z|)')
    )
    
    alphastd = sqrt(diag(object@AlphaCov))
    res@alphacoef = cbind(
      object@Alpha, alphastd, object@Alpha / alphastd,
      2 * pnorm(abs(object@Alpha), 0, alphastd, lower.tail = FALSE)
    )
    dimnames(res@alphacoef) = list(
      c('(Intercept)', colnames(object@data$Z_Sa)[-1]),
      c('Estimate', 'Std. error', 'Z value', 'Pr(>|Z|)')
    )
    
    if (object@method %in% c('TSMB', 'GTSMB')) {
      res@gammacoef$gamma = object@Gamma
      rownames(res@gammacoef$gamma) = colnames(object@data$Z_Sa)
      colnames(res@gammacoef$gamma) = colnames(object@data$X_Sa)
      rownames(res@gammacoef$gamma)[1] = '(Intercept)'
      colnames(res@gammacoef$gamma)[1] = '(Intercept)'
    }
    
    return(res)
  }
)
