#' Hierarchical Model-Based estimation
#' @encoding UTF-8
#' @param y_S Response object that can be coerced into a column vector. The
#' \code{_S} denotes that \code{y} is part of the sample \emph{S}, with
#' \eqn{N_S \le N_{Sa} \le N_U}{N_S \le N_Sa \le N_U}.
#' @param X_S Object of predictors variables that can be coerced into a matrix.
#' The rows of \code{X_S} correspond to the rows of \code{y_S}.
#' @param X_Sa Object of predictor variables that can be coerce into a matrix.
#' The set \emph{Sa} is the intermediate sample.
#' @param Z_Sa Object of predictor variables that can be coerce into a matrix.
#' The set \emph{Sa} is the intermediate sample, and the Z-variables often some
#' sort of auxiliary, inexpensive data. The rows of \code{Z_Sa} correspond to
#' the rows of \code{X_Sa}
#' @param Z_U Object of predictor variables that can be coerce into a matrix.
#' The set \emph{U} is the universal population sample.
#' @return A fitted object of class HMB.
#' @details
#' The HMB assumes two models
#' \deqn{y = \boldsymbol{x} \boldsymbol{\beta} + \epsilon}{
#'       y = x \beta + \epsilon}
#' \deqn{\boldsymbol{x} \boldsymbol{\beta} = \boldsymbol{z} \boldsymbol{\alpha} + u}{
#'       x \beta = z \alpha + u}
#' \deqn{\epsilon \perp u}{\epsilon indep. u}
#' For a sample from the superpopulation, the HMB assumes
#' \deqn{E(\boldsymbol{\epsilon}) = \mathbf{0},
#'       E(\boldsymbol{\epsilon} \boldsymbol{\epsilon}^T) = \omega^2 \mathbf{I}}{
#'       E(\epsilon) = 0, E(\epsilon \epsilon') = \omega^2 I}
#' \deqn{E(\boldsymbol{u}) = \mathbf{0},
#'       E(\boldsymbol{u} \boldsymbol{u}^T) = \sigma^2 \mathbf{I}}{
#'       E(u) = 0, E(u u') = \sigma^2 I}
#' @seealso \code{\link{summary}},
#' \code{\link{getSpec}}.
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
#' hmb_model = hmb_nonlin(y_S, X_S, X_Sa, Z_Sa, Z_U)
#' hmb_model
#' @references Saarela, S., Wästlund, A., Holmström, E., Mensah, A.A., Holm, S., Nilsson, M., Fridman, J. & Ståhl, G. (2020). 
#' Mapping aboveground biomass and its uncertainty using LiDAR and field data, accounting for tree-level allometric and LiDAR model errors,
#' \emph{Forest Ecosystems, 7(43),} 1-17.

#' @export
hmb_nonlin = function(
  y_S,
  X_S,
  X_Sa,
  Z_Sa,
  Z_U) {
  popCheck(y_S, X_S, X_Sa, Z_Sa, Z_U)

  h = new("HMB")
  h@method = 'HMB'

  h@n = list(
    U = nrow(Z_U),
    Sa = nrow(X_Sa),
    S = nrow(X_S)
  )

  h@data = list(
    y_S = as.matrix(y_S),
    X_S = as.matrix(X_S),
    X_Sa = as.matrix(X_Sa),
    Z_Sa = as.matrix(Z_Sa),
    Z_U = as.matrix(Z_U)
  )

  model = cpp_hmb(
    h@data$y_S,
    h@data$X_S,
    h@data$X_Sa,
    h@data$Z_Sa,
    h@data$Z_U)

  h@resids = list(
    SigmaConst = model$sigma2^0.5,
    OmegaConst = model$omega2^0.5
  )

  h@Alpha = model$Alpha
  h@AlphaCov = model$AlphaCov
  h@Beta = model$Beta
  h@BetaCov = model$BetaCov
  h@mu = model$mu
  h@muVar = model$muVar
  h@predict = model$predict

  return(h)
}
