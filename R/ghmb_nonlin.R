#' Generalized Nonlinear Hierarchical Model-Based (GNHMB) estimation method
#' @encoding UTF-8
#' @param y_S Response object that can be coerced into a column vector. The
#' \code{_S} denotes that \code{y} is part of the data set \emph{S}.
#' @param Xtilde_S Object of partial derivatives with respect to corresponding model parameters that can be coerced into a matrix.
#' The rows of \code{Xtilde_S} correspond to the rows of \code{y_S}.
#' @param Xtilde_Sa Object of partial derivatives with respect to corresponding model parameters that can be coerced into a matrix.
#' The set \emph{Sa} is the intermediate data set.
#' @param Ztilde_Sa Object of partial derivatives with respect to corresponding model parameters that can be coerced into a matrix.
#' The set \emph{Sa} is the intermediate dataset. The rows of \code{Ztilde_Sa} correspond to
#' the rows of \code{Xtilde_Sa}
#' @param Ztilde_U Object of partial derivatives with respect to corresponding model parameters that can be coerced into a matrix.
#' The set \emph{U} is the target population.
#' @param Omega_S The covariance structure of \eqn{\boldsymbol{\epsilon}_{S}}{
#' \epsilon_S}, up to a constant.
#' @param Sigma_Sa The covariance structure of \eqn{\boldsymbol{u}_{Sa}}{
#' u_Sa}, up to a constant.
#' @details
#' The GNHMB assumes two nonlinear models
#' \deqn{y = f(\boldsymbol{x}, \boldsymbol{\beta} )+ \epsilon}{
#'       y = f(x, \beta) + \epsilon}
#' \deqn{f(\boldsymbol{x}, \boldsymbol{\beta}) = g(\boldsymbol{z}, \boldsymbol{\alpha}) + \boldsymbol{u}}{
#'       f(x, \beta) = g(z, \alpha) + u}
#' \deqn{\epsilon \perp u}{\epsilon indep. u}
#' For a sample from the superpopulation, the GNHMB assumes
#' \deqn{E(\boldsymbol{\epsilon}) = \mathbf{0},
#'       E(\boldsymbol{\epsilon} \boldsymbol{\epsilon}^T) = \omega^2 \boldsymbol{\Omega}}{
#'       E(\epsilon) = 0, E(\epsilon \epsilon') = \omega^2 \Omega}
#' \deqn{E(\boldsymbol{u}) = \mathbf{0},
#'       E(\boldsymbol{u} \boldsymbol{u}^T) = \sigma^2 \boldsymbol{\Sigma}}{
#'       E(u) = 0, E(u u') = \sigma^2 \Sigma}
#' @return A fitted object of class HMB. 
#' @seealso {[summary()]}, {[getSpec()]}.
#' @examples
#' pop_U    = sample(nrow(HMB_data), 20000)
#' pop_Sa   = sample(pop_U, 2500)
#' pop_S    = sample(pop_U, 300)
#'
#' y_S      = HMB_data[pop_S, "GSV"]
#' X_S      = HMB_data[pop_S, c("pVeg")]
#' X_Sa     = HMB_data[pop_Sa, c("pVeg")]
#' Z_Sa     = HMB_data[pop_Sa, c("B20", "B30")]
#' Z_U      = HMB_data[pop_U, c("B20", "B30")]
#'
#' X_S  = as.matrix(cbind(1, X_S));
#' X_Sa = as.matrix(cbind(1, X_Sa));
#' Z_Sa = as.matrix(cbind(1, Z_Sa));
#' Z_U  = as.matrix(cbind(1, Z_U));
#'
#' Omega_S  = diag(1, nrow(X_S))
#' Sigma_Sa = diag(1, nrow(Z_Sa))
#'
#' f_M1 = function(x,a,b)a*x^b;
#' M1_nonlin <- nls(y_S ~ f_M1(X_S[,-1],a,b), start = list(a=2, b=0.5));
#' 
#' derivatives <- Deriv(f_M1, c("a","b"), combine='cbind');
#' Xtilde_S =  as.matrix(derivatives(X_S[,-1],  
#' coef(M1_nonlin)[1], coef(M1_nonlin)[2]));
#' Xtilde_Sa = as.matrix(derivatives(X_Sa[,-1], 
#' coef(M1_nonlin)[1], coef(M1_nonlin)[2]));
#'
#' f_M2 = function(z1,z2,a0,a1,a2)(a0 + a1*z1 + a2*z2)^2;
#' M2_nonlin <- nls(yHat_Sa ~ f_M2(Z_Sa[,2],Z_Sa[,3],a0,a1,a2), 
#' start = list(a0=0.8, a1=-0.001, a2=-0.002));
#' 
#' derivatives2 <- Deriv(f_M2, c("a0","a1", "a2"), combine='cbind');
#' Ztilde_Sa = as.matrix(derivatives2(Z_Sa[,2], Z_Sa[,3], 
#' coef(M2_nonlin)[1], coef(M2_nonlin)[2], coef(M2_nonlin)[3]));
#' Ztilde_U =  as.matrix(derivatives2(Z_U[,2],  Z_U[,3],  
#' coef(M2_nonlin)[1], coef(M2_nonlin)[2], coef(M2_nonlin)[3]));
#'
#' ghmb_model = ghmb_nonlin(
#'   y_S, Xtilde_S, Xtilde_Sa, Ztilde_Sa, Ztilde_U, Omega_S, Sigma_Sa);
#' ghmb_model
#' @references Saarela, S., Wästlund, A., Holmström, E., Mensah, A.A., Holm, S., Nilsson, M., Fridman, J. & Ståhl, G. (2020). 
#' Mapping aboveground biomass and its uncertainty using LiDAR and field data, accounting for tree-level allometric and LiDAR model errors,
#' \emph{Forest Ecosystems, 7(43),} 1-17.
#' @export
ghmb_nonlin = function(
  y_S,
  X_S,
  X_Sa,
  Z_Sa,
  Z_U,
  Omega_S,
  Sigma_Sa) {
  popCheck(y_S, X_S, X_Sa, Z_Sa, Z_U)

  if (
    missingArg(Omega_S) ||
    missingArg(Sigma_Sa)
  ) {
    stop('Missing necessary arguments.')
  }

  if (
    nrow(Z_Sa) != nrow(Sigma_Sa) ||
    nrow(Sigma_Sa) != ncol(Sigma_Sa)
  ) {
    stop('Sigma_Sa has incorrect dimensions.')
  }

  if (
    nrow(X_S) != nrow(Omega_S) ||
    nrow(Omega_S) != ncol(Omega_S)
  ) {
    stop('Omega_S has incorrect dimensions.')
  }



  ## Initialize HMB
  h = new("HMB")
  h@method = 'GHMB'

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
    Z_U = as.matrix(Z_U),
    Omega_S = as.matrix(Omega_S),
    Sigma_Sa = as.matrix(Sigma_Sa)
  )

  model = cpp_ghmb(
    h@data$y_S,
    h@data$X_S,
    h@data$X_Sa,
    h@data$Z_Sa,
    h@data$Z_U,
    h@data$Omega_S,
    h@data$Sigma_Sa)

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
