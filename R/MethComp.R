#' Generate Report for Estimation Method Comparison
#'
#' @param n number of observations
#' @param SampFr SampFr
#' @param SampFrlim SampFrlim
#' @param EnLargeFactor Enlarge factor
#' @param TileSide tile size
#' @param PixelSize pixel size
#' @param meanY_S mean Y of S
#' @param sigmaY_S sigma Y of S
#' @param meanX_S mean X of S
#' @param sigmaX_S sigma X of S
#' @param meanZ_S mean Z of Z
#' @param sigmaZ_S sigma Z of Z
#' @param meanX_Sa mean X of Sa
#' @param sigmaX_Sa sigma X of Sa
#' @param meanZ_Sa mean Z of Sa
#' @param sigmaZ_Sa sigma Z of Sa
#' @param meanZ_U mean Z of U
#' @param CorrYX correlation Y and X
#' @param CorrYZ correlation Y and Z
#' @param CorrXZ correlation X and Z
#' @param AutoCorrF autocorrelation F
#' @param AutoCorrG autocorrelation G
#' @param AutoCorrG_star autocorrelation G_star
#' @param AutoCorr autocorrelation
#' @param module module
#' @param ylim ylim
#' @param nameForestAtt name of forest attributes
#' @param nameRSdata name of remotely sensed data
#' @param nameStudyArea name of study area
#' @param output_file output file location
#'
#' @return NULL
#' @examples
#' 
#' n = 100;
#' SampFr = 0.02;
#' SampFrlim = 0.1;
#' EnLargeFactor = 9;
#' TileSide = 1; # in [km]
#' 
#' meanY_S = 426.47;
#' sigmaY_S = 62201.45^0.5;
#' 
#' meanX_S = 826.27;
#' meanX_Sa = 822.43;
#' meanX_U = 822.43;
#' sigmaX_S = 338765.20^0.5;
#' sigmaX_Sa = 288367.00^0.5;
#' 
#' meanZ_S = 36.91;
#' meanZ_Sa = 37.46;
#' meanZ_U = 37.31;
#' sigmaZ_S = 38.16^0.5;
#' sigmaZ_Sa = 73.86^0.5;
#' 
#' CorrYX = 0.715;
#' CorrYZ = 0.349;
#' CorrXZ = 0.748;
#' 
#' AutoCorrF = 0.7; 
#' AutoCorrG_star = 0.8; 
#' AutoCorrG = 0.9;
#' AutoCorr = TRUE;
#' 
#' PixelSize = 25^2; 
#' 
#' module = "All";
#' ylim = c(40, 40, 50, 40, 100);
#' nameForestAtt = "AGB";
#' nameRSdata = c("GEDI", "TanDEM-X");
#' nameStudyArea = "TEF";
#' 
#' resultsTEF = MethComp(n, SampFr, SampFrlim, EnLargeFactor, TileSide, PixelSize, 
#' meanY_S,  sigmaY_S, meanX_S,  sigmaX_S, meanZ_S,  sigmaZ_S,  
#' meanX_Sa, sigmaX_Sa, meanZ_Sa, sigmaZ_Sa, meanZ_U, 
#' CorrYX, CorrYZ, CorrXZ, AutoCorrF, AutoCorrG, AutoCorrG_star, AutoCorr,
#' module, ylim, nameForestAtt, nameRSdata, nameStudyArea);
#' resultsTEF
#' 
#' @references Saarela, S., Holm, S., Healey, S.P., Patterson, P.L., Yang, Z., Andersen, H.E., 
#' Dubayah, R.O., Qi, W., Duncanson, L.I., Armston, J.D., Gobakken, T., Næsset, E., Ekström, M. & Ståhl, G. (2022). 
#' Comparing frameworks for biomass prediction for the Global Ecosystem Dynamics Investigation. 
#' \emph{Remote Sensing of Environment 278,} 113074.
#' @export
#'
MethComp <- function(n, SampFr, SampFrlim, EnLargeFactor, TileSide, PixelSize,
                     meanY_S,  sigmaY_S,
                     meanX_S,  sigmaX_S,
                     meanZ_S,  sigmaZ_S,
                     meanX_Sa, sigmaX_Sa,
                     meanZ_Sa, sigmaZ_Sa,
                     meanZ_U,
                     CorrYX, CorrYZ, CorrXZ,
                     AutoCorrF = 0.8, AutoCorrG = 0.95, AutoCorrG_star = 0.8, AutoCorr = TRUE,
                     module = "All", ylim = c(15, 15, 50, 20, 100),
                     nameForestAtt = "AGB",
                     nameRSdata = c("GEDI", "TanDEM-X"),
                     nameStudyArea = " ",
                     output_file = NULL) {

  listData = list(n = n,
                  SampFr = SampFr,
                  SampFrlim = SampFrlim,
                  EnLargeFactor = EnLargeFactor,
                  TileSide = TileSide,
                  PixelSize = PixelSize,
                  nameStudyArea = nameStudyArea,
                  nameForestAtt = nameForestAtt,
                  nameRSdata = nameRSdata,
                  meanY_S  = meanY_S,
                  sigmaY_S  = sigmaY_S,
                  meanX_S  = meanX_S,
                  sigmaX_S  = sigmaX_S,
                  meanZ_S  = meanZ_S,
                  sigmaZ_S  = sigmaZ_S,
                  meanX_Sa = meanX_Sa,
                  sigmaX_Sa = sigmaX_Sa,
                  meanZ_Sa = meanZ_Sa,
                  sigmaZ_Sa = sigmaZ_Sa,
                  meanX_U = meanX_Sa,
                  meanZ_U = meanZ_U,
                  CorrYX = CorrYX,
                  CorrYZ = CorrYZ,
                  CorrXZ = CorrXZ,
                  AutoCorrX = AutoCorrF,
                  AutoCorrZ = AutoCorrG,
                  AutoCorrZ_Sa = AutoCorrG_star,
                  AutoCorr = AutoCorr,
                  #module = module,
                  ylim = ylim);

  requireNamespace("rmarkdown")
  RMDname = system.file("rmd", "method_report.Rmd", package=getPackageName())
  rmarkdown::render(RMDname, params=listData, output_file = output_file, output_format="html_document")
}
