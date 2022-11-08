#' Generate Report for Estimation Method Comparison
#'
#' @param n n
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
#' @param nameRSdata name of remote sensing data
#' @param nameStudyArea name of study area
#' @param output_file output file location
#'
#' @return NULL
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
