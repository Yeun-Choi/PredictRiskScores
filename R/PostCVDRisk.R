#' PREDICT CVD (2017) Risk Score for People With CVD
#'
#' \code{PostCVDRisk} calculates the 2 year absolute risk of cardiovascular disease (CVD) for people with a history of atherosclerotic CVD. The outcome of future CVD
#' is defined as hospitalisation for acute coronary syndrome, heart failure, stroke or other cerebrovascular disease, peripheral vascular disease, or cardiovascular
#' death.
#'
#' @usage PostCVDRisk(dat, sex, age, eth, nzdep, smoker, diabetes,
#'             af, hf, othervd, bpl, lld, athrombi, sbp, tchdl, bmi,
#'             scr, hba1c, cvddays...)
#'
#' @inheritParams NoPriorCVDRisk
#' @inheritParams PriorT2DRisk
#' @param hf      heart failure status
#' @param othervd prior angina, peripheral vascular disease, or non-hospitalised cerebrovascular disease not associated with stroke or TIA
#' @param scr     most recent value of serum creatinine in micromol/L
#' @param cvddays time in days since most recent CVD event
#'
#' @inherit NoPriorCVDRisk details
#'
#' @return
#' returns either a single 5-year CVD risk estimate, or a numeric vector of risk estimates if \code{dat} is provided.
#' Input values for each parameter must conform to the following convention:
#'
#' \item{sex}{label or encode as one of the following:
#'            \itemize{
#'              \item M, Male, 1
#'              \item F, Female, 0
#'              }}
#' \item{age}{numeric value for years of age between 20 and 110}
#' \item{eth}{label or encode as one of the following:
#'            \itemize{
#'              \item NZ European, European, NZEO, Euro, E, 1, 10, 11, or 12
#'              \item Maori, NZMaori, NZ Maori, M, 2, or 21
#'              \item Pacific, Pacific Islander, PI, P, 3, 30, 31, 32, 33, 34, 35, 36, or 37
#'              \item Indian, Fijian Indian, South Asian, IN, I, or 43
#'              \item Asian, Other Asian, SE Asian, East Asian, Chinese, ASN, A, 4, 40, 41, 42, or 44
#'              \item note: Other Asian includes non-Indian South Asian
#'              }}
#' \item{nzdep}{numeric value between 1 and 5}
#' \item{smoker}{label or encode as one of the following:
#'            \itemize{
#'              \item Y, Yes, Smoker, 1, T, TRUE
#'              \item N, No, Non-smoker, 0, F, FALSE
#'              }}
#' \item{diabetes\cr af hf othervd bpl lld\cr athrombi}{label or encode as one of the following:
#'            \itemize{
#'              \item Y, Yes, 1, T, TRUE
#'              \item N, No, 0, F, FALSE
#'              }}
#' \item{sbp tchdl}{numeric value of measured result. Note:
#'            \itemize{
#'              \item SBP and total:HDL values must be available
#'              }}
#' \item{bmi scr\cr hba1c}{numeric value of calculated BMI, and measured serum creatinine and hba1c. If a value is unknown, then input as \code{NA}}
#' \item{cvddays}{numeric value of number of days since last ACS event. Note:
#'            \itemize{
#'              \item If the days since most recent CVD event is not known, then keep as \code{NA}
#'              \item Ensure that \code{othervd} is checked if any prior angina or peripheral vascular disease.
#'              \item If \code{othervd} is selected, of if \code{cvddays} is unknown, then will default to 1826.
#'              }}
#' \item{...}{further arguments:
#'            \itemize{
#'              \item \code{dp} numeric value to set decimal place; default is 4
#'              \item \code{allow.age} logical. Whether or not age range is extended outside of 30 - 74; default is TRUE. If set to FALSE, then \code{NA} is returned as risk estimate.
#'              \item \code{allow.na} logical. Whether or not missing values for binary variables and smoking status are treated as 0; default is TRUE. If set to FALSE, then \code{NA} is returned as risk estimate.
#'              }}
#'
#' @inheritSection NoPriorCVDRisk See Also
#'
#' @author
#' Billy Wu (R Developer) and Katrina Poppe (Principal Investigator)
#'
#' @references
#' This equation is yet to be published. However, it is an update to the previously published equation in Heart (Poppe et al. 2017).\cr
#' \href{https://heart.bmj.com/content/103/12/891.1}{Full Article}
#'
#' @export
#' @examples
#' # As a calculator (dataset not provided)
#' PostCVDRisk(sex=0, age=65, eth=In, nzdep=5, smoker=0, diabetes=0, af=0,
#'             hf=1, othervd=0,  bpl=1, lld=1, athrombi=1, sbp=118, tchdl=3.3,
#'             bmi=NA, scr=52, hba1c=NA, cvddays=60)
#'
#' PostCVDRisk(sex="F", age=76, eth="Indian", nzdep=5, smoker=0, diabetes=0,
#'             af=0, hf=1, othervd=0,  bpl=1, lld=1, athrombi=1, sbp=118,
#'             tchdl=3.3, bmi=NA, scr=52, hba1c=NA, cvddays=365,
#'             allow.age=F, allow.na=F)
#'
#' # As a vectoriser (dataset provided)
#' PostCVDRisk(TEST, sex=sex, age=age, eth=eth, nzdep=nzdep, smoker=smoker,
#'             diabetes=diabetes, af=af, hf=hf, othervd=othervd, cvddays=days,
#'             bmi=bmi, sbp=sbp, tchdl=tchdl, hba1c=hba1c, scr=scr, bpl=bpl,
#'             lld=lld, athrombi=athromb)
#'
# --- Code ---
PostCVDRisk <- function(dat, sex, age, eth, diabetes, smoker, mi_stroke, hf, bmi, sbp, tchdl, scr, statin, bpl, ...){

  # Params
  demo.vars   <- c("sex", "age", "eth")
  smk.vars    <- c("smoker")
  bin.vars    <- c("diabetes", "mi_stroke", "hf", "statin", "bpl")
  num.vars    <- c("sbp")
  numNA.vars  <- c("tchdl")
  lvl.vars    <- c("bmi", "scr")

  # Calls
  call      <- gsub("()", "",  match.call()[1])
  is.table  <- deparse(substitute(dat))!=""
  input     <- as.list(match.call()[-1])

  if(length(list(...)) == 0){

    dp        <- 4
    allow.age <- TRUE
    allow.na  <- TRUE

  } else {

    default <- setdiff(c("dp", "allow.age", "allow.na"), names(list(...)))

    if(length(default) %in% 1:2){

      lapply(default,
             function(x){

               if(x == "dp"){
                 val <- 4
               } else if(x == "allow.na") {
                 val <- TRUE
               } else {
                 val <- TRUE
               }
               assign(x, val, envir = parent.frame(2))
             })
    }

    lapply(names(list(...)),
           function(x)
             assign(x, unlist(list(...)[x]),
                    envir = parent.frame(2)))

  }

  # ParamCheck
  vars <- c(demo.vars, bin.vars, smk.vars, num.vars, numNA.vars)

  ParamCheck(input, vars, call, is.table, allow.age, allow.na)

  # Values
  f.ind <- which(tolower(input$sex) %in% ok.female)
  m.ind <- which(tolower(input$sex) %in% ok.male)

    demo.vals <- list(male     = +(input$sex %in% ok.male),
                      age50_59 = +(input$age %in% 50:59),
                      age60_69 = +(input$age %in% 60:69),
                      age70_79 = +(input$age >= 70),
                      maori    = +(tolower(input$eth) %in% ok.maori),
                      pacific  = +(tolower(input$eth) %in% ok.pi),
                      indian   = +(tolower(input$eth) %in% ok.indian),
                      asian    = +(tolower(input$eth) %in% ok.asian),
                      smoker   = +(tolower(input$smoker) %in% ok.smoker))

  bin.vals <- sapply(bin.vars,
                     function(x){
                       +(tolower(input[[x]]) %in% ok.true)
                     },
                     USE.NAMES = TRUE,
                     simplify = FALSE)

  num.vals <- sapply("tchdl",    # SBP is reclassified
                     function(x){
                       as.numeric(input[[x]])
                     },
                     USE.NAMES = TRUE,
                     simplify = FALSE)

  sbp     <- list(sbplt100   = +(input$sbp < 100),
                  sbp120_140 = +(input$sbp >= 120 & input$sbp <= 139),
                  sbp140_160 = +(input$sbp >= 140 & input$sbp <= 159),
                  sbpge160   = +(input$sbp >= 160))

  bmi     <- list(bmilt20    = +(input$bmi < 20 & !is.na(input$bmi)),
                  bmi20_25   = +(input$bmi %in% 20:24 & !is.na(input$bmi)),
                  bmi30_35   = +(input$bmi %in% 30:34 & !is.na(input$bmi)),
                  bmi35_40   = +(input$bmi %in% 35:39 & !is.na(input$bmi)),
                  bmige40    = +(input$bmi >= 40 & !is.na(input$bmi)))

  scr     <- list(creat100_149 = +(input$scr >= 100 & input$scr <= 149 & !is.na(input$scr)),
                  creatge150   = +(input$scr >= 150 & !is.na(input$scr)))

  bin.vals$othervd <- NULL

  # nb: Order to match coeffs list
  values <- c(demo.vals, bin.vals, sbp, num.vals, bmi, scr, hba1c, days) # Order sensitive!

  # Coefficients
    values <- c(demo.vals, bin.vals, sbp, num.vals, bmi, scr) # Order sensitive!
    
    # Coefficients
    coeffs <- list(
      male          = 0.03587575,
      age50_59      = 0.05342182,
      age60_69      = 0.24286832,
      age70_79      = 0.52462088,
      maori         = 0.23015796,
      pacific       = 0.41315010,
      indian        = 0.16911057,
      asian         = -0.50879097,
      smoker        = 0.23873539,
      
      diabetes      = 0.31073697,
      mi_stroke     = 0.42372976,
      hf            = 1.07493660,
      statin        = 0.09119116,
      bplower       = 0.13462192,
      
      sbplt100      = 0.44988432,
      sbp120_140    = -0.01460494,
      sbp140_160    = 0.09897459,
      sbpge160      = 0.33063441,
      
      tchdl         = 0.10795178,
      
      bmilt20       = 0.81581102,      
      bmi20_25      = 0.09252985,      
      bmi30_35      = -0.16091766,
      bmi35_40      = -0.26400754,
      bmige40       = -0.38904755,

      creat100_149  = 0.29119313,
      creatge150    = 0.67363029)
    
    # Calculations
    value.score <- Map("*", values, coeffs)
    sum.score   <- Reduce("+", value.score)
    risk.score  <- (1 - 0.94235 ^ exp(sum.score - 1.577184))

  rounded.val <- as.numeric(formatC(round(risk.score, dp),
                                    format = 'f',
                                    digits = dp))

  if(length(ls(pattern = "inval.")) >= 1){

    rounded.val <- replace(rounded.val,
                           unlist(mget(ls(pattern = "inval."))),
                           NA)
  }

  return(rounded.val)

}


