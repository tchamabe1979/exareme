#' Information of SAPS II score and outcome of 1,000 ICU patients.
#'
#' A dataset containing clinical information of 1,000 patients admitted to Italian Intesive Care Units
#' joining the GiViTI network (\emph{Gruppo Italiano per la valutazione degli interventi in Terapia Intensiva},
#' Italian Group for the Evaluation of the Interventions in Intensive Care Units). The data has
#' been collected within the ProSAFE project, an Italian observational study based on a continuous
#' data collection of clinical data in more than 200 Italian ICUs. The purpose of the project is a
#' continuous surveillance of the quality of care provided in the participating centres.
#' The actual values of the variables have been modified to protect subject confidentiality.
#'
#' The data contain the information to apply the SAPSII model, a prognostic model developed to
#' predict hospital mortality (Le Gall et al., 1993).
#' Both the computed SAPSII score and the associated probability of death are variables of the dataset.
#' The score is an integer number ranging from 0 to 163 describing the severity of the patient (the higher
#' the score, the more severe the patient). The probability is computed from the score through the formula
#' reported in the original paper. The dataset contains also the hospital survival of the patients.
#'
#' @format A data frame with 1000 rows and 33 variables. The dataset contains, for each predictor of the SAPSII score,
#' both the clinical information and the weight of that variable in the score (the variable with the suffix '_NUM').
#' \describe{
#'   \item{outcome}{hospital outcome, numeric binary variable with values 1 (deceased) and 0 (alive).}
#'   \item{probSaps}{probability estimated by the SAPSII prognostic model.}
#'   \item{sapsScore}{SAPSII score.}
#'   \item{age,age_NUM}{age, factor variable with levels (in years): '<40', '40-59', '60-69', '70-74', '75-80', '>=80'.}
#'   \item{adm,adm_NUM}{type of admission, factor variable with 3 levels: 'unschSurg' (unscheduled surgery), 'med' (medical), 'schSurg' (scheduled surgery).}
#'   \item{chronic,chronic_NUM}{chronic diseases, factor variable with 4 levels: 'noChronDis' (no chronic disease), 'metCarc' (metastatic carcinoma), 'hemMalig' (hematologic malignancy), 'aids' (AIDS).}
#'   \item{gcs,gcs_NUM}{Glasgow Coma Scale, factor variable with 5 levels: '3-5', '6-8', '9-10', '11-13', '14-15'.}
#'   \item{BP,BP_NUM}{systolic blood pressure, factor variable with 4 levels (in mmHg): '<70', '70-99', '100-199', '>=200'.}
#'   \item{HR,HR_NUM}{heart rate, factor variable with 5 levels: '<40', '40-69', '70-119', '120-159', '>=160'}
#'   \item{temp,temp_NUM}{temperature, factor variable with 2 levels (in Celsius degree): '<39', '>=39'.}
#'   \item{urine,urine_NUM}{urine output, factor variable with 3 levels (in L/24h): '<0.5', '0.5-0.99', '>=1'.}
#'   \item{urea,urea_NUM}{serum urea, factor variable with 3 levels (in g/L): '<0.60', '0.60-1.79', '>=1.80'.}
#'   \item{WBC,WBC_NUM}{wbc, factor variable with 3 levels (in 1/mm3): '<1', '1-19', '>=20'.}
#'   \item{potassium,potassium_NUM}{potassium, factor variable with 3 levels (in mEq/L): '<3', '3-4.9', '>=5'.}
#'   \item{sodium,sodium_NUM}{sodium, factor variable with 3 levels (in mEq/L): '<125', '125-144', '>=145'.}
#'   \item{HCO3,HCO3_NUM}{HCO3, factor variable with 3 levels (in mEq/L): '<15',  '15-19',  '>=20'.}
#'   \item{bili,bili_NUM}{bilirubin, factor variable with 3 levels (in mg/dL): '<4', '4-5.9', '>=6'.}
#'   \item{paFiIfVent,paFiIfVent_NUM}{mechanical ventilation and CPAP PaO2/FIO2, factor variable with 4 levels (PaO2/FIO2 in mmHg): 'noVent' (not ventilated), 'vent_<100' (ventialated and Pa02/FI02 <100), 'vent_100-199' (ventialated and Pa02/FI02 in 100-199), 'vent_>=200' (ventialated and Pa02/FI02 >= 200).}
#' }
#' @source \url{http://www.giviti.marionegri.it/Default.asp} (in Italian only)
#' @references Le Gall, Jean-Roger, Stanley Lemeshow, and Fabienne Saulnier. "A new simplified acute physiology score (SAPS II) based on a European/North American multicenter study." \emph{Jama} 270, no. 24 (1993): 2957-2963.
#'
#'  The GiViTI Network, \emph{Prosafe Project - 2014 report}. Sestante Edizioni: Bergamo, 2015. \url{http://www.giviti.marionegri.it/Download/ReportPROSAFE_2014_EN_Polivalenti_ITALIA.pdf}.
#'
#'
"icuData"
