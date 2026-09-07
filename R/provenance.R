# Optional input-source sensitivity for the supplied annual GSS demonstration.
# Source flags identify input provenance, not independent/effective observations.
audit_input_provenance <- function(data, cell_source, measures, predictor,
                                  covariates=character(), own_lag=FALSE,
                                  lag_suffix="Lag", missing="common",
                                  se_method="classical", primary=NULL) {
  if (!exists("fit_landscape",mode="function")) stop("Source R/model.R before R/provenance.R.")
  missing<-match.arg(missing,c("common","per_outcome"))
  se_method<-match.arg(se_method,c("classical","HC3"))
  validate_roles(data,measures,predictor,covariates)
  if (!is.data.frame(cell_source)) stop("Provide the annual item-source data frame.")
  valid_years<-function(x) is.numeric(x) && !anyNA(x) && all(is.finite(x)) && all(x==as.integer(x)) && !anyDuplicated(x)
  if (!"year" %in% names(data) || !valid_years(data$year)) stop("Analysis data need unique, finite calendar years in a numeric year column.")
  if (!"year" %in% names(cell_source) || !valid_years(cell_source$year)) stop("Cell-source data need unique, finite calendar years in a numeric year column.")
  if (anyDuplicated(names(cell_source)) || length(setdiff(measures,names(cell_source)))) stop("Cell-source data need exactly named source columns for every requested outcome.")
  idx<-match(data$year,cell_source$year)
  if (anyNA(idx)) stop("The cell-source record does not cover every analysis year.")
  src<-as.matrix(cell_source[idx,measures,drop=FALSE])
  if (anyNA(src) || any(!src %in% c("extract","carried"))) stop("Current outcome source flags must be extract or carried for every analysis row.")
  previous_source_idx<-match(data$year-1L,cell_source$year)
  lag_src<-as.matrix(cell_source[previous_source_idx,measures,drop=FALSE])
  if (any(!is.na(lag_src) & !lag_src %in% c("extract","carried"))) stop("Lag-year source flags must be extract or carried when available.")
  colnames(lag_src)<-measures
  recomputed<-fit_landscape(data,measures,predictor,covariates,own_lag,lag_suffix,missing,se_method)
  if (!is.null(primary)) {
    if (!is.data.frame(primary) || anyDuplicated(primary$item) || !setequal(primary$item,measures)) stop("Primary landscape must contain exactly the requested outcomes.")
    primary<-primary[match(measures,primary$item),,drop=FALSE]
    check_fields<-c("n","df","b","se","ci_lo","ci_hi","t","p","status")
    if (!all(check_fields %in% names(primary)) || !isTRUE(all.equal(primary[check_fields],recomputed[check_fields],tolerance=1e-8,check.attributes=FALSE)))
      stop("Primary landscape does not match the supplied data and model settings; rebuild it before attaching provenance.")
  } else primary<-recomputed
  shared<-c(predictor,covariates)
  usable_columns<-lapply(measures,function(item) c(shared,item,if (own_lag) paste0(item,lag_suffix)))
  names(usable_columns)<-measures
  valid_columns<-vapply(usable_columns,function(cols) all(cols %in% names(data)) && all(vapply(data[cols],is.numeric,logical(1))),logical(1))
  common_mask<-if (missing=="common") {
    if (all(valid_columns)) finite_rows(data,unique(unlist(usable_columns,use.names=FALSE))) else rep(FALSE,nrow(data))
  } else NULL
  masks<-setNames(lapply(measures,function(item) {
    if (!valid_columns[[item]]) rep(FALSE,nrow(data)) else if (missing=="common") common_mask else finite_rows(data,usable_columns[[item]])
  }),measures)
  if (own_lag) {
    previous_data_idx<-match(data$year-1L,data$year)
    for (item in measures) if (valid_columns[[item]]) {
      lag_value<-data[[paste0(item,lag_suffix)]]
      previous_value<-data[[item]][previous_data_idx]
      both<-is.finite(lag_value) & is.finite(previous_value)
      if (any(abs(lag_value[both]-previous_value[both])>1e-8*pmax(1,abs(previous_value[both]))))
        stop(paste("Supplied own-lag is not the previous calendar-year outcome for",item,"; source flags cannot be assigned to that lag."))
    }
  }
  restricted_masks<-setNames(lapply(measures,function(item) masks[[item]] & src[,item]=="extract"),measures)
  restricted<-do.call(rbind,lapply(measures,function(item) {
    keep<-restricted_masks[[item]]
    # Values and lag columns are never regenerated; only primary fitting rows are restricted.
    if (sum(keep)>=3L && valid_columns[[item]])
      fit_landscape(data[keep,,drop=FALSE],item,predictor,covariates,own_lag,lag_suffix,missing="per_outcome",se_method=se_method)
    else data.frame(item=item,n=sum(keep),df=if(valid_columns[[item]]) sum(keep)-2L-length(covariates)-as.integer(own_lag) else NA_integer_,
      b=NA_real_,se=NA_real_,ci_lo=NA_real_,ci_hi=NA_real_,t=NA_real_,p=NA_real_,status="Insufficient source-restricted primary rows",stringsAsFactors=FALSE)
  }))
  rownames(restricted)<-NULL
  add_family<-function(x) { x$bonferroni_universe_k<-length(measures);x$p_bonferroni_universe<-pmin(1,x$p*length(measures));x }
  current<-add_family(primary);restricted<-add_family(restricted)
  count_source<-function(flags,mask,label) sum(flags[mask]==label,na.rm=TRUE)
  summary<-do.call(rbind,lapply(measures,function(item) {
    keep<-masks[[item]];subset<-restricted_masks[[item]];i<-match(item,measures)
    data.frame(item=item,primary_n=current$n[i],
      primary_current_extract_n=count_source(src[,item],keep,"extract"),
      primary_current_carried_n=count_source(src[,item],keep,"carried"),
      primary_own_lag_extract_n=if (own_lag) count_source(lag_src[,item],keep,"extract") else NA_integer_,
      primary_own_lag_carried_n=if (own_lag) count_source(lag_src[,item],keep,"carried") else NA_integer_,
      primary_own_lag_unknown_n=if (own_lag) sum(is.na(lag_src[keep,item])) else NA_integer_,
      restricted_n=restricted$n[i],
      restricted_own_lag_carried_n=if (own_lag) count_source(lag_src[,item],subset,"carried") else NA_integer_,
      restricted_own_lag_unknown_n=if (own_lag) sum(is.na(lag_src[subset,item])) else NA_integer_,
      primary_status=current$status[i],restricted_status=restricted$status[i],stringsAsFactors=FALSE)
  }))
  settings<-list(measures=measures,predictor=predictor,covariates=covariates,own_lag=own_lag,lag_suffix=lag_suffix,
    missing=missing,se_method=se_method,family_k=length(measures),source_rule="current outcome is marked extract, intersected with that outcome's actual primary fitting rows",
    lag_rule="supplied previous-calendar-year lag is unchanged",restricted_missing_policy="outcome-specific subset of primary rows")
  notes<-c(
    "Source flags distinguish extract-derived current outcomes from values inherited from the completed annual file. Carried is not a last-observation-carried-forward algorithm label.",
    "Counts describe usable calendar-year rows and source labels, not effective sample size or independent observations.",
    "Each sensitivity model retains only its own primary rows whose current outcome is marked extract. All predictor, covariate and supplied calendar-year lag values remain unchanged; no lag is rebuilt after filtering.",
    if (own_lag) "Inherited lagged outcome values can remain in this restricted analysis; their counts are reported explicitly." else "This model does not include an outcome own-lag; lag-source counts are not applicable.",
    "The restricted analyses can use different row sets even when the primary fits used common rows. This is an outcome-input sample restriction, not an interpolation, cluster-robust, or serial-correlation-robust uncertainty correction.",
    paste("Uncertainty uses the same",se_method,"estimator as the primary fits. Optional Bonferroni values use all",length(measures),"requested outcomes, including failed fits."),
    "The item-source record does not establish the external predictor or covariate provenance, model validity, conceptual equivalence, or the original study's selection history."
  )
  list(summary=summary,current_landscape=current,restricted_landscape=restricted,settings=settings,method_note=notes)
}

reproduce_input_provenance <- function(analysis_csv,source_csv,settings,primary_csv=NULL) {
  if (is.character(settings) && length(settings)==1L) settings<-jsonlite::read_json(settings,simplifyVector=TRUE)
  if (!is.list(settings)) stop("Provide a settings list or JSON settings path.")
  model<-if (!is.null(settings[["model"]])) settings[["model"]] else settings
  get<-function(key,fallback) if(is.null(model[[key]])) fallback else model[[key]]
  measures<-as.character(unlist(get("measures",NULL),use.names=FALSE))
  if (!length(measures)) stop("Reproduction settings must name the complete requested outcome family.")
  data<-read.csv(analysis_csv,check.names=FALSE,na.strings=c("","NA"))
  source<-read.csv(source_csv,check.names=FALSE)
  primary<-if (!is.null(primary_csv)) read.csv(primary_csv,check.names=FALSE) else NULL
  audit_input_provenance(data,source,measures,as.character(unlist(get("predictor",NULL))),
    covariates=as.character(unlist(get("covariates",character()))),own_lag=get("own_lag",FALSE),lag_suffix=get("lag_suffix","Lag"),
    missing=get("missing","common"),se_method=get("se_method","classical"),primary=primary)
}
