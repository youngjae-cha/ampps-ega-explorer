# Independent roundtrip QA of the real server's integrated annual-input export.
# Writes generated artifacts only below qa/input_export; historical inputs stay read-only.
args<-commandArgs(trailingOnly=FALSE)
script<-sub("^--file=","",args[grepl("^--file=",args)][1L])
app_dir<-normalizePath(file.path(dirname(script),".."),mustWork=TRUE)
qa_dir<-file.path(app_dir,"qa","input_export");dir.create(qa_dir,recursive=TRUE,showWarnings=FALSE)
run_dir<-tempfile("run_",tmpdir=qa_dir);dir.create(run_dir)
notes<-character()
assert<-function(x,label) { if(!isTRUE(x)) stop(paste("FAIL:",label));notes<<-c(notes,paste("PASS:",label));cat(tail(notes,1),"\n") }
paths<-file.path(app_dir,c("app.R","R/model.R","R/export.R","R/provenance.R","R/ega.R","data/gss_year.csv","data/gss_year_cell_source.csv"))
before<-tools::md5sum(paths)
old<-getwd();setwd(app_dir)
app_env<-new.env(parent=globalenv());sys.source("app.R",envir=app_env,chdir=TRUE)
captured<-new.env(parent=emptyenv())
shiny::testServer(app_env$server,{
  session$setInputs(source_mode="demo",focal=c("Happy","Trust","Fair"),boundary="community",
    result_scope="local",plot_metric="t",multiplicity=FALSE,question="GSS worked example",
    population="GSS example",analysis_unit="Year",network_unit="Respondent",rationale="Example",
    timing="Retrospective reconstruction",record="",interpretation="",annotation_focal="Happy")
  captured$demo<-payload()
  captured$demo_report<-report_text()
  # Mimic a browser reflecting server-side source-reset controls.
  session$setInputs(source_mode="upload",predictor="X",candidates=sprintf("Item%02d",1:12),
    covariates=character(),own_lag=FALSE,lag_suffix="Lag",missing="common",se_method="classical",
    min_n=10,max_missing=.2,bootstrap=FALSE,boot_iter="100",seed=20260907,
    question="Software fixture",population="Simulated rows",analysis_unit="Synthetic row",
    network_unit="Synthetic row",rationale="Software test",timing="Not documented here",record="",interpretation="")
  path<-file.path(app_dir,"examples/upload_example.csv")
  session$setInputs(data_csv=data.frame(name="upload_example.csv",size=unname(file.info(path)$size),type="text/csv",datapath=path,stringsAsFactors=FALSE))
  session$setInputs(predictor="X",candidates=sprintf("Item%02d",1:12),covariates=character(),
    focal=c("Item01","Item02"),run=1L,
    question="Software fixture",population="Simulated rows",analysis_unit="Synthetic row",
    network_unit="Synthetic row",rationale="Software test",timing="Not documented here",record="",interpretation="")
  if(!is.null(rv$error)) stop(rv$error)
  captured$upload<-payload()
  captured$upload_report<-report_text()
  captured$upload_source<-rv$settings$source
})
assert(!is.null(captured$demo[["input_provenance"]]),"real demo server payload includes input-provenance object")
assert(is.null(captured$upload[["input_provenance"]]),"real user-upload payload has no GSS input-provenance object")
assert(identical(captured$upload_source,"upload"),"upload fixture ran as genuine uploaded-data analysis")
assert(!any(grepl("Annual input sensitivity|HAPPY|Happy|[.]140196|31 usable|27 usable",captured$upload_report)),"user-upload reporting text contains no inherited GSS sensitivity narrative or numbers")

demo_zip<-file.path(run_dir,"demo_default.zip")
app_env$export_bundle(demo_zip,captured$demo,setNames(file.path(app_dir,"data/gss_year.csv"),"analysis_input.csv"))
demo_entries<-unzip(demo_zip,list=TRUE)$Name
writeLines(demo_entries,file.path(run_dir,"demo_archive_entries.txt"))
required<-c("input_source_counts.csv","input_sensitivity_supplied.csv","input_sensitivity_extract.csv",
  "input_sensitivity_notes.txt","R/provenance.R","reproduce_input_sensitivity.R",
  "inputs/analysis_input.csv","provenance/gss_year_cell_source.csv")
assert(all(required %in% demo_entries),"demo ZIP includes complete sensitivity results, source labels, module and execution script")
notes<-c(notes,paste("OBSERVED: demo default ZIP contains",length(demo_entries),"file entries. Required content is checked individually."))
cat(tail(notes,1),"\n")
assert(!any(startsWith(demo_entries,"/")) && !any(grepl("(^|/)[.][.](/|$)",demo_entries)),"archive topology contains only relative paths without parent traversal")
extract<-file.path(run_dir,"demo_unzipped");dir.create(extract);unzip(demo_zip,exdir=extract)
manifest_lines<-readLines(file.path(extract,"MANIFEST_MD5.txt"))
manifest_paths<-substring(manifest_lines,34L)
manifest_hashes<-substring(manifest_lines,1L,32L)
assert(all(file.exists(file.path(extract,manifest_paths))) && identical(unname(tools::md5sum(file.path(extract,manifest_paths))),manifest_hashes),"all archived manifest file hashes validate after extraction")
settings<-jsonlite::read_json(file.path(extract,"settings.json"),simplifyVector=TRUE)
assert(!is.null(settings$input_sensitivity) && identical(settings$input_sensitivity$family_k,44L),"demo settings retain exact44-outcome sensitivity policy")
ref_counts<-read.csv(file.path(extract,"input_source_counts.csv"),check.names=FALSE)
ref_supplied<-read.csv(file.path(extract,"input_sensitivity_supplied.csv"),check.names=FALSE)
ref_extract<-read.csv(file.path(extract,"input_sensitivity_extract.csv"),check.names=FALSE)
fi<-match(c("Happy","Trust","Fair"),ref_counts$item)
assert(all(ref_counts$primary_n==46L) && identical(ref_counts$restricted_n[fi],c(31L,27L,27L)),"exported source counts retain46 versus31/27/27")
assert(identical(ref_counts$restricted_own_lag_carried_n[fi],c(15L,18L,18L)),"exported restricted fits disclose15/18/18 inherited lags")
hi<-match("Happy",ref_extract$item)
assert(abs(ref_extract$p_bonferroni_universe[hi]-.140196802594528)<1e-9,"exported restricted HAPPY adjusted p=.1401968")
setwd(extract)
execution<-system2(file.path(R.home("bin"),"Rscript"),"reproduce_input_sensitivity.R",stdout=TRUE,stderr=TRUE)
setwd(app_dir)
writeLines(execution,file.path(run_dir,"reproduction_stdout.txt"))
assert(is.null(attr(execution,"status")),"actual exported reproduce_input_sensitivity.R executes successfully")
reproduced_counts<-read.csv(file.path(extract,"input_source_counts_reproduced.csv"),check.names=FALSE)
reproduced<-read.csv(file.path(extract,"input_sensitivity_extract_reproduced.csv"),check.names=FALSE)
assert(identical(ref_counts,reproduced_counts),"all44 exported source-count rows reproduce exactly")
fields<-c("n","df","b","se","ci_lo","ci_hi","t","p","p_bonferroni_universe")
errors<-vapply(fields,function(nm) max(abs(reproduced[[nm]]-ref_extract[[nm]]),na.rm=TRUE),numeric(1))
assert(max(errors)<1e-10 && nrow(reproduced)==44L && identical(reproduced$item,ref_extract$item),"all44 restricted estimates/SE/CI/t/p/adjusted-p reproduce")
write.csv(data.frame(field=names(errors),max_abs_diff=unname(errors)),file.path(run_dir,"roundtrip_numeric_differences.csv"),row.names=FALSE)
primary_fields<-c("n","df","b","se","ci_lo","ci_hi","t","p")
assert(isTRUE(all.equal(ref_supplied[primary_fields],captured$demo$landscape[primary_fields],tolerance=1e-12,check.attributes=FALSE)),"supplied sensitivity landscape equals unchanged actual primary fits")

upload_zip<-file.path(run_dir,"upload_default.zip")
app_env$export_bundle(upload_zip,captured$upload)
upload_entries<-unzip(upload_zip,list=TRUE)$Name
writeLines(upload_entries,file.path(run_dir,"upload_archive_entries.txt"))
assert(!any(grepl("^inputs/|^provenance/|^input_source|^input_sensitivity|^reproduce_input_sensitivity",upload_entries)),"default upload ZIP includes neither raw rows nor GSS-specific sensitivity artifacts")
ue<-file.path(run_dir,"upload_unzipped");dir.create(ue);unzip(upload_zip,exdir=ue)
us<-jsonlite::read_json(file.path(ue,"settings.json"),simplifyVector=TRUE)
assert(is.null(us$input_sensitivity) && identical(us$network$source,"computed"),"upload settings contain no GSS sensitivity policy and retain computed network provenance")
ut<-c(readLines(file.path(ue,"reporting_text.txt"),warn=FALSE),readLines(file.path(ue,"report.html"),warn=FALSE))
assert(!any(grepl("Annual input sensitivity|HAPPY|Happy|[.]140196|31 usable|27 usable",ut)),"upload text/HTML have no GSS sensitivity values or narrative")
after<-tools::md5sum(paths)
assert(identical(before,after),"app, modules and historical GSS inputs unchanged during read-only export QA")
write.csv(data.frame(path=names(before),md5=unname(before),unchanged=unname(before)==unname(after)),file.path(run_dir,"source_manifest.csv"),row.names=FALSE)
writeLines(c(notes,"ALL INPUT-EXPORT TESTS PASSED"),file.path(run_dir,"QA_LOG.txt"))
writeLines(c(paste("Latest run:",run_dir),paste("Completed:",format(Sys.time(),tz="UTC",usetz=TRUE)),"PASS"),file.path(qa_dir,"LATEST.txt"))
setwd(old)
cat("ALL INPUT-EXPORT TESTS PASSED\nArtifacts:",run_dir,"\n")
