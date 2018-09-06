library(workflows)

indir <- file.path('tests', 'input')
outdir <- file.path('tests', 'output', 'sge')

input_filename <- file.path(indir, 'input-df.csv')

input_object <- structure(list(df=readr::read_csv(input_filename), meta=input_filename), class='arbitraryObject')

w <- newWorkflow(input_object, working_directory=outdir, checkpoint=TRUE)

ws <- scatter(w, elements='df', by='chunk_factor')

ws <- delay(ws, function(x) {
  x$means <- data.frame(mean(x$df$vals))
  x
})

ws <- delay(ws, function(x) {
  x$medians <- data.frame(median(x$df$vals))
  x
})

w2  <- execute(ws, locality='sge', submit_script=file.path('tests', 'input', 'sge-template.sh'))
w2g <- gather(w2, c('df', 'means', 'medians'))

write_workflow(w2g, file.path(outdir, 'output-workflow.rds'))
wl <- load_workflow(file.path(outdir, 'output-workflow.rds'))

wfchk <- newWorkflow(working_directory=outdir, load_checkpoint='latest')
wfchk.g <- gather(wfchk, c('df', 'means', 'medians'))
