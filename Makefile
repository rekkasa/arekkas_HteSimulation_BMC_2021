RAW = data/raw
IDS = $(shell seq 1 66)
DIR = $(addprefix $(RAW)/scenario_, $(IDS))
EVALFILES = $(addsuffix /evaluation.rds, $(DIR))

print-% :
	@echo '$*=$($*)'


$(EVALFILES) : code/SimulationScript.R\
	       data/processed/analysisIds.csv\
	       data/processed/analysisIdsInteractions.csv
	$< $@


extras/protocol/protocol.pdf : extras/protocol/protocol.rmd\
		               extras/protocol/references.bib\
			       extras/protocol/jamia.csl\
	                       data/processed/analysisIds.csv\
	                       data/processed/analysisIdsInteractions.csv\
	                       figures/deviate_linear_08.png\
                               figures/deviate_quadratic_08.png
	R -e 'rmarkdown::render("extras/protocol/protocol.rmd", output_format = "all")'

extras/outline/outline.pdf : extras/outline/outline.rmd\
			     data/raw/scenario_1/evaluation.rds
	R -e 'rmarkdown::render("extras/outline/outline.rmd", output_format = "all")'


figures/deviate_linear_08.png figures/deviate_quadratic_08.png figures/deviate_linear_absolute_08.png figures/deviate_quadratic_absoltue_08.png : code/PlotDeviations.R\
                                                                                                                                                  code/helpers/PlotGammas.R\
																		  data/processed/analysisIds.csv\
																		  data/processed/analysisIdsInteractions.csv
	$<

figures/rmse_constant.png figures/rmse_constant.tiff : code/RmseConstant.R\
                                                       code/helpers/CreateManuscriptPlots.R\
						       data/processed/rmse.csv
	$<

figures/discrimination_interactions.png figures/discrimination_interactions.tiff : code/DiscriminationInteractionPlots.R\
                                                                                   code/helpers/CreateManuscriptPlots.R\
						                                   data/processed/discrimination.csv
	$<

figures/calibration_interactions.png :code/CalibrationInteractionPlots.R\
                                      code/helpers/CreateManuscriptPlots.R\
                                      data/processed/discrimination.csv
	$<


figures/rmse_interactions.png figures/rmse_interactions.tiff : code/InteractionPlots.R\
                                                               code/helpers/CreateManuscriptPlots.R\
						               data/processed/rmse.csv
	$<

figures/deviationsManuscript.png : code/PlotDeviationsManuscript.R\
                                   code/helpers/PlotGammas.R
	$<

figures/rmse_nl_auc.tiff figures/rmse_nl_auc.png : code/NlAucPlots.R\
		                                   code/helpers/CreateManuscriptPlots.R\
					           data/processed/rmse.csv
	$<

figures/rmse_nl_n.tiff figures/rmse_nl_n.png : code/NonLinearityNPatientsPlots.R\
		                               code/helpers/CreateManuscriptPlots.R\
					       data/processed/rmse.csv
	$<

figures/rmse_base.tiff figures/rmse_base.png : code/RmseBase.R\
		                               code/helpers/CreateManuscriptPlots.R\
					       data/processed/rmse.csv
	$<

figures/rmse_sample_size.tiff figures/rmse_sample_size.png : code/RmseSampleSize.R\
		                                             code/helpers/CreateManuscriptPlots.R\
						             data/processed/rmse.csv
	$<

figures/rmse_auc.tiff figures/rmse_auc.png : code/RmseAuc.R\
		                             code/helpers/CreateManuscriptPlots.R\
					     data/processed/rmse.csv
	$<

figures/calibration_base.tiff figures/calibration_base.png : code/CalibrationBase.R\
		                                             code/helpers/CreateManuscriptPlots.R\
							     data/processed/calibration.csv
	$<

figures/discrimination_base.tiff figures/discrimination_base.png : code/DiscriminationBase.R\
		                                                   code/helpers/CreateManuscriptPlots.R\
								   data/processed/discrimination.csv
	$<

figures/rmse_n_auc.tiff figures/rmse_n_auc.png : code/NPatientsAucPlots.R\
		                                 code/helpers/CreateManuscriptPlots.R\
						 data/processed/rmse.csv
	$<

figures/gusto.tiff figures/gusto.png : code/GustoPlot.R\
                                       data/raw/gusto.rda
	$<

data/raw/gusto.rda : code/GetGustoData.sh
	$<

data/processed/analysisIds.csv : code/WriteAnalysisIds.R
	$<


data/processed/rmse.csv data/processed/discrimination.csv data/processed/calibration.csv : code/MergeResults.R\
	$<


data/processed/analysisIdsInteractions.csv : code/WriteAnalysisIdsInteractions.R
	$<

submission/arxiv.sty : code/GetArxivStyle.sh
	$<


submission/manuscript.pdf submission/manuscript.docx : submission/manuscript.rmd\
	                                               submission/arxiv.sty\
                                                       submission/references.bib\
                                                       data/processed/rmse.csv\
                                                       data/processed/discrimination.csv\
                                                       data/processed/calibration.csv\
                                                       figures/rmse_base.png\
                                                       figures/rmse_auc.png\
                                                       figures/rmse_sample_size.png\
                                                       figures/calibration_base.png\
                                                       figures/discrimination_base.png\
                                                       figures/gusto.png
	R -e 'rmarkdown::render("submission/manuscript.rmd", output_format = "all")'

submission/supplement.pdf : submission/supplement.rmd\
	                    data/raw/gusto.rda\
	                    figures/deviate_linear_08.png\
			    figures/deviate_quadratic_08.png\
			    figures/deviate_linear_absolute_08.png\
			    figures/deviate_quadratic_absolute_08.png\
			    figures/rmse_interactions.png\
			    figures/discrimination_interactions.png\
			    figures/calibration_interactions.png
	R -e 'rmarkdown::render("submission/supplement.rmd", output_format = "all")'

.PHONY:
data : $(EVALFILES)
clean:
	rm -rf data/raw/scenario_* data/processed/*.csv data/raw/gusto.rda figures/*.png figures/*.tiff; code/Clean.R
