RAW = data/raw
IDS = $(shell seq 1 648)
DIR = $(addprefix $(RAW)/scenario_, $(IDS))
EVALFILES = $(addsuffix /evaluation.rds, $(DIR))

print-% :
	@echo '$*=$($*)'

$(EVALFILES) : code/SimulationScript.R\
	       data/processed/analysisIds.csv\
	       data/processed/analysisIdsInteractions.csv
	$< $@

figures/deviate_linear_08.png figures/deviate_quadratic_08.png figures/deviate_linear_absolute_08.png figures/deviate_quadratic_absoltue_08.png : code/PlotDeviations.R\
	code/helpers/PlotGammas.R\
	code/helpers/PlotDeviationsFunctions.R\
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

figures/rmse_nl_auc.tiff figures/rmse_nl_auc.png : code/NlAucPlots.R\
	code/helpers/CreateManuscriptPlots.R\
	data/processed/rmse.csv
	$<

figures/rmse_nl_n.tiff figures/rmse_nl_n.png : code/NonLinearityNPatientsPlots.R\
	code/helpers/CreateManuscriptPlots.R\
	data/processed/rmse.csv
	$<

figures/rmse_moderate_base.tiff : code/PlotRmse.R\
	code/helpers/CreateManuscriptPlots.R\
	code/helpers/PlotResult.R\
	code/helpers/Absolute.R\
	data/processed/rmse.csv\
	data/processed/analysisIds.csv
	$< moderate 4250 0.75 base

figures/rmse_moderate_sample_size.tiff : code/PlotRmse.R\
	code/helpers/CreateManuscriptPlots.R\
	code/helpers/PlotResult.R\
	code/helpers/Absolute.R\
	data/processed/rmse.csv\
	data/processed/analysisIds.csv
	$< moderate 17000 0.75 sample_size

figures/rmse_moderate_auc.tiff : code/PlotRmse.R\
	code/helpers/CreateManuscriptPlots.R\
	code/helpers/PlotResult.R\
	code/helpers/Absolute.R\
	data/processed/rmse.csv\
	data/processed/analysisIds.csv
	$< moderate 4250 0.85 auc

figures/rmse_absent_base.tiff : code/PlotRmse.R\
	code/helpers/CreateManuscriptPlots.R\
	code/helpers/PlotResult.R\
	code/helpers/Absolute.R\
	data/processed/rmse.csv\
	data/processed/analysisIds.csv
	$< absent 4250 0.75 base

figures/rmse_absent_sample_size.tiff : code/PlotRmse.R\
	code/helpers/CreateManuscriptPlots.R\
	code/helpers/PlotResult.R\
	code/helpers/Absolute.R\
	data/processed/rmse.csv\
	data/processed/analysisIds.csv
	$< absent 17000 0.75 sample_size

figures/rmse_absent_auc.tiff : code/PlotRmse.R\
	code/helpers/CreateManuscriptPlots.R\
	code/helpers/PlotResult.R\
	code/helpers/Absolute.R\
	data/processed/rmse.csv\
	data/processed/analysisIds.csv
	$< absent 4250 0.85 auc

figures/rmse_high_base.tiff : code/PlotRmse.R\
	code/helpers/CreateManuscriptPlots.R\
	code/helpers/PlotResult.R\
	code/helpers/Absolute.R\
	data/processed/rmse.csv\
	data/processed/analysisIds.csv
	$< high 4250 0.75 base

figures/rmse_high_sample_size.tiff : code/PlotRmse.R\
	code/helpers/CreateManuscriptPlots.R\
	code/helpers/PlotResult.R\
	code/helpers/Absolute.R\
	data/processed/rmse.csv\
	data/processed/analysisIds.csv
	$< high 17000 0.75 sample_size

figures/rmse_high_auc.tiff : code/PlotRmse.R\
	code/helpers/CreateManuscriptPlots.R\
	code/helpers/PlotResult.R\
	code/helpers/Absolute.R\
	data/processed/rmse.csv\
	data/processed/analysisIds.csv
	$< high 4250 0.85 auc

figures/calibration_moderate_base.tiff : code/CalibrationBase.R\
	code/helpers/CreateManuscriptPlots.R\
	code/helpers/PlotResult.R\
	data/processed/calibration.csv
	$< moderate 4250 0.75 base

figures/calibration_moderate_sample_size.tiff : code/CalibrationBase.R\
	code/helpers/CreateManuscriptPlots.R\
	code/helpers/PlotResult.R\
	data/processed/calibration.csv
	$< moderate 17000 0.75 sample_size

figures/calibration_moderate_auc.tiff : code/CalibrationBase.R\
	code/helpers/CreateManuscriptPlots.R\
	code/helpers/PlotResult.R\
	data/processed/calibration.csv
	$< moderate 4250 0.85 auc

figures/discrimination_moderate_base.tiff : code/DiscriminationBase.R\
	code/helpers/CreateManuscriptPlots.R\
	code/helpers/PlotResult.R\
	data/processed/discrimination.csv\
	data/processed/analysisIds.csv
	$< moderate 4250 0.75 base

figures/discrimination_moderate_sample_size.tiff : code/DiscriminationBase.R\
	code/helpers/CreateManuscriptPlots.R\
	code/helpers/PlotResult.R\
	data/processed/discrimination.csv\
	data/processed/analysisIds.csv
	$< moderate 17000 0.75 sample_size

figures/discrimination_moderate_auc.tiff : code/DiscriminationBase.R\
	code/helpers/CreateManuscriptPlots.R\
	code/helpers/PlotResult.R\
	data/processed/discrimination.csv\
	data/processed/analysisIds.csv
	$< moderate 4250 0.85 auc

figures/rmse_n_auc.tiff : code/NPatientsAucPlots.R\
	code/helpers/CreateManuscriptPlots.R\
	data/processed/rmse.csv
	$<

figures/rmse_interaction.tiff : code/PlotRmseInteractions.R\
	code/helpers/CreateManuscriptPlots.R\
	code/helpers/PlotResult.R\
	data/processed/discrimination.csv\
	data/processed/analysisIdsInteractions.csv
	$<


figures/gusto.tiff : code/GustoPlot.R\
	data/raw/gusto.rda
	$<

figures/selected_model_adaptive_base.tiff : code/SelectedModelAdaptive.R\
	data/processed/analysisIds.csv\
	data/processed/adaptiveModel.csv
	$< moderate 4250 0.75 base

figures/selected_model_adaptive_sample_size.tiff : code/SelectedModelAdaptive.R\
	data/processed/analysisIds.csv\
	data/processed/adaptiveModel.csv
	$< moderate 17000 0.75 sample_size

figures/selected_model_adaptive_auc.tiff : code/SelectedModelAdaptive.R\
	data/processed/analysisIds.csv\
	data/processed/adaptiveModel.csv
	$< moderate 4250 0.85 auc

data/raw/gusto.rda : code/GetGustoData.sh
	$<

data/processed/analysisIds.csv : code/WriteAnalysisIds.R
	$<

data/processed/rmse.csv data/processed/discrimination.csv data/processed/calibration.csv data/processed/adaptiveModel.csv: code/MergeResults.R
	$<

data/processed/adaptiveSelections.csv : code/CreateAdaptiveSelections.R\
	data/processed/adaptiveModel.csv
	$<

data/processed/rmseDistribution.csv : code/CreateDistribution.R\
	code/helpers/GetMedians.R\
	data/processed/rmse.csv
	$< rmse

data/processed/discriminationDistribution.csv : code/CreateDistribution.R\
	code/helpers/GetMedians.R\
	data/processed/discrimination.csv
	$< discrimination

data/processed/analysisIdsInteractions.csv : code/WriteAnalysisIdsInteractions.R
	$<

data/processed/gustoPerformanceMetrics.csv : code/GustoPerformanceMetrics.R\
	data/raw/gusto.rda
	$<

submission/arxiv.sty : code/GetArxivStyle.sh
	$<

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

submission/manuscript.pdf submission/manuscript.docx : submission/manuscript.rmd\
	submission/arxiv.sty\
	submission/references.bib\
	data/raw/gusto.rda\
	data/processed/adaptiveModel.csv\
	data/processed/gustoPerformanceMetrics.csv\
	data/processed/adaptiveSelections.csv\
	data/processed/rmseDistribution.csv\
	data/processed/discriminationDistribution.csv\
	data/processed/calibration.csv\
	figures/rmse_moderate_base.tiff\
	figures/rmse_moderate_sample_size.tiff\
	figures/rmse_moderate_auc.tiff\
	figures/calibration_moderate_base.tiff\
	figures/discrimination_moderate_base.tiff\
	figures/gusto.tiff
	R -e 'rmarkdown::render("submission/manuscript.rmd", output_format = "all")'

submission/supplement.pdf : submission/supplement.rmd\
	data/raw/gusto.rda\
	data/processed/gustoPerformanceMetrics.csv\
	data/processed/analysisIds.csv\
	figures/selected_model_adaptive_base.tiff\
	figures/selected_model_adaptive_sample_size.tiff\
	figures/selected_model_adaptive_auc.tiff\
	figures/discrimination_moderate_sample_size.tiff\
	figures/discrimination_moderate_auc.tiff\
	figures/calibration_moderate_sample_size.tiff\
	figures/calibration_moderate_auc.tiff\
	figures/rmse_high_base.tiff\
	figures/rmse_high_sample_size.tiff\
	figures/rmse_high_auc.tiff\
	figures/rmse_interaction.tiff
	# figures/deviate_linear_08.png\
	# figures/deviate_quadratic_08.png\
	# figures/deviate_linear_absolute_08.png\
	# figures/deviate_quadratic_absolute_08.png
	R -e 'rmarkdown::render("submission/supplement.rmd", output_format = "all")'

.PHONY:
data : $(EVALFILES)
restore:
	code/Clean.R
clean:
	rm -rf data/raw/scenario_* data/processed/*.csv data/raw/gusto.rda figures/*.png figures/*.tiff; code/Clean.R
