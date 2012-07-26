# Test functionality using testthat library
# 
# Author: Andrie
#------------------------------------------------------------------------------


context("Surveyor class objects")


{
  path <- tempdir()
  latexPath <- file.path(path, "latex")
  dir.create(latexPath, recursive=TRUE, showWarnings=FALSE)
  graphPath <- file.path(latexPath, "graphics")
  dir.create(graphPath, recursive=TRUE, showWarnings=FALSE)
  
  
  
  q_data <- data.frame(
  		Q1=c("Yes", "No", "Yes", "Yes"),
  		Q4_1 = c(1, 2, 1, 2), 
  		Q4_3=c(3, 4, 4, 3), 
  		Q4_2=c(5, 5, 6, 6), 
      crossbreak=factor(c("A", "A", "B", "B")), 
      crossbreak2=factor(c("D", "E", "D", "E")),
      weight=c(0.9, 1.1, 0.8, 1.2)
  )
  q_text <- c("Question 1", 
  		"Question 4: red", "Question 4: blue", "Question 4: green", 
  		"crossbreak",
  		"crossbreak2",
  		"weight")
  names(q_text) <- names(q_data)
  
  names_cqrw <- c("cbreak", "question", "response", "weight")
  
  varlabels(q_data) <- q_text
  q_data <- as.surveydata(q_data, renameVarlabels=FALSE)
  s <- as.surveyor(q_data, crossbreak=list(breaks=q_data$crossbreak), q_data$weight)
  
}






#------------------------------------------------------------------------------


test_that("Surveyor objects are defined properly", {
			
			expect_is(s, "surveyor")
			expect_is(s$sdata, "surveydata")
			expect_equal(s$sdata, q_data)
      expect_equal(unlist(unname(s$crossbreak)), q_data$crossbreak)
      expect_equal(s$weight, q_data$weight)
      expect_equal(is.surveyor(s), TRUE)
      expect_equal(is.surveyor(q_data), FALSE)
			#expect_that(as.surveyor(q_data), throws_error())
			#expect_that(surveyorDefaults(pathLatex=file.path("random")), throws_error())
			#expect_that(surveyorDefaults(path_graph=file.path("random")), throws_error())
      
			
		})


