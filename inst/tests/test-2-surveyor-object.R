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

#------------------------------------------------------------------------------


test_that("Subset returns correct object", {
      
      ss <- s
      
      x1 <- subset(ss, subset=Q1=="Yes")
      x2 <- subset(ss, subset=Q1=="No")
      x3 <- subset(ss, subset=Q1==NA)
      x4 <- subset(ss)
      x4
      
      expect_is(x1, "surveyor")
      expect_is(x2, "surveyor")
      expect_is(x3, "surveyor")
      expect_is(x4, "surveyor")
      
      expect_equal(x1$sdata, with(s$sdata, s$sdata[Q1=="Yes", ]))
      expect_equal(x2$sdata, with(s$sdata, s$sdata[Q1=="No", ]))
      expect_equal(x3$sdata, with(s$sdata, s$sdata[Q1==NA, ]))
      expect_equal(x4, ss)
      
      expect_equal(x1$weight, with(s$sdata, s$sdata$weight[Q1=="Yes"]))
      expect_equal(x2$weight, with(s$sdata, s$sdata$weight[Q1=="No"]))
      expect_equal(x3$weight, with(s$sdata, s$sdata$weight[Q1==NA]))

      expect_equal(x1$crossbreak, list(breaks=with(s$sdata, s$sdata$crossbreak[Q1=="Yes"])))
      expect_equal(x2$crossbreak, list(breaks=with(s$sdata, s$sdata$crossbreak[Q1=="No"])))
      expect_equal(x3$crossbreak, list(breaks=with(s$sdata, s$sdata$crossbreak[Q1==NA])))
      
    })

