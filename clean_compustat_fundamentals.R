#!/usr/bin/env Rscript
start_time <- Sys.time()
###################################################################
###################################################################
meta.file="clean_compustat_fundamentals.R"
meta.author="Steffen Nauhaus"
meta.email="steffen.nauhaus@unige.ch"

# Function to clean compustat fundamentals files

clean_compustat_fundamentals <- function(DTC) {
	
	# Compustat column names and clean names
	cols_firm <- c(

		# ID variabless
		"Firm" =  "gvkey",
		"CUSIP" =  "cusip",
		"Date" =  "datadate",
		"Year" =  "fyear",
		"Name" =  "conm",
		"SIC" =  "sic",
		"Internat" = "idbflag",
		"Description" = "busdesc",
		"Country" = "loc",
		"URL" = "weburl",
	  
		# Value variables
		"Assets" =  "at", # Assets - Total
		"Capex" =  "capx", # Capital Expenditures
		"CashFlow" =  "oancf", # Operating Activities Net Cash Flow
		"CurrentAssets" =  "act", # Current Assets - Total
		"CurrentLiabilities" =  "lct", # Current Liabilities - Total
		"Debt" = "dt", # Total Debt Including Current  
		"EBIT" = "ebit", # Earnings Before Interest And Taxes
		"EBITDA" = "ebitda", # Earnings Before Interest
		"Equity" =  "ceq", # Common/Ordinary Equity - Total
		"InterestExp" = "xint", # Interest And Related Expense - Total
		"Liabilities" = "lt", # Liabilities - Total --> compare w/ Debt
		"NetIncome" =  "ni", # Net Income (Loss)
		"OperatingIncome" = "oiadp", # Operating Income After Depreciation
		"Price" = "prcc_c", # Price Close - Annual - Calendar
		"RD" =  "xrd", # Research And Development Expense, old name
		"Sales" =  "sale", # Sales/Turnover (Net)
		"Shares" =  "csho", # Common Shares Outstanding, old name
		"Taxes" = "txdb", # Deferred Taxes (Balance Sheet)
		"WorkingCapital" =  "wcap" # Working Capital (Balance Sheet)

	)
	

	# Keep only specified columns
	DTC <- DTC[, .SD, .SDcols=cols_firm]


	# Change variable names
	setnames(DTC, cols_firm, names(cols_firm))
	

	# Adjust value column type to numeric
	fv <- grep("\\bAssets", names(cols_firm)) # first value column (Assets)
	val_cols <- colnames(DTC)[fv:length(colnames(DTC))]
	DTC[, c(val_cols) := lapply(.SD, as.numeric), .SDcols = val_cols]


	# Format datadate
	DTC[, Date := as.character(ymd(Date))]


	# # Replace erroenous observations by NA
	# # Value column is negative due to accountings, which is theoretically impossible for some of them. This can be the case for: at, ceq, csho, prcc_c, sale, teq,
	DTC[Assets < 0, Assets := NA]
	DTC[Shares < 0, Shares := NA]
	# DTC[Sales < 0, Sales := NA]
	# DTC[RD < 0, RD := NA]


	# Make firm names unique (in case of duplicates)
	DTC[, Name := unique(Name)[1], by=Firm]
	
	
	## Fix data date
	# Compustat data has many duplicates because they store data adjustments. datadate (Date) is the actual date of the observation, srcdate (sDate) when it was released. The largest srcdate (i.e., the most recent) for each fiscal year (fyear) identifies the most recent data point, which is the one we select. I solve this problem by merging.
	DTC <- merge(
		DTC[, .("Date" = max(Date)), by = list(Firm, Year)], # Latest Year only
	    DTC, # Full table
	    by=c("Date", "Firm", "Year"), all.x=TRUE
	)
		
	
	# Add number of observations per firm
	DTC[, N := .N, keyby=Firm]
	
	
	# Return clean table
	return(DTC)
}