# # # # #
#  Intensity Variablity Analyzer  
#
#  This programme calculates:
#  1) The standard deviation of average syllabic intensity levels *stdevM*;
#  2) The variation coefficient of (or normalized) stdevM *varcoM*;
#  3) The raw pairwise variability index of average syllabic intensity levels *rPVIm*;
#  4) The normalized pairwise variability index of average syllabic intensity levels *nPVIm*;
#  5) The standard deviation of syllable peak intensity levels *stdevP*;
#  6) The variation coefficient of (or normalized) stdevP *varcoP*;
#  7) The raw pairwise variability index of syllable peak intensity levels *rPVIp*;
#  8) The normalized pairwise variability index of syllable peak intensity levels *nPVIp*. 
#
#  Author: Lei He (lei.he@uzh.ch; helei@rocketmail.com), Phonetics Lab, University of Zurich
#
#  Created and debugged: 06.02.2014 - 26.02.2014
#  Independent variables specification added:  02.04.2014
# # # # #


# 1 # GUI to submit parameters for creating and querying the intensity objects and other analysis parameters

form Intensity Variability Analyzer
	comment Please specify analysis parameters:
	   real Minimum_pitch_(Hz) 100
	   real Time_step_(s) 0.0
	   boolean Subtract_mean 1
	comment Please choose the averaging method when extracting the mean intensity:
	   optionmenu Averaging_method 3
	   option energy
	   option sones
	   option dB
	comment Please choose the interpolation method when extracting the peak intensity:
	   optionmenu Interpolation 3
	   option Nearest
	   option Linear
	   option Cubic
	   option Sinc70
	   option Sinc700
	comment Tier for mean intensity analysis:
	   positive Tier_number_mean 6
	   comment Tier for peak intensity analysis:
	   positive Tier_number_peak 7
	   comment Please specify the name of the results table (without spaces):
	   sentence Results_table intensityVariability
	comment Please specify the pause label:
	   word Pause_label sil
	integer Exclude_intervals_from_start_in_mean_tier 2
	integer Exclude_intervals_from_end_in_mean_tier 1
	integer Exclude_intervals_from_start_in_peak_tier 2
	integer Exclude_intervals_from_end_in_peak_tier 0
	comment Please specify the working directory and check to show counter:
	   word Dir /Users/leihe/Documents/tevoid/wavAndTextgrid/
	boolean Show_counter 1
endform

# 2 # GUI to specify independent variables

beginPause: "Independent Variables"
	comment: "Names of the independent variables."
	   word: "Independent variable I", "subjectID"
	   word: "Independent variable II", "sex"
	   word: "Independent variable III", "speechMode"
	   word: "Independent variable IV", "sentence"
	comment: "Location of variable digits"
	   integer: "Variable I starts_at_digit", 1
	   integer: "Number of digits of variable I", 2
	   integer: "Variable II starts at digit", 3
	   integer: "Number of digits of variable_II", 1
	   integer: "Variable III starts at digit", 5
	   integer: "Number of digits of variable III", 4
	   integer: "Variable IV starts at digit", 10
	   integer: "Number of digits of variable IV", 3
endPause: "OK", 1

# 3 # Create strings as file list

do("Create Strings as file list...", "wavFileList", "'dir$'*.wav")
    nFiles = do ("Get number of strings")

# 4 # Create table object

do ("Create Table with column names...", results_table$, nFiles, 
    ..."'independent_variable_I$' 'independent_variable_II$' 'independent_variable_III$' 'independent_variable_IV$' 
    ...stdevM varcoM rPVIm nPVIm stdevP varcoP rPVIp nPVIp")

# 5 # Looping through all the sound files to create intensity objects, query intensity objects, and calculate the metrics.

for iFile from 1 to nFiles
	select Strings wavFileList
	   wavFileName$ = do$ ("Get string...", iFile)       ; wavFileName$ -> name of the .wav sound file  
	   sound = do("Read from file...", "'dir$''wavFileName$'")
	   nameSansExt$ = selected$("Sound")                 ; nameSansExt$ -> the file name without any extension
	   txtgrid = do("Read from file...", "'dir$''nameSansExt$'.TextGrid")
	select txtgrid
	   numOfInM = do ("Get number of intervals...", tier_number_mean)   ;numOfInM -> the number of intervals in the mean tier
	   numOfInP = do ("Get number of intervals...", tier_number_peak)   ;numOfInP -> the number of intervals in the peak tier
	select sound
	   intensity = To Intensity: minimum_pitch, time_step, subtract_mean

  # Defining the position of variable string #
  variable_I$ = mid$("'nameSansExt$'", variable_I_starts_at_digit, number_of_digits_of_variable_I)
  variable_II$ = mid$("'nameSansExt$'", variable_II_starts_at_digit, number_of_digits_of_variable_II)
  variable_III$ = mid$("'nameSansExt$'", variable_III_starts_at_digit, number_of_digits_of_variable_III)
  variable_IV$ = mid$("'nameSansExt$'", variable_IV_starts_at_digit, number_of_digits_of_variable_IV)

  # Initializing some terms for metrics calculation in mean tier #
  number_of_intervals_m = 0          ; number of intervals in mean tier for step-wise addition 
  sum_of_interval_intensity_m = 0    ; sum of interval intensity in mean tier for step-wise addition
  sqrIntensity_m = 0                 ; interval intensity squared in mean tier
  sumOfSqrIntensity_m = 0            ; sum of squared interval intensity in mean tier
  iNumM = 0                          ; index of interval position to calculate PVI in mean tier
  absDiffM = 0                       ; absolute difference between adjacent interval intensity levels in mean tier
  absDiffNorM = 0                    ; normalized absDiffM

	# 5.1 #  Calculation on the mean tier
	for j from (1+exclude_intervals_from_start_in_mean_tier) to (numOfInM-exclude_intervals_from_end_in_mean_tier)	
		select txtgrid
		  startPointM = do ("Get start point...", tier_number_mean, j)       ; start time of interval in mean tier
		  endPointM = do ("Get end point...", tier_number_mean, j)           ; end time of interval in mean tier
		  label$ = do$ ("Get label of interval...", tier_number_mean, j)     ; labels of each interval in mean tier
		select intensity
		  intensityM = do ("Get mean...", startPointM, endPointM, averaging_method$)  ; average interval intensity level in mean tier
		  if label$ <> pause_label$
		  	number_of_intervals_m += 1
		  	sum_of_interval_intensity_m += intensityM
		  	sqrIntensity_m = intensityM ^ 2
		 	sumOfSqrIntensity_m += sqrIntensity_m
		  	intensityM[number_of_intervals_m] = intensityM
		  endif
	endfor

	   # Calculation of stdevM and varcoM #
	   stdevM = sqrt((sumOfSqrIntensity_m - ((sum_of_interval_intensity_m)^2)/number_of_intervals_m)/(number_of_intervals_m-1))
	   meanM = sum_of_interval_intensity_m / number_of_intervals_m
	   varcoM = 100*stdevM / meanM
    
	for iNumM to number_of_intervals_m-1
		db1 = intensityM[iNumM]     ; left interval value of the adjacent pair
		db2 = intensityM[iNumM+1]   ; right interval value of the adjacent pair
		absDiffM += abs(db1-db2)
		absDiffNorM += abs(db1-db2)/(0.5*db1+0.5*db2)
	endfor
           # Calculation of rPVIm and nPVIm #
	   rPVIm = absDiffM/(number_of_intervals_m-1)
	   nPVIm = absDiffNorM*100/(number_of_intervals_m-1)

  # Initializing some terms for metrics calculation in peak tier #
  number_of_intervals_p = 0
  sum_of_interval_intensity_p = 0
  sqrIntensity_p = 0
  sumOfSqrIntensity_p = 0
  iNumP = 0
  absDiffP = 0
  absDiffNorP = 0

	# 5.2 # Calculation of the peak tier 
	for k from (1+exclude_intervals_from_start_in_peak_tier) to (numOfInP-exclude_intervals_from_end_in_peak_tier)
		select txtgrid
		  startPointP = do ("Get start point...", tier_number_peak, k)
		select intensity
		  intensityP = do ("Get value at time...", startPointP, interpolation$)
		  number_of_intervals_p += 1
		  sum_of_interval_intensity_p += intensityP
		  sqrIntensity_p = intensityP ^ 2
		  sumOfSqrIntensity_p += sqrIntensity_p
		  intensityP[number_of_intervals_p] = intensityP
	endfor  

	   # Calculation of stdevP and varcoP #
	   stdevP = sqrt((sumOfSqrIntensity_p - ((sum_of_interval_intensity_p)^2)/number_of_intervals_p)/(number_of_intervals_p-1))
	   meanP = sum_of_interval_intensity_p / number_of_intervals_p
	   varcoP = 100*stdevP / meanP

	for iNumP to number_of_intervals_p-1
		db11 = intensityP [iNumP]
		db22 = intensityP [iNumP+1]
		absDiffP += abs (db11 - db22)
		absDiffNorP += abs (db11 - db22)/(0.5*db11 + 0.5*db22)
	endfor
	   # Calculation of rPVIp and nPVIp #
	   rPVIp = absDiffP/(number_of_intervals_p-1)
	   nPVIp = absDiffNorP*100/(number_of_intervals_p-1)

	# 5.3 # Submitting values to the table object
	select Table 'results_table$'
	   do ("Set string value...", iFile, "'independent_variable_I$'", variable_I$)
	   do ("Set string value...", iFile, "'independent_variable_II$'", variable_II$)
	   do ("Set string value...", iFile, "'independent_variable_III$'", variable_III$)
	   do ("Set string value...", iFile, "'independent_variable_IV$'", variable_IV$)
	   do ("Set numeric value...", iFile, "stdevM", stdevM) 
	   do ("Set numeric value...", iFile, "varcoM", varcoM)
	   do ("Set numeric value...", iFile, "rPVIm", rPVIm)
	   do ("Set numeric value...", iFile, "nPVIm", nPVIm)
	   do ("Set numeric value...", iFile, "stdevP", stdevP) 
	   do ("Set numeric value...", iFile, "varcoP", varcoP)
	   do ("Set numeric value...", iFile, "rPVIp", rPVIp)
	   do ("Set numeric value...", iFile, "nPVIp", nPVIp)

	# Counter #
	if show_counter = 1
		writeInfoLine ("*** Calculation in process ***")
		appendInfoLine(" ")
		appendInfoLine (">>>  ",nameSansExt$)
		appendInfoLine((iFile/nFiles)*100,"% finished")
		appendInfoLine ((1-(iFile/nFiles))*100,"% left")
	endif

# 6 # Tidying up!
select txtgrid
  plus sound
  plus intensity
  Remove
endfor
  
select Strings wavFileList
Remove


### END ###
