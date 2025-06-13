# wnv-ss-wkly_report

Current revision to make weekly report scripts be able to run independently of the the config file.
Both 1_datasheet_clean and 2_pcr_platemap_read_clean.R can now function this way.
Added 0_check_load_data_to googledrive data using a function fun_gsheet_pull_prompt

https://docs.google.com/document/d/1gUdxBmV-fIB8R1mVgvBGPy5UIVws_OQNCkH_ia1f8vQ/edit?tab=t.0

V2
Setup:
Config
1. run config_weekly.R with desired arguments to configure your weekly run parameters
    a. Includes input directory to allow for more flexible testing 
    b. Week Filter and Year filter to ensure that the correct files have been uploaded
    c. thresholds for calling positives
2. config settings are saved to a dynamically named file to the config/config_weekly_settings folder
3. The next chunk in the pipeline will read in the latest config file
converts scripts that ran pipeline to functions 
