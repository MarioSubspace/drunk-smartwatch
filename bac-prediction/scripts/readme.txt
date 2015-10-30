
  README :: SCRIPTS

================================================================================
   HOW TO RUN R SCRIPTS
--------------------------------------------------------------------------------
+ These steps are executed in the command-line.

--------------------------------------------------------------------------------
   BATCH MODE
--------------------------------------------------------------------------------
01] Rscript <script.r> <arguments>

--------------------------------------------------------------------------------
   INTERACTIVE MODE
--------------------------------------------------------------------------------
01] R
02] source("script.r")

================================================================================
   SCRIPT DESCRIPTIONS
--------------------------------------------------------------------------------
+ To clean up data, run in order:
  01] concatenate.r - if there are multiple files from same session.
  02] make_complete.r - removes rows with empty data.
  03] interpolate.r - interpolates the BAC from the 25 min interval updates.
+ functions.r - contains all the experiment functions.
+ other files - artifacts, no longer in use.
