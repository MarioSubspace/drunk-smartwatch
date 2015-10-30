
  README :: OVERVIEW

================================================================================
   GIT SETUP
--------------------------------------------------------------------------------
01] Download and install Git on your system.
02] If you don't have a GitLab profile set up, do so at:
    gitlab.cs.txstate.edu
03] Add your SSH key to your GitLab profile.
    03a] To do this go to: GitLab > Profile Settings > SSH Keys,
         then follow the instructions for generating an SSH key.
    03b] This may be more difficult on Windows, you may need to download and
         install a Unix-like environment like Cygwin, or an SSH client.

================================================================================
   PROJECT DOWNLOAD
--------------------------------------------------------------------------------
01] Create a directory in which you want extract the project directory to.
    01a] I created a "Git" directory in my home directory for Git projects.
02] If you are not in the command-line, open it up and change directory "cd"
    into this directory.
03] Download the project files with the following command:
    git clone git@gitlab.cs.txstate.edu:mag262/drunk-smartwatch.git

================================================================================
   GIT WORKFLOW
--------------------------------------------------------------------------------
01] Before making any changes, make sure to pull the latest changes with:
    git update
02] After making changes you can check the status of whats changed with:
    git status
03] Stage modified and new files for a commit by running:
    git add <filename>
    03a] Unstage files using:
         git reset <filename>
    03b] Delete (actually delete) files using:
         git rm <filename>
         This also works for files that have already been deleted locally, but
         are still in the online repository.
04] Commit staged files and changes with:
    git commit -m "Comments about your commit"
05] Push the changes to the online repository with:
    git push

================================================================================
   PROJECT DIRECTORY DESCRIPTIONS
--------------------------------------------------------------------------------
+ android-app: contains source code for the data collection application.
+ bac-prediction: contains source code and data for the machine learning bits.
+ conference: files submitted to ICSH 2015.
+ manuscript: LaTeX files for the manuscript.

================================================================================
   ANDROID SOURCE CODE SETUP
--------------------------------------------------------------------------------
01] Download and install Android Studio.
02] Choose "Load Existing Project" (or something like that).
    02a] Choose the "android-app" directory.
03] Android Studio should automatically notice that the file is under version
    control. Go ahead and click "Add root" to link the VCS with Android Studio.
04] Make sure to Update before editing and Commit changes after finished.
    04a] Only commit source code, resource files, and AndroidManifest.xml.
         You can select what to commit after clicking Commit. Leave other
         properties files out of it unless there's a non-system-specific
         configuration change you made (e.g., adding library dependencies
         to Gradle).

================================================================================
   R Setup
--------------------------------------------------------------------------------
01] Download and install R ( https://www.r-project.org/ ).
02] Make sure the R binaries are in the system path. Installation should do this
    automatically for Mac, for Windows file should be in the Program Files dir.

================================================================================
   LaTeX Setup
--------------------------------------------------------------------------------
01] Easiest way is to download and use Texmaker to write and compile the paper.
    01a] Note that the standard sequence to get references to show up is:
         pdflatex > bibtex > pdflatex > pdflatex
    01b] Otherwise you could just spam these until it works, the following
         step discusses the command-line, makefile method.
02] I've included a makefile if you want to use the command-line.
    02a] First, make sure the LaTeX binaries are in the system path. You may
         have to download them first if Texmaker isn't installed.
    02b] In a Linux, Mac, or Cygwin terminal, 'cd' to the manuscript directory
         and then run the following:
         'make all' to compile the PDF of the paper, or
         'make clean' to remove unnecessary generated files.
