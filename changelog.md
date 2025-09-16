# E4D_HR_User_Guide.pdf
 - Added in moving average info for .inv files. Latex now auto compiles.

# install.py
 - Fixed an issue where the user was always prompted to reinstall all modules even if they only selected one
 - Added an uninstall option to cleanly uninstall.
 - Added optional argument parsing to allow bypassing the menu
 - Cleaner formatting
 
# BuildMesh.f90, Master.f90
 - tetgen output is now saved to tetgen_output.log
 - triangle output is now saved to triangle_output.log
 - All mesh files are now saved to a folder called mesh. This includes .trn
