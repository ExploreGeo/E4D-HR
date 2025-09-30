# E4D_HR_User_Guide.pdf
 - Added in moving average info for .inv files. Latex now auto compiles.

# install.py
 - Fixed an issue where the user was always prompted to reinstall all modules even if they only selected one
 - Added an uninstall option to cleanly uninstall.
 - Added optional argument parsing to allow bypassing the menu
 - Cleaner formatting
 - Added a debugging install mode
 
# BuildMesh.f90, Master.f90
 - tetgen output is now saved to tetgen_output.log
 - triangle output is now saved to triangle_output.log
 - All mesh files are now saved to a folder called mesh. This includes .trn
 - If maxvol is specified with capital E e.g. 1E12, no issues.
 
# E4d.f90, READ_INP.f90, SLAVE.f90, MASTER.f90
 - Consolidated the dc.inv and ip.inv files into one e4d.inp file.
 - E4D can now run multiple times from one call. It will generate the mesh and do the inversion.
 - Still won't work if you try multiple inversions, although it is theoretically possible.

# Report.f90
 - Inversion settings formatting cleaned.
