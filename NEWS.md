# smplot2 0.2.4
* Debugged sm_pointplot(); now it contains the forget argument, which can be used to forget the defaults.

# smplot2 0.2.3.1
* Reduced some redundant codes in sm_put_together(), but the function works as the same as before.

# smplot2 0.2.3
* Defaults of visualization functions are now kept even when users call upon list() for each argument unless users directly replace them with another input value.
* Defaults can be forgotten using the forget argument.

# smplot2 0.2.2.3
* sm_statCorr() also computes R2, not just R.
* sm_slope_mean() is a shortcut of sm_slope for plotting a mean slope chart.
* sm_put_together() accepts character strings as inputs for arguments ylabel, ylabel2, xlabel, xlabel2 and title. These relative text sizes can be adjusted using the labelRatio argument, whose default size is optimized automatically.


# smplot2 0.2.2.2

* hRatio and hRatio2 are now automatically optimized in sm_put_together() unless users supply their separate inputs.
* sm_add_rect(), sm_add_polygon(), sm_add_line() and sm_add_arrow() have been removed because they are redundant.

# smplot2 0.2.2.1

* wRatio and wRatio2 are now automatically optimized in sm_put_together() unless the user supplies their separate inputs.

# smplot2 0.2.2

* tickRatio is now automatically optimized in sm_put_together() unless the user supplies a separate input.

# smplot2 0.2.1

* Plotting outputs have been modularized. 
* Outputs from AUC and slope all have been tibblelized.
* Reduced external dependencies.

# smplot2 0.2.0

* `sm_put_together()` has been debugged.

# smplot2 0.1.0

* Added a `NEWS.md` file to track changes to the package.
