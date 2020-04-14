# Setting up LaTeX for use with the csasdown package

csasdown works with TeX Live on Unix or MikTeX on Windows, so you do not need to install [tinytex](https://yihui.org/tinytex/#for-r-users) if you already have one of these; however, you may find tinytex more convenient regardless. tinytex automates the process of downloading necessary packages for you whereas MikTeX often involves manual intervention. If you choose to install tinytex, you may choose to remove TeX Live or MikTeX first to keep things simple.

On Windows, it is important to make sure that any previous installations of MikTeX are *completely* removed before trying to install tinytex. To make sure of this:

1. Uninstall MikTex as you would a normal program on Windows, using the Add/Remove Programs dialog.
2. Navigate to `C:\Users\your_user_name\AppData\Roaming` and check to see if there is a MikTex directory there. If so, delete it.
3. Search your entire `C:\` drive for any files or directories containing "miktex". Delete any that are found. There are usually some remaining in at least `C:\Users\your_user_name\AppData\Local\Temp` and `C:\Documents and Settings\your_user_name\AppData\Local\Temp`.


To install tinytex, run the following code. In Windows, use the Rgui provided with your R installation instead of the RStudio IDE to avoid problems.

```r
install.packages("tinytex")
tinytex::install_tinytex()
```

Once complete, there will be a message like this:

```
Please quit and reopen your R session and IDE (if you are using one, such as RStudio or Emacs)
and check if tinytex:::is_tinytex() is TRUE.
```

Do as it says and your tinytex installation should be successfully installed if you get **TRUE**. You are now ready to knit a csasdown document. The first time you do, it will take about 15 minutes due to tinytex downloading and installing all the required packages, but after the first time it should only take about 8 seconds. The messages when you are knitting that tell you tinytex is installing a new package look like this:

```
tlmgr.pl: package repository http://ctan.mirror.rafal.ca/systems/texlive/tlnet (not verified: gpg unavailable)
[1/1, ??:??/??:??] install: tabu [24k]
running mktexlsr ...
done running mktexlsr.
tlmgr.pl: package log updated: C:/Users/your_user_name/AppData/Roaming/TinyTeX/texmf-var/web2c/tlmgr.log
```
[Back to README](README.md)

