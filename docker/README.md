# Docker compilation for csasdown

Open a command line and navigate to the docker directories where the `Dockerfile` resides.

1. Compile a `cgrandin/admb` image and push to `cgrandin/admb`. The necessary files are located at [https://github.com/pbs-assess/gfiscam/tree/wsl2/docker]

1. Compile a `cgrandin/csasdown` image and push to `cgrandin/csasdown`. The commands to build and push are:
 - `docker build . --no-cache -t cgrandin/csasdown`
 - `docker push cgrandin/csasdown`

# Files in this directory

1. `Dockerfile` - Main file used to build the Docker image
1. `Makefile` - Used to build the Docker image and push it if you would rather type less
1. `install-base.sh` - A script called from within `install-unx.sh`
1. `install-unx.sh` - A script called to install tinytex on a Unix machine
1. `install_packages.R` - An R script called from the Dockerfile used to install R packages
1. `pkgs-custom.txt` - A list of custom packages installed by `install-base.sh`
1. `tinytex.profile` - A file copied onto the filesystem by `install-base.sh`

`install-base.sh`, `install-unx.sh`, `pkgs-custom.txt`, and `tinytex.profile` are not used in this build process, instead they are downloaded automatically by the web-based script executed in the `Dockerfile`. They are included here as a backup in case those links ever become broken or the files are removed from the internet.
