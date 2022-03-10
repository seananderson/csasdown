# Docker compilation for csasdown

Open an Ubuntu session (WSL2) and navigate to the docker directories where the `Dockerfile` resides.

1. Compile a `cgrandin/admb` image and push to `cgrandin/admb`. The necessary files are located at [https://github.com/pbs-assess/gfiscam/tree/wsl2/docker]

1. Compile a `cgrandin/csasdown` image and push to `cgrandin/csasdown`. The commands to build and push are:
 - `docker build . --no-cache -t cgrandin/csasdown`
 - `docker push cgrandin/csasdown`
