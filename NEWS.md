# csasdown 0.0.0.9000

- Adapted huskydown for CSAS Res Docs

# huskydown 0.0.4

- new version of Pandoc
- Source-Code-Pro changed location on GitHub, so changed method of getting them
- tinytex burst onto the scene, simplying LaTeX installs.

# huskydown 0.0.4

- CI on OSX and Linux, collect artefacts and issue PDF
- use this to ensure the builds succeed and we get artefacts:
git commit -am 0.0.4
git tag -m 0.0.4 0.0.4
git push origin --tags
- or is this really necessary? no we have a build that produced artefacts without tags
- do check the GITHUB_TOKEN on Travis-CI since I regenerate this often

# huskydown 0.0.3

- Add example output files

# huskydown 0.0.2

- Add continuous integration via Docker container

# huskydown 0.0.1

- Replaced Reed College templates with UW templates, tested and debugged.
- PDF version working for UW template

# thesisdown 0.0.2

- Changed default filename (in directions) to be `index` instead of `skeleton` to match with gitbook requirements
- Added preliminary files `00--prelim.Rmd` and changed `abstract.Rmd` to `00-abstract.Rmd` to match
with renaming of files so that `index.Rmd` always comes last alphabetically
- Removed _book and _bookdown_files directories since they are stored at <https://github.com/ismayc/thesisdown_book/tree/gh-pages>

# thesisdown 0.0.1

- Initial release
- PDF version working
