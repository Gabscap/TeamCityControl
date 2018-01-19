# TeamCityDownloader

TeamCityDownloader downloads Artifacts from [TeamCity CI](https://www.jetbrains.com/teamcity/).

## Installation
Install [Stack](https://www.haskellstack.org/), then
```
stack build
```

## Usage

```
TeamCityDownloader

Usage: TeamCityDownloader [-c|--config PATH] [-d|--debug] PROJECT [BUILD]

Available options:
  -h,--help                Show this help text
  -c,--config PATH         Path to config yml file
  -d,--debug               Debug mode
  PROJECT                  Name of project
  BUILD                    Build number

```

This command sets up autocompletion
```
TeamCityDownloader --bash-completion-script $(which TeamCityDownloader) > /path/to/bash_completion.d/TeamCityDownloader
```
`/path/to/bash_completion.d/` is distribution dependent, but usually `/etc/bash_completion.d/`.

## Config

This program searches for configs in `XDG_CONFIG_DIR/TCD/config.yml` (typically `$HOME/.config/TCD/config.yml`).
```
tcUrl: https://teamcityUrl/
user: username
pass: password
targetDir: /home/user/artifacts/
excepts:
  ExceptionalProject: /home/user/ExceptionalProject/
```

* *tcUrl* URL of TeamCity CI
* *user* Username. This user must have read access (The default role "Project viewer" is fine)
* *pass* Password
* *targetDir* Default download directory for artifacts
* *excepts* A list of download directory exceptions
