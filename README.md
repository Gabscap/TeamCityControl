# TeamCityControl

TeamCityControl downloads Artifacts from [TeamCity CI](https://www.jetbrains.com/teamcity/).

## Installation
Install [Stack](https://www.haskellstack.org/), then
```
stack build
```

## Usage

```
tccontrol

Usage: tccontrol [-c|--config PATH] [-v|--verbose] COMMAND

Available options:
  -h,--help                Show this help text
  -c,--config PATH         Path to config yml file
  -v,--verbose             Verbose mode

Available commands:
  list                     List all projects
  ls                       List all projects
  download                 Download project
  dl                       Download project
  build                    Build project
```
Use `tccontrol SUBCOMMAND -h` for further information.

This command sets up autocompletion
```
tccontrol --bash-completion-script $(which tccontrol) > /path/to/bash_completion.d/tccontrol
```
`/path/to/bash_completion.d/` is distribution dependent, but usually `/etc/bash_completion.d/`.

## Config

This program searches for configs in `XDG_CONFIG_DIR/TCControl/config.yml` (typically `$HOME/.config/TCControl/config.yml`).
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
