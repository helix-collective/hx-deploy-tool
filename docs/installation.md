# Building

Binary releases are published on the project [release page][releases]. 

Alternatively, you can build from source. The tool is written in haskell, and
requires the [stack build tool][stack] to be installed.

Then to build a native executable, run in the repository root directory:

```
stack setup
stack build
```

Alternatively, stack can build under docker. This is useful if you wish
to build a linux executable suitable for deployment. This script

```
./scripts/docker-build.sh
```

leaves a compressed executable at `/tmp/hx-deploy-tool.x86_64-linux.gz`


# Installation

`hx-deploy-tool` is a standalone executable, which loads a single configuration
file on startup. But default this is expected at `../etc/hx-deploy-tool.json`
relative the the installed executable, although the config file path can be
overidden by setting the `HX_DEPLOY_CONFIG` environment variable.

The configuration file must be json formatted, and be consistent with the
[ToolConfig][toolconfig-adl] ADL type (ADL is the [Algebraic Data Language][adl]).
[An example configuration file can be found at: ](docs/example_deploy.json)

Additionally, aws credentials must be supplied and will be expected at either:
$HOME/.aws/credentials file
standard environment variables
EC2 instance metadata


[releases]:https://github.com/helix-collective/hx-deploy-tool/releases
[stack]:https://docs.haskellstack.org/en/stable/README/ 
[toolconfig-adl]:https://github.com/helix-collective/hx-deploy-tool/blob/master/adl/config.adl#L11
[adl]:https://github.com/timbod7/adl


