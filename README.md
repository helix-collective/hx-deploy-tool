# Overview

A small tool to automate docker-compose deployments. It pulls config
and release zip files from S3, unpacks into versioned directories,
and runs commands to stop and start them as required.

```
$ hx-deploy-tool
Usage:
  hx-deploy-tool help
  hx-deploy-tool fetch-context [--retry]
  hx-deploy-tool list-releases
  hx-deploy-tool unpack <release> <todir>
  hx-deploy-tool select <release>
  hx-deploy-tool show-log
  hx-deploy-tool aws-docker-login-cmd

The config file is read from the file specified with HX_DEPLOY_CONFIG.
It defaults to ../etc/hx-deploy-tool.json (relative to the executable).

$ hx-deploy-tool help
hx-deploy-tool is a simple tool to manage docker based application
deployments at helix.

A software release is a (typically small) zip archive containing
pure configuration, typically a docker-compose file and other
configuration files templates. `hx-deploy-tool` can fetch such
releases from AWS S3, unpack them, configure it with appropriate
environmental details and start the system.

The hx-deploy-tool itself has a json formatted configuration file
with a schema specfied in ADL (see config.adl in the source
distribution). This specifies:

* the local directories where releases are unpacked,
* the AWS S3 location where release archives are installed.
* the environment information available to configure packages

See the source repository (https://github.com/helix-collective/hx-deploy-tool)
for details.

The following subcommands are available:

# hx-deploy-tool help

Shows this help text.

# hx-deploy-tool fetch context [--retry]

Downloads the environmental information files from AWS S3. The
`--retry` option is useful during system bootstrap, and it is necessary
to wait for the information files to be created.

# hx-deploy-tool list-releases

Shows the release archives available in S3, most recent first.

# hx-deploy-tool unpack <release> <todir>

Unpack and configure the specified  release into the given directory.

# hx-deploy-tool aws-docker-login-cmd

Assuming this is run on a AWS EC2 instance, this subcommand runs
the appropriate docker login command using the instance profile
to permit access to configured ECR repositories. This command is
typically run on first boot.

# hx-deploy-tool select <release>

The command combines all of the necessary functions to replace
any existing running release with the specified release.
Specifically it:

 - Fetches the release archive from S3
 - Unpacks it
 - configures it for the local environment
 - runs the prestart script. This will typically pull
   necessary docker images.
 - stops the current release (if any)
 - starts the new release
 - switches the `current` symlink to point to the new release

# hx-deploy-tool show-log

Show the history of releases deployed via the select command.
```

The tool is configured with an ADL specified json
file of type [ToolConfig][1]. Each release also has a json file of
type [ReleaseConfig][2].

[1]:adl/config.adl
[2]:adl/release.adl

# Building

Compile the tool with `stack build`.
