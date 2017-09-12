# Overview

A small tool to automate docker-compose deployments. It pulls config
and release zip files from S3, unpacks into versioned directories,
and runs commands to stop and start them as required.

```
Usage:
  hx-deploy-tool fetch-context
  hx-deploy-tool unpack <releaseid> <todir>
  hx-deploy-tool select <releaseid>

The config file is read from the file specified with HX_DEPLOY_CONFIG.
It defaults to ../etc/hx-deploy-tool.json (relative to the executable).
```

The tool is configured with an ADL specified json
file of type [ToolConfig][1]. Each release also has a json file of
type [ReleaseConfig][2].

[1]:adl/config.adl
[2]:adl/release.adl

# Building

Compile the tool with `stack build`.
