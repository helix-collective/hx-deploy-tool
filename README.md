# Overview

A small tool to automate docker-compose deployments. It pulls config
and release zip files from S3, unpacks into versioned directories,
and runs commands to stop and start them as required.

```
$ hx-deploy-tool
General Usage:
  hx-deploy-tool help
  hx-deploy-tool fetch-context [--retry]
  hx-deploy-tool list-releases
  hx-deploy-tool unpack <release> <todir>
  hx-deploy-tool show-log
  hx-deploy-tool aws-docker-login-cmd

Deployment with a proxy:
  hx-deploy-tool proxy-status
  hx-deploy-tool proxy-deploy <release>
  hx-deploy-tool proxy-undeploy <release>
  hx-deploy-tool proxy-connect <endpoint> <release>
  hx-deploy-tool proxy-disconnect <endpoint>

Deployment without a proxy:
  hx-deploy-tool select <release>

The config file is read from the file specified with HX_DEPLOY_CONFIG.
It defaults to ../etc/hx-deploy-tool.json (relative to the executable).
```

More information is available in the [help text](doc/help.md)
