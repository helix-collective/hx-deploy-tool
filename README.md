# hx-deploy-tool

A small tool to automate the annoying bits of deployment, making it more likely engineers do deployments right(tm)
and make deployments a joy. Can work atop docker or 'bare metal'. It pulls config and release zip files from
various sources (eg. S3), unpacks into versioned directories, and runs commands to start, stop, and expose
them as required.

It's the spiritual successor to https://github.com/helix-collective/camus

# Conceptual Model

  - No Server, Remote state is 'dumb', and just that (a config file on S3, for example), any bit of remote state
    is only ever updated by a single conceptual actor (can be read by many)
  - Multiple deploys can be run at once, each will be on a different port.
  - Deploys can be 'connected' to well named endpoints 'main' for example, is serving live traffic, and canary for the   deploy that's next slated to be set as main. By default this is managed via an nginx frontendproxy (that's run in docker), however, can be extended to support other methods.
  - A stated design goal is each exposed command isn't meant to be a complex abstraction, but rather a convenience. If required, a support engineer should be able to execute the commands by hand (albiet it might be a painful and long process)
  - A second stated design goal is rollbacks and roll-forwards should be fast, easy and have 0 downtime. By default the various connect/disconnect commands cause the nginx frontend proxy to reconfigure and reload, forwarding traffic to the specified running deploy.

# Requirements:
 - Configuration file: see docs/example_deploy.json for an example and adl/config.adl for specification in ADL. defaults to ../etc/hx-deploy-tool.json
 - AWS Credentials: see https://aws.amazon.com/blogs/security/a-new-and-standardized-way-to-manage-credentials-in-the-aws-sdks/ 
 - S3 buckets with necessary permissions for storing state and retrieving releases
 - Folders with necessary permissions for writing logs
 -  Release Binaries:
    Stored in S3 in a .zip file
    Contains - binary to be deployed
    release.json - config for deployment including predeploy, deploy and stopping commands to execute, and references to template files 
    Template files - mustache template files for configuration of other components such as proxy, fluentd and docker compose (based on deploy configuration).
//todo add refernce to wiki where list of mustache parameters would be detailed.

# Command Overview

```
$ hx-deploy-tool
General Usage:
  hx-deploy-tool help
  hx-deploy-tool list-releases
  hx-deploy-tool show-log
  hx-deploy-tool --version

Deployment:
  hx-deploy-tool status [--show-slaves]
  hx-deploy-tool start <release>
  hx-deploy-tool stop <release>
  hx-deploy-tool restart-frontend-proxy
  hx-deploy-tool connect <endpoint> <release>
  hx-deploy-tool disconnect <endpoint>

Plumbing/Low Level Operations:
  hx-deploy-tool fetch-context [--retry]
  hx-deploy-tool unpack <release> <todir>
  hx-deploy-tool aws-docker-login-cmd
  hx-deploy-tool expand-template <templatePath> <destPath>
  hx-deploy-tool show-default-nginx-config
  hx-deploy-tool generate-ssl-certificate
  hx-deploy-tool slave-update [--repeat n]

The config file is read from the file specified with HX_DEPLOY_CONFIG.
It defaults to ../etc/hx-deploy-tool.(json|yaml) relative to the executable.
```

# Additional Information

- [Installation and configuration](docs/installation.md)
- [Usage](docs/help.md)

