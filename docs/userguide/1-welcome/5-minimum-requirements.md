# Minimum Requirements

## Minimum Requirements - no proxy sidecar

At a minimum, you will need the following (with example files included in the release):

- Server where you will be deploying your release
- Configuration file (examples included in the release) or [here](https://github.com/helix-collective/hx-deploy-tool/tree/master/templates/)
- Release store - keeps all published [release zip archives](... link to details of the zip structure...). The release store may either be a path in the local file system or a path in an s3 bucket.
- Release archive - At least one versioned .zip file needs to be in the release store and must contain at a minium:
  - release.json - (examples included in the release) - a config for deployment including predeploy, deploy and stop commands to execute. Also declares template files.
- S3 bucket(s) or directory with necessary permissions for r/w - release store and optional state store
- The following working directories, with write permissions for the user(s) who will run camus2 (can be changed by specifying in config file).
  - /opt/deploys     # where releases get unpacked for deployment
  - /opt/config  # where deployment contexts get written
  - /opt/var/log/    # where the log file gets written


[//todo add refernce to wiki where list of mustache parameters would be detailed.](//todo)

---

- [Index](/hx-deploy-tool/index)
- [Developers Guide/Source code](https://github.com/helix-collective/hx-deploy-tool)
