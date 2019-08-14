# Minimum Requirements

## Minimum Requirements - no proxy sidecar

At a minimum, you will need the following (with example files included in the release):

- Server where you will be deploying your release
- Configuration file (examples included in the release)
- Release archive - A versioned .zip file Stored in S3 containing:
  - release.json - (examples included in the release) - a config for deployment including predeploy, deploy and stop commands to execute. Also declares template files.
  - **_Optional_** Template files - mustache template files for configuration of other components such as proxy, fluentd and docker compose (based on deploy configuration).
  - **_Optional_** Deployment Scripts
  - **_Optional_** Binary to be deployed (the deploy command/script can source the binary)
- **_Optional_** AWS Credentials: see the [official AWS credential page](https://aws.amazon.com/blogs/security/a-new-and-standardized-way-to-manage-credentials-in-the-aws-sdks/)
- S3 bucket(s) with necessary permissions for r/w - release store and optional state store
- Folders with necessary permissions for writing logs
- Log folder/file
- Location/folder to use as working directory (where releases will be extracted to)


[//todo add refernce to wiki where list of mustache parameters would be detailed.](//todo)

---


---

- [Index](https://helix-collective.github.io/hx-deploy-tool/)
- [Userguide home](https://helix-collective.github.io/hx-deploy-tool/)
- [Developers Guide/Source code](https://github.com/helix-collective/hx-deploy-tool)
