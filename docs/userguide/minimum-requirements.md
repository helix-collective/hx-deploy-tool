# Minimum Requirements

## Minimum Requirements - no proxy sidecar

At a minimum, you will need the following (with example files included in the release):

- Server where you will be deploying your release
- Configuration file [example noproxy confguration]()
- Release file - A versioned .zip file Stored in S3 containing:
  - release.json - Release configuration [example release confguration]()  - a config for deployment including predeploy, deploy and stop commands to execute. Also declares template files.
  - AWS Credentials: see the [official AWS credential page](https://aws.amazon.com/blogs/security/a-new-and-standardized-way-to-manage-credentials-in-the-aws-sdks/)
  - **_Optional_** Template files - mustache template files for configuration of other components such as proxy, fluentd and docker compose (based on deploy configuration).
  - **_Optional_** Deployment Scripts
  - **_Optional_** Binary to be deployed (the deploy command/script can source the binary)
- S3 buckets with necessary permissions for storing state and retrieving releases
- Folders with necessary permissions for writing logs
- Log folder/file
- Location/folder to use as working directory (where releases will be extracted to)


//todo add refernce to wiki where list of mustache parameters would be detailed.

## Minimum Requirements - with a proxy sidecar



---

- [Index]()
- [Developers Guide/Source code](https://helix-collective.github.io/hx-deploy-tool/)
