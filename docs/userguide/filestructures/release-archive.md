# Configuring Release.json

//todo explain everything that happens in the release file.



- **_Optional_** Template files - mustache template files for configuration of other components such as proxy, fluentd and docker compose (based on deploy configuration).
  - **_Optional_** Deployment Scripts
  - **_Optional_** Binary to be deployed (the deploy command/script can source the binary)
- **_Optional_** AWS Credentials: see the [official AWS credential page](https://aws.amazon.com/blogs/security/a-new-and-standardized-way-to-manage-credentials-in-the-aws-sdks/)