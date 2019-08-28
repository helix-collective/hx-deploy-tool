Camus2 is a simple tool to manage docker based application
deployments at helix.

A software release is a (typically small) zip archive containing pure
configuration, typically a docker-compose file and other configuration
files templates. `c2` can fetch such releases from AWS S3,
unpack them, configure it with appropriate environmental details and
start the system.

camus2 itself has a json formatted configuration file with
a schema specfied in ADL (see config.adl in the source
distribution). This specifies:

* the local directories where releases are unpacked,
* the AWS S3 location where release archives are installed.
* the environment information available to configure packages

The tool supports two deployment modes:

* without a reverse proxy
* with a reverse proxy

Without a proxy, the tool expects the single deployed release to
directly open the apppropriate external network ports. With a proxy,
the tool supports multiple deployed releases, telling each a local
port to use. The tool manages an nginx reverse proxy which connects
the external network ports to the deployed releases, allowing dynamic
reconfiguration, and hence a no down time release process. Reverse
proxy mode is enabled if the configuration file specifies one or more
proxy endpoints.

# Deployments with a proxy
The following subcommands are used to manage reverse proxy deployments:

## c2 status
Show the proxy system status: specifically the endpoints and live
deploys.

## c2 start <release>
Create and start a deployment (if it's not already running)

## c2 stop <release>
Stop and remove a deployment

## c2 connect <endpoint> <release>
Connect an endpoint to a running deployment

## c2 disconnect <endpoint>
Disconnect an endpoint

## c2 slave-update [--repeat n]
If the tool is configured with proxy remote state, this fetches
the master state from S3, and updates the local state to match.
With a repeat argument, the command will repeat every n seconds,
running indefinitely.

# Deployments without a proxy
The following subcommands are used to manage proxyless deployments:

## c2 start <release>
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

# Misc subcommands
The following other subcommands are available:

## c2 help
Shows this help text.

## c2 fetch-context [--retry]
Downloads the environmental information files from AWS S3. The
`--retry` option is useful during system bootstrap, and it is
necessary to wait for the information files to be created.

## c2 list-releases
Shows the release archives available in S3, most recent first.

## c2 unpack <release> <todir>
Unpack and configure the specified release into the given directory.

## c2 expand-template <templatePath> <destPath>
Injects the config contexts specified in `configSources` into a template
found at `templatePath` and saves the resulting file to `destPath`

## c2 aws-docker-login-cmd
Assuming this is run on a AWS EC2 instance, this subcommand runs
the appropriate docker login command using the instance profile
to permit access to configured ECR repositories. This command is
typically run on first boot.

## c2 show-log
Show the history of releases deployed via the start command.

## c2 show-default-nginx-config
Outputs the default template for the nginx config.


- [Userguide home](https://helix-collective.github.io/hx-deploy-tool/docs/1-user-guide.html)