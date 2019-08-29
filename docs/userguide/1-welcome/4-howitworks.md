# How does Camus2 work?

Simplified deployments
![local state](https://docs.google.com/drawings/d/e/2PACX-1vTsSP9bvwlSGS3YCjgTH0PjZV8wL5rZaBDMetwWUBE3FerO7aQuezJhAv2J-QyZpx-0jbj4yJTxAQlz/pub?w=669&h=314)

## Repeatable consistent deployments using a config and release file

Camus2 bundles deployment actions together by reading the environment configuration from the config file, and when instructed, accessing the configured release store to deploy a relevant release.

Releases contain at least a release.json file that list the relevant commands to execute in order to; prepare (prestart), start, and stop a release.

## Logging of actions

Each action and result is logged in a specified log file, allowing you to peruse deployment history.
In addition, the latest state is logged to a separate state file for quick reference without having to search through logs, you can locate the local state in `{{RELEASES DIRECTORY}}/frontend-proxy/state.json`. This state file will exist even if state is managed remotely and camus2 is run in slave mode.

## Deployment steps

Running `c2 start {{RELEASE}}` will:

- download the noted release archive to the specified local directory
- Unpack the release
- Turn all relevant mustache templates into their final artefacts based on the tags specified in the config file
- Run the prestart command - we typically use this to pull  the necessary docker images from docker or a specified containter store
- Stops the current release (only for non-proxy deployments)
- Run the start command
- Switches the `current` symlink to point to the new release
log the successful actions and errors to the local log file
- If all steps were succesful, generate a new state file to reflect the new state (this will replace old state files)

Most of these actions can be executed indivdually, however,  consistency and repeatability would be circumvented. The intent is to make those individual commands available to allow troubleshooting when required.

## Reverse Proxy configuration and management

When an proxy endpoint is specified in the config file, and is required - either by connecting a release to it, or by creating an ssl certificate - camus2 will create a reverse proxy it calls `frontend-proxy`.

It does this by:

- Using the ports and endpoint information specified in the config file, creating a nginx config via an internally maintained mustache template. - `c2 show-default-nginx-config` will show the config file. You can use your own if the default does not meet your needs.
- Running a `docker-compose -up` command to download and run the latest nginx docker container, configured using the artefact generated in the previous step.
- When required, connect the reverse proxy docker container, to a specified port/release by updating the nginx config, an reloading the reverse proxy with the new config.

## Blue/Green switching

Either an existing release has been deployed and connected to an endpoint, or you are making a release live for the first time.
It is worth noting that camus2 will not let you stop a release if it is connected to a reverse proxy endpoint.
Running `c2 connect {{RELEASE}}` will:

- If not already running, download the latest nginx docker container
- Create an nginx config file using the camus2 config parameters
- Start the ngingx container if required, or reload with the latest parameters connected to the new release
- If state is managed locally, it will also look for an ssl certificate, and serve it at port 443
- Log results to the local log file
- If all steps were succesful, generate a new state file to reflect the new state (this will replace old state files)

## SSl certificate generation

When specified in the config, and at the execution of `c2 generate-ssl-certificate`, camus2 will:

- Run the latest [letsencrypt](https://letsencrypt.org/about/) docker container
- Use the configured details to generate a certificate covering the specified endpoints (proxy enpoints of type `httpsWithRedirect`)
- Place the certificate file in the configured directory
- Trigger nginx to reload its parameters (which includes serving the cert at port 443 from the specified directory)
- Log the results to the local log file
- If all steps were succesful, generate a new state file to reflect the new state (this will replace old state files)

**NOTE** certificates are only expected/possible when state is managed locally, and deployed in proxy mode

## Autoscaling deployments (remote state)

![Autoscaling deployments](https://docs.google.com/drawings/d/e/2PACX-1vTHqDew6xjw9QDTvnoN_V0L_k4qC8J4nQh5OxuddJn37xUv-XFLs6gewnPwA5_LaTnO0d4yj09vJGsV/pub?w=688&h=479)

If camus2 is configured with a remote state store - the "remoteStateS3" parameter within the proxy mode config - camus2 can be run in slave or master mode.

Normal commands like `c2 start {{RELEASE}}` will automatically be executed in master mode, which means it does not actually deploy a release, it instructs all slaves to deploy a release by writing the master state file. Camus2 will still ensure that deployment rules are adhered to e.g. respecting blue/green deployment and not allowing a release to be stopped if it is still connected to an endpoint.

When running camus2 in slave mode with `c2 slave-update --repeat [n]`, camus2 will inspect the master state file in the remote state store every [n] seconds, and compare it with its own state which it manages using the local working directory - in the [release folder]/frontend-proxy/state.json (it only logs its state to the remote store).

If there is a delta between the two states, the camus2 slave will alter its own environment to match the master state. This will include running all the necessary commands to start and connect the necessary releases (see "Deployment steps" above), and to stop and remove old releases, updating any configuration etc.

Each slave will log its own actions and results locally, and update the state store with its 'slave state'.

All instances of camus2, whether master or slave, are expected to have identical configurations, with its execution with `slave-update` denoting that it should treat the config differently.

SSL termination isn't supported for autoscaling as a loadbalancer is expected to separate the slaves from traffic, and to manage ssl termination.

---

- [Index](/hx-deploy-tool/docs/index)
- [Developers Guide/Source code](https://github.com/helix-collective/hx-deploy-tool)
