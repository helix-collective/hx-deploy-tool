# Camus2 Features

## Deploy without a proxy - simple deployments

Camus2 takes a release archive from an archive store, unpacks it, and runs whatever scripts or commands that you have specified. This is the most basic action you can do with camus2.
This is great when you just want to run a binary, or execute a couple of commands or scripts in order.

The [simple example](/hx-deploy-tool/docs/userguide/simple-example) details how to quickly get started.

## Deployments with a reverse proxy sidecar

A feature that is baked into camus2, is to download and connect the standard nginx reverse proxy docker container, to a specified port.
All you have to do is specify proxy endpoints and ports in the config file.

The [proxy example](/hx-deploy-tool/docs/userguide/proxy-example) details step by step how to deploy and connect a release to a reverse proxy endpoint.

## Blue/Green deployments

Baked into the design of camus2 is 0% downtime blue/green deployments. Camus2 will not allow you to stop a release if it is connected to a reverse proxy endpoint. It will allow you to connect a new release to the endpoint, and then stop the previous release.

You can of course disconnect the endpoint by running the `disconnect` command manually, but that would be an active decision to have an outage.

## Deployments with managed ssl termination

If you are still in the early stages of your application or service, you may be exposing the app directly to the internet, and require ssl termination.
Utilising the open source [letsencrypt](https://letsencrypt.org/about/), and specifying the necesary details in the config, camus2 can create a certificate allow nginx to use it for ssl termination

## Deployments in an autoscaling group (slave mode)

Your application/service is really getting traction, and you need to scale it to multiple machines. Camus2 can monitor a statefile on an S3 bucket, and it will automatically compare the local state (including releases, endpoints etc.), and alter the local state to mirror the specification in the state file. It will do so in a blue/green manner which can be ensured by using camus2 to alter the statefile.
Slaves will log their state to the S3 bucket also, allowing you to easily confirm that all the slaves are in the correct state. The assumption is that you will have a simple load balancer such as AWS ALB behind which your application runs, which also manages ssl termination.

## Mustache templates for anything in your deployment

In the release.json file, you can specify a list of template files using [mustache](https://mustache.github.io/mustache.5.html) notation that are bundled in your release.
Camus2 will use the tags specified in the config file to turn the templates into release specific artefacts.
A simple example is that we use the {{ports.http}} tag to inject the port that we configure in the config file, into both the nginx config, and the docker-compose config in our [proxy deployment example](/hx-deploy-tool/docs/userguide/2-stepbystepexamples/2-proxy-example).

Additionally, you can specify your own tags in the config file. A run down of the standard tags and how to use the custom tags are detailed in the [template anything](/hx-deploy-tool/docs/userguide/3-reference/3-templateanything) section

## AWS health checks

Additionally, you can connect your AWS autoscaling group and load balancer to a health check endpoint you can specify in the configuration file. This along with deploying in slave mode in an autoscale group, can ensure that faulty applications/services are recycled.

## Logging and state tracking

Each instance of camus2, whether in slave or master mode, will log its actions and state. This allows you to investigate when there is a problem with a specific deployment.

---

- [Index](/hx-deploy-tool/docs/index)
- [Developers Guide/Source code](https://github.com/helix-collective/hx-deploy-tool)
