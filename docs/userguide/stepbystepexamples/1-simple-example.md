# 1. Simple Example

Here is how to do a simple deployment:

## 1.1. Download the latest release of camus2

Seems a little obvious, but the camus2 release includes a bunch of goodies that you will need to make the deployment work.
You can find the minimum requirements [here](/hx-deploy-tool/docs/userguide/minimum-requirements), so have a read before you try to get things running.

We are going to use camus2 to deploy a release on a single machine, without a proxy.

## 1.2. Prepare your environment

In the latest camus2 release package, you will find several useful files to help you get started:

- camus2.json - this file tells camus2 where to look for releases, where to deploy them to, and, in which mode (noproxy/proxy with local state/proxy with remote state)
- release.json - include this file in your release archive, it will tell camus2 how to deploy your release.
- a couple of other files, but lets set them aside until we do a proxy deploy.

We will be assuming that your simple deployment requires `docker-compose up` to be executed.
This means that you need to have docker-compose installed on your machine, and that it needs access to whichever docker repository your image is stored in.

You will also need a location to store your release archives, your logs, and a location where the releases will be unpacked.
Make sure all the locations have the neccesary rights (and exist), and specify them in the config file.

## 1.3. Prepare the configuration files

For our simple deployment, our release archive will include the following docker-compose.yml:

```
version: "3"
services:
  web:
    image: httpd
    deploy:
      replicas: 1

```

When executed with `docker-compose up`, this will download the Apache http webserver image, and start it.

Our release will also include a release.json with the following:

```
{
  "prestartCommand": "",
  "startCommand": "docker-compose up",
  "stopCommand": "docker-compose stop && docker-compose rm -f && docker system prune -f",
  "templates": []
}
```
Take these 2 files, and add them to a .zip archive. Lets call it test1.zip
You can create test2.zip if you want to test having multiple release versions.
Put them in a folder where you would expect your releases.
We will be going with `/tmp/releases`

All the location and deployment parameters are defined in camus2.json, and for our example, looks like this:

```
{
    "releasesDir": "/tmp/deployments/releases",
    "contextCache": "/tmp/deployments/cache",
    "logFile": "/tmp/deployments/logs",
    "releases": {
      "localdir": "/tmp/releases"
    }
}
```

The sample [camus2.json](/hx-deploy-tool/blob/master/docs/templates/example_deploy.json) that is included in the camus2 release has descriptive values, or you can read more about using it [here //todo](/hx-deploy-tool/)

Copy the executable binary that you downloaded as part of the latest release to a suitable folder for execution.

The configuration file is expected at ../etc/camus2.json, releative to where you place the binary. You specify an alternate location and name for this file using a `HX_DEPLOY_CONFIG' environment variable.

## 1.4. Deploy our test

Let's make sure camus2 can find our releases, run `./camus2 list-releases`
You should see the release archives listed, in our case, we have 2:

```
test2.zip
test1.zip
```

Now simply starting a release will unpack it, and execute the pre-start, stop, and start commands listed in your release.
If you are using our example configurations, './camus2 start test1.zip` should do all that. In the cli output, look for the IP address that gets asigned to the container, or use the [docker cli](https://docs.docker.com/engine/reference/commandline/inspect/) to get the IP address of the container. Visit the IP in a browser, and the Apache docker image will print its test message on your screen:

```
It Works!
```
You can stop the release by running `./camus2 stop test1.zip`

**Congratulations, you've completed a deployment with camus2!!!**

If you want to see the history captured in the logs, hx-deploytool will fetch and display that for you if you run `./camus2 show-log`. Keep in mind, the docker output isn't logged, so you wont see the docker message in the logs.

Now lets step into a more complicated deployment...

[Deploy with a proxy sidecar](/hx-deploy-tool/docs/userguide/proxy-example)

---

- [Index](/hx-deploy-tool/docs/userguide/index)
- [Userguide home](/hx-deploy-tool/docs/1-user-guide)
- [Developers Guide/Source code](https://helix-collective.github.io/hx-deploy-tool/)
