# 1. Basic Example

Here is how to do a simple deployment:

## 1.1. Download the latest release of camus2

Seems a little obvious, but the camus2 release includes a bunch of goodies that you will need to make the deployment work.
Have a look and make sure you meet the [minimum requirements](/hx-deploy-tool/docs/userguide/1-welcome/5-minimum-requirements).

We are going to use camus2 to deploy a release on a single machine, without a proxy.

## 1.2. Prepare your environment

In the latest camus2 release package, you will find several useful files to help you get started:

- camus2_basic.yaml - this file tells camus2 where to look for releases, where to deploy them to, and, in which mode (noproxy/proxy with local state/proxy with remote state)
- release.json - include this file in your release archive, it will tell camus2 how to deploy your release.
- a couple of other files, but lets set them aside until we do a proxy deploy.

We will be assuming that your simple deployment requires `docker-compose up` to be executed.
This means that you need to have docker-compose installed on your machine, and that it needs access to whichever docker repository your image is stored in.

You will also need a location to store your release archives, your logs, and a location where the releases will be unpacked.
Make sure all the locations have the neccesary rights (and exist), and specify them in the config file.

## 1.3. Prepare the configuration files

For our simple deployment, our release archive will include the following docker-compose.yml:

```yaml
version: "3"
services:
  web:
    image: httpd
    deploy:
      replicas: 1

```

When executed with `docker-compose up`, this will download the Apache http webserver image, and start it.

Our release will also include a release.json with the following:

```json
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

All the location and deployment parameters are defined in camus2.yaml, and for our example, looks like this:

```yaml
deploysDir: /tmp/deployments/releases
logFile: /tmp/deployments/logs
releases:
  localdir: /tmp/releases

```

You can read more about the release archive and configuration in [Managing your release archive](/hx-deploy-tool/docs/userguide/3-reference/2-release-archive), and [Camus2 configuration](/hx-deploy-tool/docs/userguide/3-reference/1-camus2-config)

Copy the executable binary that you downloaded as part of the latest release to a suitable folder for execution.

The configuration file is expected at ../etc/camus2.(yaml|json), relative to where you place the binary. You can specify an alternate location and name for this file using an `CAMUS2_CONFIG' environment variable. You may have to rename the file, or specify the name of your file in the environment variable if you used the sample file.

## 1.4. Deploy our test

Let's make sure camus2 can find our releases, run `./c2 list-releases`
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
You can stop the release by running `./c2 stop test1.zip`

**Congratulations, you've completed a deployment with camus2!!!**

If you want to see the history captured in the logs, hx-deploytool will fetch and display that for you if you run `./c2 show-log`. Keep in mind, the docker output isn't logged, so you wont see the docker message in the logs.

Now lets step into a more complicated deployment...

[Deploy with a proxy sidecar](/hx-deploy-tool/docs/userguide/proxy-example)

---

- [Index](/hx-deploy-tool/docs/index)
- [Developers Guide/Source code](https://github.com/helix-collective/hx-deploy-tool)
