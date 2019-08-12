# 1. Example

Here is how to do a simple deployment:

## 1.1. Download the latest release of hx-deploy-tool

Seems a little obvious, but the release includes a bunch of goodies that you will need to make the deployment work.
You can find the minimum requirements [here](), so have a read before you try to get things running.

We are going to use hx-deploy-tool to deploy a release on a single machine, without a proxy.

## 1.2. Prepare your environment

In the latest release package, you will find several usefull files to help you get started:

- hx-deploy-tool.json - this file tells hx-deploy-tool where to look for releases, where to deploy them to, and, in which mode (noproxy/proxy with local state/proxy with remote state)
- release.json - include this file in your release archive, it will tell hx-deploy-tool how to deploy your release.
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
    image: hello-world
    deploy:
      replicas: 1

```

When executed with `docker-compose up`, this will download the docker hello-world image, and start it.

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

All the location and deployment parameters are defined in hx-deploy-tool.json, and for our example, looks like this:

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

The sample [hx-deploy-tool.json](https://github.com/helix-collective/hx-deploy-tool/blob/master/docs/templates/example_deploy.json) that is included in the hx-deploy-tool release has descriptive values, or you can read more about using it [here]()

Copy the executable binary that you downloaded as part of the latest release to a suitable folder for execution.

The configuration file is expected at ../etc/hx-deploy-tool.json, releative to where you place the binary. You specify an alternate location and name for this file using a `HX_DEPLOY_CONFIG' environment variable.

## Deploy our test

Let's make sure hx-deploy-tool can find our releases, run './hx-deploy-tool list-releases'
You should see the release archives listed, in our case, we have 2:

```
test2.zip
test1.zip
```

Now simply starting a release will unpack it, and execute the pre-start, stop, and start commands listed in your release.
If you are using our example configurations, './hx-deploy-tool start test1.zip` should do all that. The hello world docker image will print it's hello world message on your screen:

```
Hello from Docker!
This message shows that your installation appears to be working correctly.
```
You can stop the release by running './hx-deploy-tool stop test1.zip`

**Congratulations, you've completed a deployment with hx-deploy-tool!!!**

# Deploy with a Proxy



---

- [Index]()
- [Developers Guide/Source code](https://helix-collective.github.io/hx-deploy-tool/)
