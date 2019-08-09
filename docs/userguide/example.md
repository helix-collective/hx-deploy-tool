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

Also make sure to grant the necessary permissions to the locations where the release will be unpacked, and the logs will be written to. These locations are defined in hx-deploy-tool.json.

Our sample hx-deploy-tool.json looks like this:

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

We will be assuming that your simple deployment requires `docker-compose up` to be executed.
This means that you need to have docker-compose installed on your machine, and that it needs access to whichever docker repository your image is stored in.

... configure environment add file folders etc.
... make sure docker is installed, image is ready.
... rights to run docker-compose
... access to s3.

---

- [Index]()
- [Developers Guide/Source code](https://helix-collective.github.io/hx-deploy-tool/)
