# 1. Proxy Sidecar Example

Here is how to do a deployment with a reverse proxy sidecar:

## 1.1. Download the latest release of camus2

Seems a little obvious, but the camus2 release includes a bunch of goodies that you will need to make the deployment work.
Have a look and make sure you meet the [minimum requirements](/hx-deploy-tool/docs/userguide/1-welcome/5-minimum-requirements).

We are going to use camus2 to deploy a release on a single machine, with an automatically configured nginx reverse proxy.

## 1.2. Prepare your environment

In the latest camus2 release package, you will find several useful files to help you get started:

- camus2_proxy.yaml - We will use this as a config file telling camus2 where to look for releases, where to deploy them to, and, in which mode (noproxy/proxy with local state/proxy with remote state)
- release.json - include this file in your release archive, it will tell camus2 how to deploy your release.

We will be assuming that your simple deployment requires `docker-compose up` to be executed.
This means that you need to have docker-compose installed on your machine, and that it needs access to whichever docker repository your image is stored in.

You will also need a location to store your release archives, your logs, and a location where the releases will be unpacked.
Make sure all the locations have the necessary rights (and exist), and specify them in the config file.

## 1.3. Prepare the configuration files

We will also need a docker-compose.yml, which may look as follows:

```yaml
version: "3"
services:
  web:
    # replace username/repo:tag with your name and image details
    image: httpd
    deploy:
      replicas: 1
    ports:
      - "{{ports.http}}:80"
```

Note the {{ports.http}} in the place of the host port in the docker-compose.yml.
camus2 will get this parameter from the release config (camus2.yaml), and inject it into any template with relevant tags, and create a non-template artefact with all tags replaced with values.

To that end, rename the file to docker-compose.yml.tpl so that the tool will recognise it as a template. More detail around the [mustache magic](/hx-deploy-tool/docs/userguide/3-reference/3-templateanything) available with the tool after the link.

Our release will also include a release.json with the following:

```json
{
  "prestartCommand": "",
  "startCommand": "docker-compose up -d",
  "stopCommand": "docker-compose stop && docker-compose rm -f && docker system prune -f",
  "templates": ["docker-compose.yml.tpl"]
}
```

The "templates" specifies all the template files that the tool will turn into actual files by populating all the mustache tags, and removing `.tpl`
In this scenario, we want to populate the port that nginx will use as a reverse proxy when it gets connected to the deployed container. The range of ports that nginx can use will be specified in the tool configuration below (8000 to 8100).

Take these 2 files, and add them to a .zip archive. Lets call it test1.zip
Create test2.zip with the same files so we can test having different versions deployed.
Put them in a folder where you would expect your releases.
We will be going with `/tmp/tests`

For this example we will create 2 endpoints (_main_ and _test_), and connect our 2 "versions" of our test app to these endpoints.

All the location and deployment parameters are defined in camus2.yaml, and for our example, looks like this:

```yaml
deploysDir: /tmp/deploytest/releases
logFile: /tmp/deploytest/camus2.log
releases:
  localdir: /tmp/tests
deployMode:
  proxy:
    endPoints:
      main:
        serverNames:
        - main.localhost
        etype: httpOnly
      test:
        serverNames:
        - test.localhost
        etype: httpOnly
    dynamicPortRange:
      v1: 8000
      v2: 8100

```

You can read more about the release archive and configuration in [Managing your release archive](/hx-deploy-tool/docs/userguide/3-reference/2-release-archive), and [Camus2 configuration](/hx-deploy-tool/docs/userguide/3-reference/1-camus2-config)

Copy the executable binary that you downloaded as part of the latest release to a suitable folder for execution.

The configuration file is expected at ../etc/camus2.(yaml|json), relative to where you place the binary. You can specify an alternate location and name for this file using an `CAMUS2_CONFIG' environment variable. You may have to rename the file, or specify the name of your file in the environment variable if you used the sample file.

## 1.4. Deploy our test

Let's make sure camus2 can find our releases, run `./c2 list-releases`
You should see the release archives listed:

```
test2.zip
test1.zip
```

Now simply starting a release will unpack it, and execute the pre-start, stop, and start commands listed in your release.
If you are using our example configurations, `./c2 start test1.zip` should do all that.

Lets run the command again to deploy the second release as well `./c2 start test2.zip`. Running `c2 status` should yield the following:

```
Endpoints:
  main: (main.localhost:80) -> (not connected)
  test: (test.localhost:80) -> (not connected)

Deploys:
  test1.zip: (localhost:8000)
  test2.zip: (localhost:8001)
```

This means that it successfully started containers for both releases, with the 'host name' and relevant configured ports specified.
You can also use the docker cli to confirm that both containers are running, and [docker inspect](https://docs.docker.com/engine/reference/commandline/inspect/) to get the IP address of the container. 

Now we are going to deploy a nginx proxy to manage the routing of these two containers.
Running `c2 connect main test1.zip` and `c2 connect test test2.zip` will use the parameters in the configuration to create an nginx proxy using the latest nginx docker image, and connect it using the specified ports to the relevant containers.

Running `c2 status` should now yield the following:

```
Endpoints:
  main: (main.localhost:80) -> test1.zip
  test: (test.localhost:80) -> test2.zip

Deploys:
  test1.zip: (localhost:8000)
  test2.zip: (localhost:8001)
```

You can again use the docker cli to confirm the nginx container is now running. If you want to permanently stop the nginx container you will have to do so manually, however, any new proxy configurations that need to be applied and/or connected will result in the container being restarted or rebuilt if necessary.

Visiting `main.localhost:8000` or `test.localhost:8001` in your browser should now show the Apache test message:

```
It Works!
```

Before you can stop the container, you need to disconnect it from the endpoint, or connect another release to the endpoint.
If you execute `./c2 disconnect test`, `test.localhost:8001` will now show the nginx 503 error as it is no longer connected to a container.
You can stop the release by running `./c2 stop test1.zip`

**Congratulations, you've completed a proxy deployment with camus2!!!**

# Advanced concepts

The following modifications to the config (camus2.yaml) can immediately make the tool more useful:

## Storing releases on S3

This can easily be done by replacing
```yaml
releases:
  localdir: /tmp/tests
```

with

```yaml
releases:
  S3: {{YOUR S3 LOCATION}}
```

## Generating SSL certificates

If you set your endpoint as `etype: httpsWithRedirect`, camus2 will use letsencrypt to generate and attached an ssl certificate.

___

More details can be found in the how it works section, or the example config file.

---

- [Index](/hx-deploy-tool/docs/index)
- [Developers Guide/Source code](https://github.com/helix-collective/hx-deploy-tool)
