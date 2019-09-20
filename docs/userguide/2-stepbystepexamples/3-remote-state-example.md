# Autoscaling deployments (remote state) example

Deploying in an autoscaling group is similar to, and builds on the [reverse proxy sidecar](/hx-deploy-tool/docs/userguide/2-stepbystepexamples/2-proxy-example) example, so if you have that set up already, you can add the remote s3 and slave label to your config, and skip to **Deploy our test** (but recommending stopping all containers and emptying the release and log directories so you can follow the example)

## 1.1. Download the latest release of camus2

Seems a little obvious, but the release includes a bunch of goodies that you will need to make the deployment work.
Have a look and make sure you meet the [minimum requirements](/hx-deploy-tool/docs/userguide/1-welcome/5-minimum-requirements).

We are going to use camus2 to deploy a release on a single machine, with an automatically configured nginx reverse proxy.

## 1.2. Prepare your environment

In the latest release package, you will find several useful files to help you get started:

- camus2_remote-state.yaml - We will use this as a config file to tell camus2 where to look for releases, where to deploy them to, and, in which mode (noproxy/proxy with local state/proxy with remote state)
- release.json - include this file in your release archive, it will tell camus2 how to deploy your release.

As we will be connecting to s3 for storing our state files, you will need to configure your system with aws credentials, and have an s3 bucket with read and write permissions for those credentials. Camus2 accepts any of the three official methods of managing credentials. You can find the official documentation here: [AWS Credentials](https://aws.amazon.com/blogs/security/a-new-and-standardized-way-to-manage-credentials-in-the-aws-sdks/)

===

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
camus2 will get this parameter from the release config (camus2.json), and inject it into any template with relevant tags, and create a non-template artefact with all tags replaced with values.

To that end, rename the file to docker-compose.yml.tpl so that the tool will recognise it as a template. More detail around the mustache magid in [Template anything](/hx-deploy-tool/docs/userguide/3-reference/3-templateanything) available with the tool after the link.

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
We will be going with `/tmp/releases`

For this example we will create 2 endpoints (_main_ and _test_), and connect our 2 "versions" of our test app to these endpoints. When specifying these proxy endpoints, we are also specifying a remote state location. This tells camus2 that you are in a multi-server environment.
Running the normal `c2 start {{RELEASE}}` command will only result in writing to a master state file in the S3 bucket specified.
To actually deploy a release, we will have to run camus2 in slave mode.

If you were running your application on multiple servers, it would be logical to store the releases in S3 also, but for this example, we will still store it locally.

All the location and deployment parameters are defined in camus2.yaml, and for our example, looks like this:

```yaml
releasesDir: /tmp/deploytest/releases
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
    remoteStateS3:
      just: s3://{S3 BUCKET FOR TRACKING APP STATE}
    slaveLabel:
      label: myslave
    dynamicPortRange:
      v1: 8000
      v2: 8100

```

This differs from the proxy example only in specifying 2 additional parameters, the location of the remote state store, and a label for the slave.

The slave label is used for logging, and, if ommitted, camus2 will query the aws api for the ec2 instance name/label, which will make it easy to identify the slave in ec2 from the slave status on s3.

You can read more about the release archive and configuration in [Managing your release archive](/hx-deploy-tool/docs/userguide/3-reference/2-release-archive), and [Camus2 configuration](/hx-deploy-tool/docs/userguide/3-reference/1-camus2-config)

Copy the executable binary that you downloaded as part of the latest camus2 release to a suitable folder for execution.

The configuration file is expected at ../etc/camus2.(yaml|json), relative to where you place the binary. You can specify an alternate location and name for this file using an `CAMUS2_CONFIG' environment variable. You may have to rename the file, or specify the name of your file in the environment variable if you used the sample file.

## 1.4. Deploy our test

Let's make sure camus2 can find our releases, run `./c2 list-releases`
You should see the release archives listed:

```
test2.zip
test1.zip
```

When we run `./c2 start test1.zip`, it will check the release directory to confirm the release exists, and then create the master state file in the specified S3 bucket with a `/master/` prefix.

You can verify that no docker container is running on your local machine by running `docker ps`, and the state.json in the s3 bucket will contain the same information as running `./c2 status`

```
Endpoints:
  main: (main.localhost:80) -> (not connected)
  test: (test.localhost:80) -> (not connected)

Deploys:
  test1.zip: (localhost:8000)
```

We need to run c2 in slave mode to actually unpack the release locally.

Running `./c2 slave-update` will read the master state from s3, and make sure the local machine in the same state.
In this scenario, it will will unpack the release, and execute the pre-start, stop, and start commands listed in your release. If all those steps were successful, running `./c2 status --show-slaves ` should result in the following status:

```
Endpoints:
  main: (main.localhost:80) -> (not connected)
  test: (test.localhost:80) -> (not connected)

Deploys:
  test1.zip: (localhost:8000)
----------------------------------------------------------------------
Slave: myslave
Updated: 2019-08-23 05:38:36 UTC
Endpoints:
  main: (main.localhost:80) -> (not connected)
  test: (test.localhost:80) -> (not connected)

Deploys:
  test1.zip: (localhost:8000)

```

The first status is the master state, and after the line, it will list all the slave statuses.
You can also use the docker cli to confirm that a container is running for test1, and [docker inspect](https://docs.docker.com/engine/reference/commandline/inspect/) to get the IP address of the container.

Switching back to master mode, lets connect the container to the main endpoint, because going to `main.localhost:80` will currently result in an error: `./c2 connect main test1.zip` will update the remote state. Running `./c2 status --show-slaves ` will show that the slave has not update yet:

```
Endpoints:
  main: (main.localhost:80) -> test1.zip
  test: (test.localhost:80) -> (not connected)

Deploys:
  test1.zip: (localhost:8000)
----------------------------------------------------------------------
Slave: myslave
Updated: 2019-08-23 05:38:36 UTC
Endpoints:
  main: (main.localhost:80) -> (not connected)
  test: (test.localhost:80) -> (not connected)

Deploys:
  test1.zip: (localhost:8000)

```  

This is because we haven't told the slave to update yet. `./c2 slave-update` will get camus2 to read the master state, download and start the nginx reverse proxy, and connect it to the test1 container at the specified port - updating the slave state in the remote store once successful.
`./c2 status --show-slaves ` will now show the slave state as connected to main

```
Endpoints:
  main: (main.localhost:80) -> test1.zip
  test: (test.localhost:80) -> (not connected)

Deploys:
  test1.zip: (localhost:8000)
----------------------------------------------------------------------
Slave: myslave
Updated: 2019-08-23 05:58:09 UTC
Endpoints:
  main: (main.localhost:80) -> test1.zip
  test: (test.localhost:80) -> (not connected)

Deploys:
  test1.zip: (localhost:8000)
```

Visiting `main.localhost:8000` in your browser should now show the Apache test message:

```
It Works!
```
however,  `test.localhost:8001` should still show an error page.

If you are in production, you don't want to log into every slave to run camus2, so, if you baked it into your server image, you can run it with `./c2 slave-update --repeat n` to have it running continually, and verifying the slave is up to date every n seconds.

Lets run `./c2 slave-update --repeat 120` to run camus2 in slave mode, checking state every 120 seconds. This should allow us to start and connect in master mode in between slave updates, and demonstrate that the slave will do all the necessary steps to bring the slave state equal to master.

Open another terminal, and we will deploy the second test in master mode. Wait for the slave to say 'Fetching state...', and then run `./c2 start test2.zip`, `./c2 connect test test2.zip` and `./c2 status --show-slaves` keep an eye on the slave terminal, which will unpack and run test2, and then connect it to the test endpoint when it runs again.

Your status output should look as follows, with the slave not yet running test2.zip:

```
Endpoints:
  main: (main.localhost:80) -> test1.zip
  test: (test.localhost:80) -> test2.zip

Deploys:
  test1.zip: (localhost:8000)
  test2.zip: (localhost:8001)
----------------------------------------------------------------------
Slave: myslave
Updated: 2019-08-23 06:30:13 UTC
Endpoints:
  main: (main.localhost:80) -> test1.zip
  test: (test.localhost:80) -> (not connected)

Deploys:
  test1.zip: (localhost:8000)
```

once you see the slave has updated, run `./c2 status --show-slaves` again. It should show test2.zip connected to the test endpoint:

```
Endpoints:
  main: (main.localhost:80) -> test1.zip
  test: (test.localhost:80) -> test2.zip

Deploys:
  test1.zip: (localhost:8000)
  test2.zip: (localhost:8001)
----------------------------------------------------------------------
Slave: myslave
Updated: 2019-08-23 06:32:22 UTC
Endpoints:
  main: (main.localhost:80) -> test1.zip
  test: (test.localhost:80) -> test2.zip

Deploys:
  test1.zip: (localhost:8000)
  test2.zip: (localhost:8001)
```
Visiting `main.localhost:8000` or `test.localhost:8001` in your browser should now show the Apache test message:

```
It Works!
```

You can now test that both slave and master will respect your blue/green decisions, by running `./c2 stop test1.zip` and getting the following error:

```
Exception: deploy is connected to main
CallStack (from HasCallStack):
  error, called at src/Commands/ProxyMode.hs:105:33 in main:Commands.ProxyMode
```
Visiting `main.localhost:8000` or `test.localhost:8001` in your browser should still show the Apache test message:

```
It Works!
```

If you execute `./c2 disconnect test`, `test.localhost:8001` will now show the nginx 503 error as it is no longer connected to a container.
You can stop the release by running `./c2 stop test1.zip`

**Congratulations, you've completed a slave mode deployment with camus2!!!**

---

---

- [Index](/hx-deploy-tool/docs/index)
- [Developers Guide/Source code](https://github.com/helix-collective/hx-deploy-tool)
