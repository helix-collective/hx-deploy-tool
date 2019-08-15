# Welcome

This user guide is aimed at end users who would like to use [hx-deploy-tool](https://github.com/helix-collective/hx-deploy-tool/releases) to manage their deployments with a simple tool rather than authoring and maintaining scripts
If you would like to read developer documentation and source code, you can find it [here](https://github.com/helix-collective/hx-deploy-tool)

If you are looking for a tl/dr cookbook example using the tool, follow the link to our [simple example page](https://helix-collective.github.io/hx-deploy-tool/docs/userguide/simple-example), the [proxy example](https://helix-collective.github.io/hx-deploy-tool/docs/userguide/proxy-example.md)
or, jump to our userguide [Index](https://helix-collective.github.io/hx-deploy-tool/)

## What is hx-deploy-tool

A small yet versatile tool to automate the annoying bits of deployment, making it more likely engineers do deployments right(tm) and make deployments a joy.
Can work with any deployment scripts ('bare metal') and designed to work well with docker in aws.
It pulls config and release zip files from various sources (eg. local or S3), unpacks into versioned directories, and runs commands to start, stop, and expose them as required.

## Why use hx-deploy-tool

You have a single application or service, and when deploying, need to orcestrate various actions, such as deploying the containers to a single or multiple servers, setting up reverse proxies, managing associating certificates, making sure all the containers are the same version of the application, making sure the logs get ingested etc. etc.

hx-deploy-tool provides a reliable way to deploy all those pieces, and ensure state and version is managed at the same time. It encapsulates all the steps of a deployment, without the need to write script after script. Simply create a docker-compose file, and a couple of moustache templates, and suddently with hx-deploy tool, your deployments are consistent, repeatable, reproducable and highly configureable. On top of that, you can run the tool in slave mode, and it will monitor a blob store for the latest state, and ensure the server it is running on, is in that state (release version, configuration etc.)

There is a gap between running software on a single server, and having large container clusters and using various tools for container orhestration. This tool plugs that gap, and can provide consistency and reliability in deployments until you have grown to a place where you need what container orchestration tools provide.


## When to consider using another tool

You have a plethora of applcations and microservices, for which you need to manage , deployment, scaling (per service/application and independent of), orchestration, load balancing and service distribution across multiple servers,
it sounds like you may benefit from using one of the many orchestration tools available.


---

- [Index](https://helix-collective.github.io/hx-deploy-tool/)
- [Developers Guide/Source code](https://github.com/helix-collective/hx-deploy-tool)
