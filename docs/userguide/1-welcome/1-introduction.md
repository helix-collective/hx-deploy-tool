# Introduction

## Quick start
Grab the latest release from [camus2](https://github.com/helix-collective/hx-deploy-tool/releases) gitub, and have a look at the minimum requirements, and you are good to go!

If you are looking for a tl/dr cookbook example using camus2, follow the link to our [simple example page](/hx-deploy-tool/docs/userguide/2-stepbystepexamples/1-basic-example), the [proxy example](/hx-deploy-tool/docs/userguide/2-stepbystepexamples/2-proxy-example)
or, jump to our userguide [Index](/hx-deploy-tool/docs/index.html) for more

## Why use camus2

You've decided that you want finer control over your environment than running it in an application or function PAAS, and you need to manage your deployments.
You have a single application or service, and when deploying, need to orcestrate various actions, such as deploying the containers to a single or multiple servers, setting up reverse proxies, managing associating certificates, making sure all the containers are the same version of the application, making sure the logs get ingested etc. etc.

camus2 provides a reliable way to deploy all those pieces, and ensure state and version is managed at the same time. It encapsulates all the steps of a deployment, without the need to write script after script. Simply create a docker-compose file, and a couple of moustache templates, and suddenly with hx-deploy tool, your deployments are consistent, repeatable, reproducable and highly configureable. On top of that, you can run the tool in slave mode, and it will monitor a blob store for the latest state, and ensure the server it is running on, is in that state (release version, configuration etc.)

There is a gap between running software on a single server, and having large container clusters and using various tools for container orchestration. This tool plugs that gap, and can provide consistency and reliability in deployments until you have grown to a place where you need what container orchestration tools provide.


## When to consider using another tool

You have a plethora of applications and microservices, for which you need to manage deployment, scaling (per service/application and/or per server independent of each other), orchestration, load balancing and service distribution across multiple servers,
it sounds like you may benefit from using one of the many orchestration tools available.

---

- [Index](/hx-deploy-tool/docs/index)
- [Developers Guide/Source code](https://github.com/helix-collective/hx-deploy-tool)
