# Welcome

This user guide is aimed at end users who would like to use [hx-deploy-tool](https://github.com/helix-collective/hx-deploy-tool/releases) to manage their deployments with a simple tool rather than authoring and maintaining scripts
If you would like to read developer documentation and source code, you can find it [here](https://helix-collective.github.io/hx-deploy-tool/)

If you are looking for a tl/dr cookbook example using the tool, follow the link to our [example page]()
or, jump to our userguide [Index]()

## What is hx-deploy-tool

A small yet versatile tool to automate the annoying bits of deployment, making it more likely engineers do deployments right(tm) and make deployments a joy.
Can work with any deployment scripts ('bare metal') and designed to work well with docker in aws.
It pulls config and release zip files from various sources (eg. local or S3), unpacks into versioned directories, and runs commands to start, stop, and expose them as required.

## Why use hx-deploy-tool

There is a gap between running software on a single server, and having large container clusters and using various tools for container orhestration. This tool plugs that gap, and can provide consistency and reliability in deployments until you have grown to a place where you need what container orchestration tools provide.

## When to consider using another tool

If you are looking for a single tool to manage deployment, scaling, orchestration, load balancing... and more, it sounds like you ay benefit from using one of the many orchestration tools available.
//todo, insert more.

---

- [Index]()
- [Developers Guide/Source code](https://helix-collective.github.io/hx-deploy-tool/)
