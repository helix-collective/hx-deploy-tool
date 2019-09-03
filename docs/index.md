# Index

## What is camus2

Camus2 (pronounced cahmu tu) is a small yet versatile tool to automate the annoying bits of deployment, making it more likely engineers do deployments right(tm) and make deployments a joy.
Can work with any deployment scripts ('bare metal') and designed to work well with docker in aws.
It pulls config and release zip files from various sources (eg. local or S3), unpacks into versioned directories, and runs commands to start, stop, and expose them as required.

This user guide is aimed at end users who would like to use [camus2](https://github.com/helix-collective/hx-deploy-tool/releases) to manage their deployments with a simple tool rather than authoring and maintaining scripts.

If you would like to read developer documentation and source code, you can find it [here](https://github.com/helix-collective/hx-deploy-tool)

Below are some useful articles to get you started:

<p>
<ul>
  {% for page in site.pages %}
    {% if page.url != ('/assets/css/style.css' or '/docs/') %}
    <li>
      <a href="/hx-deploy-tool{{ page.url }}">{{ page.title }}</a>
    </li>
    {% endif %}
  {% endfor %}
</ul>
</p>


as used by:
![Praxis Helix](https://www.helixta.com.au/assets/images/praxis-helix-logo.png)
---
