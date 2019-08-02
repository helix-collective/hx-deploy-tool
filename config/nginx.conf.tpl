user  nginx;
worker_processes  1;

error_log  /dev/stderr;
pid        /var/run/nginx.pid;

events {
worker_connections  1024;
}

http {
  include       /etc/nginx/mime.types;
  default_type  application/octet-stream;

  access_log  /dev/stdout;
  error_log   /dev/stderr;

  sendfile        on;
  server_names_hash_bucket_size 128;

  keepalive_timeout  65;
  client_max_body_size 0;

  proxy_buffering on;
  proxy_temp_path proxy_temp 1 2;
  proxy_http_version 1.1;

  charset utf-8;

{{#healthCheck}}
  # Redirect AWS health checks to the first configured endpoint
  server {
    listen 80 default_server;
    location {{incomingPath}} {
      proxy_set_header Host $host;
      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
      proxy_pass http://localhost:{{outgoingPort}}{{outgoingPath}};
    }
  }
{{/healthCheck}}

{{#endPoints}}
{{#http}}
{{#port}}
  server {
    listen 80;
    server_name {{serverNames}};
    location / {
      proxy_set_header Host $host;
      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
      proxy_pass http://localhost:{{port}};
      proxy_send_timeout          300;
      proxy_read_timeout          300;
    }
  }
{{/port}}
{{^port}}
  server {
    listen 80;
    server_name {{serverNames}};
    return 503;
  }
{{/port}}
{{/http}}
{{#https}}
{{#port}}
  server {
    listen 80;
    server_name {{serverNames}};
    location '/.well-known/acme-challenge' {
        default_type "text/plain";
        alias {{letsencryptWwwDir}}/.well-known/acme-challenge;
    }
    location / {
      return 301 https://$server_name$request_uri;
    }
  }
  server {
    listen       443 ssl;
    server_name {{serverNames}};
    ssl_certificate {{sslCertPath}};
    ssl_certificate_key {{sslCertKeyPath}};
    location / {
      proxy_set_header Host $host;
      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
      proxy_pass http://localhost:{{port}};
      proxy_send_timeout          300;
      proxy_read_timeout          300;
    }
  }
{{/port}}
{{^port}}
  server {
    listen 80;
    server_name {{serverNames}};
    location '/.well-known/acme-challenge' {
        default_type "text/plain";
        alias {{letsencryptWwwDir}}/.well-known/acme-challenge;
    }
    location / {
      return 503;
    }
  }
{{#sslCertPath}}
  server {
    listen 443;
    server_name {{serverNames}};
    ssl_certificate {{sslCertPath}};
    ssl_certificate_key {{sslCertKeyPath}};
    return 503;
  }
{{/sslCertPath}}
{{/port}}
{{/https}}
{{/endPoints}}

}
