FROM weld/fedora:24

COPY entrypoint.sh /usr/local/bin/entrypoint.sh
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]

COPY import /usr/local/bin
COPY schema.sql /root/schema.sql
