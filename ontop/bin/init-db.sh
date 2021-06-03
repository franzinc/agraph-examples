#!/bin/bash
set -e

ls -1 /tmp |
	grep "sql" |
	xargs -P 0 -d '\n' -I{} sh -c "psql -d \"$POSTGRES_DB\" -U \"$POSTGRES_USER\" -a -q -f {}"
