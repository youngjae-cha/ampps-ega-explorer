#!/bin/zsh
set -eu
APP_DIR="${0:A:h}"
if ! command -v Rscript >/dev/null 2>&1; then
  print 'R is required. Install R, then run install_dependencies.R.'
  read -r '?Press Return to close.'
  exit 1
fi
# A second launch opens the running Explorer instead of starting a second server.
APP_PORT="${AMPPS_PORT:-7860}"
APP_URL="http://127.0.0.1:${APP_PORT}"
if command -v curl >/dev/null 2>&1 && curl --silent --max-time 2 "$APP_URL" | /usr/bin/grep -q 'EGA Explorer'; then
  open "$APP_URL"
  exit 0
fi
export AMPPS_OPEN_BROWSER=1
exec Rscript "$APP_DIR/run_app.R"
