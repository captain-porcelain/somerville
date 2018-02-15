#!/usr/bin/env fish

# xvfb-run --auto-servernum  --server-args="-screen 0, 640x480x24" 

clj -C:hibiscus -R:hibiscus -m hibiscus.core
