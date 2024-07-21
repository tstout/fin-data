#!/bin/bash

clojure -J-Dcom.sun.management.jmxremote \
-J-Dcom.sun.management.jmxremote.port=8004 \
-J-Dcom.sun.management.jmxremote.authenticate=false \
-J-Dcom.sun.management.jmxremote.ssl=false \
-J-Djava.rmi.server.hostname=stout-pi4.local \
-M:fin-data server
