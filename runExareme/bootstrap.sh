#!/usr/bin/env bash

MASTER_FLAG="master"
IP=$(wget http://ipinfo.io/ip -qO -)
CONSULPORT=":8500"
CONSULURL=$IP$CONSULPORT
EXAREME_HOME=$(pwd)
NODE_NAME=$(hostname)
EXAREME_ACTIVE_WORKERS_PATH="active_workers"
EXAREME_MASTER_PATH="master"

mkdir -p  /tmp/demo/db/
if [ -z ${CONSULURL} ]; then echo "CONSULURL is unset"; exit; fi

#todo think what will happen with cluster
#service ssh restart
echo "Waiting for Consul to be initialized...."
while [ "$(curl -s ${CONSULURL}/v1/health/state/passing | jq -r '.[].Status')" != "passing" ]; do	#wait until CONSUL is up and running
    sleep 2
done

#todo what happens with datasets
#echo '*/15  *  *  *  *    /root/exareme/set-local-datasets.sh' >> /etc/crontabs/root
#crond

if [ "$MASTER_FLAG" != "master" ]; then #this is a worker
    DESC="exareme-worker"
    echo -n $NODE_NAME > $EXAREME_HOME/etc/exareme/name
    while [ "$(curl -o -i -s -w "%{http_code}\n" ${CONSULURL}/v1/kv/${EXAREME_MASTER_PATH}/?keys)" != "200" ]; do
        echo "Waiting for master node to be initialized...."
        sleep 2
    done
    #todo what happens with datasets
    #. /root/exareme/set-local-datasets.sh
    curl -s $CONSULURL/v1/kv/$EXAREME_MASTER_PATH/$(curl -s $CONSULURL/v1/kv/$EXAREME_MASTER_PATH/?keys | jq -r '.[]' | sed "s/$EXAREME_MASTER_PATH\///g")?raw > $EXAREME_HOME/etc/exareme/master
    MASTER_NAME=$(curl -s $CONSULURL/v1/kv/$EXAREME_MASTER_PATH/?keys | jq -r '.[]' | sed "s/$EXAREME_MASTER_PATH\///g")
    SH=$(cat  $EXAREME_HOME/etc/exareme/master)
    SPACE=' '
    $EXAREME_HOME/bin/start-worker.sh
    if [ "$(curl -o -i -s -w "%{http_code}\n" ${CONSULURL}/v1/kv/${EXAREME_ACTIVE_WORKERS_PATH}/{$NODE_NAME}?keys)" = "200" ]; then
        ssh -oStrictHostKeyChecking=no $SH """sed -i  "/`echo $NODE_NAME`/d"  $EXAREME_HOME/etc/exareme/workers; curl localhost:9091/remove/worker?IP=$IP"""       #sed -i == delete line from etc/exareme/worker
        curl -X DELETE $CONSULURL/v1/kv/$EXAREME_ACTIVE_WORKERS_PATH/$NODE_NAME
    fi
    curl -X PUT -d @- $CONSULURL/v1/kv/$EXAREME_ACTIVE_WORKERS_PATH/$NODE_NAME <<< $IP
    echo $IP$SPACE$NODE_NAME | ssh -oStrictHostKeyChecking=no $SH "cat >>  $EXAREME_HOME/etc/exareme/workers"     #write workers's IP into master's worker file
    while [ ! -f /tmp/exareme/var/log/$DESC.log ]; do
        echo "Trying to connect to master with IP "$SH" and NAME "$MASTER_NAME"."
        sleep 2             #catch log file, match error "unable to connect to master re-run start-worker.sh
    done

    tail -f /tmp/exareme/var/log/$DESC.log | while read LOGLINE
    do
        [[ "${LOGLINE}" == *"Worker node started."* ]] && pkill -P $$ tail
        echo " Waiting to establish connection with master's IP "$SH" and name "$MASTER_NAME".."
        sleep 2
        if [[ "${LOGLINE}" == *"Cannot connect to"* ]]; then
            echo "Can not establish connection with master node. Is master node running? Terminating worker node "$NODE_NAME"..."
            if [ -f /tmp/exareme/var/run/*.pid ]; then
                kill -9 $( cat /tmp/exareme/var/run/*.pid)
                rm /tmp/exareme/var/run/*.pid
                echo "Stopped."
            else
                echo "Already stopped, no action taken."
            fi
            break
        fi
    done
    echo -e "\nConnected to master with IP "$SH" and name "$MASTER_NAME"."

#this is the master
else
    DESC="exareme-master"
    echo -n $NODE_NAME >  $EXAREME_HOME/etc/exareme/name
    #todo what happens with datasets
    #. /root/exareme/set-local-datasets.sh
    echo $IP > $EXAREME_HOME/etc/exareme/master
    #Master re-booted
    if [ "$(curl -o -i -s -w "%{http_code}\n" ${CONSULURL}/v1/kv/${EXAREME_MASTER_PATH}/?keys)" = "200" ]; then
        if [ "$(curl -o -i -s -w "%{http_code}\n" ${CONSULURL}/v1/kv/${EXAREME_ACTIVE_WORKERS_PATH}/?keys)" = "200" ]; then  #workers connected to him
            for i in `curl -s $CONSULURL/v1/kv/${EXAREME_ACTIVE_WORKERS_PATH}/?keys | jq -r '.[]' | sed "s/${EXAREME_ACTIVE_WORKERS_PATH}\///g"` ; do
                IP=$(curl -s $CONSULURL/v1/kv/${EXAREME_ACTIVE_WORKERS_PATH}/$i?raw)
                SPACE=' '
                echo $IP$SPACE$i >>  $EXAREME_HOME/etc/exareme/workers
                ssh -oStrictHostKeyChecking=no $IP date
            done
            $(pwd)/bin/exareme-admin.sh --stop
            $(pwd)/bin/exareme-admin.sh --update
            sleep 2
            $(pwd)/bin/exareme-admin.sh --start
        else    #no workers connected to him
            $(pwd)/bin/exareme-admin.sh --stop
            sleep 2
            $(pwd)/bin/exareme-admin.sh --start
            echo "Master node with IP "$IP" and NAME " $NODE_NAME" trying to re-boot..."
            while [ ! -f /tmp/exareme/var/log/$DESC.log ]; do
                echo "Master node with IP "$IP" and NAME " $NODE_NAME" re-booted..."
            done
        fi
    #Master just created
    else
        $(pwd)/bin/exareme-admin.sh --start --local

        echo "Initializing master node with IP "$IP" and NAME " $NODE_NAME"..."
        while [ ! -f /tmp/exareme/var/log/$DESC.log ]; do
           echo "Initializing master node with IP "$IP" and NAME " $NODE_NAME"..."
        done
    fi
    curl -X PUT -d @- $CONSULURL/v1/kv/$EXAREME_MASTER_PATH/$NODE_NAME <<< $IP
fi

#todo is this necessary? If yes, when?
#Running something in foreground, otherwise the container will stop
#while true
#do
  #tail -f /tmp/exareme/var/log/$DESC.log & wait ${!}
#done
